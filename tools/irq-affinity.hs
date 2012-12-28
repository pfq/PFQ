--
-- Copyright (c) 2013 Bonelli Nicola <nicola.bonelli@antifork.org>
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--

{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.List(isInfixOf)
import Data.List.Split(splitOn)
import Control.Monad.State

import Numeric(showHex,readHex)

import Data.Bits
import Data.Data

import System.Console.CmdArgs
import System.IO.Unsafe

proc_interrupt, proc_cpuinfo :: String

proc_interrupt = "/proc/interrupts"
proc_cpuinfo   = "/proc/cpuinfo"


type Device = String


data MSI =  Rx | Tx | TxRx | None 
            deriving (Data, Typeable, Eq, Read)

instance Show MSI where
    show Rx   = "rx"
    show Tx   = "tx"
    show TxRx = "TxRx"
    show None = ""

-- Command line options 
--

data Options = Options 
               {
                firstcore :: Int,
                exclude   :: [Int],
                algorithm :: String,
                msitype   :: MSI,
                devices   :: [Device]
               } deriving (Data, Typeable, Show)


type BindState = StateT (Options, Int)


-- default options
--

options :: Mode (CmdArgs Options)
options = cmdArgsMode $ Options 
                        { 
                         firstcore = 0       &= typ "CORE" &= help "first core involved",
                         exclude   = []      &= typ "CORE" &= help "exclude core from binding",
                         algorithm = ""      &= help "binding algorithm: naive, round-robin, even, odd, all-in:id, comb:id",
                         msitype   = TxRx    &= typ "MSI"  &= help "MSI type: TxRx, Rx, Tx or None",  
                         devices   = []      &= args
                        } &= summary "irq-affinity: advanced Linux interrupt affinity binding." &= program "irq-affinity"


-- binding algorithm
--

data Alg = Alg 
            {
                firstCore :: Int,
                stepCore  :: Int,
                runFilt   :: Int -> Bool
            } 

makeAlg :: [String] -> Int -> Alg
makeAlg ["naive"]         n = Alg n     1 none    
makeAlg ["round-robin"]   _ = Alg (-1)  1 none    
makeAlg ["even"]          _ = Alg (-1)  1 even    
makeAlg ["odd"]           _ = Alg (-1)  1 odd    
makeAlg ["all-in", n]     _ = Alg (read n) 0 none  
makeAlg ["comb", s]       n = Alg (n) (read s) none   
makeAlg _ _                 = error "Unknown algorithm"   


none :: a -> Bool
none _ = True


-- main:
--
 
main :: IO ()
main = cmdArgsRun options >>= \ops -> do
    let apply = forM_ (devices ops) $ \dev -> dispatch (algorithm ops) dev
    evalStateT apply (ops, (firstcore ops))  


-- dispatch command:
--

dispatch :: String -> (String -> BindState IO ())
dispatch "" = showBinding 
dispatch _  = makeBinding 


-- makeBinding algorithm:
--

makeBinding :: String -> BindState IO ()
makeBinding dev = do
    (op,start) <- get
    let msi  = msitype op
        alg  = makeAlg (splitOn ":" $ algorithm op) (firstcore op)
        irq  = getInterrupts dev msi
        core = mkBinding dev (exclude op) start alg msi  
    lift $ putStrLn $ "Setting binding for device " ++ dev ++ 
        case msi of { None -> ":"; _ -> " (" ++ show msi ++ "):" }
    lift $ when (null irq) $ error $ "irq(s) not found for dev " ++ dev ++ "!"
    lift $ when (null core) $ error "no eligible cores found!"
    lift $ mapM_ setIrqAffinity $ zip irq core
    put (op, last core + 1)


-- show current binding:
--

showBinding :: String -> BindState IO ()
showBinding dev = do
    (op,_) <- get
    let msi = msitype op
        irq = getInterrupts dev msi
    lift $ putStrLn $ "Binding for device " ++ dev ++ 
        case msi of { None -> ":";  _ -> " (" ++ show msi ++ "):" }
    lift $ when (null irq) $ error $ "irq vector not found for dev " ++ dev ++ "!"
    lift $ forM_ irq $ \n -> do
            getIrqAffinity n >>= \cs -> putStrLn $ "   irq " ++ show n ++ " -> core " ++ show cs


-- set irq affinity for the given (irq,core) pair 
--

setIrqAffinity :: (Int, Int) -> IO ()
setIrqAffinity (irq, core) = do
    putStrLn $ "   irq " ++ show irq ++ " -> core " ++ show core
    writeFile ("/proc/irq/" ++ show irq ++ "/smp_affinity") (showHex (makeCpuMask [core]) "")   


getIrqAffinity :: Int -> IO [Int]
getIrqAffinity irq = do
    s <- readFile ("/proc/irq/" ++ show irq ++ "/smp_affinity")
    return $ getCpusFromMask . fst . head $ readHex s 


makeCpuMask :: [Int] -> Int
makeCpuMask = foldr (\cpu mask -> mask .|. (1 `shiftL` cpu)) 0 


getCpusFromMask :: Int -> [Int]
getCpusFromMask mask  = [ n | n <- [0 .. 127], let p2 = 1 `shiftL` n, mask .&. p2 /= 0] 


-- given a device and a binding algorithm, create the eligible list of core
--

mkBinding :: Device -> [Int] -> Int -> Alg -> MSI -> [Int]
mkBinding dev excl f (Alg f' s filt) msi = 
    take nq [ n | let f''= if (f' == -1) then f else f', 
                      x <- [f'', f''+s .. 64], 
                      let n = x `mod` getNumberOfPhyCores, 
                      filt n, 
                      n `notElem` excl ]    
        where nq = getNumberOfQueues dev msi


getDeviceName :: String -> MSI -> String
getDeviceName dev None = dev
getDeviceName dev msi  = dev ++ "-" ++ show msi


-- the following actions can be unsafe IO because the files they parse are not mutable
--

getNumberOfQueues :: Device -> MSI -> Int
getNumberOfQueues dev msi = unsafePerformIO $ readFile proc_interrupt >>= \file -> 
    return $ length $ filter (isInfixOf $ getDeviceName dev msi) $ lines file 
    

getInterrupts :: Device -> MSI -> [Int]
getInterrupts dev msi = unsafePerformIO $ readFile proc_interrupt >>= \file ->  
    return $ map (read . takeWhile (/= ':')) $ filter (isInfixOf $ getDeviceName dev msi) $ lines file 


getNumberOfPhyCores :: Int
getNumberOfPhyCores = unsafePerformIO $ readFile proc_cpuinfo >>= \file ->  
    return $ length $ filter (isInfixOf "processor") $ lines file 

