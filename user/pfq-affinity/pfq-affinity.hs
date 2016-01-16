--
-- Copyright (c) 2015 Nicola Bonelli <nicola@pfq.io>
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

import Data.Maybe
import Data.List(isInfixOf)
import Data.List.Split(splitOneOf)
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
    {   firstcore :: Int
    ,   exclude   :: [Int]
    ,   algorithm :: String
    ,   msitype   :: Maybe MSI
    ,   devices   :: [Device]
    } deriving (Data, Typeable, Show)


type BindStateT = StateT (Options, Int)


-- default options
--

options :: Mode (CmdArgs Options)
options = cmdArgsMode $ Options
    {   firstcore = 0       &= typ "CORE" &= help "first core involved"
    ,   exclude   = []      &= typ "CORE" &= help "exclude core from binding"
    ,   algorithm = ""      &= help "binding algorithm: round-robin, multiple/n, even, odd, all-in:id, step:id, custom:step/multi"
    ,   msitype   = Nothing &= typ "MSI" &= help "MSI type: TxRx, Rx, Tx or None"
    ,   devices   = []      &= args
    } &= summary "pfq-affinity: advanced Linux interrupt affinity binding." &= program "pfq-affinity"


-- binding algorithm
--

data IrqBinding = IrqBinding
    {   firstCore :: Int
    ,   stepCore  :: Int
    ,   multi     :: Int
    ,   runFilter :: Int -> Bool
    }

makeIrqBinding :: [String] -> Int -> IrqBinding
makeIrqBinding ["round-robin"]    first  = IrqBinding (-1)      1       1       none
makeIrqBinding ["naive"]          first  = IrqBinding first     1       1       none
makeIrqBinding ["multiple", m]    first  = IrqBinding first     1    (read m)   none
makeIrqBinding ["even"]           first  = IrqBinding first     1       1       even
makeIrqBinding ["odd"]            first  = IrqBinding first     1       1       odd
makeIrqBinding ["all-in", n]      _      = IrqBinding (read n)  0       1       none
makeIrqBinding ["step",   s]      first  = IrqBinding first   (read s)  1       none
makeIrqBinding ["custom", s, m]   first  = IrqBinding first   (read s) (read m) none

makeIrqBinding _ _                      = error "PFQ: unknown IRQ binding algorithm"


none :: a -> Bool
none _ = True


-- main:

main :: IO ()
main = cmdArgsRun options >>= \ops -> do
    let apply = forM_ (devices ops) $ \dev -> dispatch (algorithm ops) dev
    evalStateT apply (ops, firstcore ops)


-- dispatch command:
--

dispatch :: String -> String -> BindStateT IO ()
dispatch "" = showBinding
dispatch _  = makeBinding


-- makeBinding algorithm:
--

makeBinding :: String -> BindStateT IO ()
makeBinding dev = do
    (op,start) <- get
    let msi  = msitype op
        alg  = makeIrqBinding (splitOneOf ":/" $ algorithm op) (firstcore op)
        irq  = getInterrupts dev msi
        core = mkBinding dev (exclude op) start alg msi
    lift $ do
        putStrLn $ "Setting binding for device " ++ dev ++
            case msi of { Nothing -> ":"; Just None -> "(none):"; _ -> " (" ++ show msi ++ "):" }
        when (null irq) $ error $ "PFQ: irq(s) not found for dev " ++ dev ++ "!"
        when (null core) $ error "PFQ: no eligible cores found!"
        mapM_ setIrqAffinity $ zip irq core
    put (op, last core + 1)


-- show current binding:
--

showBinding :: String -> BindStateT IO ()
showBinding dev = do
    (op,_) <- get
    let msi = msitype op
        irq = getInterrupts dev msi
    lift $ do
        putStrLn $ "Binding for device " ++ dev ++
            case msi of { Nothing -> ":"; Just None -> "(none):";  _ -> " (" ++ show (fromJust msi) ++ "):" }
        when (null irq) $ error $ "PFQ: irq vector not found for dev " ++ dev ++ "!"
        forM_ irq $ \n ->
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

mkBinding :: Device -> [Int] -> Int -> IrqBinding -> Maybe MSI -> [Int]
mkBinding _   _  _ _ Nothing = error "PFQ: to create IRQ bindings you need to specify the MSI type"
mkBinding dev excl f (IrqBinding f' step multi filt) msi =
    take nqueue [ n | let f''= if f' == -1 then f else f',
                      x <- [f'', f''+ step .. ] >>= replicate multi,
                      let n = x `mod` getNumberOfPhyCores,
                      filt n,
                      n `notElem` excl ]
        where nqueue = getNumberOfQueues dev msi


getDeviceName :: String -> Maybe MSI -> String
getDeviceName dev Nothing     = dev
getDeviceName dev (Just None) = dev
getDeviceName dev (Just msi ) = dev ++ "-" ++ show msi


-- the following actions can be unsafe IO because the files they parse are not mutable
--

getNumberOfQueues :: Device -> Maybe MSI -> Int
getNumberOfQueues dev msi = unsafePerformIO $ readFile proc_interrupt >>= \file ->
    return $ length $ filter (isInfixOf $ getDeviceName dev msi) $ lines file


getInterrupts :: Device -> Maybe MSI -> [Int]
getInterrupts dev msi = unsafePerformIO $ readFile proc_interrupt >>= \file ->
    return $ map (read . takeWhile (/= ':')) $ filter (isInfixOf $ getDeviceName dev msi) $ lines file


{-# NOINLINE getNumberOfPhyCores #-}
getNumberOfPhyCores :: Int
getNumberOfPhyCores = unsafePerformIO $ readFile proc_cpuinfo >>= \file ->
    return $ length $ filter (isInfixOf "processor") $ lines file

