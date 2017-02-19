--
-- Copyright (c) 2015-16 Nicola Bonelli <nicola@pfq.io>
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
import Control.Exception

import Numeric(showHex,readHex)

import Data.Bits
import Data.Data

import System.Console.ANSI
import System.Console.CmdArgs
import System.IO.Unsafe
import System.IO.Error
import System.Exit

proc_interrupt, proc_cpuinfo :: String


bold  = setSGRCode [SetConsoleIntensity BoldIntensity]
red   = setSGRCode [SetColor Foreground Vivid Red]
blue  = setSGRCode [SetColor Foreground Vivid Blue]
reset = setSGRCode []


putStrBoldLn :: String -> IO ()
putStrBoldLn msg = putStrLn $ bold ++ msg ++ reset


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
    ,   algorithm :: Maybe String
    ,   msitype   :: Maybe MSI
    ,   core      :: Bool
    ,   arguments :: [Device]
    } deriving (Data, Typeable, Show)


type BindStateT = StateT (Options, Int)

type CpuMask = Integer

-- default options
--

options :: Mode (CmdArgs Options)
options = cmdArgsMode $ Options
    {   firstcore = 0       &= typ "CORE" &= help "First core involved"
    ,   exclude   = []      &= typ "CORE" &= help "Exclude core from binding"
    ,   algorithm = Nothing               &= help "Binding algorithm: round-robin, naive, multiple/n, raster/n, even, odd, all-in:id, step:id, custom:step/multi"
    ,   msitype   = Nothing &= typ "MSI"  &= help "MSI type: TxRx, Rx, Tx or None"
    ,   core      = False                 &= help "Inspect cores irq"
    ,   arguments = []                    &= args
    } &= summary "pfq-affinity: Linux advanced interrupt-affinity binding." &= program "pfq-affinity"


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
makeIrqBinding ["multiple", m]    first  = IrqBinding (-1)      1    (read m)   none
makeIrqBinding ["raster", m]      first  = IrqBinding first     1    (read m)   none
makeIrqBinding ["even"]           first  = IrqBinding first     1       1       even
makeIrqBinding ["odd"]            first  = IrqBinding first     1       1       odd
makeIrqBinding ["all-in", n]      _      = IrqBinding (read n)  0       1       none
makeIrqBinding ["step",   s]      first  = IrqBinding first   (read s)  1       none
makeIrqBinding ["custom", s, m]   first  = IrqBinding first   (read s) (read m) none
makeIrqBinding _ _ =  error "pfq-affinity: unknown IRQ binding algorithm"


none :: a -> Bool
none _ = True


-- main:
--

main :: IO ()
main = handle (\(ErrorCall msg) -> putStrBoldLn (red ++ msg ++ reset) *> exitWith (ExitFailure 1)) $ do
    ops <- cmdArgsRun options
    let runcmd = forM_ (arguments ops) $ \dev -> dispatch dev
    evalStateT runcmd (ops, firstcore ops)

-- dispatch command:
--

dispatch :: String -> BindStateT IO ()
dispatch arg = do
    (op, start) <- get
    case () of
        _ | Just _  <- algorithm op -> makeBinding arg
          | Nothing <- algorithm op -> if core op
                                            then showIRQ arg
                                            else showBinding arg

-- makeBinding algorithm:
--

makeBinding :: String -> BindStateT IO ()
makeBinding dev = do
    (op,start) <- get
    let msi  = msitype op
        alg  = makeIrqBinding (splitOneOf ":/" $ fromJust (algorithm op)) (firstcore op)
        irq  = getInterruptsByDevice dev msi
        core = mkBinding dev (exclude op) start alg msi
    lift $ do
        putStrLn $ "Setting binding for device " ++ dev ++
            case msi of { Nothing -> ":"; Just None -> "(none):"; _ -> " (" ++ show msi ++ "):" }
        when (null irq) $ error ("pfq-affinity: irq(s) not found for dev " ++ dev ++ "!")
        when (null core) $ error "pfq-affinity: no eligible cores found!"
        mapM_ setIrqAffinity $ zip irq core
    put (op, last core + 1)

-- show current binding of a given device:
--

showBinding :: String -> BindStateT IO ()
showBinding dev = do
    (op,_) <- get
    let msi = msitype op
        irq = getInterruptsByDevice dev msi
    lift $ do
        putStrLn $ "Binding for device " ++ dev ++
            case msi of { Nothing -> ":"; Just None -> "(none):";  _ -> " (" ++ show (fromJust msi) ++ "):" }
        when (null irq) $ error $ "pfq-affinity: irq vector not found for dev " ++ dev ++ "!"
        forM_ irq $ \n ->
            getIrqAffinity n >>= \cs -> putStrLn $ "   irq " ++ show n ++ " -> core " ++ show cs


-- show IRQ per core:
--

showIRQ :: String -> BindStateT IO ()
showIRQ core = do
    (op,_) <- get
    let irq = getInterrupts
    lift $ do
        putStrLn $ "Core " ++ core ++ ":"
        mat <- forM irq $ \n ->
            getIrqAffinity (read $ fst n) >>= \l -> if read core `elem` l
                                                    then return $ Just (n, l)
                                                    else return Nothing
        let out = map fst $ catMaybes mat
        forM_ out $ \(n,descr) ->
            putStrLn $ "  irq -> " ++ n ++ "\t(" ++ descr ++ ")"


-- set irq affinity for the given (irq,core) pair
--

setIrqAffinity :: (Int, Int) -> IO ()
setIrqAffinity (irq, core) = do
    putStrLn $ "   irq " ++ show irq ++ " -> core " ++ show core ++ " {mask = " ++ mask ++ "}"
    writeFile ("/proc/irq/" ++ show irq ++ "/smp_affinity") mask
      where mask = showMask $ makeCpuMask [core]


getIrqAffinity :: Int -> IO [Int]
getIrqAffinity irq =
    (getCpusListFromMask . readMask) <$> readFile ("/proc/irq/" ++ show irq ++ "/smp_affinity")


intersperseEvery n x xs = zip xs [1 .. l] >>= ins
  where ins (x',k) = if k `mod` n == 0 && k /= l then [x',x] else [x']
        l = length xs

showMask :: CpuMask -> String
showMask mask = reverse . intersperseEvery 8 ',' . reverse $ showHex mask ""


readMask :: String -> CpuMask
readMask = fst . head . readHex . filter (/= ',')


makeCpuMask :: [Int] -> CpuMask
makeCpuMask = foldr (\cpu mask -> mask .|. (1 `shiftL` cpu)) (0 :: CpuMask)


getCpusListFromMask :: CpuMask -> [Int]
getCpusListFromMask mask  = [ n | n <- [0 .. 4095], let p2 = 1 `shiftL` n, mask .&. p2 /= 0 ]


-- given a device and a binding algorithm, create the eligible list of core
--

mkBinding :: Device -> [Int] -> Int -> IrqBinding -> Maybe MSI -> [Int]
mkBinding _   _  _ _ Nothing = error "pfq-affinity: to create IRQ bindings you must specify the MSI type"
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


getInterruptsByDevice :: Device -> Maybe MSI -> [Int]
getInterruptsByDevice dev msi = unsafePerformIO $ readFile proc_interrupt >>= \file ->
    return $ map (read . takeWhile (/= ':')) $ filter (isInfixOf $ getDeviceName dev msi) $ lines file


{-# NOINLINE getInterrupts #-}
getInterrupts :: [(String, String)]
getInterrupts = unsafePerformIO $ readFile proc_interrupt >>= \file ->
    return $ map (\(f,s) -> (show f, last . words $ s)) (concatMap reads $ lines file  :: [(Int, String)])


{-# NOINLINE getNumberOfPhyCores #-}
getNumberOfPhyCores :: Int
getNumberOfPhyCores = unsafePerformIO $ readFile proc_cpuinfo >>= \file ->
    return $ length $ filter (isInfixOf "processor") $ lines file

