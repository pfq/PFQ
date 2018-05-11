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
{-# LANGUAGE RecordWildCards #-}

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
import System.Environment (withArgs)
import System.Exit

bold, red, blue, reset :: String

bold  = setSGRCode [SetConsoleIntensity BoldIntensity]
red   = setSGRCode [SetColor Foreground Vivid Red]
blue  = setSGRCode [SetColor Foreground Vivid Blue]
reset = setSGRCode []


putStrBoldLn :: String -> IO ()
putStrBoldLn msg = putStrLn $ bold ++ msg ++ reset

proc_interrupt, proc_cpuinfo :: String

proc_interrupt = "/proc/interrupts"
proc_cpuinfo   = "/proc/cpuinfo"


type Device = String


data MSI =  Rx | Tx | TxRx | Other
            deriving (Data, Typeable, Eq, Read)

instance Show MSI where
    show Rx   = "rx"
    show Tx   = "tx"
    show TxRx = "TxRx"
    show Other = ""


-- Command line options
--


data Options = Options
    {   firstcpu  :: Int
    ,   exclude   :: [Int]
    ,   algorithm :: Maybe String
    ,   oneToMany :: Bool
    ,   msitype   :: Maybe MSI
    ,   showCPU   :: [Int]
    ,   bindIRQ   :: Device
    ,   arguments :: [String]
    } deriving (Data, Typeable, Show)


type BindStateT = StateT (Options, Int)
type CpuMask = Integer

-- default options
--

options :: Mode (CmdArgs Options)
options = cmdArgsMode $ Options
    {   firstcpu  = 0       &= typ "CPU"   &= help "First cpu involved"
    ,   exclude   = []      &= typ "CPU"   &= help "Exclude cpu from binding"
    ,   algorithm = Nothing                &= help "Algorithm: round-robin, naive, multiple/n, raster/n, even, odd, any, all-in:id, step:id, custom:step/multi"
    ,   oneToMany = False &= explicit &= name "one-to-many" &= help "Bind each IRQ to every eligible CPU. Note: by default irq affinity is set one-to-one"
    ,   msitype   = Nothing &= typ "MSI"   &= help "MSI type: TxRx, Rx, Tx or Other"
    ,   showCPU   = []  &= explicit &= name "cpu" &= help "Display IRQs of the given CPUs"
    ,   bindIRQ   = def &= explicit &= name "bind" &= help "Set the IRQs affinity of the given device (e.g., --bind eth0 1 2)"
    ,   arguments = []                     &= args
    } &= summary "pfq-affinity: Linux Interrupt-affinity bind tool." &= program "pfq-affinity"


-- binding algorithm
--

data IrqBinding = IrqBinding
    {   firstCpu  :: Int
    ,   stepCpu   :: Int
    ,   multi     :: Int
    ,   runFilter :: Int -> Bool
    }


makeIrqBinding :: [String] -> Int -> IrqBinding
makeIrqBinding ["round-robin"]    _      = IrqBinding (-1)      1       1       none
makeIrqBinding ["naive"]          first  = IrqBinding first     1       1       none
makeIrqBinding ["multiple", m]    _      = IrqBinding (-1)      1    (read m)   none
makeIrqBinding ["raster", m]      first  = IrqBinding first     1    (read m)   none
makeIrqBinding ["even"]           first  = IrqBinding first     1       1       even
makeIrqBinding ["odd"]            first  = IrqBinding first     1       1       odd
makeIrqBinding ["any"]            _      = IrqBinding 0         0       0       none
makeIrqBinding ["all-in", n]      _      = IrqBinding (read n)  0       1       none
makeIrqBinding ["step",   s]      first  = IrqBinding first   (read s)  1       none
makeIrqBinding ["custom", s, m]   first  = IrqBinding first   (read s) (read m) none
makeIrqBinding _ _ =  error "pfq-affinity: unknown IRQ binding algorithm"

none :: a -> Bool
none _ = True


-- main function
--

main :: IO ()
main = handle (\(ErrorCall msg) -> putStrBoldLn (red ++ msg ++ reset) *> exitWith (ExitFailure 1)) $ do
    opt' <- cmdArgsRun options
    evalStateT (runCmd opt') (opt', firstcpu opt')


-- dispatch commands
--

runCmd :: Options -> BindStateT IO ()
runCmd Options{..}
    | Just _  <- algorithm  = forM_ arguments $ \dev -> bindDevice dev
    | not $ null showCPU    = liftIO $ mapM_ showIRQ showCPU
    | not $ null bindIRQ    = runBinding bindIRQ (map read arguments)
    | not $ null arguments  = mapM_ showBinding arguments
    | otherwise             = liftIO $ withArgs ["--help"] $ void (cmdArgsRun options)


-- bindDevice
--

bindDevice :: String -> BindStateT IO ()
bindDevice dev = do
    (op,start) <- get
    let msi  = msitype op
        alg  = makeIrqBinding (splitOneOf ":/" $ fromJust (algorithm op)) (firstcpu op)
        cpus = mkEligibleCPUs dev (exclude op) start alg msi
    runBinding dev cpus


-- show IRQ affinity of a given device
--

showBinding :: String -> BindStateT IO ()
showBinding dev = do
    (op,_) <- get
    let msi = msitype op
        irq = getInterruptsByDevice dev msi
    lift $ do
        putStrLn $ "Binding for device " ++ dev ++
            case msi of { Nothing -> ":"; Just Other -> "(other):";  _ -> " (" ++ show (fromJust msi) ++ "):" }
        when (null irq) $ error $ "pfq-affinity: irq vector not found for dev " ++ dev ++ "!"
        forM_ irq $ \n ->
            getIrqAffinity n >>= \cs -> putStrLn $ "   irq " ++ show n ++ " -> CPU " ++ show cs


-- show IRQ list of a given cpu
--

showIRQ :: Int -> IO ()
showIRQ cpu = do
    irqs <- getInterrupts
    putStrLn $ "CPU " ++ show cpu ++ ":"
    mat <- forM irqs $ \n ->
        getIrqAffinity (fst n) >>= \l -> if cpu `elem` l
                                            then return $ Just (n, l)
                                            else return Nothing
    let out = map fst $ catMaybes mat
    forM_ out $ \(n,descr) ->
        putStrLn $ "  irq -> " ++ show n ++ "\t(" ++ descr ++ ")"


-- runBinding

runBinding :: String -> [Int] -> BindStateT IO ()
runBinding dev cpus = do
    (op, _) <- get

    let msi  = msitype op
        irqs = getInterruptsByDevice dev msi

    lift $ do
        putStrLn $ "Setting binding for device " ++ dev ++
            case msi of { Nothing -> ":"; Just Other -> "(other):"; _ -> " (" ++ show msi ++ "):" }
        when (null irqs) $ error ("pfq-affinity: IRQs not found for the " ++ dev ++ " device!")
        when (null cpus) $ error "pfq-affinity: No eligible cpu found!"

        let doBind = if oneToMany op
                        then bindOneToMany
                        else bindOneToOne
        doBind irqs cpus

    put (op, last cpus + 1)


bindOneToOne :: [Int] -> [Int] -> IO ()
bindOneToOne irqs cpus = forM_ (zip irqs cpus) $ \(irq,cpu) -> setIrqAffinity irq [cpu]


bindOneToMany :: [Int] -> [Int] -> IO ()
bindOneToMany irqs cpus = forM_ irqs $ \irq -> setIrqAffinity irq cpus


-- set IRQ affinity for the given (irq,cpu) pair

setIrqAffinity :: Int -> [Int] -> IO ()
setIrqAffinity irq cpus = do
    putStrLn $ "   irq " ++ show irq ++ " -> CPU " ++ show cpus ++ " {mask = " ++ mask' ++ "}"
    writeFile ("/proc/irq/" ++ show irq ++ "/smp_affinity") mask'
      where mask' = showMask $ makeCpuMask cpus



-- get IRQ affinity for the given irq

getIrqAffinity :: Int -> IO [Int]
getIrqAffinity irq =
    getCpusListFromMask . readMask <$> readFile ("/proc/irq/" ++ show irq ++ "/smp_affinity")


-- utilities
--

intersperseEvery :: Int -> t -> [t] -> [t]
intersperseEvery n x xs = zip xs [1 .. l] >>= ins
  where ins (x',k) = if k `mod` n == 0 && k /= l then [x',x] else [x']
        l = length xs


showMask :: CpuMask -> String
showMask mask' = reverse . intersperseEvery 8 ',' . reverse $ showHex mask' ""


readMask :: String -> CpuMask
readMask = fst . head . readHex . filter (/= ',')


makeCpuMask :: [Int] -> CpuMask
makeCpuMask = foldr (\cpu mask' -> mask' .|. (1 `shiftL` cpu)) (0 :: CpuMask)


getCpusListFromMask :: CpuMask -> [Int]
getCpusListFromMask mask'  = [ n | n <- [0 .. 4095], let p2 = 1 `shiftL` n, mask' .&. p2 /= 0 ]


-- given a device and a bind-algorithm, create the list of eligible cpu
--

mkEligibleCPUs :: Device -> [Int] -> Int -> IrqBinding -> Maybe MSI -> [Int]
mkEligibleCPUs _   _  _ _ Nothing = error "pfq-affinity: to create IRQ bindings you must specify the MSI type"
mkEligibleCPUs _ excl _ (IrqBinding 0 0 0 _) _ = [ n | n <- [0 .. getNumberOfPhyCores-1], n `notElem` excl ]
mkEligibleCPUs dev excl f (IrqBinding f' step multi' filt) msi =
    take nqueue [ n | let f''= if f' == -1 then f else f',
                      x <- [f'', f''+ step .. ] >>= replicate multi',  -- make the list of eligible CPUs
                      let n = x `mod` getNumberOfPhyCores,             -- modulo number of max CPUs
                      filt n,                                          -- whose elements pass the given predicate
                      n `notElem` excl ]                               -- and are not prensent in the exclusion list
        where nqueue = getNumberOfQueues dev msi



getDeviceName :: String -> Maybe MSI -> String
getDeviceName dev Nothing      = dev
getDeviceName dev (Just Other) = dev
getDeviceName dev (Just msi )  = dev ++ "-" ++ show msi


-- the following actions can be unsafe IO because the files they parse are not mutable
--

getNumberOfQueues :: Device -> Maybe MSI -> Int
getNumberOfQueues dev msi = unsafePerformIO $ readFile proc_interrupt >>= \file ->
    return $ length $ filter (getDeviceName dev msi `isInfixOf`) $ lines file


getInterruptsByDevice :: Device -> Maybe MSI -> [Int]
getInterruptsByDevice dev msi = unsafePerformIO $ readFile proc_interrupt >>= \file ->
    return $ map (read . takeWhile (/= ':')) $ filter (getDeviceName dev msi `isInfixOf`) $ lines file


getInterrupts :: IO [(Int, String)]
getInterrupts = readFile proc_interrupt >>= \file ->
    return $ map (\(i,l) -> (i, last . words $ l)) (concatMap reads $ lines file  :: [(Int, String)])


{-# NOINLINE getNumberOfPhyCores #-}
getNumberOfPhyCores :: Int
getNumberOfPhyCores = unsafePerformIO $ readFile proc_cpuinfo >>= \file ->
    return $ length $ filter ("processor" `isInfixOf`) $ lines file

