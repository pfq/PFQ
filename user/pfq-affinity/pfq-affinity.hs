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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}

module Main where

import Data.Maybe
import Data.List(nub, sort)
import Data.List.Split(splitOneOf)
import Control.Monad.State
import Control.Exception
import Text.Regex.Posix

import Numeric(showHex,readHex)

import Text.Printf
import Data.Bits
import Data.Data
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Console.ANSI
import System.Console.CmdArgs
import System.IO.Unsafe
import System.Environment (withArgs)
import System.Exit

bold, red, blue, green, reset :: T.Text

bold  = T.pack $ setSGRCode [SetConsoleIntensity BoldIntensity]
red   = T.pack $ setSGRCode [SetColor Foreground Vivid Red]
blue  = T.pack $ setSGRCode [SetColor Foreground Vivid Blue]
green = T.pack $ setSGRCode [SetColor Foreground Vivid Green]
reset = T.pack $ setSGRCode []


putStrBoldLn :: T.Text -> IO ()
putStrBoldLn msg = T.putStrLn $ bold <> msg <> reset

proc_interrupt, proc_cpuinfo, proc_irq :: String

proc_irq = "/proc/irq/"
proc_interrupt = "/proc/interrupts"
proc_cpuinfo   = "/proc/cpuinfo"


type Device = String


-- Command line options
--

data Options = Options
    {   firstcpu    :: Int
    ,   exclude     :: [Int]
    ,   algorithm   :: Maybe String
    ,   oneToMany   :: Bool
    ,   showCPU     :: [Int]
    ,   showAllCPUs :: Bool
    ,   bindIRQ     :: Device
    ,   arguments   :: [String]
    } deriving (Data, Typeable, Show)


type BindStateT = StateT (Options, Int)
type CpuMask = Integer

-- default options
--

options :: Mode (CmdArgs Options)
options = cmdArgsMode $ Options
    {   firstcpu    = 0       &= typ "CPU"   &= help "First cpu involved"
    ,   exclude     = []      &= typ "CPU"   &= help "Exclude cpu from binding"
    ,   algorithm   = Nothing                &= help "Algorithm: round-robin, naive, multiple/n, raster/n, even, odd, any, all-in:id, step:id, custom:step/multi"
    ,   oneToMany   = False   &= explicit    &= name "one-to-many" &= help "Bind each IRQ to every eligible CPU. Note: by default irq affinity is set one-to-one"
    ,   showCPU     = []      &= explicit    &= name "cpu"  &= help "Display IRQs of the given CPUs set"
    ,   showAllCPUs = False                  &= help "Display IRQs for all CPUs avaialables"
    ,   bindIRQ     = def     &= explicit    &= name "bind" &= help "Set the IRQs affinity of the given device (e.g., --bind eth0 1 2)"
    ,   arguments   = []                     &= args
    } &= summary "pfq-affinity: Linux Interrupt-Affinity bind tool." &= program "pfq-affinity"


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
main = handle (\(ErrorCall msg) -> putStrBoldLn (red <> T.pack msg <> reset) *> exitWith (ExitFailure 1)) $ do
    opt' <- cmdArgsRun options
    evalStateT (runCmd opt') (opt', firstcpu opt')


-- dispatch commands
--

runCmd :: Options -> BindStateT IO ()
runCmd Options{..}
    | Just _  <- algorithm  = forM_ arguments $ \dev -> bindDevice dev
    | showAllCPUs           = liftIO $ showAllCpuIRQs (T.pack <$> arguments)
    | not $ null showCPU    = liftIO $ mapM_ (showIRQ (T.pack <$> arguments)) showCPU
    | not $ null bindIRQ    = runBinding bindIRQ (map read arguments)
    | not $ null arguments  = mapM_ showBinding arguments
    | otherwise             = liftIO $ withArgs ["--help"] $ void (cmdArgsRun options)


-- bindDevice
--

bindDevice :: String -> BindStateT IO ()
bindDevice dev = do
    (op,start) <- get
    let alg  = makeIrqBinding (splitOneOf ":/" $ fromJust (algorithm op)) (firstcpu op)
        cpus = mkEligibleCPUs dev (exclude op) start alg
    runBinding dev cpus


-- show IRQ affinity of a given device
--


showBinding :: String -> BindStateT IO ()
showBinding dev = do
    -- (op,_) <- get
    let irq = getInterruptsByDevice dev

    lift $ do
        let cpus = nub . sort . concat $ getIrqAffinity . fst <$> irq

        putStrLn $ "IRQ binding for device " <> dev <> " on cpu "  <> show cpus <> " (" <>  show (length irq) <> " irqs): "

        when (null irq) $
            error $ "pfq-affinity: irq vector not found for dev " <> dev <> "!"

        forM_ irq $ \(n,descr) -> do
            let cs = getIrqAffinity n
            printf "  irq %s%d%s:%s%s%s -> cpu %v\n" red  n reset green descr reset (show cs)


-- show cpus irq map
--

showAllCpuIRQs ::[T.Text] -> IO ()
showAllCpuIRQs filts = do
    let irqs = getInterrupts
    forM_ [0..getNumberOfPhyCores-1] $ \cpu -> do
        mat <- forM irqs $ \n -> do
                let cs = getIrqAffinity (fst n)
                if cpu `elem` cs
                    then return $ Just (n, cs)
                    else return Nothing
        putStr $ "  cpu " <> show cpu <> " -> "
        forM_ (fst <$> catMaybes mat) $ \(n,descr) -> do
            let xs  = fmap (T.unpack descr =~) (T.unpack <$> filts) :: [Bool]
            when (or xs || null filts) $
                printf "%s%d%s:%s%s%s " red n reset green descr reset
        T.putStr "\n"

-- show IRQ list of a given cpu
--

showIRQ :: [T.Text] -> Int -> IO ()
showIRQ filts cpu = do
    let irqs = getInterrupts
    putStrLn $ "CPU " <> show cpu <> ":"

    mat <- forM irqs $ \n -> do
        let cs = getIrqAffinity (fst n)
        if cpu `elem` cs
            then return $ Just (n, cs)
            else return Nothing

    let out = map fst $ catMaybes mat
    forM_ out $ \(n,descr) -> do
        let xs  = fmap (T.unpack descr =~) (T.unpack <$> filts) :: [Bool]
        when (or xs || null filts) $
            printf "  irq %s%d%s:%s%s%s\n" red n reset green descr reset

-- runBinding

runBinding :: String -> [Int] -> BindStateT IO ()
runBinding dev cpus = do
    (op, _) <- get

    let irqs = fst <$> getInterruptsByDevice dev

    lift $ do
        putStrLn $ "Setting binding for device " <> dev <>":"
        when (null irqs) $ error ("pfq-affinity: IRQs not found for the " <> dev <> " device!")
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
    putStrLn $ "  irq " <> show irq <> " -> CPU " <> show cpus <> " {mask = " <> mask' <> "}"
    writeFile (proc_irq <> show irq <> "/smp_affinity") mask'
      where mask' = showMask $ makeCpuMask cpus

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

mkEligibleCPUs :: Device -> [Int] -> Int -> IrqBinding -> [Int]
mkEligibleCPUs _ excl _ (IrqBinding 0 0 0 _) = [ n | n <- [0 .. getNumberOfPhyCores-1], n `notElem` excl ]
mkEligibleCPUs dev excl f (IrqBinding f' step multi' filt) =
    take nqueue [ n | let f''= if f' == -1 then f else f',
                      x <- [f'', f''+ step .. ] >>= replicate multi',  -- make the list of eligible CPUs
                      let n = x `mod` getNumberOfPhyCores,             -- modulo number of max CPUs
                      filt n,                                          -- whose elements pass the given predicate
                      n `notElem` excl ]                               -- and are not prensent in the exclusion list
        where nqueue = getNumberOfQueues dev


-- the following actions can be unsafe IO because the files they parse are immutable

-- get IRQ affinity, that is the list of the CPUs the given irq is bound to

{-# NOINLINE getIrqAffinity #-}
getIrqAffinity :: Int -> [Int]
getIrqAffinity irq =  unsafePerformIO $
    getCpusListFromMask . readMask <$> readFile (proc_irq <> show irq <> "/smp_affinity")


{-# NOINLINE getInterrupts #-}
getInterrupts :: [(Int, T.Text)]
getInterrupts = unsafePerformIO $ readFile proc_interrupt >>= \file ->
    return $ map (\(i,l) -> (i, T.pack . last . words $ l)) (concatMap reads $ lines file :: [(Int, String)])


{-# NOINLINE getNumberOfQueues #-}
getNumberOfQueues :: Device -> Int
getNumberOfQueues dev = unsafePerformIO $ T.readFile proc_interrupt >>= \file ->
    return $ length $ filter (=~ dev) $ (lines . T.unpack) file


{-# NOINLINE getInterruptsByDevice #-}
getInterruptsByDevice :: Device -> [(Int, T.Text)]
getInterruptsByDevice dev = unsafePerformIO $ readFile proc_interrupt >>= \file ->
    return $ map (\l -> (read . T.unpack $ T.takeWhile (/= ':') l, last . T.words $ l)) $ T.pack <$> filter (=~ dev) (lines file)


{-# NOINLINE getNumberOfPhyCores #-}
getNumberOfPhyCores :: Int
getNumberOfPhyCores = unsafePerformIO $ T.readFile proc_cpuinfo >>= \file ->
    return $ length $ filter ("processor" `T.isInfixOf`) $ T.lines file

