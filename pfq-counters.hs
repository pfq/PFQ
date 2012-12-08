{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import qualified Network.PFq as Q

import Foreign
import System.Environment
import System.Time

import Control.Monad
import Control.Concurrent

import Data.Maybe
import Data.List.Split
import Data.Data
import Data.Typeable

import qualified Data.HashSet as HS
import System.Console.CmdArgs


data Key = Key Word32 Word32 Word16 Word16
            deriving (Eq, Show)


data State a = State { sCounter :: MVar a,
                       sFlow    :: MVar a, 
                       sSet     :: HS.HashSet Key
                     }

type Queue = Int


data Binding = Binding { devs      :: [String],
                         coreNum   :: Int,
                         groupId   :: Int,
                         queues    :: [Queue] 
                       } deriving (Eq, Show)


makeBinding :: String -> Binding
makeBinding s = case splitOn "." s of
                        ds : []         ->  Binding (splitOn ":" ds) 0 42 [-1]
                        ds : c : []     ->  Binding (splitOn ":" ds) (read c) 42 [-1]
                        ds : c : g : [] ->  Binding (splitOn ":" ds) (read c) (read g) [-1]
                        ds : c : g : qs ->  Binding (splitOn ":" ds) (read c) (read g) (map read qs)

-- Command line options 
--
data Options = Options 
               {
                steering :: Maybe String,
                caplen   :: Int,
                offset   :: Int,
                slots    :: Int,
                bindings :: [String]
               } deriving (Data, Typeable, Show)


-- default options
--
options = cmdArgsMode $ Options { steering = Nothing &= help "Steering function (ie: steer-ipv4-addr)",
                                  caplen   = 64,
                                  offset   = 0,
                                  slots    = 262144,
                                  bindings = [] &= typ "BINDING" &= help "Where BINDING = eth0:eth1:...:core[gid.queue.queue...]"
                                } &= summary "PFq multi-threaded packet counter."


-- main function
--

main :: IO ()
main = do
    opt <- cmdArgsRun options
    putStrLn $ "[pfq] " ++ show (opt)
    cs  <- runThreads opt 
    t   <- getClockTime
    dumpStat cs t


dumpStat :: (RealFrac a) => [MVar a] -> ClockTime -> IO ()
dumpStat cs t0 = do
             threadDelay 1000000
             t <- getClockTime
             cs' <- mapM (\v -> swapMVar v 0) cs
             let delta = diffUSec t t0
             let rate = (sum cs' * 1000000) / fromIntegral delta  
             putStrLn $ "Total rate pkt/sec: " ++ show ((truncate rate) :: Integer)
             dumpStat cs t


diffUSec :: ClockTime -> ClockTime -> Int
diffUSec t1 t0 = (tdSec delta * 1000000) + truncate ((fromIntegral(tdPicosec delta) / 1000000) :: Double)
                    where delta = diffClockTimes t1 t0


runThreads :: (Num a) => Options -> IO [MVar a]
runThreads opt | []     <- bindings opt = return []
runThreads opt | (b:bs) <- bindings opt = do
                 c <- newMVar 0
                 f <- newMVar 0
                 _ <- forkOn (coreNum b') (
                          do
                          fp <- Q.openNoGroup (caplen opt) (offset opt) (slots opt)
                          withForeignPtr fp  $ \q -> do
                              Q.joinGroup q (groupId b') [Q.class_default] Q.policy_shared
                              forM_ (devs b') $ \dev ->
                                forM_ (queues b') $ \queue ->
                                  Q.bindGroup q (groupId b') dev queue
                              when (isJust $ steering opt) (Q.steeringFunction q (groupId b') (fromJust $ steering opt))
                              Q.enable q 
                              recvLoop q (State c f HS.empty) >> return ()  
                          )
                 putStrLn $ "[pfq] " ++ show(b') ++ " @core " ++ show (coreNum b') ++ " started!"
                 liftM2 (:) (return c) (runThreads opt{ bindings = bs })
                where b' = makeBinding (head $ bindings opt)

recvLoop :: (Num a) => Ptr Q.PFqTag -> State a -> IO Int
recvLoop q state = do 
    netQueue <- Q.read q 10000
    case (Q.qLen netQueue) of 
        0 ->  recvLoop q state
        _ ->  do
              modifyMVar_ (sCounter state) $ \c -> return (c + fromIntegral (Q.qLen netQueue))
              recvLoop q state



