{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import qualified Network.PFq as Q

import Foreign
-- import System.Environment
import System.Time

import Control.Monad
import Control.Applicative
import Control.Concurrent

import Data.Maybe
import Data.List.Split
import Data.Data
-- import Data.Typeable

import qualified Data.HashSet as HS
import qualified Data.Map as M

import System.Console.CmdArgs


data Key = Key Word32 Word32 Word16 Word16
            deriving (Eq, Show)


data State a = State { sCounter :: MVar a,
                       sFlow    :: MVar a, 
                       sSet     :: HS.HashSet Key
                     }


-- Command line options 
--
data Options = Options 
               {
                caplen   :: Int,
                offset   :: Int,
                slots    :: Int,
                steering :: [String],
                thread   :: [String]
               } deriving (Data, Typeable, Show)


-- default options
--
options = cmdArgsMode $ Options { 
                                  caplen   = 64,
                                  offset   = 0,
                                  slots    = 262144,
                                  steering = [] &= typ "FUNCTION"  &= help "Where FUNCTION = function-name[:gid] (ie: steer-ipv4-addr)",
                                  thread   = [] &= typ "BINDING" &= help "Where BINDING = eth0:...:ethx[.core[.gid[.queue.queue...]]]"
                                } &= summary "PFq multi-threaded packet counter." &= program "pfq-counters"


-- Group Options
--
        
type Queue = Int                                 
type Gid   = Int

data Binding = Binding { 
                         devs      :: [String],
                         coreNum   :: Int,
                         groupId   :: Int,
                         queues    :: [Queue] 
                       } deriving (Eq, Show)


makeBinding :: String -> Binding
makeBinding s = case splitOn "." s of
                        []              ->  error "makeBinding: empty string"
                        ds : []         ->  Binding (splitOn ":" ds) 0 42 [-1]
                        ds : c : []     ->  Binding (splitOn ":" ds) (read c) 42 [-1]
                        ds : c : g : [] ->  Binding (splitOn ":" ds) (read c) (read g) [-1]
                        ds : c : g : qs ->  Binding (splitOn ":" ds) (read c) (read g) (map read qs)


makeFun :: String -> (Gid, String)
makeFun s =  case splitOn ":" s of
                []     -> error "makeFun: empty string"
                n : [] -> (-1, n)
                n : ns -> (read $ head ns, n)
                
-- main function
--

main :: IO ()
main = do
    op <- cmdArgsRun options
    putStrLn $ "[pfq] " ++ show op
    cs  <- runThreads op (M.fromList $ map makeFun (steering op)) 
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


runThreads :: (Num a) => Options -> M.Map Gid String -> IO [MVar a]
runThreads op ms 
    | []     <- thread op = return []
    | (_:ts) <- thread op = do
        c <- newMVar 0
        f <- newMVar 0
        _ <- forkOn (coreNum binding) (
                 do
                 fp <- Q.openNoGroup (caplen op) (offset op) (slots op)
                 withForeignPtr fp  $ \q -> do
                     Q.joinGroup q (groupId binding) [Q.class_default] Q.policy_shared
                     forM_ (devs binding) $ \dev ->
                       forM_ (queues binding) $ \queue ->
                         Q.bindGroup q (groupId binding) dev queue
                     when (isJust sf) ((putStrLn $ "[pfq] Using steering " ++ fromJust sf ++ " for gid " ++ show(groupId binding) ++ "!") >>
                                       Q.steeringFunction q (groupId binding) (fromJust sf)) 
                     Q.enable q 
                     recvLoop q (State c f HS.empty) >> return ()  
                 )
        putStrLn $ "[pfq] " ++ show binding ++ " @core " ++ show (coreNum binding) ++ " started!"
        liftM2 (:) (return c) (runThreads op{ thread = ts } ms)
        where binding = makeBinding (head $ thread op)
              sf = M.lookup (groupId binding) ms <|> M.lookup (-1) ms 


recvLoop :: (Num a) => Ptr Q.PFqTag -> State a -> IO Int
recvLoop q state = do 
    netQueue <- Q.read q 20000
    case (Q.qLen netQueue) of 
        0 ->  recvLoop q state
        _ ->  do
              modifyMVar_ (sCounter state) $ \c -> return (c + fromIntegral (Q.qLen netQueue))
              recvLoop q state

