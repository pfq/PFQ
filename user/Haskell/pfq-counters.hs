{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import qualified Network.PFq as Q

import Foreign
-- import System.Environment
import System.Time
import System.Exit

import Control.Monad as M
import Control.Applicative
import Control.Concurrent
import Control.Exception

import Data.Maybe
import Data.List
import Data.List.Split
import Data.Data
-- import Data.Typeable

import qualified Data.Set as S
import qualified Data.Map as M

import System.Console.CmdArgs


data Key = Key Word32 Word32 Word16 Word16
            deriving (Eq, Show)


data State a = State { sCounter :: MVar a,
                       sFlow    :: MVar a, 
                       sSet     :: S.Set Key
                     }


-- Command line options 
--
data Options = Options 
               {
                caplen   :: Int,
                offset   :: Int,
                slots    :: Int,
                function :: [String],
                thread   :: [String]
               } deriving (Data, Typeable, Show)


-- default options
--
options = cmdArgsMode $ Options { 
                                  caplen   = 64,
                                  offset   = 0,
                                  slots    = 262144,
                                  function = [] &= typ "FUNCTION"  &= help "Where FUNCTION = function-name[>=>fun>=>fun][.gid] (ie: steer-ipv4)",
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
                        ds : []         ->  Binding (splitOn ":" ds) 0 0 [-1]
                        ds : c : []     ->  Binding (splitOn ":" ds) (read c) 0 [-1]
                        ds : c : g : [] ->  Binding (splitOn ":" ds) (read c) (read g) [-1]
                        ds : c : g : qs ->  Binding (splitOn ":" ds) (read c) (read g) (map read qs)


makeFun :: String -> (Gid, [String])
makeFun s =  case splitOn "." s of
                []     -> error "makeFun: empty string"
                fs : [] -> (-1,             map (filter (/= ' ')) $ splitOn ">=>" fs)
                fs : n  -> (read $ head n,  map (filter (/= ' ')) $ splitOn ">=>" fs)
                
-- main function
--

main :: IO ()
main = do
    op <- cmdArgsRun options
    putStrLn $ "[pfq] " ++ show op
    cs  <- runThreads op (M.fromList $ map makeFun (function op)) 
    t   <- getClockTime
    dumpStat cs t


dumpStat :: (RealFrac a) => [MVar a] -> ClockTime -> IO ()
dumpStat cs t0 = do
    threadDelay 1000000
    t <- getClockTime
    cs' <- mapM (`swapMVar` 0) cs
    M.void( when ((-1) `elem` cs') exitFailure)
    let delta = diffUSec t t0
    let rate = (sum cs' * 1000000) / fromIntegral delta  
    putStrLn $ "Total rate pkt/sec: " ++ show (truncate rate :: Integer)
    dumpStat cs t


diffUSec :: ClockTime -> ClockTime -> Int
diffUSec t1 t0 = (tdSec delta * 1000000) + truncate ((fromIntegral(tdPicosec delta) / 1000000) :: Double)
                    where delta = diffClockTimes t1 t0


runThreads :: (Num a) => Options -> M.Map Gid [String] -> IO [MVar a]
runThreads op ms = 
    forM (thread op) $ \tb -> do
        let binding = makeBinding tb
            sf = M.lookup (groupId binding) ms <|> M.lookup (-1) ms 
        c <- newMVar 0
        f <- newMVar 0
        _ <- forkOn (coreNum binding) ( 
                 handle ((\e ->  M.void (putStrLn ("[pfq] Exception: " ++ show e) >> swapMVar c (-1))) :: SomeException -> IO ()) $ do 
                 fp <- Q.openNoGroup (caplen op) (offset op) (slots op)
                 withForeignPtr fp  $ \q -> do
                     Q.joinGroup q (groupId binding) [Q.class_default] Q.policy_shared
                     forM_ (devs binding) $ \dev ->
                       forM_ (queues binding) $ \queue ->
                         Q.setPromisc q dev True >> Q.bindGroup q (groupId binding) dev queue
                     when (isJust sf) $ putStrLn ("[pfq] Gid " ++ show (groupId binding) ++ " is using continuation: " ++ intercalate " >=> " (fromJust sf)) >>
                                        forM_ (zip (fromJust sf) [0,1..]) 
                                            (\(name,ix) -> Q.groupFunction q (groupId binding) ix name) 
                     Q.enable q 
                     M.void (recvLoop q (State c f S.empty)) 
                 )
        putStrLn $ "[pfq] " ++ show binding ++ " @core " ++ show (coreNum binding) ++ " started!"
        return c 


recvLoop :: (Num a) => Ptr Q.PFqTag -> State a -> IO Int
recvLoop q state = do 
    netQueue <- Q.read q 20000
    case Q.qLen netQueue of 
        0 ->  recvLoop q state
        _ ->  do
              modifyMVar_ (sCounter state) $ \c -> return (c + fromIntegral (Q.qLen netQueue))
              recvLoop q state

