{-# LANGUAGE NamedFieldPuns #-}

module Control.Concurrent.ProcessPool
  ( Pool
  , startPool
  , submitJob
  , waitJob
  ) where

import Control.Concurrent
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.Async (Async)
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Control.Exception
import Control.Monad

import Data.UUID
import qualified Data.UUID.V4 as UUID4
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))

-- TODO @incomplete: shutdown the pool gracefully

-- | A process pool that can concurrently execute a predefined
-- number of homogenous jobs. Internally, a thread is used to
-- manage the jobs.
data Pool a = Pool
  { managerThreadId :: ThreadId
  -- TODO @incomplete: Put these three in a single TVar,
  -- to facilitate atomic modification of the state.
  , jobSlots :: TSem
  , jobQueue :: TQueue (Job a)
  , jobResults :: TVar (Map UUID (TMVar (Async a)))}


data Job a = Job
  { jobId :: UUID
  , computation :: IO a}


-- | Start a new 'Pool', which can concurrently execute
-- up to the specified number of jobs.
startPool :: Int -> IO (Pool a)
startPool maxConcurrentJobs = do
  jobSlots <- atomically $ newTSem maxConcurrentJobs
  jobQueue <- atomically newTQueue
  jobResults <- atomically $ newTVar Map.empty

  managerThreadId <- forkIO $ forever $ do
    -- wait for the next job
    job <- atomically $ readTQueue jobQueue
    -- wait for an empty slot, and then spawn a new thread to execute the job
    bracket_
      (atomically $ waitTSem jobSlots)
      (atomically $ signalTSem jobSlots)
      (spawnJob jobResults job)

  let pool = Pool
        { managerThreadId = managerThreadId
        , jobSlots = jobSlots
        , jobQueue = jobQueue
        , jobResults = jobResults}

  return pool


spawnJob :: TVar (Map UUID (TMVar (Async a))) -> Job a -> IO ()
spawnJob resultsVar Job{jobId, computation} = do
  putStrLn $ "starting job " ++ show jobId
  result <- Async.async computation
  atomically $ do
    results <- readTVar resultsVar
    putTMVar (results ! jobId) result


-- | Submit a job to the pool, returns an ID that can be waited on.
submitJob :: Pool a -> IO a -> IO UUID
submitJob Pool{jobQueue, jobResults} computation = do
  jobId <- UUID4.nextRandom
  atomically $ do
    let job = Job{jobId, computation}
    initResultSlot jobResults jobId
    writeTQueue jobQueue job
    return jobId


initResultSlot :: TVar (Map UUID (TMVar (Async a))) -> UUID -> STM ()
initResultSlot resultsVar jobId = do
  v <- newEmptyTMVar
  results <- readTVar resultsVar
  let newResults = Map.insert jobId v results
  writeTVar resultsVar newResults


-- | Blocks until a job's result is available.
waitJob :: Pool a -> UUID -> IO a
waitJob Pool{jobResults=resultsVar} jobId = do
  res <- atomically $ do
    results <- readTVar resultsVar
    readTMVar $ results ! jobId
  Async.wait res
