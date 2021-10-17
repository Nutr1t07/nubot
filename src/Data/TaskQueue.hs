module Data.TaskQueue where

import Data.Queue
import Data.IORef
import Control.Monad (unless)

data TaskQueue = TaskQueue {
    queue :: QueueIO (IO ())
  , isRunning :: IORef Bool
  , onFinishedFunc :: IO ()
} 

emptyTaskQueue :: IO () -> IO TaskQueue
emptyTaskQueue f = do
  x <- emptyQueueIO
  y <- newIORef False
  pure $ TaskQueue x y f

addTask :: TaskQueue -> IO () -> IO ()
addTask taskQueue x = do
  let realQueue = queue taskQueue
  pushIO realQueue x
  isRunning' <- readIORef $ isRunning taskQueue
  unless isRunning' $ do
    writeIORef (isRunning taskQueue) True
    performTasks taskQueue

performTasks :: TaskQueue -> IO ()
performTasks taskQueue = do
  let realQueue = queue taskQueue
  popItem <- popIO realQueue
  case popItem of
    Just f -> f >> performTasks taskQueue
    Nothing -> onFinishedFunc taskQueue >> writeIORef (isRunning taskQueue) False 
