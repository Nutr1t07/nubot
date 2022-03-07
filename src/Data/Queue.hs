module Data.Queue where

import Data.IORef
    ( IORef, modifyIORef, newIORef, readIORef, writeIORef )

data QueueIO a = QueueIO (IORef [a]) (IORef [a])

emptyQueueIO :: IO (QueueIO a)
emptyQueueIO = do
    xs <- newIORef []
    ys <- newIORef []
    pure $ QueueIO xs ys

pushIO :: QueueIO a -> a -> IO ()
pushIO (QueueIO xs ys) x = modifyIORef ys (x:)

popIO :: QueueIO a -> IO (Maybe a)
popIO (QueueIO xs' ys') = do
  xs <- readIORef xs'
  ys <- readIORef ys'
  case (xs, ys) of
      ([], [])  -> pure Nothing
      ([], yss) -> do
          let revYs = reverse yss
          writeIORef xs' (tail revYs)
          writeIORef ys' []
          pure $ Just (head revYs)
      (xss, _)  -> do
          modifyIORef xs' tail
          pure $ Just (head xss)

emptyIO :: QueueIO a -> IO Bool
emptyIO (QueueIO xs' ys') = do
  xs <- readIORef xs'
  ys <- readIORef ys'
  case (xs, ys) of
      ([], []) -> pure True
      _        -> pure False



data Queue a = Queue [a] [a] deriving (Show)

emptyQueue :: Queue a
emptyQueue = Queue [] []

push :: Queue a -> a -> Queue a
push (Queue xs ys) x = Queue xs (x:ys)

pop :: Queue a -> (Maybe a, Queue a)
pop x@(Queue [] []) = (Nothing, x)
pop (Queue [] xs) = let rxs = reverse xs in
    (Just (head rxs), Queue (tail rxs) [])
pop (Queue xs ys) = (Just (head xs), Queue (tail xs) ys)

empty :: Queue a -> Bool
empty (Queue [] []) = True
empty _ = False