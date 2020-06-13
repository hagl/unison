{-# LANGUAGE OverloadedStrings #-}

-- | A utility type for saving memory in the presence of many duplicate ByteStrings, etc. If you have data that may be
-- a redundant duplicate, try pinning it to a pin board, and use the result of that operation instead.
--
--   Without a pin board:
--
--     x ───── "38dce848c8c829c62"
--     y ───── "38dce848c8c829c62"
--     z ───── "d2518f260535b927b"
--
--   With a pin board:
--
--     x ───── "38dce848c8c829c62" ┄┄┄┄┄┐
--     y ────────┘                     board
--     z ───── "d2518f260535b927b" ┄┄┄┄┄┘
--
--   ... and after x is garbage collected:
--
--             "38dce848c8c829c62" ┄┄┄┄┄┐
--     y ────────┘                     board
--     z ───── "d2518f260535b927b" ┄┄┄┄┄┘
--
--   ... and after y is garbage collected:
--
--                                     board
--     z ───── "d2518f260535b927b" ┄┄┄┄┄┘
module Unison.Util.PinBoard3
  ( PinBoard,
    new,
    pin,
    pinWith,
  )
where

import Data.Foldable (find)
import qualified Data.HashTable.IO as HashTable
import Data.Hashable (Hashable, hash)
import System.Mem.Weak (Weak, deRefWeak, mkWeakPtr)
import Unison.Prelude

type HashTable k v =
  HashTable.LinearHashTable k v

-- | A "pin board" is a place to pin values; semantically, it's a set, but differs in a few ways:
--
--   * Pinned values aren't kept alive by the pin board, they might be garbage collected at any time.
--   * If you try to pin a value that's already pinned (per its Eq instance), the pinned one will be returned
--     instead.
--   * It has a small API: just 'new' and 'pin'.
newtype PinBoard a
  = PinBoard (HashTable Int (Bucket a))

new :: MonadIO m => m (PinBoard a)
new =
  liftIO (PinBoard <$> HashTable.new)

pin :: forall a m. (Eq a, Hashable a, MonadIO m) => PinBoard a -> a -> m a
pin board x =
  pinWith board (hash x) x

-- | Like 'pin', but accepts a hash key manually, rather than using 'Hashable'.
pinWith :: forall a m. (Eq a, MonadIO m) => PinBoard a -> Int -> a -> m a
pinWith (PinBoard board) n x = liftIO do
  HashTable.mutateIO board n \case
    Nothing -> (,x) . Just <$> newBucket x finalizer
    Just bucket ->
      bucketFind bucket x >>= \case
        -- Hash collision: the bucket has things in it, but none are the given value. Insert.
        Nothing -> (,x) . Just <$> bucketAdd bucket x finalizer
        -- The thing being inserted already exists; return it.
        Just y -> pure (Just bucket, y)
  where
    -- When each thing pinned here is garbage collected, compact its bucket.
    finalizer :: IO ()
    finalizer =
      HashTable.mutateIO board n (maybe (pure (Nothing, ())) (fmap (,()) . bucketCompact))

-- | A bucket of weak pointers to different values that all share a hash.
newtype Bucket a
  = Bucket [Weak a] -- Invariant: non-empty list

-- | A singleton bucket.
newBucket :: a -> IO () -> IO (Bucket a)
newBucket =
  bucketAdd (Bucket [])

-- | Add a value to a bucket.
bucketAdd :: Bucket a -> a -> IO () -> IO (Bucket a)
bucketAdd (Bucket weaks) x finalizer = do
  weak <- mkWeakPtr x (Just finalizer)
  pure (Bucket (weak : weaks))

-- | Drop all garbage-collected values from a bucket. If none remain, returns Nothing.
bucketCompact :: Bucket a -> IO (Maybe (Bucket a))
bucketCompact (Bucket weaks) =
  bucketFromList <$> mapMaybeM (\w -> (w <$) <$> deRefWeak w) weaks

-- | Look up a value in a bucket per its Eq instance.
bucketFind :: Eq a => Bucket a -> a -> IO (Maybe a)
bucketFind bucket x =
  find (== x) <$> bucketToList bucket

bucketFromList :: [Weak a] -> Maybe (Bucket a)
bucketFromList = \case
  [] -> Nothing
  weaks -> Just (Bucket weaks)

bucketToList :: Bucket a -> IO [a]
bucketToList (Bucket weaks) =
  mapMaybeM deRefWeak weaks
