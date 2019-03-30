module Effect.Aff.Parallel
  ( parallelize
  , parallelize_
  ) where

import Prelude

import Data.Array (foldMap, mapWithIndex, replicate, snoc, sortWith, uncons)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Aff (Aff, parallel, sequential)
import Effect.Class (liftEffect)
import Effect.Ref (Ref, new, read, write)


-- | Parallelize asynchronous computations with specified number of fibers.
-- | The results of computations are stored in an array in the same order.
-- | If one computation fails, all remaining computations are canceled.
parallelize ∷ ∀ a. Int → Array (Aff a) → Aff (Array a)
parallelize num affs = do
  ref ← liftEffect $ new $ mapWithIndex Tuple affs
  results ← sequential $ foldMap parallel $ replicate num $ fiber ref []
  pure $ snd <$> sortWith fst results

  where
    fiber ref results = do
      aff ← liftEffect $ pop ref
      maybe (pure results) (fiber ref <<< snoc results <=< sequence) aff


-- | Parallelize asynchronous computations with specified number of fibers.
-- | The results of computations are ignored, except for effects.
-- | If one computation fails, all remaining computations are canceled.
parallelize_ ∷ ∀ a. Int → Array (Aff a) → Aff Unit
parallelize_ num affs = do
  ref ← liftEffect $ new affs
  sequential $ foldMap parallel $ replicate num $ fiber ref

  where
    fiber ref = do
      aff ← liftEffect $ pop ref
      maybe (pure unit) (_ *> fiber ref) aff


pop ∷ ∀ a. Ref (Array a) → Effect (Maybe a)
pop ref = do
  rest ← read ref
  case uncons rest of
    Just { head, tail } → Just head <$ write tail ref
    Nothing → pure Nothing
