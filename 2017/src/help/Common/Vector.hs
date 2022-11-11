module Common.Vector
    ( maximum
    , minimum
    ) where

import Data.Vector (Vector)
import Data.Vector qualified as Vec
import Prelude hiding (maximum, minimum)


-- | The default implementations of 'Vec.maximum' is partial, whereas this
-- version is not.
maximum :: Ord a => a -> Vector a -> a
maximum = Vec.foldl' max

-- | The default implementations of 'Vec.minimum' is partial, whereas this
-- version is not.
minimum :: Ord a => a -> Vector a -> a
minimum = Vec.foldl' min
