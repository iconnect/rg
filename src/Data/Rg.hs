{-# LANGUAGE RecordWildCards        #-}

module Data.Rg
  ( Rg(..)
  , Range
  , newStartOfRangeFromList
  , newStartOfRangeFromVector
  , extractRange
  ) where

import           Data.Array
import           Data.Maybe
import qualified Data.Vector        as V


-- | Rg acts a bit like a Bounded Enum, but the size of the enumeration
-- can be dynamically determined from each value in the type (see 'sizeRg')
class Rg rg where
  -- | the number of values in the enumeration; sizeRg sz > 0
  sizeRg      :: rg -> Int

  -- | the nth item in the enumeration (first is 0)
  toRg        :: rg -> Int -> Maybe rg

  -- | place in the enumation (first is 0)
  fromRg      :: rg -> Int

  -- | first item in the enumeration
  minRg       :: rg -> rg
  minRg    = fromMaybe oops . flip toRg 0
    where
      oops = error "minRg: no minimum value in range"

  -- | last item in the enumeration
  maxRg       :: rg -> rg
  maxRg rg = fromMaybe oops $ toRg rg n
    where
      n    = sizeRg rg - 1
      oops = error "maxRg: no maximum value in range"

  -- | next item in the enumeration (Nothing if already last)
  succRg      :: rg -> Maybe rg
  succRg rg = toRg rg $ fromRg rg + 1

  -- | previous item in the enumeration (Nothing if already first)
  predRg      :: rg -> Maybe rg
  predRg rg = toRg rg $ fromRg rg - 1

  -- | list given items in the enumeration
  allListRg   :: rg -> [rg]
  allListRg rg = listRg rg [0..]

  -- | list given items in the enumeration, stopping as soon as an index is
  -- out of range
  listRg      :: rg -> [Int] -> [rg]
  listRg rg is = catMaybes $ takeWhile isJust [ toRg rg i | i<-is ]

  -- | list given items in the enumeration as a 'V.Vector'
  allVectorRg :: rg -> [rg]
  allVectorRg rg = listRg rg [0..]

  -- | list the items in the enumeration as a 'V.Vector', stopping as soon as an
  -- index is out of range
  vectorRg    :: rg -> [Int] -> V.Vector rg
  vectorRg rg is = V.fromList $ listRg rg is


data Range a =
  Range
    { _rg_size  :: Int
    , _rg_elem  :: Int
    , _rg_array :: Array Int a
    }
  deriving (Show)

newStartOfRangeFromList :: [a] -> Range a
newStartOfRangeFromList xs =
  Range
    { _rg_size  = sz
    , _rg_elem  = 0
    , _rg_array = listArray (0,sz-1) xs
    }
  where
    sz = length xs

newStartOfRangeFromVector :: V.Vector a -> Range a
newStartOfRangeFromVector v =
  Range
    { _rg_size  = sz
    , _rg_elem  = 0
    , _rg_array = listArray (0,sz-1) $ V.toList v
    }
  where
    sz = V.length v

extractRange :: Range a -> a
extractRange Range{..} = _rg_array ! _rg_elem

instance Rg (Range a) where
  sizeRg      = _rg_size
  toRg   rg i = case 0 <= i && i < _rg_size rg of
    False -> Nothing
    True  -> Just rg { _rg_elem = i }
  fromRg      = _rg_elem
