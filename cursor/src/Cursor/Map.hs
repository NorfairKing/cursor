{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module Cursor.Map where

import GHC.Generics (Generic)

import Data.Validity
import Data.Validity.Tree ()

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

import Cursor.NonEmpty

newtype MapCursor k v = MapCursor
    { mapCursorList :: NonEmptyCursor (KeyValueCursor k v)
    } deriving (Show, Eq, Generic, Functor)

instance (Validity k, Validity v) => Validity (MapCursor k v)

data KeyValueCursor k v = KeyValueCursor
    { keyValueKey :: k
    , keyValueValue :: v
    } deriving (Show, Eq, Generic, Functor)

instance (Validity k, Validity v) => Validity (KeyValueCursor k v)

makeMapCursor :: NonEmpty (k, v) -> MapCursor k v
makeMapCursor = makeMapCursorWithSelection 0

makeMapCursorWithSelection :: Int -> NonEmpty (k, v) -> MapCursor k v
makeMapCursorWithSelection i ne =
    MapCursor
        { mapCursorList =
              makeNonEmptyCursorWithSelection i $
              NE.map (uncurry makeKeyValueCursor) ne
        }

makeKeyValueCursor :: k -> v -> KeyValueCursor k v
makeKeyValueCursor k v = KeyValueCursor {keyValueKey = k, keyValueValue = v}

singletonKeyValueCursor :: k -> v -> MapCursor k v
singletonKeyValueCursor k v = makeMapCursor $ (k, v) :| []

rebuildMapCursor :: MapCursor k v -> NonEmpty (k, v)
rebuildMapCursor =
    NE.map rebuildKeyValueCursor . rebuildNonEmptyCursor . mapCursorList

rebuildKeyValueCursor :: KeyValueCursor k v -> (k, v)
rebuildKeyValueCursor KeyValueCursor {..} = (keyValueKey, keyValueValue)
