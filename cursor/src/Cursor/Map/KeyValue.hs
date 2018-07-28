{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module Cursor.Map.KeyValue
    ( KeyValueCursor(..)
    , makeKeyValueCursor
    , rebuildKeyValueCursor
    , keyValueCursorKeyL
    , keyValueCursorValueL
    , keyValueCursorSelectKey
    , keyValueCursorSelectValue
    , keyValueCursorToggleSelected
    , KeyValueToggle(..)
    ) where

import GHC.Generics (Generic)

import Data.Validity

import Lens.Micro

data KeyValueCursor k v = KeyValueCursor
    { keyValueCursorKey :: k
    , keyValueCursorValue :: v
    , keyValueCursorToggle :: KeyValueToggle
    } deriving (Show, Eq, Generic, Functor)

instance (Validity k, Validity v) => Validity (KeyValueCursor k v)

makeKeyValueCursor :: k -> v -> KeyValueCursor k v
makeKeyValueCursor k v =
    KeyValueCursor
        { keyValueCursorKey = k
        , keyValueCursorValue = v
        , keyValueCursorToggle = KeySelected
        }

rebuildKeyValueCursor :: KeyValueCursor k v -> (k, v)
rebuildKeyValueCursor KeyValueCursor {..} =
    (keyValueCursorKey, keyValueCursorValue)

keyValueCursorKeyL :: Lens' (KeyValueCursor k v) k
keyValueCursorKeyL =
    lens keyValueCursorKey $ \kvc k -> kvc {keyValueCursorKey = k}

keyValueCursorValueL :: Lens' (KeyValueCursor k v) v
keyValueCursorValueL =
    lens keyValueCursorValue $ \kvc v -> kvc {keyValueCursorValue = v}

keyValueCursorSelectKey :: KeyValueCursor k v -> KeyValueCursor k v
keyValueCursorSelectKey kvc = kvc {keyValueCursorToggle = KeySelected}

keyValueCursorSelectValue :: KeyValueCursor k v -> KeyValueCursor k v
keyValueCursorSelectValue kvc = kvc {keyValueCursorToggle = ValueSelected}

keyValueCursorToggleSelected :: KeyValueCursor k v -> KeyValueCursor k v
keyValueCursorToggleSelected kvc =
    kvc
        { keyValueCursorToggle =
              case keyValueCursorToggle kvc of
                  KeySelected -> ValueSelected
                  ValueSelected -> KeySelected
        }

data KeyValueToggle
    = KeySelected
    | ValueSelected
    deriving (Show, Eq, Generic)

instance Validity KeyValueToggle
