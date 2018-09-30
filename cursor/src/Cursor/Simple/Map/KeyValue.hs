{-# LANGUAGE PatternSynonyms #-}

module Cursor.Simple.Map.KeyValue
    ( KeyValueCursor
    , pattern KVC.KeyValueCursorKey
    , pattern KVC.KeyValueCursorValue
    , KVC.makeKeyValueCursorKey
    , KVC.makeKeyValueCursorValue
    , rebuildKeyValueCursor
    , KVC.keyValueCursorSelection
    , mapKeyValueCursor
    , keyValueCursorSelectKey
    , keyValueCursorSelectValue
    , keyValueCursorToggleSelected
    , KVC.KeyValueToggle(..)
    ) where

import qualified Cursor.Map.KeyValue as KVC

type KeyValueCursor k v = KVC.KeyValueCursor k v k v

rebuildKeyValueCursor :: KeyValueCursor k v -> (k, v)
rebuildKeyValueCursor = KVC.rebuildKeyValueCursor id id

mapKeyValueCursor ::
       (k -> l) -> (v -> w) -> KeyValueCursor k v -> KeyValueCursor l w
mapKeyValueCursor f g = KVC.mapKeyValueCursor f g f g

keyValueCursorSelectKey :: KeyValueCursor k v -> KeyValueCursor k v
keyValueCursorSelectKey = KVC.keyValueCursorSelectKey id id

keyValueCursorSelectValue :: KeyValueCursor k v -> KeyValueCursor k v
keyValueCursorSelectValue = KVC.keyValueCursorSelectValue id id

keyValueCursorToggleSelected :: KeyValueCursor k v -> KeyValueCursor k v
keyValueCursorToggleSelected = KVC.keyValueCursorToggleSelected id id id id
