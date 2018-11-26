{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.Cycle.Gen
    (
    ) where

import Cursor.Cycle

import Data.GenValidity

import Cursor.List.Gen ()

instance GenUnchecked a => GenUnchecked (CycleCursor a)

instance GenValid a => GenValid (CycleCursor a)
