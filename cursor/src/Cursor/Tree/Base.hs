{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

module Cursor.Tree.Base
    ( singletonTreeCursor
    , makeTreeCursor
    , makeTreeCursorWithSelection
    , rebuildTreeCursor
    , mapTreeCursor
    , currentTree
    , makeTreeCursorWithAbove
    ) where

import Data.Tree
import Data.Validity
import Data.Validity.Tree ()

import GHC.Generics (Generic)

import Control.Applicative
import Control.Monad

import Lens.Micro

import Cursor.Tree.Types
import Cursor.Types

singletonTreeCursor :: a -> TreeCursor a b
singletonTreeCursor v =
    TreeCursor
    {treeAbove = Nothing, treeCurrent = v, treeBelow = ClosedForest []}

makeTreeCursor :: (b -> a) -> CTree b -> TreeCursor a b
makeTreeCursor g (CNode v fs) =
    TreeCursor {treeAbove = Nothing, treeCurrent = g v, treeBelow = fs}

makeTreeCursorWithSelection ::
       (a -> b)
    -> (b -> a)
    -> TreeCursorSelection
    -> CTree b
    -> Maybe (TreeCursor a b)
makeTreeCursorWithSelection f g sel = walkDown sel . makeTreeCursor g
  where
    walkDown SelectNode tc = pure tc
    walkDown (SelectChild i s) TreeCursor {..} =
        (walkDown s =<<) $
        case splitAt i $ unpackCForest treeBelow of
            (_, []) -> Nothing
            (lefts, current:rights) ->
                Just $
                makeTreeCursorWithAbove g current $
                Just $
                TreeAbove
                { treeAboveLefts = reverse lefts
                , treeAboveAbove = treeAbove
                , treeAboveNode = f treeCurrent
                , treeAboveRights = rights
                }

rebuildTreeCursor :: (a -> b) -> TreeCursor a b -> CTree b
rebuildTreeCursor f TreeCursor {..} =
    wrapAbove treeAbove $ CNode (f treeCurrent) treeBelow
  where
    wrapAbove Nothing t = t
    wrapAbove (Just TreeAbove {..}) t =
        wrapAbove treeAboveAbove $
        CNode treeAboveNode $
        openForest $ concat [reverse treeAboveLefts, [t], treeAboveRights]

mapTreeCursor :: (a -> c) -> (b -> d) -> TreeCursor a b -> TreeCursor c d
mapTreeCursor f g TreeCursor {..} =
    TreeCursor
    { treeAbove = fmap g <$> treeAbove
    , treeCurrent = f treeCurrent
    , treeBelow = fmap g treeBelow
    }

currentTree :: (a -> b) -> TreeCursor a b -> CTree b
currentTree f TreeCursor {..} = CNode (f treeCurrent) treeBelow

makeTreeCursorWithAbove ::
       (b -> a) -> CTree b -> Maybe (TreeAbove b) -> TreeCursor a b
makeTreeCursorWithAbove g (CNode a forest) mta =
    TreeCursor {treeAbove = mta, treeCurrent = g a, treeBelow = forest}
