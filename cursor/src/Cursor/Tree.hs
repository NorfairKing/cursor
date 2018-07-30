{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

module Cursor.Tree
    ( TreeCursor(..)
    , TreeAbove(..)
    , singletonTreeCursor
    , makeTreeCursor
    , rebuildTreeCursor
    , treeCursorSelectPrev
    , treeCursorSelectNext
    , treeCursorSelectFirst
    , treeCursorSelectLast
    , treeCursorSelectAbove
    , treeCursorSelectBelow
    , treeCursorSelectBelowAtPos
    , treeCursorInsert
    , treeCursorInsertAndSelect
    , treeCursorAppend
    , treeCursorAppendAndSelect
    , treeCursorRemoveElem
    , treeCursorDeleteElem
    , treeCursorSwapPrev
    , treeCursorSwapNext
    ) where

import Control.Applicative
import Data.Tree
import Data.Validity
import Data.Validity.Tree ()
import GHC.Generics (Generic)

data TreeCursor a = TreeCursor
    { treeAbove :: Maybe (TreeAbove a)
    , treeCurrent :: a
    , treeBelow :: Forest a
    } deriving (Show, Eq, Generic, Functor)

currentTree :: TreeCursor a -> Tree a
currentTree TreeCursor {..} = Node treeCurrent treeBelow

instance Validity a => Validity (TreeCursor a)

data TreeAbove a = TreeAbove
    { treeAboveLefts :: [Tree a]
    , treeAboveAbove :: Maybe (TreeAbove a)
    , treeAboveNode :: a
    , treeAboveRights :: [Tree a]
    } deriving (Show, Eq, Generic, Functor)

instance Validity a => Validity (TreeAbove a)

makeTreeCursor :: Tree a -> TreeCursor a
makeTreeCursor (Node v fs) =
    TreeCursor {treeAbove = Nothing, treeCurrent = v, treeBelow = fs}

makeTreeCursorWithAbove :: Tree a -> Maybe (TreeAbove a) -> TreeCursor a
makeTreeCursorWithAbove (Node a forrest) mta =
    TreeCursor {treeAbove = mta, treeCurrent = a, treeBelow = forrest}

singletonTreeCursor :: a -> TreeCursor a
singletonTreeCursor v =
    TreeCursor {treeAbove = Nothing, treeCurrent = v, treeBelow = []}

rebuildTreeCursor :: TreeCursor a -> Tree a
rebuildTreeCursor TreeCursor {..} =
    (case treeAbove of
         Nothing -> id
         Just ta -> (\n -> go n ta))
        (Node treeCurrent treeBelow)
  where
    go :: Tree a -> TreeAbove a -> Tree a
    go t TreeAbove {..} =
        (case treeAboveAbove of
             Nothing -> id
             Just ta -> (\n -> go n ta))
            (Node
                 treeAboveNode
                 (reverse treeAboveLefts ++ [t] ++ treeAboveRights))

treeCursorSelectPrev :: TreeCursor a -> Maybe (TreeCursor a)
treeCursorSelectPrev tc@TreeCursor {..} =
    case treeAbove of
        Nothing -> Nothing
        Just ta ->
            case treeAboveLefts ta of
                [] -> Nothing
                tree:xs ->
                    Just . makeTreeCursorWithAbove tree $
                    Just
                        ta
                        { treeAboveLefts = xs
                        , treeAboveRights = currentTree tc : treeAboveRights ta
                        }

treeCursorSelectNext :: TreeCursor a -> Maybe (TreeCursor a)
treeCursorSelectNext tc@TreeCursor {..} =
    case treeAbove of
        Nothing -> Nothing
        Just ta ->
            case treeAboveRights ta of
                [] -> Nothing
                tree:xs ->
                    Just . makeTreeCursorWithAbove tree . Just $
                    ta
                    { treeAboveLefts = currentTree tc : treeAboveLefts ta
                    , treeAboveRights = xs
                    }

treeCursorSelectFirst :: TreeCursor a -> TreeCursor a
treeCursorSelectFirst tc =
    case treeCursorSelectPrev tc of
        Nothing -> tc
        Just tc' -> treeCursorSelectFirst tc'

treeCursorSelectLast :: TreeCursor a -> TreeCursor a
treeCursorSelectLast tc =
    case treeCursorSelectNext tc of
        Nothing -> tc
        Just tc' -> treeCursorSelectLast tc'

treeCursorInsert :: Tree a -> TreeCursor a -> Maybe (TreeCursor a)
treeCursorInsert tree tc@TreeCursor {..} =
    case treeAbove of
        Nothing -> Nothing
        Just ta ->
            let newTreeAbove = ta {treeAboveLefts = tree : treeAboveLefts ta}
            in Just tc {treeAbove = Just newTreeAbove}

treeCursorAppend :: Tree a -> TreeCursor a -> Maybe (TreeCursor a)
treeCursorAppend tree tc@TreeCursor {..} =
    case treeAbove of
        Nothing -> Nothing
        Just ta ->
            let newTreeAbove = ta {treeAboveRights = tree : treeAboveRights ta}
            in Just tc {treeAbove = Just newTreeAbove}

treeCursorInsertAndSelect :: Tree a -> TreeCursor a -> Maybe (TreeCursor a)
treeCursorInsertAndSelect tree tc@TreeCursor {..} =
    case treeAbove of
        Nothing -> Nothing
        Just ta ->
            let newTreeAbove =
                    ta {treeAboveRights = currentTree tc : treeAboveRights ta}
            in Just . makeTreeCursorWithAbove tree $ Just newTreeAbove

treeCursorAppendAndSelect :: Tree a -> TreeCursor a -> Maybe (TreeCursor a)
treeCursorAppendAndSelect tree tc@TreeCursor {..} =
    case treeAbove of
        Nothing -> Nothing
        Just ta ->
            let newTreeAbove =
                    ta {treeAboveLefts = currentTree tc : treeAboveLefts ta}
            in Just . makeTreeCursorWithAbove tree $ Just newTreeAbove

treeCursorDeleteElemAndSelectPrevious :: TreeCursor a -> Maybe (TreeCursor a)
treeCursorDeleteElemAndSelectPrevious TreeCursor {..} =
    case treeAbove of
        Nothing -> Nothing
        Just ta ->
            case treeAboveLefts ta of
                [] -> Nothing
                tree:xs ->
                    Just . makeTreeCursorWithAbove tree $
                    Just ta {treeAboveLefts = xs}

treeCursorDeleteElemAndSelectNext :: TreeCursor a -> Maybe (TreeCursor a)
treeCursorDeleteElemAndSelectNext TreeCursor {..} =
    case treeAbove of
        Nothing -> Nothing
        Just ta ->
            case treeAboveRights ta of
                [] -> Nothing
                tree:xs ->
                    Just . makeTreeCursorWithAbove tree $
                    Just ta {treeAboveRights = xs}

treeCursorSelectAbove :: TreeCursor a -> Maybe (TreeCursor a)
treeCursorSelectAbove tc@TreeCursor {..} =
    case treeAbove of
        Nothing -> Nothing
        Just TreeAbove {..} ->
            let newForrest =
                    reverse treeAboveLefts ++
                    [currentTree tc] ++ treeAboveRights
                newTree = Node treeAboveNode newForrest
            in Just $ makeTreeCursorWithAbove newTree treeAboveAbove

treeCursorSelectBelowAtPos :: Int -> TreeCursor a -> Maybe (TreeCursor a)
treeCursorSelectBelowAtPos pos TreeCursor {..} =
    case splitAt pos treeBelow of
        (_, []) -> Nothing
        (lefts, current:rights) ->
            Just $
            makeTreeCursorWithAbove current $
            Just $
            TreeAbove
            { treeAboveLefts = reverse lefts
            , treeAboveAbove = treeAbove
            , treeAboveNode = treeCurrent
            , treeAboveRights = rights
            }

treeCursorSelectBelow :: TreeCursor a -> Maybe (TreeCursor a)
treeCursorSelectBelow = treeCursorSelectBelowAtPos 0

treeCursorRemoveElem :: TreeCursor a -> Maybe (TreeCursor a)
treeCursorRemoveElem tc =
    treeCursorDeleteElemAndSelectPrevious tc <|>
    treeCursorDeleteElemAndSelectNext tc

treeCursorDeleteElem :: TreeCursor a -> Maybe (TreeCursor a)
treeCursorDeleteElem tc =
    treeCursorDeleteElemAndSelectNext tc <|>
    treeCursorDeleteElemAndSelectPrevious tc

treeCursorSwapPrev :: TreeCursor a -> Maybe (TreeCursor a)
treeCursorSwapPrev tc = do
    above <- treeAbove tc
    let t = currentTree tc
    (above', t') <-
        case treeAboveLefts above of
            [] -> Nothing
            (l:ls) -> Just (above {treeAboveLefts = t : ls}, l)
    pure $ makeTreeCursorWithAbove t' $ Just above'

treeCursorSwapNext :: TreeCursor a -> Maybe (TreeCursor a)
treeCursorSwapNext tc = do
    above <- treeAbove tc
    let t = currentTree tc
    (above', t') <-
        case treeAboveRights above of
            [] -> Nothing
            (r:rs) -> Just (above {treeAboveRights = t : rs}, r)
    pure $ makeTreeCursorWithAbove t' $ Just above'
