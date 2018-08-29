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
    , treeCursorSelectBelowAtStart
    , treeCursorSelectBelowAtEnd
    , treeCursorSelectBelowAtPos
    , treeCursorSelectPrevOnSameLevel
    , treeCursorSelectNextOnSameLevel
    , treeCursorSelectAbovePrev
    , treeCursorSelectAboveNext
    , treeCursorInsert
    , treeCursorInsertAndSelect
    , treeCursorAppend
    , treeCursorAppendAndSelect
    , treeCursorAddChildAtPos
    , treeCursorAddChildAtStart
    , treeCursorAddChildAtEnd
    , treeCursorDeleteElemAndSelectPrevious
    , treeCursorDeleteElemAndSelectNext
    , treeCursorRemoveElem
    , treeCursorDeleteElem
    , treeCursorSwapPrev
    , treeCursorSwapNext
    , treeCursorAboveL
    , treeCursorCurrentL
    , treeCursorBelowL
    , treeAboveLeftsL
    , treeAboveAboveL
    , treeAboveNodeL
    , treeAboveRightsL
    ) where

import Data.Tree
import Data.Validity
import Data.Validity.Tree ()

import GHC.Generics (Generic)

import Control.Applicative

import Lens.Micro

import Cursor.Types

data TreeCursor a = TreeCursor
    { treeAbove :: Maybe (TreeAbove a)
    , treeCurrent :: a
    , treeBelow :: Forest a
    } deriving (Show, Eq, Generic, Functor)

currentTree :: TreeCursor a -> Tree a
currentTree TreeCursor {..} = Node treeCurrent treeBelow

treeCursorAboveL :: Lens' (TreeCursor a) (Maybe (TreeAbove a))
treeCursorAboveL = lens treeAbove $ \tc ta -> tc {treeAbove = ta}

treeCursorCurrentL :: Lens' (TreeCursor a) a
treeCursorCurrentL = lens treeCurrent $ \tc a -> tc {treeCurrent = a}

treeCursorBelowL :: Lens' (TreeCursor a) (Forest a)
treeCursorBelowL = lens treeBelow $ \tc tb -> tc {treeBelow = tb}

instance Validity a => Validity (TreeCursor a)

data TreeAbove a = TreeAbove
    { treeAboveLefts :: [Tree a]
    , treeAboveAbove :: Maybe (TreeAbove a)
    , treeAboveNode :: a
    , treeAboveRights :: [Tree a]
    } deriving (Show, Eq, Generic, Functor)

instance Validity a => Validity (TreeAbove a)

treeAboveLeftsL :: Lens' (TreeAbove a) [Tree a]
treeAboveLeftsL = lens treeAboveLefts $ \ta tal -> ta {treeAboveLefts = tal}

treeAboveAboveL :: Lens' (TreeAbove a) (Maybe (TreeAbove a))
treeAboveAboveL = lens treeAboveAbove $ \ta taa -> ta {treeAboveAbove = taa}

treeAboveNodeL :: Lens' (TreeAbove a) a
treeAboveNodeL = lens treeAboveNode $ \ta a -> ta {treeAboveNode = a}

treeAboveRightsL :: Lens' (TreeAbove a) [Tree a]
treeAboveRightsL = lens treeAboveRights $ \ta tar -> ta {treeAboveRights = tar}

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
treeCursorSelectPrev tc =
    treeCursorSelectAbovePrev tc <|> treeCursorSelectPrevOnSameLevel tc <|>
    treeCursorSelectAbove tc

treeCursorSelectNext :: TreeCursor a -> Maybe (TreeCursor a)
treeCursorSelectNext tc =
    treeCursorSelectBelowAtStart tc <|> treeCursorSelectNextOnSameLevel tc <|>
    treeCursorSelectAboveNext tc

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

treeCursorSelectBelowAtStart :: TreeCursor a -> Maybe (TreeCursor a)
treeCursorSelectBelowAtStart = treeCursorSelectBelowAtPos 0

treeCursorSelectBelowAtEnd :: TreeCursor a -> Maybe (TreeCursor a)
treeCursorSelectBelowAtEnd tc =
    treeCursorSelectBelowAtPos (length (treeBelow tc) - 1) tc

treeCursorSelectPrevOnSameLevel :: TreeCursor a -> Maybe (TreeCursor a)
treeCursorSelectPrevOnSameLevel tc@TreeCursor {..} =
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

treeCursorSelectNextOnSameLevel :: TreeCursor a -> Maybe (TreeCursor a)
treeCursorSelectNextOnSameLevel tc@TreeCursor {..} =
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

-- | Go back and down as far as necessary to find a previous element on a level below
treeCursorSelectAbovePrev :: TreeCursor a -> Maybe (TreeCursor a)
treeCursorSelectAbovePrev tc = treeCursorSelectPrevOnSameLevel tc >>= go
  where
    go :: TreeCursor a -> Maybe (TreeCursor a)
    go tc_ =
        case treeCursorSelectBelowAtEnd tc_ of
            Nothing -> pure tc_
            Just tc_' -> go tc_'

-- | Go up as far as necessary to find a next element on a level above and forward
treeCursorSelectAboveNext :: TreeCursor a -> Maybe (TreeCursor a)
treeCursorSelectAboveNext tc = do
    tc' <- treeCursorSelectAbove tc
    case treeCursorSelectNextOnSameLevel tc' of
        Nothing -> treeCursorSelectAboveNext tc'
        Just tc'' -> pure tc''

treeCursorInsert :: Tree a -> TreeCursor a -> Maybe (TreeCursor a)
treeCursorInsert tree tc@TreeCursor {..} =
    case treeAbove of
        Nothing -> Nothing
        Just ta ->
            let newTreeAbove = ta {treeAboveLefts = tree : treeAboveLefts ta}
            in Just tc {treeAbove = Just newTreeAbove}

treeCursorInsertAndSelect :: Tree a -> TreeCursor a -> Maybe (TreeCursor a)
treeCursorInsertAndSelect tree tc@TreeCursor {..} =
    case treeAbove of
        Nothing -> Nothing
        Just ta ->
            let newTreeAbove =
                    ta {treeAboveRights = currentTree tc : treeAboveRights ta}
            in Just . makeTreeCursorWithAbove tree $ Just newTreeAbove

treeCursorAppend :: Tree a -> TreeCursor a -> Maybe (TreeCursor a)
treeCursorAppend tree tc@TreeCursor {..} =
    case treeAbove of
        Nothing -> Nothing
        Just ta ->
            let newTreeAbove = ta {treeAboveRights = tree : treeAboveRights ta}
            in Just tc {treeAbove = Just newTreeAbove}

treeCursorAppendAndSelect :: Tree a -> TreeCursor a -> Maybe (TreeCursor a)
treeCursorAppendAndSelect tree tc@TreeCursor {..} =
    case treeAbove of
        Nothing -> Nothing
        Just ta ->
            let newTreeAbove =
                    ta {treeAboveLefts = currentTree tc : treeAboveLefts ta}
            in Just . makeTreeCursorWithAbove tree $ Just newTreeAbove

treeCursorAddChildAtPos :: Int -> Tree a -> TreeCursor a -> TreeCursor a
treeCursorAddChildAtPos i t tc =
    let (before, after) = splitAt i $ treeBelow tc
    in tc {treeBelow = before ++ [t] ++ after}

treeCursorAddChildAtStart :: Tree a -> TreeCursor a -> TreeCursor a
treeCursorAddChildAtStart t tc = tc {treeBelow = t : treeBelow tc}

treeCursorAddChildAtEnd :: Tree a -> TreeCursor a -> TreeCursor a
treeCursorAddChildAtEnd t tc = tc {treeBelow = treeBelow tc ++ [t]}

treeCursorDeleteElemAndSelectPrevious ::
       TreeCursor a -> Maybe (DeleteOrUpdate (TreeCursor a))
treeCursorDeleteElemAndSelectPrevious TreeCursor {..} =
    case treeAbove of
        Nothing -> Just Deleted
        Just ta ->
            case treeAboveLefts ta of
                [] -> Nothing
                tree:xs ->
                    Just . Updated . makeTreeCursorWithAbove tree $
                    Just ta {treeAboveLefts = xs}

treeCursorDeleteElemAndSelectNext ::
       TreeCursor a -> Maybe (DeleteOrUpdate (TreeCursor a))
treeCursorDeleteElemAndSelectNext TreeCursor {..} =
    case treeAbove of
        Nothing -> Just Deleted
        Just ta ->
            case treeAboveRights ta of
                [] -> Nothing
                tree:xs ->
                    Just . Updated . makeTreeCursorWithAbove tree $
                    Just ta {treeAboveRights = xs}

treeCursorRemoveElem :: TreeCursor a -> DeleteOrUpdate (TreeCursor a)
treeCursorRemoveElem tc =
    joinDeletes
        (treeCursorDeleteElemAndSelectPrevious tc)
        (treeCursorDeleteElemAndSelectNext tc)

treeCursorDeleteElem :: TreeCursor a -> DeleteOrUpdate (TreeCursor a)
treeCursorDeleteElem tc =
    joinDeletes
        (treeCursorDeleteElemAndSelectNext tc)
        (treeCursorDeleteElemAndSelectPrevious tc)

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
