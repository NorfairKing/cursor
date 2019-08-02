{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

module Cursor.List
  ( ListCursor(..)
  , emptyListCursor
  , makeListCursor
  , makeListCursorSeq
  , makeListCursorWithSelection
  , makeListCursorWithSelectionSeq
  , rebuildListCursor
  , rebuildListCursorSeq
  , listCursorNull
  , listCursorLength
  , listCursorIndex
  , listCursorSelectPrev
  , listCursorSelectNext
  , listCursorSelectIndex
  , listCursorSelectStart
  , listCursorSelectEnd
  , listCursorPrevItem
  , listCursorNextItem
  , listCursorInsert
  , listCursorAppend
  , listCursorRemove
  , listCursorDelete
  , listCursorSplit
  , listCursorCombine
  , traverseListCursor
  , foldListCursor
  ) where

import GHC.Generics (Generic)

import Data.Foldable
import qualified Data.Sequence as S
import Data.Sequence (Seq(..), ViewL(..), ViewR(..), (<|), (|>))
import Data.Validity
import Data.Validity.Sequence ()

import Cursor.Types

data ListCursor a =
  ListCursor
    { listCursorPrev :: Seq a
    , listCursorNext :: Seq a
    }
  deriving (Show, Eq, Generic, Functor)

instance Validity a => Validity (ListCursor a)

emptyListCursor :: ListCursor a
emptyListCursor = ListCursor {listCursorPrev = S.empty, listCursorNext = S.empty}

makeListCursor :: [a] -> ListCursor a
makeListCursor = makeListCursorSeq . S.fromList

makeListCursorSeq :: Seq a -> ListCursor a
makeListCursorSeq as = ListCursor {listCursorPrev = S.empty, listCursorNext = as}

makeListCursorWithSelection :: Int -> [a] -> Maybe (ListCursor a)
makeListCursorWithSelection i = makeListCursorWithSelectionSeq i . S.fromList

makeListCursorWithSelectionSeq :: Int -> Seq a -> Maybe (ListCursor a)
makeListCursorWithSelectionSeq i as
  | i < 0 = Nothing
  | i > length as = Nothing
  | otherwise = Just ListCursor {listCursorPrev = S.take i as, listCursorNext = S.drop i as}

rebuildListCursor :: ListCursor a -> [a]
rebuildListCursor = toList . rebuildListCursorSeq

rebuildListCursorSeq :: ListCursor a -> Seq a
rebuildListCursorSeq ListCursor {..} = listCursorPrev <> listCursorNext

listCursorNull :: ListCursor a -> Bool
listCursorNull ListCursor {..} = null listCursorPrev && null listCursorNext

listCursorLength :: ListCursor a -> Int
listCursorLength = length . rebuildListCursor

listCursorIndex :: ListCursor a -> Int
listCursorIndex = length . listCursorPrev

listCursorSelectPrev :: ListCursor a -> Maybe (ListCursor a)
listCursorSelectPrev tc =
  case S.viewr $ listCursorPrev tc of
    EmptyR -> Nothing
    cs :> c -> Just ListCursor {listCursorPrev = cs, listCursorNext = c <| listCursorNext tc}

listCursorSelectNext :: ListCursor a -> Maybe (ListCursor a)
listCursorSelectNext tc =
  case S.viewl $ listCursorNext tc of
    EmptyL -> Nothing
    c :< cs -> Just ListCursor {listCursorPrev = listCursorPrev tc |> c, listCursorNext = cs}

listCursorSelectIndex :: Int -> ListCursor a -> ListCursor a
listCursorSelectIndex ix_ lc =
  let ls = rebuildListCursorSeq lc
   in case S.splitAt ix_ ls of
        (l, r) -> ListCursor {listCursorPrev = l, listCursorNext = r}

listCursorSelectStart :: ListCursor a -> ListCursor a
listCursorSelectStart tc =
  case listCursorSelectPrev tc of
    Nothing -> tc
    Just tc' -> listCursorSelectStart tc'

listCursorSelectEnd :: ListCursor a -> ListCursor a
listCursorSelectEnd tc =
  case listCursorSelectNext tc of
    Nothing -> tc
    Just tc' -> listCursorSelectEnd tc'

listCursorPrevItem :: ListCursor a -> Maybe a
listCursorPrevItem lc =
  case S.viewr $ listCursorPrev lc of
    EmptyR -> Nothing
    _ :> c -> Just c

listCursorNextItem :: ListCursor a -> Maybe a
listCursorNextItem lc =
  case S.viewl $ listCursorNext lc of
    EmptyL -> Nothing
    c :< _ -> Just c

listCursorInsert :: a -> ListCursor a -> ListCursor a
listCursorInsert c lc = lc {listCursorPrev = listCursorPrev lc |> c}

listCursorAppend :: a -> ListCursor a -> ListCursor a
listCursorAppend c lc = lc {listCursorNext = c <| listCursorNext lc}

listCursorRemove :: ListCursor a -> Maybe (DeleteOrUpdate (ListCursor a))
listCursorRemove tc =
  case S.viewr $ listCursorPrev tc of
    EmptyR ->
      case S.viewl $ listCursorNext tc of
        EmptyL -> Just Deleted
        _ -> Nothing
    prev :> _ -> Just $ Updated $ tc {listCursorPrev = prev}

listCursorDelete :: ListCursor a -> Maybe (DeleteOrUpdate (ListCursor a))
listCursorDelete tc =
  case S.viewl $ listCursorNext tc of
    EmptyL ->
      case S.viewr $ listCursorPrev tc of
        EmptyR -> Just Deleted
        _ -> Nothing
    _ :< next -> Just $ Updated $ tc {listCursorNext = next}

listCursorSplit :: ListCursor a -> (ListCursor a, ListCursor a)
listCursorSplit ListCursor {..} =
  ( ListCursor {listCursorPrev = listCursorPrev, listCursorNext = S.empty}
  , ListCursor {listCursorPrev = S.empty, listCursorNext = listCursorNext})

listCursorCombine :: ListCursor a -> ListCursor a -> ListCursor a
listCursorCombine lc1 lc2 =
  ListCursor {listCursorPrev = rebuildListCursorSeq lc1, listCursorNext = rebuildListCursorSeq lc2}

traverseListCursor :: (Seq a -> Seq a -> f b) -> ListCursor a -> f b
traverseListCursor = foldListCursor

foldListCursor :: (Seq a -> Seq a -> b) -> ListCursor a -> b
foldListCursor func ListCursor {..} = func listCursorPrev listCursorNext
