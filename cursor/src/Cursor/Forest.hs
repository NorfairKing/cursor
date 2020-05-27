{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Cursor.Forest
  ( ForestCursor(..)
  , makeForestCursor
  , rebuildForestCursor
  , drawForestCursor
  , mapForestCursor
  , forestCursorListCursorL
  , forestCursorSelectedTreeL
  , forestCursorSelectPrevTreeCursor
  , forestCursorSelectNextTreeCursor
  , forestCursorSelectFirstTreeCursor
  , forestCursorSelectLastTreeCursor
  , forestCursorSelectPrev
  , forestCursorSelectNext
  , forestCursorSelectPrevOnSameLevel
  , forestCursorSelectNextOnSameLevel
  , forestCursorSelectFirst
  , forestCursorSelectLast
  , forestCursorSelectFirstOnSameLevel
  , forestCursorSelectLastOnSameLevel
  , forestCursorSelectAbove
  , forestCursorSelectBelowAtPos
  , forestCursorSelectBelowAtStart
  , forestCursorSelectBelowAtEnd
  , forestCursorSelection
  , forestCursorSelectIndex
  , forestCursorOpenCurrentForest
  , forestCursorCloseCurrentForest
  , forestCursorToggleCurrentForest
  , forestCursorOpenCurrentForestRecursively
  , forestCursorToggleCurrentForestRecursively
  , forestCursorInsertEntireTree
  , forestCursorAppendEntireTree
  , forestCursorInsertAndSelectTreeCursor
  , forestCursorAppendAndSelectTreeCursor
  , forestCursorInsertTree
  , forestCursorAppendTree
  , forestCursorInsertAndSelectTree
  , forestCursorAppendAndSelectTree
  , forestCursorInsert
  , forestCursorAppend
  , forestCursorInsertAndSelect
  , forestCursorAppendAndSelect
  , forestCursorAddChildTreeToNodeAtPos
  , forestCursorAddChildTreeToNodeAtStart
  , forestCursorAddChildTreeToNodeAtEnd
  , forestCursorAddChildToNodeAtPos
  , forestCursorAddChildToNodeAtStart
  , forestCursorAddChildToNodeAtEnd
  , forestCursorAddChildTreeToNodeAtPosAndSelect
  , forestCursorAddChildTreeToNodeAtStartAndSelect
  , forestCursorAddChildTreeToNodeAtEndAndSelect
  , forestCursorAddChildToNodeAtPosAndSelect
  , forestCursorAddChildToNodeAtStartAndSelect
  , forestCursorAddChildToNodeAtEndAndSelect
  , forestCursorRemoveElemAndSelectPrev
  , forestCursorDeleteElemAndSelectNext
  , forestCursorRemoveElem
  , forestCursorDeleteElem
  , forestCursorRemoveSubTreeAndSelectPrev
  , forestCursorDeleteSubTreeAndSelectNext
  , forestCursorRemoveSubTree
  , forestCursorDeleteSubTree
  , forestCursorAddRoot
  , forestCursorSwapPrev
  , forestCursorSwapNext
  , forestCursorPromoteElem
  , forestCursorPromoteSubTree
  , forestCursorDemoteElem
  , forestCursorDemoteSubTree
  , forestCursorDemoteElemUnder
  , forestCursorDemoteSubTreeUnder
  , CTree(..)
  , makeCTree
  , cTree
  , rebuildCTree
  , CForest(..)
  , makeCForest
  , cForest
  , rebuildCForest
  , traverseForestCursor
  , foldForestCursor
  ) where

import Control.Applicative
import Control.DeepSeq
import Cursor.List.NonEmpty
import Cursor.Tree
import Cursor.Types
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe
import Data.Tree
import Data.Validity
import Data.Validity.Tree ()
import GHC.Generics (Generic)
import Lens.Micro

newtype ForestCursor a b =
  ForestCursor
    { forestCursorListCursor :: NonEmptyCursor (TreeCursor a b) (CTree b)
    }
  deriving (Show, Eq, Generic)

instance (Validity a, Validity b) => Validity (ForestCursor a b)

instance (NFData a, NFData b) => NFData (ForestCursor a b)

makeForestCursor :: (b -> a) -> NonEmpty (CTree b) -> ForestCursor a b
makeForestCursor g = ForestCursor . makeNonEmptyCursor (makeTreeCursor g)

rebuildForestCursor :: (a -> b) -> ForestCursor a b -> NonEmpty (CTree b)
rebuildForestCursor f = rebuildNonEmptyCursor (rebuildTreeCursor f) . forestCursorListCursor

drawForestCursor :: (Show a, Show b) => ForestCursor a b -> String
drawForestCursor ForestCursor {..} =
  drawForest $
  map showCTree (reverse $ nonEmptyCursorPrev forestCursorListCursor) ++
  [treeCursorWithPointer $ nonEmptyCursorCurrent forestCursorListCursor] ++
  map showCTree (nonEmptyCursorNext forestCursorListCursor)

mapForestCursor :: (a -> c) -> (b -> d) -> ForestCursor a b -> ForestCursor c d
mapForestCursor f g = forestCursorListCursorL %~ mapNonEmptyCursor (mapTreeCursor f g) (fmap g)

forestCursorListCursorL ::
     Lens (ForestCursor a b) (ForestCursor c d) (NonEmptyCursor (TreeCursor a b) (CTree b)) (NonEmptyCursor (TreeCursor c d) (CTree d))
forestCursorListCursorL = lens forestCursorListCursor $ \fc lc -> fc {forestCursorListCursor = lc}

forestCursorSelectedTreeL :: Lens' (ForestCursor a b) (TreeCursor a b)
forestCursorSelectedTreeL = forestCursorListCursorL . nonEmptyCursorElemL

forestCursorSelectPrevTreeCursor ::
     (a -> b) -> (b -> a) -> ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorSelectPrevTreeCursor f g =
  forestCursorListCursorL $ nonEmptyCursorSelectPrev (rebuildTreeCursor f) (makeTreeCursor g)

forestCursorSelectNextTreeCursor ::
     (a -> b) -> (b -> a) -> ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorSelectNextTreeCursor f g =
  forestCursorListCursorL $ nonEmptyCursorSelectNext (rebuildTreeCursor f) (makeTreeCursor g)

forestCursorSelectFirstTreeCursor :: (a -> b) -> (b -> a) -> ForestCursor a b -> ForestCursor a b
forestCursorSelectFirstTreeCursor f g =
  forestCursorListCursorL %~ nonEmptyCursorSelectFirst (rebuildTreeCursor f) (makeTreeCursor g)

forestCursorSelectLastTreeCursor :: (a -> b) -> (b -> a) -> ForestCursor a b -> ForestCursor a b
forestCursorSelectLastTreeCursor f g =
  forestCursorListCursorL %~ nonEmptyCursorSelectLast (rebuildTreeCursor f) (makeTreeCursor g)

forestCursorSelectNext :: (a -> b) -> (b -> a) -> ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorSelectNext f g fc =
  (fc & forestCursorSelectedTreeL (treeCursorSelectNext f g)) <|>
  forestCursorSelectNextTreeCursor f g fc

forestCursorSelectPrev :: (a -> b) -> (b -> a) -> ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorSelectPrev f g fc =
  (fc & forestCursorSelectedTreeL (treeCursorSelectPrev f g)) <|>
  (forestCursorSelectPrevTreeCursor f g fc >>=
   forestCursorSelectedTreeL (treeCursorSelectBelowAtEndRecursively f g)) <|>
  forestCursorSelectPrevTreeCursor f g fc

forestCursorSelectNextOnSameLevel ::
     (a -> b) -> (b -> a) -> ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorSelectNextOnSameLevel f g fc =
  (fc & forestCursorSelectedTreeL (treeCursorSelectNextOnSameLevel f g)) <|>
  forestCursorSelectNextTreeCursor f g fc

forestCursorSelectPrevOnSameLevel ::
     (a -> b) -> (b -> a) -> ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorSelectPrevOnSameLevel f g fc =
  (fc & forestCursorSelectedTreeL (treeCursorSelectPrevOnSameLevel f g)) <|>
  forestCursorSelectPrevTreeCursor f g fc

forestCursorSelectLastOnSameLevel :: (a -> b) -> (b -> a) -> ForestCursor a b -> ForestCursor a b
forestCursorSelectLastOnSameLevel f g fc =
  case forestCursorSelectNextOnSameLevel f g fc of
    Nothing -> fc
    Just fc' -> forestCursorSelectLastOnSameLevel f g fc'

forestCursorSelectFirstOnSameLevel :: (a -> b) -> (b -> a) -> ForestCursor a b -> ForestCursor a b
forestCursorSelectFirstOnSameLevel f g fc =
  case forestCursorSelectPrevOnSameLevel f g fc of
    Nothing -> fc
    Just fc' -> forestCursorSelectLastOnSameLevel f g fc'

forestCursorSelectFirst :: (a -> b) -> (b -> a) -> ForestCursor a b -> ForestCursor a b
forestCursorSelectFirst f g fc =
  case forestCursorSelectPrevTreeCursor f g fc of
    Just fc' -> forestCursorSelectFirst f g fc'
    Nothing ->
      case forestCursorSelectPrev f g fc of
        Just fc' -> forestCursorSelectFirst f g fc'
        Nothing -> fc

forestCursorSelectLast :: (a -> b) -> (b -> a) -> ForestCursor a b -> ForestCursor a b
forestCursorSelectLast f g fc =
  case forestCursorSelectNextTreeCursor f g fc of
    Just fc' -> forestCursorSelectLast f g fc'
    Nothing ->
      case forestCursorSelectNext f g fc of
        Just fc' -> forestCursorSelectLast f g fc'
        Nothing -> fc

forestCursorSelectAbove :: (a -> b) -> (b -> a) -> ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorSelectAbove f g = forestCursorSelectedTreeL $ treeCursorSelectAbove f g

forestCursorSelectBelowAtPos ::
     (a -> b) -> (b -> a) -> Int -> ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorSelectBelowAtPos f g i = forestCursorSelectedTreeL $ treeCursorSelectBelowAtPos f g i

forestCursorSelectBelowAtStart ::
     (a -> b) -> (b -> a) -> ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorSelectBelowAtStart f g = forestCursorSelectedTreeL $ treeCursorSelectBelowAtStart f g

forestCursorSelectBelowAtEnd :: (a -> b) -> (b -> a) -> ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorSelectBelowAtEnd f g = forestCursorSelectedTreeL $ treeCursorSelectBelowAtEnd f g

forestCursorSelection :: ForestCursor a b -> Int
forestCursorSelection fc = nonEmptyCursorSelection $ fc ^. forestCursorListCursorL

forestCursorSelectIndex ::
     (a -> b) -> (b -> a) -> Int -> ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorSelectIndex f g i =
  forestCursorListCursorL (nonEmptyCursorSelectIndex (rebuildTreeCursor f) (makeTreeCursor g) i)

forestCursorOpenCurrentForest :: ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorOpenCurrentForest = forestCursorSelectedTreeL treeCursorOpenCurrentForest

forestCursorCloseCurrentForest :: ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorCloseCurrentForest = forestCursorSelectedTreeL treeCursorCloseCurrentForest

forestCursorToggleCurrentForest :: ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorToggleCurrentForest = forestCursorSelectedTreeL treeCursorToggleCurrentForest

forestCursorOpenCurrentForestRecursively :: ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorOpenCurrentForestRecursively =
  forestCursorSelectedTreeL treeCursorOpenCurrentForestRecursively

forestCursorToggleCurrentForestRecursively :: ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorToggleCurrentForestRecursively =
  forestCursorSelectedTreeL treeCursorToggleCurrentForestRecursively

forestCursorInsertEntireTree :: Tree b -> ForestCursor a b -> ForestCursor a b
forestCursorInsertEntireTree t = forestCursorListCursorL %~ nonEmptyCursorInsert (makeCTree t)

forestCursorInsertAndSelectTreeCursor ::
     (a -> b) -> TreeCursor a b -> ForestCursor a b -> ForestCursor a b
forestCursorInsertAndSelectTreeCursor f tc =
  forestCursorListCursorL %~ nonEmptyCursorInsertAndSelect (rebuildTreeCursor f) tc

forestCursorAppendEntireTree :: Tree b -> ForestCursor a b -> ForestCursor a b
forestCursorAppendEntireTree t = forestCursorListCursorL %~ nonEmptyCursorAppend (makeCTree t)

forestCursorAppendAndSelectTreeCursor ::
     (a -> b) -> TreeCursor a b -> ForestCursor a b -> ForestCursor a b
forestCursorAppendAndSelectTreeCursor f tc =
  forestCursorListCursorL %~ nonEmptyCursorAppendAndSelect (rebuildTreeCursor f) tc

forestCursorInsertTree :: Tree b -> ForestCursor a b -> ForestCursor a b
forestCursorInsertTree t fc =
  fromMaybe (forestCursorInsertEntireTree t fc) $
  fc & forestCursorSelectedTreeL (treeCursorInsert t)

forestCursorInsertAndSelectTree ::
     (a -> b) -> (b -> a) -> Tree b -> ForestCursor a b -> ForestCursor a b
forestCursorInsertAndSelectTree f g t fc =
  fromMaybe (forestCursorInsertAndSelectTreeCursor f (makeTreeCursor g $ makeCTree t) fc) $
  fc & forestCursorSelectedTreeL (treeCursorInsertAndSelect f g t)

forestCursorAppendTree :: Tree b -> ForestCursor a b -> ForestCursor a b
forestCursorAppendTree t fc =
  fromMaybe (forestCursorAppendEntireTree t fc) $
  fc & forestCursorSelectedTreeL (treeCursorAppend t)

forestCursorAppendAndSelectTree ::
     (a -> b) -> (b -> a) -> Tree b -> ForestCursor a b -> ForestCursor a b
forestCursorAppendAndSelectTree f g t fc =
  fromMaybe (forestCursorAppendAndSelectTreeCursor f (makeTreeCursor g $ makeCTree t) fc) $
  fc & forestCursorSelectedTreeL (treeCursorAppendAndSelect f g t)

forestCursorInsert :: b -> ForestCursor a b -> ForestCursor a b
forestCursorInsert b = forestCursorInsertTree $ Node b []

forestCursorInsertAndSelect :: (a -> b) -> (b -> a) -> b -> ForestCursor a b -> ForestCursor a b
forestCursorInsertAndSelect f g b = forestCursorInsertAndSelectTree f g $ Node b []

forestCursorAppend :: b -> ForestCursor a b -> ForestCursor a b
forestCursorAppend b = forestCursorAppendTree $ Node b []

forestCursorAppendAndSelect :: (a -> b) -> (b -> a) -> b -> ForestCursor a b -> ForestCursor a b
forestCursorAppendAndSelect f g b = forestCursorAppendAndSelectTree f g $ Node b []

forestCursorAddChildTreeToNodeAtPos :: Int -> Tree b -> ForestCursor a b -> ForestCursor a b
forestCursorAddChildTreeToNodeAtPos i t = forestCursorSelectedTreeL %~ treeCursorAddChildAtPos i t

forestCursorAddChildTreeToNodeAtStart :: Tree b -> ForestCursor a b -> ForestCursor a b
forestCursorAddChildTreeToNodeAtStart t = forestCursorSelectedTreeL %~ treeCursorAddChildAtStart t

forestCursorAddChildTreeToNodeAtEnd :: Tree b -> ForestCursor a b -> ForestCursor a b
forestCursorAddChildTreeToNodeAtEnd t fc =
  fc & forestCursorSelectedTreeL %~ treeCursorAddChildAtEnd t

forestCursorAddChildToNodeAtPos :: Int -> b -> ForestCursor a b -> ForestCursor a b
forestCursorAddChildToNodeAtPos i b = forestCursorAddChildTreeToNodeAtPos i $ Node b []

forestCursorAddChildToNodeAtStart :: b -> ForestCursor a b -> ForestCursor a b
forestCursorAddChildToNodeAtStart b = forestCursorAddChildTreeToNodeAtStart $ Node b []

forestCursorAddChildToNodeAtEnd :: b -> ForestCursor a b -> ForestCursor a b
forestCursorAddChildToNodeAtEnd b = forestCursorAddChildTreeToNodeAtEnd $ Node b []

forestCursorAddChildTreeToNodeAtPosAndSelect ::
     (a -> b) -> (b -> a) -> Int -> Tree b -> ForestCursor a b -> ForestCursor a b
forestCursorAddChildTreeToNodeAtPosAndSelect f g i t =
  forestCursorSelectedTreeL %~ treeCursorAddChildAtPosAndSelect f g i t

forestCursorAddChildTreeToNodeAtStartAndSelect ::
     (a -> b) -> (b -> a) -> Tree b -> ForestCursor a b -> ForestCursor a b
forestCursorAddChildTreeToNodeAtStartAndSelect f g t =
  forestCursorSelectedTreeL %~ treeCursorAddChildAtStartAndSelect f g t

forestCursorAddChildTreeToNodeAtEndAndSelect ::
     (a -> b) -> (b -> a) -> Tree b -> ForestCursor a b -> ForestCursor a b
forestCursorAddChildTreeToNodeAtEndAndSelect f g t fc =
  fc & forestCursorSelectedTreeL %~ treeCursorAddChildAtEndAndSelect f g t

forestCursorAddChildToNodeAtPosAndSelect ::
     (a -> b) -> (b -> a) -> Int -> b -> ForestCursor a b -> ForestCursor a b
forestCursorAddChildToNodeAtPosAndSelect f g i b =
  forestCursorAddChildTreeToNodeAtPosAndSelect f g i $ Node b []

forestCursorAddChildToNodeAtStartAndSelect ::
     (a -> b) -> (b -> a) -> b -> ForestCursor a b -> ForestCursor a b
forestCursorAddChildToNodeAtStartAndSelect f g b =
  forestCursorAddChildTreeToNodeAtStartAndSelect f g $ Node b []

forestCursorAddChildToNodeAtEndAndSelect ::
     (a -> b) -> (b -> a) -> b -> ForestCursor a b -> ForestCursor a b
forestCursorAddChildToNodeAtEndAndSelect f g b =
  forestCursorAddChildTreeToNodeAtEndAndSelect f g $ Node b []

forestCursorRemoveElemAndSelectPrev ::
     (b -> a) -> ForestCursor a b -> Maybe (DeleteOrUpdate (ForestCursor a b))
forestCursorRemoveElemAndSelectPrev g fc =
  case fc &
       focusPossibleDeleteOrUpdate
         forestCursorSelectedTreeL
         (treeCursorDeleteElemAndSelectPrevious g) of
    Just Deleted ->
      fc &
      focusPossibleDeleteOrUpdate
        forestCursorListCursorL
        (nonEmptyCursorRemoveElemAndSelectPrev (makeTreeCursor g))
    r -> r

forestCursorDeleteElemAndSelectNext ::
     (b -> a) -> ForestCursor a b -> Maybe (DeleteOrUpdate (ForestCursor a b))
forestCursorDeleteElemAndSelectNext g fc =
  case fc &
       focusPossibleDeleteOrUpdate forestCursorSelectedTreeL (treeCursorDeleteElemAndSelectNext g) of
    Just Deleted ->
      fc &
      focusPossibleDeleteOrUpdate
        forestCursorListCursorL
        (nonEmptyCursorDeleteElemAndSelectNext (makeTreeCursor g))
    r -> r

forestCursorRemoveElem :: (b -> a) -> ForestCursor a b -> DeleteOrUpdate (ForestCursor a b)
forestCursorRemoveElem g fc =
  (fc & forestCursorSelectedTreeL (treeCursorRemoveElem g)) <|>
  (fc & forestCursorListCursorL (nonEmptyCursorRemoveElem (makeTreeCursor g)))

forestCursorDeleteElem :: (b -> a) -> ForestCursor a b -> DeleteOrUpdate (ForestCursor a b)
forestCursorDeleteElem g fc =
  (fc & forestCursorSelectedTreeL (treeCursorDeleteElem g)) <|>
  (fc & forestCursorListCursorL (nonEmptyCursorDeleteElem (makeTreeCursor g)))

forestCursorRemoveSubTreeAndSelectPrev ::
     (b -> a) -> ForestCursor a b -> Maybe (DeleteOrUpdate (ForestCursor a b))
forestCursorRemoveSubTreeAndSelectPrev g fc =
  joinPossibleDeletes
    (fc &
     focusPossibleDeleteOrUpdate
       forestCursorSelectedTreeL
       (treeCursorDeleteSubTreeAndSelectPrevious g))
    (fc &
     focusPossibleDeleteOrUpdate
       forestCursorListCursorL
       (nonEmptyCursorRemoveElemAndSelectPrev (makeTreeCursor g)))

forestCursorDeleteSubTreeAndSelectNext ::
     (b -> a) -> ForestCursor a b -> Maybe (DeleteOrUpdate (ForestCursor a b))
forestCursorDeleteSubTreeAndSelectNext g fc =
  joinPossibleDeletes
    (fc &
     focusPossibleDeleteOrUpdate forestCursorSelectedTreeL (treeCursorDeleteSubTreeAndSelectNext g))
    (fc &
     focusPossibleDeleteOrUpdate
       forestCursorListCursorL
       (nonEmptyCursorDeleteElemAndSelectNext (makeTreeCursor g)))

forestCursorRemoveSubTree :: (b -> a) -> ForestCursor a b -> DeleteOrUpdate (ForestCursor a b)
forestCursorRemoveSubTree g fc =
  (fc & forestCursorSelectedTreeL (treeCursorRemoveSubTree g)) <|>
  (fc & forestCursorListCursorL (nonEmptyCursorRemoveElem (makeTreeCursor g)))

forestCursorDeleteSubTree :: (b -> a) -> ForestCursor a b -> DeleteOrUpdate (ForestCursor a b)
forestCursorDeleteSubTree g fc =
  (fc & forestCursorSelectedTreeL (treeCursorDeleteSubTree g)) <|>
  (fc & forestCursorListCursorL (nonEmptyCursorDeleteElem (makeTreeCursor g)))

forestCursorAddRoot :: (a -> b) -> (b -> a) -> ForestCursor a b -> a -> TreeCursor a b
forestCursorAddRoot f g fc v =
  makeTreeCursor g $ CNode (f v) $ OpenForest $ rebuildForestCursor f fc

-- | Swaps the current node with the previous node on the same level
--
-- Example:
--
-- Before:
--
-- > - a
-- > - b <--
--
-- After:
--
-- > - b <--
-- > - a
forestCursorSwapPrev :: ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorSwapPrev fc@(ForestCursor ne) =
  case fc & forestCursorSelectedTreeL treeCursorSwapPrev of
    Swapped fc' -> pure fc'
    NoSiblingsToSwapWith -> Nothing
    SwapperIsTopNode ->
      case nonEmptyCursorPrev ne of
        [] -> Nothing
        (t:ts) ->
          pure $
          ForestCursor ne {nonEmptyCursorPrev = ts, nonEmptyCursorNext = t : nonEmptyCursorNext ne}

-- | Swaps the current node with the next node on the same level
--
-- Example:
--
-- Before:
--
-- > - a <--
-- > - b
--
-- After:
--
-- > - b
-- > - a <--
forestCursorSwapNext :: ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorSwapNext fc@(ForestCursor ne) =
  case fc & forestCursorSelectedTreeL treeCursorSwapNext of
    Swapped fc' -> pure fc'
    NoSiblingsToSwapWith -> Nothing
    SwapperIsTopNode ->
      case nonEmptyCursorNext ne of
        [] -> Nothing
        (t:ts) ->
          pure $
          ForestCursor ne {nonEmptyCursorPrev = t : nonEmptyCursorPrev ne, nonEmptyCursorNext = ts}

-- | Promotes the current node to the level of its parent.
--
-- Example:
--
-- Before:
--
-- > - a
-- >   |- b
-- >   |  |- c
-- >   |- d <--
-- >   |  |- e
-- >   |- f
-- >      |- g
-- > - h
--
-- After:
--
-- > - a
-- >   |- b
-- >   |  |- c
-- >   |  |- e
-- >   |- f
-- >      |- g
-- > - d <--
-- > - h
forestCursorPromoteElem :: (a -> b) -> (b -> a) -> ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorPromoteElem f g fc@(ForestCursor ne) =
  case fc & forestCursorSelectedTreeL (treeCursorPromoteElem f g) of
    PromotedElem fc' -> pure fc'
    CannotPromoteTopElem -> Nothing
    NoSiblingsToAdoptChildren -> Nothing
    NoGrandparentToPromoteElemUnder -> do
      let tc = fc ^. forestCursorSelectedTreeL
      ta <- treeAbove tc
      lefts <-
        case treeBelow tc of
          EmptyCForest -> pure $ treeAboveLefts ta
          _ ->
            case treeAboveLefts ta of
              [] -> Nothing
              (CNode t ls:ts) ->
                pure $ CNode t (openForest $ unpackCForest ls ++ unpackCForest (treeBelow tc)) : ts
      let ta' = ta {treeAboveLefts = lefts}
      let tc' = tc {treeAbove = Just ta'}
      tc'' <-
        case treeCursorDeleteSubTree g tc' of
          Deleted -> Nothing -- Cannot happen, otherwise we would have gotten 'CannotPromoteTopNode'.
          Updated tc'' -> pure tc''
      pure $
        ForestCursor $
        ne
          { nonEmptyCursorPrev = rebuildTreeCursor f tc'' : nonEmptyCursorPrev ne
          , nonEmptyCursorCurrent =
              singletonTreeCursor $ treeCurrent $ fc ^. forestCursorSelectedTreeL
          }

-- | Promotes the current node to the level of its parent.
--
-- Example:
--
-- Before:
--
-- >  - a
-- >    |- b
-- >    |  |- c
-- >    |- d <--
-- >    |  |- e
-- >    |- f
-- >       |- g
-- >  - h
--
-- After:
--
-- >
-- > - a
-- >   |- b
-- >   |  |- c
-- >   |- f
-- >      |- g
-- > - d <--
-- >   |- e
-- > - h
forestCursorPromoteSubTree :: (a -> b) -> (b -> a) -> ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorPromoteSubTree f g fc@(ForestCursor ne) =
  case fc & forestCursorSelectedTreeL (treeCursorPromoteSubTree f g) of
    Promoted fc' -> pure fc'
    CannotPromoteTopNode -> Nothing
    NoGrandparentToPromoteUnder ->
      case treeCursorDeleteSubTree g $ fc ^. forestCursorSelectedTreeL of
        Deleted -> Nothing -- Cannot happen, otherwise we would have gotten 'CannotPromoteTopNode'.
        Updated tc' ->
          pure $
          ForestCursor $
          ne
            { nonEmptyCursorPrev = rebuildTreeCursor f tc' : nonEmptyCursorPrev ne
            , nonEmptyCursorCurrent = (fc ^. forestCursorSelectedTreeL) {treeAbove = Nothing}
            }

-- | Demotes the current node to the level of its children.
--
-- Example:
--
-- Before:
--
-- > - a
-- >   |- b
-- > - c <--
-- >   |- d
-- > - e
--
-- After:
--
-- > - a
-- >   |- b
-- >   |- c <--
-- >   |- d
-- > - e
forestCursorDemoteElem :: (a -> b) -> (b -> a) -> ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorDemoteElem f g fc@(ForestCursor ne) =
  case fc & forestCursorSelectedTreeL (treeCursorDemoteElem f g) of
    Demoted fc' -> pure fc'
    CannotDemoteTopNode ->
      case nonEmptyCursorPrev ne of
        [] -> Nothing
        (CNode v vts:ts) -> do
          let CNode v' vts' = rebuildTreeCursor f (fc ^. forestCursorSelectedTreeL)
          let n' =
                CNode v $
                openForest $ unpackCForest vts ++ CNode v' emptyCForest : unpackCForest vts'
          tc <- makeTreeCursorWithSelection f g (SelectChild (lengthCForest vts) SelectNode) n'
          pure $ ForestCursor ne {nonEmptyCursorPrev = ts, nonEmptyCursorCurrent = tc}
    NoSiblingsToDemoteUnder -> Nothing

-- | Demotes the current subtree to the level of its children.
--
-- Example:
--
-- Before:
--
-- >  - a
-- >    |- b
-- >  - c <--
-- >    |- d
--
-- After:
--
-- >  - a
-- >    |- b
-- >    |- c <--
-- >       |- d
forestCursorDemoteSubTree :: (a -> b) -> (b -> a) -> ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorDemoteSubTree f g fc@(ForestCursor ne) =
  case fc & forestCursorSelectedTreeL (treeCursorDemoteSubTree f g) of
    Demoted fc' -> pure fc'
    CannotDemoteTopNode ->
      case nonEmptyCursorPrev ne of
        [] -> Nothing
        (CNode v vts:ts) -> do
          let n' =
                CNode v $
                openForest $
                unpackCForest vts ++ [rebuildTreeCursor f (fc ^. forestCursorSelectedTreeL)]
          tc <- makeTreeCursorWithSelection f g (SelectChild (lengthCForest vts) SelectNode) n'
          pure $ ForestCursor ne {nonEmptyCursorPrev = ts, nonEmptyCursorCurrent = tc}
    NoSiblingsToDemoteUnder -> Nothing

-- | Demotes the current node to the level of its children, by adding two roots.
-- One for the current node and one for its children that are left behind.
--
-- Example:
--
-- Before:
--
-- >  - a <--
-- >    |- b
--
-- After:
--
-- >  - <given element 1>
-- >    |- a <--
-- >  - <given element 2>
-- >    |- b
forestCursorDemoteElemUnder :: b -> b -> ForestCursor a b -> ForestCursor a b
forestCursorDemoteElemUnder b1 b2 fc@(ForestCursor ne) =
  case fc & forestCursorSelectedTreeL (treeCursorDemoteElemUnder b1 b2) of
    Just fc' -> fc'
    Nothing ->
      let t = fc ^. forestCursorSelectedTreeL
       in ForestCursor $
          ne
            { nonEmptyCursorCurrent =
                TreeCursor
                  { treeAbove =
                      Just
                        TreeAbove
                          { treeAboveLefts = []
                          , treeAboveAbove = Nothing
                          , treeAboveNode = b1
                          , treeAboveRights = []
                          }
                  , treeCurrent = treeCurrent t
                  , treeBelow = emptyCForest
                  }
            , nonEmptyCursorNext = CNode b2 (treeBelow t) : nonEmptyCursorNext ne
            }

-- | Demotes the current subtree to the level of its children, by adding a root.
--
-- Example:
--
-- Before:
--
-- >  a <--
-- >  |- b
--
-- After:
--
-- >  <given element>
-- >  |- a <--
-- >     |- b
forestCursorDemoteSubTreeUnder :: b -> ForestCursor a b -> ForestCursor a b
forestCursorDemoteSubTreeUnder b = forestCursorSelectedTreeL %~ treeCursorDemoteSubTreeUnder b

traverseForestCursor :: ([CTree b] -> TreeCursor a b -> [CTree b] -> f c) -> ForestCursor a b -> f c
traverseForestCursor = foldForestCursor

foldForestCursor :: ([CTree b] -> TreeCursor a b -> [CTree b] -> c) -> ForestCursor a b -> c
foldForestCursor func (ForestCursor ne) = foldNonEmptyCursor func ne
