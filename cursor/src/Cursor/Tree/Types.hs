{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

module Cursor.Tree.Types
    ( TreeCursor(..)
    , treeCursorAboveL
    , treeCursorCurrentL
    , treeCursorBelowL
    , TreeAbove(..)
    , treeAboveLeftsL
    , treeAboveAboveL
    , treeAboveNodeL
    , treeAboveRightsL
    , TreeCursorSelection(..)
    -- * Collapse
    , Collapse(..)
    , makeCollapse
    , collapse
    , rebuildCollapse
    , collapseValueL
    , collapseShowL
    -- * CTree
    , CTree(..)
    , CForest
    , makeCTree
    , cTree
    , rebuildCTree
    ) where

import Data.Tree
import Data.Validity
import Data.Validity.Tree ()

import GHC.Generics (Generic)

import Control.Applicative
import Control.Monad

import Lens.Micro

import Cursor.Types

data TreeCursor a b = TreeCursor
    { treeAbove :: !(Maybe (TreeAbove b))
    , treeCurrent :: !a
    , treeBelow :: !(Collapse (CForest b))
    } deriving (Show, Eq, Generic)

currentTree :: (a -> b) -> TreeCursor a b -> CTree b
currentTree f TreeCursor {..} = CNode (f treeCurrent) treeBelow

treeCursorAboveL :: Lens' (TreeCursor a b) (Maybe (TreeAbove b))
treeCursorAboveL = lens treeAbove $ \tc ta -> tc {treeAbove = ta}

treeCursorCurrentL :: Lens' (TreeCursor a b) a
treeCursorCurrentL = lens treeCurrent $ \tc a -> tc {treeCurrent = a}

treeCursorBelowL :: Lens' (TreeCursor a b) (Collapse (CForest b))
treeCursorBelowL = lens treeBelow $ \tc tb -> tc {treeBelow = tb}

instance (Validity a, Validity b) => Validity (TreeCursor a b)

data TreeAbove b = TreeAbove
    { treeAboveLefts :: ![CTree b] -- In reverse order
    , treeAboveAbove :: !(Maybe (TreeAbove b))
    , treeAboveNode :: !b
    , treeAboveRights :: ![CTree b]
    } deriving (Show, Eq, Generic, Functor)

instance Validity b => Validity (TreeAbove b)

treeAboveLeftsL :: Lens' (TreeAbove b) [CTree b]
treeAboveLeftsL = lens treeAboveLefts $ \ta tal -> ta {treeAboveLefts = tal}

treeAboveAboveL :: Lens' (TreeAbove b) (Maybe (TreeAbove b))
treeAboveAboveL = lens treeAboveAbove $ \ta taa -> ta {treeAboveAbove = taa}

treeAboveNodeL :: Lens' (TreeAbove b) b
treeAboveNodeL = lens treeAboveNode $ \ta a -> ta {treeAboveNode = a}

treeAboveRightsL :: Lens' (TreeAbove b) [CTree b]
treeAboveRightsL = lens treeAboveRights $ \ta tar -> ta {treeAboveRights = tar}

data TreeCursorSelection
    = SelectNode
    | SelectChild Int
                  TreeCursorSelection
    deriving (Show, Eq, Generic)

instance Validity TreeCursorSelection

data Collapse a = Collapse
    { collapseValue :: !a
    , collapseShow :: !Bool
    } deriving (Show, Eq, Generic, Functor)

instance Validity a => Validity (Collapse a)

makeCollapse :: a -> Collapse a
makeCollapse = collapse True

collapse :: Bool -> a -> Collapse a
collapse b a = Collapse {collapseValue = a, collapseShow = b}

rebuildCollapse :: Collapse a -> a
rebuildCollapse = collapseValue

collapseValueL :: Lens (Collapse a) (Collapse b) a b
collapseValueL = lens collapseValue $ \c v -> c {collapseValue = v}

collapseShowL :: Lens' (Collapse a) Bool
collapseShowL = lens collapseShow $ \c b -> c {collapseShow = b}

data CTree a =
    CNode !a
          !(Collapse (CForest a))
    deriving (Show, Eq, Generic, Functor)

instance Validity a => Validity (CTree a)

type CForest a = [CTree a]

makeCTree :: Tree a -> CTree a
makeCTree = cTree True

cTree :: Bool -> Tree a -> CTree a
cTree b (Node v f) = CNode v $ collapse b $ map (cTree b) f

rebuildCTree :: CTree a -> Tree a
rebuildCTree (CNode v c) = Node v $ map rebuildCTree $ collapseValue c
