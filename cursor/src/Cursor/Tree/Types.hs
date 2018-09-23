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
    -- * CTree
    , CTree(..)
    , makeCTree
    , cTree
    , rebuildCTree
    , CForest(..)
    , makeCForest
    , cForest
    , rebuildCForest
    , emptyCForest
    , openForest
    , lengthCForest
    , unpackCForest
    ) where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Tree
import Data.Validity
import Data.Validity.Tree ()

import GHC.Generics (Generic)

import Lens.Micro

data TreeCursor a b = TreeCursor
    { treeAbove :: !(Maybe (TreeAbove b))
    , treeCurrent :: !a
    , treeBelow :: !(CForest b)
    } deriving (Show, Eq, Generic)

treeCursorAboveL :: Lens' (TreeCursor a b) (Maybe (TreeAbove b))
treeCursorAboveL = lens treeAbove $ \tc ta -> tc {treeAbove = ta}

treeCursorCurrentL :: Lens' (TreeCursor a b) a
treeCursorCurrentL = lens treeCurrent $ \tc a -> tc {treeCurrent = a}

treeCursorBelowL :: Lens' (TreeCursor a b) (CForest b)
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
    | SelectChild !Int
                  !TreeCursorSelection
    deriving (Show, Eq, Generic)

instance Validity TreeCursorSelection

data CTree a =
    CNode !a
          !(CForest a)
    deriving (Show, Eq, Generic, Functor)

instance Validity a => Validity (CTree a)

makeCTree :: Tree a -> CTree a
makeCTree = cTree False

cTree :: Bool -> Tree a -> CTree a
cTree b (Node v f) = CNode v $ cForest b f

rebuildCTree :: CTree a -> Tree a
rebuildCTree (CNode v cf) = Node v $ rebuildCForest cf

data CForest a
    = ClosedForest ![Tree a]
    | OpenForest !(NonEmpty (CTree a))
    deriving (Show, Eq, Generic, Functor)

instance Validity a => Validity (CForest a)

makeCForest :: Forest a -> CForest a
makeCForest = cForest True

cForest :: Bool -> Forest a -> CForest a
cForest b f =
    if b
        then openForest $ map (cTree b) f
        else ClosedForest f

rebuildCForest :: CForest a -> Forest a
rebuildCForest (ClosedForest f) = f
rebuildCForest (OpenForest ct) = NE.toList $ NE.map rebuildCTree ct

emptyCForest :: CForest a
emptyCForest = ClosedForest []

openForest :: [CTree a] -> CForest a
openForest ts =
    case NE.nonEmpty ts of
        Nothing -> emptyCForest
        Just ne -> OpenForest ne

lengthCForest :: CForest a -> Int
lengthCForest (ClosedForest ts) = length ts
lengthCForest (OpenForest ts) = length ts

unpackCForest :: CForest a -> [CTree a]
unpackCForest (ClosedForest ts) = map makeCTree ts
unpackCForest (OpenForest ts) = NE.toList ts
