{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Cursor.List.NonEmptySpec
  ( spec,
  )
where

import Cursor.List.NonEmpty
import qualified Data.List.NonEmpty as NE
import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
  describe "nonemptyPrepend"
    $ it "is equivalent to regular prepend"
    $ equivalentWhenFirstSucceeds
      (\(ls1, ls2) -> NE.toList . nonemptyPrepend ls1 <$> NE.nonEmpty ls2)
      (uncurry (++) :: ([Bool], [Bool]) -> [Bool])
  describe "nonemptyAppend"
    $ it "is equivalent to regular append"
    $ equivalentWhenFirstSucceeds
      (\(ls1, ls2) -> NE.toList . (`nonemptyAppend` ls2) <$> NE.nonEmpty ls1)
      (uncurry (++) :: ([Bool], [Bool]) -> [Bool])
