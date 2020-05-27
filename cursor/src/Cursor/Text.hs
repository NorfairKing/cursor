{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Cursor.Text
  ( TextCursor(..)
  , emptyTextCursor
  , makeTextCursor
  , makeTextCursorWithSelection
  , rebuildTextCursor
  , textCursorNull
  , textCursorLength
  , textCursorIndex
  , textCursorSelectPrev
  , textCursorSelectNext
  , textCursorSelectIndex
  , textCursorSelectStart
  , textCursorSelectEnd
  , textCursorPrevChar
  , textCursorNextChar
  , textCursorInsert
  , textCursorAppend
  , textCursorInsertString
  , textCursorAppendString
  , textCursorInsertText
  , textCursorAppendText
  , textCursorRemove
  , textCursorDelete
  , textCursorSplit
  , textCursorCombine
  ) where

import Control.DeepSeq
import Cursor.List
import Cursor.Types
import qualified Data.Text as T
import Data.Text (Text)
import Data.Validity
import GHC.Generics (Generic)
import Lens.Micro

-- | A cursor for single-line texts
newtype TextCursor =
  TextCursor
    { textCursorList :: ListCursor Char
    }
  deriving (Show, Eq, Generic)

instance Validity TextCursor where
  validate (TextCursor lc) =
    mconcat
      [ genericValidate lc
      , decorateList (rebuildListCursor lc) $ \c ->
          mconcat
            [ declare "The character is not a newline character" $ c /= '\n'
            , declare "The character is a safe character" $ isSafeChar c
            ]
      ]

instance NFData TextCursor

emptyTextCursor :: TextCursor
emptyTextCursor = TextCursor emptyListCursor

makeTextCursor :: Text -> Maybe TextCursor
makeTextCursor t = makeTextCursorWithSelection (T.length t) t

makeTextCursorWithSelection :: Int -> Text -> Maybe TextCursor
makeTextCursorWithSelection i t =
  case T.split (== '\n') t of
    [l] -> TextCursor <$> makeListCursorWithSelection i (T.unpack l)
    _ -> Nothing

rebuildTextCursor :: TextCursor -> Text
rebuildTextCursor = T.pack . rebuildListCursor . textCursorList

textCursorListCursorL ::
     Functor f => (ListCursor Char -> f (ListCursor Char)) -> TextCursor -> f TextCursor
textCursorListCursorL = lens textCursorList (\tc lc -> tc {textCursorList = lc})

textCursorNull :: TextCursor -> Bool
textCursorNull = listCursorNull . textCursorList

textCursorLength :: TextCursor -> Int
textCursorLength = listCursorLength . textCursorList

textCursorIndex :: TextCursor -> Int
textCursorIndex = listCursorIndex . textCursorList

textCursorSelectPrev :: TextCursor -> Maybe TextCursor
textCursorSelectPrev = textCursorListCursorL listCursorSelectPrev

textCursorSelectNext :: TextCursor -> Maybe TextCursor
textCursorSelectNext = textCursorListCursorL listCursorSelectNext

textCursorSelectIndex :: Int -> TextCursor -> TextCursor
textCursorSelectIndex ix_ = textCursorListCursorL %~ listCursorSelectIndex ix_

textCursorSelectStart :: TextCursor -> TextCursor
textCursorSelectStart = textCursorListCursorL %~ listCursorSelectStart

textCursorSelectEnd :: TextCursor -> TextCursor
textCursorSelectEnd = textCursorListCursorL %~ listCursorSelectEnd

textCursorPrevChar :: TextCursor -> Maybe Char
textCursorPrevChar = listCursorPrevItem . textCursorList

textCursorNextChar :: TextCursor -> Maybe Char
textCursorNextChar = listCursorNextItem . textCursorList

textCursorInsert :: Char -> TextCursor -> Maybe TextCursor
textCursorInsert '\n' _ = Nothing
textCursorInsert c tc =
  if isSafeChar c
    then Just (tc & textCursorListCursorL %~ listCursorInsert c)
    else Nothing

textCursorAppend :: Char -> TextCursor -> Maybe TextCursor
textCursorAppend '\n' _ = Nothing
textCursorAppend c tc =
  if isSafeChar c
    then Just (tc & textCursorListCursorL %~ listCursorAppend c)
    else Nothing

textCursorInsertString :: String -> TextCursor -> Maybe TextCursor
textCursorInsertString s tc =
  if any (\c -> c == '\n' || not (isSafeChar c)) s
    then Nothing
    else Just $ tc & textCursorListCursorL %~ listCursorInsertList s

textCursorAppendString :: String -> TextCursor -> Maybe TextCursor
textCursorAppendString s tc =
  if any (\c -> c == '\n' || not (isSafeChar c)) s
    then Nothing
    else Just $ tc & textCursorListCursorL %~ listCursorAppendList s

textCursorInsertText :: Text -> TextCursor -> Maybe TextCursor
textCursorInsertText = textCursorInsertString . T.unpack

textCursorAppendText :: Text -> TextCursor -> Maybe TextCursor
textCursorAppendText = textCursorAppendString . T.unpack

textCursorRemove :: TextCursor -> Maybe (DeleteOrUpdate TextCursor)
textCursorRemove = focusPossibleDeleteOrUpdate textCursorListCursorL listCursorRemove

textCursorDelete :: TextCursor -> Maybe (DeleteOrUpdate TextCursor)
textCursorDelete = focusPossibleDeleteOrUpdate textCursorListCursorL listCursorDelete

textCursorSplit :: TextCursor -> (TextCursor, TextCursor)
textCursorSplit tc =
  let (lc1, lc2) = listCursorSplit $ textCursorList tc
   in (TextCursor lc1, TextCursor lc2)

textCursorCombine :: TextCursor -> TextCursor -> TextCursor
textCursorCombine (TextCursor lc1) (TextCursor lc2) =
  TextCursor {textCursorList = listCursorCombine lc1 lc2}
