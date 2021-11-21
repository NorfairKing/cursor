{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Cursor.TextField
  ( TextFieldCursor (..),
    makeTextFieldCursor,
    makeTextFieldCursorWithSelection,
    rebuildTextFieldCursorLines,
    rebuildTextFieldCursor,
    emptyTextFieldCursor,
    nullTextFieldCursor,
    textFieldCursorSelection,
    textFieldCursorNonEmptyCursorL,
    textFieldCursorSelectedL,
    textFieldCursorSelectPrevLine,
    textFieldCursorSelectNextLine,
    textFieldCursorSelectFirstLine,
    textFieldCursorSelectLastLine,
    textFieldCursorSelectPrevChar,
    textFieldCursorSelectNextChar,
    textFieldCursorSelectPrevWord,
    textFieldCursorSelectNextWord,
    textFieldCursorSelectBeginWord,
    textFieldCursorSelectEndWord,
    textFieldCursorIndexOnLine,
    textFieldCursorSelectIndexOnLine,
    textFieldCursorInsertChar,
    textFieldCursorAppendChar,
    textFieldCursorInsertNewline,
    textFieldCursorAppendNewline,
    textFieldCursorRemove,
    textFieldCursorDelete,
    textFieldCursorSelectStartOfLine,
    textFieldCursorSelectEndOfLine,
  )
where

import Control.DeepSeq
import Control.Monad
import Cursor.List.NonEmpty
import Cursor.Text
import Cursor.Types
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text ()
import GHC.Generics (Generic)
import Lens.Micro

newtype TextFieldCursor = TextFieldCursor
  { textFieldCursorNonEmpty :: NonEmptyCursor TextCursor Text
  }
  deriving (Show, Eq, Generic)

instance Validity TextFieldCursor where
  validate tfc@TextFieldCursor {..} =
    mconcat
      [ genericValidate tfc,
        decorate "None of the texts contain newlines" $
          decorateList (NE.toList $ rebuildNonEmptyCursor rebuildTextCursor textFieldCursorNonEmpty) $
            \tc ->
              declare "The text of this line does not contain any newlines" $ T.all (/= '\n') tc
      ]

instance NFData TextFieldCursor

makeTextFieldCursor :: Text -> TextFieldCursor
makeTextFieldCursor = fromJust . makeTextFieldCursorWithSelection 0 0

makeTextFieldCursorWithSelection :: Int -> Int -> Text -> Maybe TextFieldCursor
makeTextFieldCursorWithSelection x y t = do
  ls <- NE.nonEmpty $ T.split (== '\n') t
  guard (x >= 0)
  guard (x < NE.length ls)
  nec <- makeNonEmptyCursorWithSelection (makeTextCursorWithSelection y) x ls
  void $ nonEmptyCursorCurrent nec
  -- This is safe because we already checked that it would work above
  pure $ TextFieldCursor (nec & nonEmptyCursorElemL %~ fromJust)

rebuildTextFieldCursorLines :: TextFieldCursor -> NonEmpty Text
rebuildTextFieldCursorLines = rebuildNonEmptyCursor rebuildTextCursor . textFieldCursorNonEmpty

rebuildTextFieldCursor :: TextFieldCursor -> Text
rebuildTextFieldCursor = T.intercalate "\n" . NE.toList . rebuildTextFieldCursorLines

emptyTextFieldCursor :: TextFieldCursor
emptyTextFieldCursor =
  TextFieldCursor {textFieldCursorNonEmpty = singletonNonEmptyCursor emptyTextCursor}

nullTextFieldCursor :: TextFieldCursor -> Bool
nullTextFieldCursor = (== emptyTextFieldCursor)

textFieldCursorSelection :: TextFieldCursor -> (Int, Int)
textFieldCursorSelection tfc =
  ( nonEmptyCursorSelection $ textFieldCursorNonEmpty tfc,
    textCursorIndex $ textFieldCursorNonEmpty tfc ^. nonEmptyCursorElemL
  )

textFieldCursorNonEmptyCursorL :: Lens' TextFieldCursor (NonEmptyCursor TextCursor Text)
textFieldCursorNonEmptyCursorL =
  lens textFieldCursorNonEmpty $ \tfc lec -> tfc {textFieldCursorNonEmpty = lec}

textFieldCursorSelectedL :: Lens' TextFieldCursor TextCursor
textFieldCursorSelectedL = textFieldCursorNonEmptyCursorL . nonEmptyCursorElemL

textFieldCursorSelectPrevLine :: TextFieldCursor -> Maybe TextFieldCursor
textFieldCursorSelectPrevLine =
  moveMWhileKeepingSelection $ nonEmptyCursorSelectPrev rebuildTextCursor unsafeMakeTextCursor

textFieldCursorSelectNextLine :: TextFieldCursor -> Maybe TextFieldCursor
textFieldCursorSelectNextLine =
  moveMWhileKeepingSelection $ nonEmptyCursorSelectNext rebuildTextCursor unsafeMakeTextCursor

moveMWhileKeepingSelection ::
  (NonEmptyCursor TextCursor Text -> Maybe (NonEmptyCursor TextCursor Text)) ->
  TextFieldCursor ->
  Maybe TextFieldCursor
moveMWhileKeepingSelection movement tfc = do
  let i = textFieldCursorIndexOnLine tfc
  let tfc' = textFieldCursorSelectIndexOnLine 0 tfc
  tfc'' <- textFieldCursorNonEmptyCursorL movement tfc'
  pure $ textFieldCursorSelectIndexOnLine i tfc''

textFieldCursorSelectFirstLine :: TextFieldCursor -> TextFieldCursor
textFieldCursorSelectFirstLine =
  moveWhileKeepingSelection $ nonEmptyCursorSelectFirst rebuildTextCursor unsafeMakeTextCursor

textFieldCursorSelectLastLine :: TextFieldCursor -> TextFieldCursor
textFieldCursorSelectLastLine =
  moveWhileKeepingSelection $ nonEmptyCursorSelectLast rebuildTextCursor unsafeMakeTextCursor

moveWhileKeepingSelection ::
  (NonEmptyCursor TextCursor Text -> NonEmptyCursor TextCursor Text) ->
  TextFieldCursor ->
  TextFieldCursor
moveWhileKeepingSelection movement tfc =
  let i = textFieldCursorIndexOnLine tfc
      tfc' = textFieldCursorSelectIndexOnLine 0 tfc
      tfc'' = tfc' & textFieldCursorNonEmptyCursorL %~ movement
   in textFieldCursorSelectIndexOnLine i tfc''

textFieldCursorSelectPrevChar :: TextFieldCursor -> Maybe TextFieldCursor
textFieldCursorSelectPrevChar = textFieldCursorSelectedL textCursorSelectPrev

textFieldCursorSelectNextChar :: TextFieldCursor -> Maybe TextFieldCursor
textFieldCursorSelectNextChar = textFieldCursorSelectedL textCursorSelectNext

textFieldCursorSelectBeginWord :: TextFieldCursor -> TextFieldCursor
textFieldCursorSelectBeginWord = textFieldCursorSelectedL %~ textCursorSelectBeginWord

textFieldCursorSelectEndWord :: TextFieldCursor -> TextFieldCursor
textFieldCursorSelectEndWord = textFieldCursorSelectedL %~ textCursorSelectEndWord

textFieldCursorSelectPrevWord :: TextFieldCursor -> TextFieldCursor
textFieldCursorSelectPrevWord = textFieldCursorSelectedL %~ textCursorSelectPrevWord

textFieldCursorSelectNextWord :: TextFieldCursor -> TextFieldCursor
textFieldCursorSelectNextWord = textFieldCursorSelectedL %~ textCursorSelectNextWord

textFieldCursorIndexOnLine :: TextFieldCursor -> Int
textFieldCursorIndexOnLine tfc = textCursorIndex $ tfc ^. textFieldCursorSelectedL

textFieldCursorSelectIndexOnLine :: Int -> TextFieldCursor -> TextFieldCursor
textFieldCursorSelectIndexOnLine ix_ = textFieldCursorSelectedL %~ textCursorSelectIndex ix_

-- |
--
-- returns 'Nothing' when given unsafe characters.
textFieldCursorInsertChar :: Char -> Maybe TextFieldCursor -> Maybe TextFieldCursor
textFieldCursorInsertChar c mtfc =
  case c of
    '\n' -> Just $ textFieldCursorInsertNewline mtfc
    _
      | isSafeChar c ->
        Just $
          fromMaybe emptyTextFieldCursor mtfc
            & textFieldCursorSelectedL %~ (fromJust . textCursorInsert c)
      | otherwise -> Nothing

-- |
--
-- returns 'Nothing' when given unsafe characters.
textFieldCursorAppendChar :: Char -> Maybe TextFieldCursor -> Maybe TextFieldCursor
textFieldCursorAppendChar c mtfc =
  case c of
    '\n' -> Just $ textFieldCursorAppendNewline mtfc
    _
      | isSafeChar c ->
        Just $
          fromMaybe emptyTextFieldCursor mtfc
            & textFieldCursorSelectedL %~ (fromJust . textCursorAppend c)
      | otherwise -> Nothing

textFieldCursorInsertNewline :: Maybe TextFieldCursor -> TextFieldCursor
textFieldCursorInsertNewline mtfc =
  let tfc = fromMaybe emptyTextFieldCursor mtfc
   in tfc
        & textFieldCursorNonEmptyCursorL
        %~ ( \lec@NonEmptyCursor {..} ->
               let (tc1, tc2) = textCursorSplit nonEmptyCursorCurrent
                in lec
                     { nonEmptyCursorPrev = rebuildTextCursor tc1 : nonEmptyCursorPrev,
                       nonEmptyCursorCurrent = tc2
                     }
           )

textFieldCursorAppendNewline :: Maybe TextFieldCursor -> TextFieldCursor
textFieldCursorAppendNewline mtfc =
  let tfc = fromMaybe emptyTextFieldCursor mtfc
   in tfc
        & textFieldCursorNonEmptyCursorL
        %~ ( \lec@NonEmptyCursor {..} ->
               let (tc1, tc2) = textCursorSplit nonEmptyCursorCurrent
                in lec
                     { nonEmptyCursorCurrent = tc1,
                       nonEmptyCursorNext = rebuildTextCursor tc2 : nonEmptyCursorNext
                     }
           )

textFieldCursorRemove :: TextFieldCursor -> Maybe (DeleteOrUpdate TextFieldCursor)
textFieldCursorRemove tfc =
  if nullTextFieldCursor tfc
    then Just Deleted
    else
      focusPossibleDeleteOrUpdate
        textFieldCursorNonEmptyCursorL
        ( \lec@NonEmptyCursor {..} ->
            case textCursorRemove nonEmptyCursorCurrent of
              Just (Updated ctc) -> Just $ Updated $ lec & nonEmptyCursorElemL .~ ctc
              _ ->
                case nonEmptyCursorPrev of
                  [] -> Nothing
                  (pl : pls) ->
                    Just $
                      Updated $
                        lec
                          { nonEmptyCursorPrev = pls,
                            nonEmptyCursorCurrent =
                              textCursorCombine (unsafeMakeTextCursor pl) nonEmptyCursorCurrent
                          }
        )
        tfc

textFieldCursorDelete :: TextFieldCursor -> Maybe (DeleteOrUpdate TextFieldCursor)
textFieldCursorDelete tfc =
  if nullTextFieldCursor tfc
    then Just Deleted
    else
      focusPossibleDeleteOrUpdate
        textFieldCursorNonEmptyCursorL
        ( \lec@NonEmptyCursor {..} ->
            case textCursorDelete nonEmptyCursorCurrent of
              Just (Updated ctc) -> Just $ Updated $ lec & nonEmptyCursorElemL .~ ctc
              _ ->
                case nonEmptyCursorNext of
                  [] -> Nothing
                  (pl : pls) ->
                    Just $
                      Updated $
                        lec
                          { nonEmptyCursorCurrent =
                              textCursorCombine nonEmptyCursorCurrent (unsafeMakeTextCursor pl),
                            nonEmptyCursorNext = pls
                          }
        )
        tfc

textFieldCursorSelectStartOfLine :: TextFieldCursor -> TextFieldCursor
textFieldCursorSelectStartOfLine = textFieldCursorSelectedL %~ textCursorSelectStart

textFieldCursorSelectEndOfLine :: TextFieldCursor -> TextFieldCursor
textFieldCursorSelectEndOfLine = textFieldCursorSelectedL %~ textCursorSelectEnd

-- Unsafe: only use for movements.
unsafeMakeTextCursor :: Text -> TextCursor
unsafeMakeTextCursor = fromJust . makeTextCursor
