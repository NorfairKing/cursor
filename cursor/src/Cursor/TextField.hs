{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Cursor.TextField
    ( TextFieldCursor(..)
    , emptyTextFieldCursor
    , makeTextFieldCursor
    , makeTextFieldCursorWithSelection
    , rebuildTextFieldCursorLines
    , rebuildTextFieldCursor
    , textFieldCursorSelection
    , textFieldCursorNonEmptyCursorL
    , textFieldCursorSelectedL
    , textFieldCursorSelectPrevLine
    , textFieldCursorSelectNextLine
    , textFieldCursorSelectFirstLine
    , textFieldCursorSelectLastLine
    , textFieldCursorSelectPrevChar
    , textFieldCursorSelectNextChar
    , textFieldCursorIndexOnLine
    , textFieldCursorSelectIndexOnLine
    , textFieldCursorInsertChar
    , textFieldCursorAppendChar
    , textFieldCursorInsertNewline
    , textFieldCursorAppendNewline
    , textFieldCursorRemove
    , textFieldCursorDelete
    , textFieldCursorSelectStartOfLine
    , textFieldCursorSelectEndOfLine
    ) where

import GHC.Generics (Generic)

import Data.Validity

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)

import Lens.Micro

import Cursor.NonEmpty
import Cursor.Text

newtype TextFieldCursor = TextFieldCursor
    { textFieldCursorNonEmpty :: NonEmptyCursor TextCursor
    } deriving (Show, Eq, Generic)

instance Validity TextFieldCursor where
    validate tfc@TextFieldCursor {..} =
        mconcat
            [ genericValidate tfc
            , decorate "None of the textcursors contain newlines" $
              mconcat $
              NE.toList $
              flip
                  NE.map
                  (NE.zip (fromJust $ NE.nonEmpty [0 ..]) $
                   rebuildNonEmptyCursor textFieldCursorNonEmpty) $ \(i, tc) ->
                  declare
                      (unwords
                           [ "The text cursor of the line at index"
                           , show (i :: Int)
                           , "does not contain any newlines"
                           ]) $
                  T.all (not . (== '\n')) $ rebuildTextCursor tc
            , decorate "The index of all except the current textcursor is 0" $
              mconcat
                  [ decorate
                        "The index of all text cursors of previous lines is 0" $
                    mconcat $
                    flip
                        map
                        (zip [0 ..] $ nonEmptyCursorPrev textFieldCursorNonEmpty) $ \(i, tc) ->
                        declare
                            (unwords
                                 [ "The text cursor of the previous line at index"
                                 , show (i :: Int)
                                 , "has index 0"
                                 ]) $
                        textCursorIndex tc == 0
                  , decorate "The index of all text cursors of next lines is 0" $
                    mconcat $
                    flip
                        map
                        (zip [0 ..] $ nonEmptyCursorNext textFieldCursorNonEmpty) $ \(i, tc) ->
                        declare
                            (unwords
                                 [ "The text cursor of the next line at index"
                                 , show (i :: Int)
                                 , "has index 0"
                                 ]) $
                        textCursorIndex tc == 0
                  ]
            ]

makeTextFieldCursor :: Text -> TextFieldCursor
makeTextFieldCursor = makeTextFieldCursorWithSelection 0 0

makeTextFieldCursorWithSelection :: Int -> Int -> Text -> TextFieldCursor
makeTextFieldCursorWithSelection x y t =
    (\tfc -> tfc & textFieldCursorSelectedL %~ textCursorSelectIndex y) $
    TextFieldCursor
        { textFieldCursorNonEmpty =
              makeNonEmptyCursorWithSelection x $
              let ls = T.splitOn "\n" t
                -- This is safe because 'splitOn' always returns a nonempty list.
               in fromJust $ NE.nonEmpty $ map makeTextCursor ls
        }

rebuildTextFieldCursorLines :: TextFieldCursor -> NonEmpty Text
rebuildTextFieldCursorLines =
    NE.map rebuildTextCursor . rebuildNonEmptyCursor . textFieldCursorNonEmpty

rebuildTextFieldCursor :: TextFieldCursor -> Text
rebuildTextFieldCursor =
    T.intercalate "\n" . NE.toList . rebuildTextFieldCursorLines

textFieldCursorSelection :: TextFieldCursor -> (Int, Int)
textFieldCursorSelection tfc =
    ( nonEmptyCursorSelection $ textFieldCursorNonEmpty tfc
    , textCursorIndex $ textFieldCursorNonEmpty tfc ^. nonEmptyCursorElemL)

emptyTextFieldCursor :: TextFieldCursor
emptyTextFieldCursor =
    TextFieldCursor
        {textFieldCursorNonEmpty = singletonNonEmptyCursor $ makeTextCursor ""}

textFieldCursorNonEmptyCursorL ::
       Lens' TextFieldCursor (NonEmptyCursor TextCursor)
textFieldCursorNonEmptyCursorL =
    lens textFieldCursorNonEmpty $ \tfc lec ->
        tfc {textFieldCursorNonEmpty = lec}

textFieldCursorSelectedL :: Lens' TextFieldCursor TextCursor
textFieldCursorSelectedL = textFieldCursorNonEmptyCursorL . nonEmptyCursorElemL

textFieldCursorSelectPrevLine :: TextFieldCursor -> Maybe TextFieldCursor
textFieldCursorSelectPrevLine =
    moveMWhileKeepingSelection nonEmptyCursorSelectPrev

textFieldCursorSelectNextLine :: TextFieldCursor -> Maybe TextFieldCursor
textFieldCursorSelectNextLine =
    moveMWhileKeepingSelection nonEmptyCursorSelectNext

moveMWhileKeepingSelection ::
       (NonEmptyCursor TextCursor -> Maybe (NonEmptyCursor TextCursor))
    -> TextFieldCursor
    -> Maybe TextFieldCursor
moveMWhileKeepingSelection movement tfc = do
    let i = textFieldCursorIndexOnLine tfc
    let tfc' = textFieldCursorSelectIndexOnLine 0 tfc
    tfc'' <- textFieldCursorNonEmptyCursorL movement $ tfc'
    pure $ textFieldCursorSelectIndexOnLine i tfc''

textFieldCursorSelectFirstLine :: TextFieldCursor -> TextFieldCursor
textFieldCursorSelectFirstLine =
    moveWhileKeepingSelection nonEmptyCursorSelectFirst

textFieldCursorSelectLastLine :: TextFieldCursor -> TextFieldCursor
textFieldCursorSelectLastLine =
    moveWhileKeepingSelection nonEmptyCursorSelectLast

moveWhileKeepingSelection ::
       (NonEmptyCursor TextCursor -> NonEmptyCursor TextCursor)
    -> TextFieldCursor
    -> TextFieldCursor
moveWhileKeepingSelection movement tfc =
    let i = textFieldCursorIndexOnLine tfc
        tfc' = textFieldCursorSelectIndexOnLine 0 tfc
        tfc'' = tfc' & textFieldCursorNonEmptyCursorL %~ movement
     in textFieldCursorSelectIndexOnLine i tfc''

textFieldCursorSelectPrevChar :: TextFieldCursor -> Maybe TextFieldCursor
textFieldCursorSelectPrevChar = textFieldCursorSelectedL textCursorSelectPrev

textFieldCursorSelectNextChar :: TextFieldCursor -> Maybe TextFieldCursor
textFieldCursorSelectNextChar = textFieldCursorSelectedL textCursorSelectNext

textFieldCursorIndexOnLine :: TextFieldCursor -> Int
textFieldCursorIndexOnLine tfc =
    textCursorIndex $ tfc ^. textFieldCursorSelectedL

textFieldCursorSelectIndexOnLine :: Int -> TextFieldCursor -> TextFieldCursor
textFieldCursorSelectIndexOnLine ix_ =
    textFieldCursorSelectedL %~ textCursorSelectIndex ix_

textFieldCursorInsertChar :: Char -> TextFieldCursor -> TextFieldCursor
textFieldCursorInsertChar c =
    case c of
        '\n' -> textFieldCursorInsertNewline
        _ -> textFieldCursorSelectedL %~ textCursorInsert c

textFieldCursorAppendChar :: Char -> TextFieldCursor -> TextFieldCursor
textFieldCursorAppendChar c =
    case c of
        '\n' -> textFieldCursorAppendNewline
        _ -> textFieldCursorSelectedL %~ textCursorAppend c

textFieldCursorInsertNewline :: TextFieldCursor -> TextFieldCursor
textFieldCursorInsertNewline =
    textFieldCursorNonEmptyCursorL %~
    (\lec@NonEmptyCursor {..} ->
         let (tc1, tc2) = textCursorSplit nonEmptyCursorCurrent
          in lec
                 { nonEmptyCursorPrev =
                       textCursorSelectIndex 0 tc1 : nonEmptyCursorPrev
                 , nonEmptyCursorCurrent = tc2
                 })

textFieldCursorAppendNewline :: TextFieldCursor -> TextFieldCursor
textFieldCursorAppendNewline =
    textFieldCursorNonEmptyCursorL %~
    (\lec@NonEmptyCursor {..} ->
         let (tc1, tc2) = textCursorSplit nonEmptyCursorCurrent
          in lec
                 { nonEmptyCursorCurrent = tc1
                 , nonEmptyCursorNext = textCursorSelectIndex 0 tc2 : nonEmptyCursorNext
                 })

textFieldCursorRemove :: TextFieldCursor -> Maybe TextFieldCursor
textFieldCursorRemove =
    textFieldCursorNonEmptyCursorL
        (\lec@NonEmptyCursor {..} ->
             case textCursorRemove nonEmptyCursorCurrent of
                 Nothing ->
                     case nonEmptyCursorPrev of
                         [] -> Nothing
                         (pl:pls) ->
                             Just $
                             lec
                                 { nonEmptyCursorPrev = pls
                                 , nonEmptyCursorCurrent =
                                       textCursorCombine
                                           pl
                                           nonEmptyCursorCurrent
                                 }
                 Just ctc -> Just $ lec & nonEmptyCursorElemL .~ ctc)

textFieldCursorDelete :: TextFieldCursor -> Maybe TextFieldCursor
textFieldCursorDelete =
    textFieldCursorNonEmptyCursorL
        (\lec@NonEmptyCursor {..} ->
             case textCursorDelete nonEmptyCursorCurrent of
                 Nothing ->
                     case nonEmptyCursorNext of
                         [] -> Nothing
                         (pl:pls) ->
                             Just $
                             lec
                                 { nonEmptyCursorCurrent =
                                       textCursorCombine
                                           nonEmptyCursorCurrent
                                           pl
                                 , nonEmptyCursorNext = pls
                                 }
                 Just ctc -> Just $ lec & nonEmptyCursorElemL .~ ctc)

textFieldCursorSelectStartOfLine :: TextFieldCursor -> TextFieldCursor
textFieldCursorSelectStartOfLine =
    textFieldCursorSelectedL %~ textCursorSelectStart

textFieldCursorSelectEndOfLine :: TextFieldCursor -> TextFieldCursor
textFieldCursorSelectEndOfLine = textFieldCursorSelectedL %~ textCursorSelectEnd
