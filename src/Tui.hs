{-# LANGUAGE OverloadedStrings #-}

module Tui where

import System.Directory

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Brick.Util
import Graphics.Vty.Input.Events
import Graphics.Vty.Attributes
import Cursor.Simple.List.NonEmpty
import qualified Data.List.NonEmpty as NE
import System.Exit
import Control.Monad.IO.Class

tui :: IO ()
tui = do
  initialState <- buildInitialState
  endState <- defaultMain tuiApp initialState
  print endState

newtype TuiState = TuiState
  { tuiStatePaths :: NonEmptyCursor (FilePath, Bool)} deriving (Show, Eq)

data ResourceName =
  ResourceName
  deriving (Show, Eq, Ord)

tuiApp :: App TuiState e ResourceName
tuiApp =
  App
    { appDraw = drawTui
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleTuiEvent
    , appStartEvent = pure
    , appAttrMap = const $ attrMap mempty [("selected", fg red)
                                          , ("isDirectory", fg green)
                                          , ("isFile", fg yellow)]
    }

buildInitialState :: IO TuiState
buildInitialState = do
  here <- getCurrentDirectory
  contents <- getDirectoryContents here
  case NE.nonEmpty contents of
    Nothing -> die "There are no contents."
    Just ne -> do
      ne' <- mapM (\fp -> do
        exist <- doesDirectoryExist fp
        if exist then pure (fp, True) else pure (fp, False)) ne
      pure TuiState { tuiStatePaths = makeNonEmptyCursor ne'}

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts = let nec = tuiStatePaths ts
              in [ vBox $ concat
                    [ map (drawPath False) $ reverse $ nonEmptyCursorPrev nec
                    , [drawPath True $ nonEmptyCursorCurrent nec]
                    , map (drawPath False) $ nonEmptyCursorNext nec
                    ]
                 ] 

drawPath :: Bool -> (FilePath, Bool) -> Widget n
drawPath b (fp, d)
  | not b         = (if d then withAttr "isDirectory" else withAttr "isFile") $ str fp
  | otherwise = withAttr "selected" $ str fp

handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt s
        EvKey KDown [] -> do
          let nec = tuiStatePaths s
          case nonEmptyCursorSelectNext nec of
            Nothing -> continue s
            Just nec' -> continue $ s {tuiStatePaths = nec'}
        EvKey KUp [] -> do
          let nec = tuiStatePaths s
          case nonEmptyCursorSelectPrev nec of
            Nothing -> continue s
            Just nec' -> continue $ s {tuiStatePaths = nec'}
        EvKey KEnter [] -> do
          let (curDir, d) = nonEmptyCursorCurrent $ tuiStatePaths s
          if d 
            then do
              (liftIO . setCurrentDirectory) curDir
              s' <- liftIO buildInitialState
              continue s'
            else continue s
        _ -> continue s
    _ -> continue s
