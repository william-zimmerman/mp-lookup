{-# LANGUAGE TemplateHaskell #-}

module UserInterface (app, initialApplicationState, Outcome (..)) where

import Brick
  ( App
      ( App,
        appAttrMap,
        appChooseCursor,
        appDraw,
        appHandleEvent,
        appStartEvent
      ),
    BrickEvent (VtyEvent),
    EventM,
    Widget,
    attrMap,
    get,
    halt,
    joinBorders,
    showFirstCursor,
    str,
    withBorderStyle,
    zoom,
    (<+>),
  )
import Brick.Forms (Form (..), editTextField, handleFormEvent, newForm, renderForm, (@@=))
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Core (emptyWidget, hLimitPercent, vLimit, viewport, vLimitPercent)
import Brick.Widgets.List (List, list, renderList, handleListEvent)
import Brick.Widgets.Table (Table, renderTable, rowBorders, surroundingBorder, table)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Text as T
import Graphics.Vty (Event (EvKey), Key (KEnter, KEsc), defAttr)
import Lens.Micro.Platform (makeLenses, modifying, view)
import Data.Vector (singleton, replicate)

data FormState = FormState
  { _fileName :: T.Text,
    _newField :: T.Text
  }
  deriving (Show)

makeLenses ''FormState

data ResourceName = FormFileName | FormNewField | Viewport | ResultList
  deriving (Eq, Ord, Show)

data ApplicationState = ApplicationState
  { _form :: Form FormState () ResourceName,
    _userMessage :: Maybe String
  }

data ListItem = ListItem {
  name :: String
}

instance Show ApplicationState where
  show (ApplicationState form' maybeMessage) = "ApplicationState {form = " ++ show (formState form') ++ ", userMessage = " ++ show maybeMessage ++ "}"

makeLenses ''ApplicationState

inputForm :: FormState -> Form FormState () ResourceName
inputForm =
  newForm
    [ (str "File name: " <+>) @@= editTextField fileName FormFileName (Just 1),
      (str "New field: " <+>) @@= editTextField newField FormNewField (Just 1)
    ]

mainTable :: ApplicationState -> Table ResourceName
mainTable (ApplicationState form' maybeMessage) =
  surroundingBorder False $
    rowBorders False $
      table
        [ 
          [vLimitPercent 100 $ hLimitPercent 100 $ renderList renderListFunc True resultList ]
        ]

renderListFunc :: Bool -> String -> Widget ResourceName
renderListFunc isSelected = case isSelected of
  True -> borderWithLabel (str "Selected") . str
  False -> str

resultList :: List ResourceName String
resultList = list ResultList (Data.Vector.replicate 5 "Hello world!") 1

ui :: ApplicationState -> Widget ResourceName
ui applicationState =
  joinBorders $
    withBorderStyle unicode $
      renderTable $
        mainTable applicationState

eventHandler :: (FilePath -> IO Outcome) -> BrickEvent ResourceName () -> EventM ResourceName ApplicationState ()
eventHandler workFunction event = do
  currentApplicationState <- get
  let currentFormState = formState $ view form currentApplicationState
  case event of
    VtyEvent (EvKey KEsc []) -> halt
    VtyEvent (EvKey KEnter []) -> do
      results <- liftIO $ workFunction (T.unpack $ view fileName currentFormState)
      case results of
        Success -> modifying userMessage (const $ Just "Done!")
        Failure errorMessage -> modifying userMessage (const $ Just errorMessage)
      return ()
    VtyEvent (event) -> handleListEvent event
    _ -> zoom form (handleFormEvent event)

type ErrorMessage = String

data Outcome = Success | Failure ErrorMessage

app :: (FilePath -> IO Outcome) -> App ApplicationState () ResourceName
app workFunction =
  App
    { appDraw = \s -> [ui s],
      appChooseCursor = showFirstCursor,
      appHandleEvent = eventHandler workFunction,
      appStartEvent = return (),
      appAttrMap = const $ attrMap defAttr []
    }

initialApplicationState :: ApplicationState
initialApplicationState = ApplicationState (inputForm initialFormState) Nothing
  where
    initialFormState :: FormState
    initialFormState = FormState T.empty T.empty
