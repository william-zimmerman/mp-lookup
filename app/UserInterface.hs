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
import Brick.Widgets.Core (emptyWidget, hLimitPercent, vLimitPercent)
import Brick.Widgets.List (GenericList, list, renderList, listReplace, listClear)
import Brick.Widgets.Table (Table, renderTable, rowBorders, surroundingBorder, table)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Text as T
import Data.Vector (Vector, empty, fromList)
import Graphics.Vty (Event (EvKey), Key (KEnter, KEsc), defAttr)
import Lens.Micro.Platform (makeLenses, modifying, view)

data FormState = FormState
  { _fileName :: T.Text
  }
  deriving (Show)

makeLenses ''FormState

data ResourceName = FormFileName | FormNewField | ResultList
  deriving (Eq, Ord, Show)

data ApplicationState = ApplicationState
  { _form :: Form FormState () ResourceName,
    _userMessage :: Maybe String,
    _list :: GenericList ResourceName Vector String
  }

instance Show ApplicationState where
  show (ApplicationState form' maybeMessage _) = "ApplicationState {form = " ++ show (formState form') ++ ", userMessage = " ++ show maybeMessage ++ "}"

makeLenses ''ApplicationState

inputForm :: FormState -> Form FormState () ResourceName
inputForm =
  newForm
    [ (str "File name: " <+>) @@= editTextField fileName FormFileName (Just 1)
    ]

mainTable :: ApplicationState -> Table ResourceName
mainTable (ApplicationState form' maybeMessage list') =
  surroundingBorder False $
    rowBorders False $
      table
        [ [hLimitPercent 100 $ borderWithLabel (str "MP Lookup v0.1") $ renderForm form'],
          [hLimitPercent 100 $ maybe emptyWidget str maybeMessage],
          [vLimitPercent 100 $ hLimitPercent 100 $ renderList renderListFunc False list']
        ]

renderListFunc :: Bool -> String -> Widget ResourceName
renderListFunc _ = str

ui :: ApplicationState -> Widget ResourceName
ui applicationState =
  joinBorders $
    withBorderStyle unicode $
      renderTable $
        mainTable applicationState

eventHandler :: (FilePath -> IO Outcome) -> BrickEvent ResourceName () -> EventM ResourceName ApplicationState ()
eventHandler workFunction brickEvent = do
  currentApplicationState <- get
  let currentFormState = formState $ view form currentApplicationState
  case brickEvent of
    VtyEvent (EvKey KEsc []) -> halt
    VtyEvent (EvKey KEnter []) -> do
      results <- liftIO $ workFunction (T.unpack $ view fileName currentFormState)
      case results of
        Success listItems -> do
          modifying UserInterface.userMessage (const Nothing)
          modifying UserInterface.list $ listReplace (fromList listItems) Nothing
        Failure errorMessage -> do
          modifying UserInterface.userMessage (const $ Just errorMessage)
          modifying UserInterface.list listClear
      return ()
    _ -> zoom form (handleFormEvent brickEvent)

type ErrorMessage = String

data Outcome = Success [String] | Failure ErrorMessage

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
initialApplicationState = ApplicationState (inputForm initialFormState) Nothing initialList
  where
    initialFormState :: FormState
    initialFormState = FormState T.empty
    initialList :: GenericList ResourceName Vector String
    initialList = Brick.Widgets.List.list ResultList Data.Vector.empty 1
