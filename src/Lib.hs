module Lib
    ( tui
    ) where


import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import qualified Brick.Widgets.Edit as E

-- | Named resoureced
type Name = ()

-- | The main application to draw the UI
tui :: IO ()
tui = do
  initialState <- buildInitialState
  endState <- defaultMain tuiApp initialState
  putStrLn "Hello"

-- | The state of the game
data TuiState =
    TuiState {  _quoteBox :: String
             ,  _typeBox  :: E.Editor String Name

             } deriving (Show)

-- | The initial config to out APP, specifying various functions
-- | appDraw: Turns the current app state into a list of layers of type Widget
tuiApp :: App TuiState e Name
tuiApp =
  App
    { appDraw = drawTui
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleTuiEvent
    , appStartEvent = pure
    , appAttrMap = const $ attrMap mempty []
    }

-- | Initial state of the APP
buildInitialState :: IO TuiState
buildInitialState = undefined

-- | Change TuiState to drawable Widgets. This does the actual drawing
drawTui :: TuiState -> [Widget Name]
drawTui = undefined

-- | Handle the events. This is where Keyboard events will be captured.
handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s e = undefined

