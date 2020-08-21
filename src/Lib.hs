{-# LANGUAGE TemplateHaskell #-}
module Lib
    ( tui
    ) where

import Lens.Micro
import Lens.Micro.TH
import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import qualified Brick.Focus as F
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.Center as C

import Brick.Widgets.Core
  ( (<+>)
  , (<=>)
  , hLimit
  , vLimit
  , str
  )

-- | Named resoureced
data Name = QuoteBox | TypeBox 
            deriving (Ord, Show, Eq)

-- | The state of the game
-- | We'll make a lens out of it, so that we have getters and setter
data TuiState =
    TuiState {  _focusRing  :: F.FocusRing Name
             ,  _quoteBox   :: String
             ,  _typeBox    :: E.Editor String Name
             }

makeLenses ''TuiState

-- | The main application to draw the UI
tui :: IO ()
tui = do
  initialState <- buildInitialState
  endState <- defaultMain tuiApp initialState
  putStrLn "Hello"


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
buildInitialState = do
    pure TuiState { _focusRing =  F.focusRing [TypeBox]
                  , _quoteBox = "Quote to type"
                  , _typeBox = E.editor TypeBox Nothing ""
                  }

-- | Change TuiState to drawable Widgets. This does the actual drawing
-- | `<=>` is a sugar for Vertical box Layout. Put widgets one above another
-- | `ts^.typeBox` means get the `_typeBox` constructor from TuiState
-- | FocusRing helps to keep focus on the Named Resource. This also gives cursor to the edit box

-- TODO: Add Focus to Editor Box to get the cursor where you can type
drawTui :: TuiState -> [Widget Name]
drawTui ts = [ui]
    where
        typeBox' = F.withFocusRing (ts^.focusRing)(E.renderEditor (str . unlines)) (ts^.typeBox)
        ui = withBorderStyle unicode $
                borderWithLabel (str "TypeRacer") $ 
                    vBox [str (ts^.quoteBox), fill ' ', hBorder] <=>
                     (hLimit 30 $ vLimit 5 typeBox')


-- | Handle the events. This is where Keyboard events will be captured.
handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s e = undefined

