{-
The code is highly motivated from 
https://github.com/callum-oakley/gotta-go-fast/blob/master/src/UI.hs
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module UI
    ( tui
    ) where

import Lens.Micro
import Lens.Micro.TH
import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Core
  ( (<+>)
  , (<=>)
  , hLimit
  , vLimit
  , str
  )

import qualified Brick.Focus                as F
import qualified Brick.Widgets.Edit         as E
import qualified Brick.Widgets.Center       as C
import qualified Brick.Types                as T
import qualified Brick.Main                 as M
import qualified Graphics.Vty               as V
import qualified Data.Text                  as T

import           Control.Monad.IO.Class (liftIO)
import           Data.Char              (isSpace)
import           Data.Maybe             (fromMaybe)
import           Data.Time              (getCurrentTime)
import           Data.Word              (Word8)
import           Game

-- | Named resoureced
type Name = () 

-- | The state of the game
-- | We'll make a lens out of it, so that we have getters and setter
-- | QUESTION: Do we really need a EDIT box? ?
data TuiState =
    TuiState {  _game       :: Game 
             ,  _quoteBox   :: String
             } deriving (Show)

makeLenses ''TuiState

-- | An attribute for HIT, Use this attribute when you have HIT action
hitAttrName :: AttrName 
hitAttrName = attrName "Hit"

missAttrName :: AttrName
missAttrName = attrName "Miss"

emptyAttrName :: AttrName
emptyAttrName = attrName "Empty"

-- | Draw the character, If Miss color the character with RED
-- | str :: String -> Widget n
drawCharacter :: Character -> Widget Name
drawCharacter (Hit c)  = withAttr hitAttrName $ str [c]
drawCharacter (Miss ' ') = withAttr missAttrName $ str ['_']
drawCharacter (Miss c) = withAttr missAttrName $ str [c]
drawCharacter (Empty c) = withAttr emptyAttrName $ str [c]

drawLine :: Line -> Widget Name
drawLine [] = emptyWidget
drawLine line = foldl1 (<+>) $ map drawCharacter line 

-- | Takes the state, Creates the characters and draws the line
-- Is there a better way to write the lenses.
-- TODO: Word Wrap for text field
drawInputText :: TuiState -> Widget Name
drawInputText st = padBottom (Pad 2)  (str input')
  where input' = T.unpack $ unInput $ (st ^. game) ^. input
        

drawQuote :: TuiState -> Widget Name
drawQuote st =  (drawLine characters)
  where input' = (st ^. game) ^. input
        quote' = (st ^. game) ^. quote
        characters = character quote' input'

drawResult :: TuiState -> Widget Name
drawResult ts = str $ 
  (show . round $ wpm $ ts^.game) ++ " words per minute is " ++ 
  (show . round $ accuracy (ts^.game) * 100) ++ "% accuracy"

-- | Change TuiState to drawable Widgets. This does the actual drawing
-- | `<=>` is a sugar for Vertical box Layout. Put widgets one above another
-- | TODO: No idea why `typebox` has space at the beginning
drawTui :: TuiState -> [Widget Name]
drawTui ts 
  | gameEnded = pure. C.center . padAll 1 $ drawResult ts
  | otherwise = [ui]
    where
        gameEnded = hasEnded (ts^.game)
        ui = withBorderStyle unicode $
                borderWithLabel (str "TypeRacer") $ 
                    vBox [(drawQuote ts), fill ' ', hBorder] 
                    <=> showCursor () (Location $ cursor (ts^.game) ) (drawInputText ts)

handleChar :: Char -> TuiState -> EventM Name (Next TuiState)
handleChar char ts 
  | isComplete game' = do
      now <- liftIO getCurrentTime
      M.continue $ ts & game %~ (stopClock now)
  | hasStarted game' = M.continue $ ts & game %~ (applyChar char)
  | otherwise   = do
      now <- liftIO getCurrentTime
      M.continue $ ts & game %~ (startClock now)
  where game' = ts^.game
   

-- | Handle the events. This is where Keyboard events will be captured.
-- | Continue takes a updated state and applies it
-- | `%~` modifies the value of a field. Like a setter
-- | `&` is the reverse of `$` ie. You give the argument first and then the function. Eg: num & add == add $ num
handleTuiEvent :: TuiState -> BrickEvent Name e -> EventM Name (Next TuiState)
handleTuiEvent ts (VtyEvent ev) = 
    case ev of
        V.EvKey V.KEsc [] -> M.halt ts
        V.EvKey (V.KChar c) [] -> handleChar c ts
        V.EvKey V.KBS [] -> M.continue $ ts & game %~ applyBackSpace

handleTuiEvent ts _ = M.continue ts

-- | Initial state of the APP
buildInitialState :: IO TuiState
buildInitialState = do
    pure TuiState { _game = initialState " Hello there. This is a big test text. Let's see"
                  , _quoteBox = " Hello there. This is a big test text. Let's see"
                  }


-- | The initial config to out APP, specifying various functions
-- | appDraw: Turns the current app state into a list of layers of type Widget
tuiApp :: V.Attr -> V.Attr -> V.Attr -> App TuiState e Name
tuiApp hitAttr missAttr emptyAttr=
  App
    { appDraw = drawTui
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleTuiEvent
    , appStartEvent = pure
    , appAttrMap = const $ 
                   attrMap 
                      V.defAttr
                      [ (hitAttrName, hitAttr)
                      , (missAttrName, missAttr)
                      , (emptyAttrName, emptyAttr)
                      ]
    }

-- | The main application to draw the UI
tui :: IO ()
tui = do
  initialState <- buildInitialState
  endState <- defaultMain (tuiApp hitAttr missAttr emptyAttr) initialState
  putStrLn $ show endState 
    where 
      hitAttr = fg . V.ISOColor $ 2
      missAttr = fg . V.ISOColor $ 1
      emptyAttr = fg. V.ISOColor $ 8

