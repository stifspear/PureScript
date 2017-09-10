module Arc where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CANVAS, drawImage, quadraticCurveTo, setStrokeStyle, arc,translate, withContext,  strokePath,rotate,  closePath, moveTo, lineTo, rect, Context2D, fillPath, setFillStyle, getContext2D,  getCanvasElementById)
import Graphics.Isometric
import Graphics.Drawing (render)
import Graphics.Canvas as C
import Data.Traversable
import Math (pi)
import Signal.DOM (animationFrame)
import Graphics.Isometric 
import Partial.Unsafe (unsafePartial)
import Flare (lift)

makeCircle :: Eff (canvas :: CANVAS) Unit
makeCircle = void $ unsafePartial do
 Just canvas <- getCanvasElementById "canvas"
 ctx <- getContext2D canvas
 void $ setFillStyle "#0000FF" ctx
 fillPath ctx $ do
	 void $ arc ctx {
	  x : 250.0
   	, y : 250.0
    	, r : 100.0
      	, start : 0.0
        , end : 360.0
	  }
    	  
makeSemiCircle1 :: Eff (canvas :: CANVAS) Unit
makeSemiCircle1 = void $ unsafePartial do 
 Just canvas <- getCanvasElementById "canvas"
 ctx <- getContext2D canvas
 void $ setFillStyle "#ffffff" ctx
 fillPath ctx $ do
	 void $ arc ctx {
	  x : 250.0
   	 ,y : 260.0
     	 ,r : 50.0
       	 ,start :1.5 * pi
	 ,end : 0.5 * pi
	  }
makeSemiCircle2 ::Eff (canvas :: CANVAS) Unit
makeSemiCircle2 = void $ unsafePartial do
 Just canvas <- getCanvasElementById "canvas"
 ctx <- getContext2D canvas
 void $ setFillStyle "#ffffff" ctx
 fillPath ctx $ do
	 void $ arc ctx {
	  x : 250.0
   	 ,y : 240.0
     	 ,r : 50.0
       	 ,start : 0.5 * pi
	 ,end : 1.5 * pi
	  }
drawLogo :: Eff (canvas :: CANVAS) Unit
drawLogo = void $ unsafePartial do
  makeCircle
  makeSemiCircle1
  makeSemiCircle2
  {--makeSemiCircle1 ctx--}
  {--makeSemiCircle2 ctx--}


rotateLogo :: C.Context2D -> Eff (canvas :: CANVAS) Unit
rotateLogo ctx = void $ unsafePartial do
  
  void $ translate { translateX : 50.0, translateY : 50.0  } ctx
  void $ rotate (1.5 * pi) ctx
  void $ translate { translateX : -50.0, translateY : -50.0 } ctx



main = do
	Just canvas <- getCanvasElementById "canvas"
 	ctx <- getContext2D canvas
	rotateLogo ctx
	drawLogo 	
