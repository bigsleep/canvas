module Main where

import Control.Monad (when, forever, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Linear (V2(..), V4(..))
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import System.Exit ( exitWith, ExitCode(..) )

import Graphics.Canvas.Types
import Graphics.Canvas.Rendering.OpenGL (render, allocateRenderResource, RenderResource)

main :: IO ()
main = do
    let width  = 640
        height = 480

        lineColor = V4 1 0 0 1
        fillColor = V4 0 1 0 1

        lineStyle = LineStyle lineColor 2
        fillStyle = FillStyle fillColor
        style = (ShapeStyle lineStyle fillStyle)

        divCount = 20
        dl = 1 / fromIntegral divCount

        drawings = do
            i <- [0..(divCount - 1)]
            j <- [0..(divCount - 1)]
            let x0 = fromIntegral i * dl
                y0 = fromIntegral j * dl
                r = 255
                triangle = Triangle (V2 x0 y0) (V2 (x0 + dl) y0) (V2 x0 (y0 + dl))
                style = ShapeStyle (LineStyle (V4 0 0 0 1) 2) (FillStyle $ V4 r 0 0 255)
            return $ ShapeDrawing style [] triangle

        canvas = Canvas (V2 0 0) (fromIntegral width) (fromIntegral height) drawings
        init = do
            resource <- allocateRenderResource
            return (resource, canvas)

    withWindow width height "canvas-sample" init onDisplay

    putStrLn "ended!"



withWindow :: Int -> Int -> String -> ResourceT IO a -> (a -> GLFW.Window -> IO ()) -> IO ()
withWindow width height title constructor f = do
    GLFW.setErrorCallback $ Just simpleErrorCallback
    r <- GLFW.init
    when r $ do
        m <- GLFW.createWindow width height title Nothing Nothing
        case m of
          (Just win) -> do
              GLFW.makeContextCurrent m
              GLFW.setWindowSizeCallback win (Just resizeWindow)
              GLFW.setWindowCloseCallback win (Just shutdown)
              runResourceT $ constructor >>= liftIO . flip f win
              GLFW.setErrorCallback $ Just simpleErrorCallback
              GLFW.destroyWindow win
          Nothing -> return ()
        GLFW.terminate
  where
    simpleErrorCallback e s =
        putStrLn $ unwords [show e, show s]


shutdown :: GLFW.WindowCloseCallback
shutdown win = do
    GLFW.destroyWindow win
    GLFW.terminate
    _ <- exitWith ExitSuccess
    return ()


onDisplay :: (RenderResource, Canvas) -> GLFW.Window -> IO ()
onDisplay (resource, canvas) win = do
  GL.clearColor GL.$= GL.Color4 1 1 1 1
  GL.clear [GL.ColorBuffer]
  render resource canvas
  GLFW.swapBuffers win

  forever $ do
     GLFW.pollEvents
     onDisplay (resource, canvas) win


resizeWindow :: GLFW.WindowSizeCallback
resizeWindow win w h =
    do
      GL.viewport   GL.$= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
      GL.matrixMode GL.$= GL.Projection
      GL.loadIdentity
      GL.ortho2D 0 (realToFrac w) (realToFrac h) 0
