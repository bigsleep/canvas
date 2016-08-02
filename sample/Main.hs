module Main where

import Control.Monad (when, forever, unless)
import Linear (V2(..))
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import System.Exit ( exitWith, ExitCode(..) )

import Graphics.Canvas.Types (Canvas(..))
import Graphics.Canvas.Rendering.OpenGL (render)

main :: IO ()
main = do
    let width  = 640
        height = 480

        canvas = Canvas (V2 0 0) (fromIntegral width) (fromIntegral height) []

    withWindow width height "canvas-sample" (return canvas) onDisplay

    putStrLn "ended!"



withWindow :: Int -> Int -> String -> IO a -> (a -> GLFW.Window -> IO ()) -> IO ()
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
              a <- constructor
              f a win
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


onDisplay :: Canvas -> GLFW.Window -> IO ()
onDisplay canvas win = do
  GL.clearColor GL.$= GL.Color4 1 1 1 1
  GL.clear [GL.ColorBuffer]
  render canvas
  GLFW.swapBuffers win

  forever $ do
     GLFW.pollEvents
     onDisplay canvas win


resizeWindow :: GLFW.WindowSizeCallback
resizeWindow win w h =
    do
      GL.viewport   GL.$= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
      GL.matrixMode GL.$= GL.Projection
      GL.loadIdentity
      GL.ortho2D 0 (realToFrac w) (realToFrac h) 0
