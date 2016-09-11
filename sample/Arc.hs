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

        lineColor = V4 0 0 0 1
        fillColor = V4 0 1 0 1

        lineStyle = LineStyle lineColor 10
        startAngle = pi / 4
        endAngle = 3 * pi / 2

        divCount = 4
        dx = fromIntegral width / fromIntegral divCount
        dy = fromIntegral height / fromIntegral divCount
        radius = dx * 0.2

        drawings = do
            i <- [0..(divCount - 1)]
            j <- [0..(divCount - 1)]
            let x0 = fromIntegral i * dx
                y0 = fromIntegral j * dy
                r = 1
                arc = Arc (V2 x0 y0) radius startAngle endAngle
            return $ PathDrawing lineStyle [] arc

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
    GL.viewport   GL.$= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))