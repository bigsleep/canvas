module Sample
    ( withWindow
    , shutdown
    , onDisplay
    , resizeWindow
    ) where

import Control.Monad (when, forever, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import System.Exit (exitWith, ExitCode(..))

import Graphics.Canvas.Types
import Graphics.Canvas.Rendering.OpenGL (render, RenderResource)

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
    resource' <- renderDisplay resource canvas
    GLFW.pollEvents
    onDisplay (resource', canvas) win

    where
    renderDisplay r c = do
        GL.clearColor GL.$= GL.Color4 1 1 1 1
        GL.clear [GL.ColorBuffer]
        r' <- render r c
        GLFW.swapBuffers win
        return r'

resizeWindow :: GLFW.WindowSizeCallback
resizeWindow win w h =
    GL.viewport   GL.$= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
