module Graphics.Canvas.Rendering.OpenGL
    ( render
    ) where

import Graphics.Canvas.Types
import qualified Graphics.Rendering.OpenGL as GL

mkShader :: BS.ByteString -> GL.ShaderType -> IO GL.Shader
mkShader src shaderType = do
    shader <- GL.createShader shaderType
    addFinalizer shader $ GL.deleteObjectName shader
    GL.shaderSourceBS shader GL.$= src
    compileAndCheck shader
    return shader

mkProgram :: [GL.Shader] -> IO GL.Program
mkProgram shaders = do
    program <- GL.createProgram
    mapM_ (GL.attachShader program) shaders
    GL.linkProgram program
    addFinalizer program $ do
        mapM_ (GL.detachShader program) shaders
        GL.deleteObjectName program
    return program
