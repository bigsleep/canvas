{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Graphics.Canvas.Rendering.OpenGL
    ( UniformInfo(..)
    , AttribInfo(..)
    , RenderInfo(..)
    , render
    , renderInternal
    , mkShader
    , mkProgram
    ) where

import Control.Exception (throwIO)
import Control.Monad (unless)
import qualified Data.ByteString as BS
import Data.FileEmbed (embedFile)
import Data.Proxy (Proxy(..))
import Foreign.Marshal.Array (withArray)
import qualified Foreign.Ptr as Ptr
import Foreign.Storable (sizeOf)
import Graphics.Canvas.Types
import qualified Graphics.Rendering.OpenGL as GL
import Linear (V2(..))
import System.Mem.Weak (addFinalizer)

data UniformLocation a = UniformLocation !(Proxy a) !GL.UniformLocation deriving (Show, Eq)

data UniformInfo = forall a. (GL.Uniform a, Show a) => UniformInfo !(UniformLocation a) !a
deriving instance Show UniformInfo

data AttribInfo = AttribInfo
    { aiAttribLocation :: !GL.AttribLocation
    , aiDataType :: !GL.DataType
    , aiNumArrayIndices :: !GL.NumArrayIndices
    , aiStride :: !GL.Stride
    , aiOffset :: !Int
    } deriving (Show, Eq)

data RenderInfo = RenderInfo
    { riBuffer :: !GL.BufferObject
    , riComponent :: !Component
    } deriving (Show)

data Component = Component
    { componentProgram :: GL.Program
    , componentMode :: GL.PrimitiveMode
    , componentAttribs :: ![AttribInfo]
    , componentUniforms :: ![UniformInfo]
    , componentIndex :: !GL.ArrayIndex
    , componentNum :: !GL.NumArrayIndices
    } deriving (Show)

render :: Canvas -> IO ()
render (Canvas o w h drawings) = do
    rs <- mkRenderInfos drawings
    mapM_ renderInternal rs

convertDrawing :: SimpleProgram -> Int -> Drawing -> ([GL.Vertex2 GL.GLdouble], [Component])
convertDrawing sp index (ShapeDrawing shapeStyle trans (Triangle p0 p1 p2)) =
    (vs, [component])
    where
    vs = map (\(V2 x y) -> GL.Vertex2 x y) [p0, p1, p2]
    SimpleProgram program attr uniformLocation = sp
    color = UniformInfo uniformLocation $ GL.Vertex4 1 0 0 0
    component = Component program GL.Triangles [attr] [color] (fromIntegral index) (fromIntegral $ length vs)

appendDrawing :: SimpleProgram -> Drawing -> (Int, [GL.Vertex2 GL.GLdouble], [Component]) -> (Int, [GL.Vertex2 GL.GLdouble], [Component])
appendDrawing sp drawing (index, vs, cs) =
    (index', vs ++ vertices, cs ++ components)
    where
    (vertices, components) = convertDrawing sp index drawing
    index' = index + length vertices

mkRenderInfos
    :: [Drawing]
    -> IO [RenderInfo]
mkRenderInfos drawings = do
    sp <- mkSimpleProgram
    let (_, vertices, components) = foldr (appendDrawing sp) (0, [], []) drawings
        vnum = length vertices
        mkBuffer ptr = do
            let size = fromIntegral $ vnum * sizeOf (head vertices)
            GL.bufferData GL.ArrayBuffer GL.$= (size, ptr, GL.StaticDraw)
    buffer <- GL.genObjectName
    GL.bindBuffer GL.ArrayBuffer GL.$= Just buffer
    withArray vertices mkBuffer
    addFinalizer buffer (GL.deleteObjectName buffer)
    return . map (RenderInfo buffer) $ components

renderInternal
    :: RenderInfo
    -> IO ()
renderInternal info = do
    GL.currentProgram GL.$= Just program
    GL.bindBuffer GL.ArrayBuffer GL.$= Just buffer
    mapM_ (bindAttrib program) attribs
    mapM_ (bindUniform program) uniforms
    GL.drawArrays mode (fromIntegral index) (fromIntegral num)

    where
    buffer = riBuffer info
    Component program mode attribs uniforms index num = riComponent info

data SimpleProgram = SimpleProgram
    { spProgram :: !GL.Program
    , spPositionAttrib :: !AttribInfo
    , spColorUniformLocation :: UniformLocation (GL.Vertex4 GL.GLfloat)
    } deriving (Show, Eq)

mkSimpleProgram :: IO SimpleProgram
mkSimpleProgram = do
    vertexShader <- mkShader GL.VertexShader $(embedFile "shader/vertex.glsl")
    fragmentShader <- mkShader GL.FragmentShader $(embedFile "shader/fragment.glsl")
    program <- mkProgram [vertexShader, fragmentShader]

    GL.attribLocation program "position" GL.$= al
    let attr = AttribInfo al GL.Float 2 (fromIntegral $ sizeOf (undefined :: GL.Vertex2 GL.GLdouble)) 0

    ul <- GL.uniformLocation program "color"
    return $ SimpleProgram program attr (UniformLocation (Proxy :: Proxy (GL.Vertex4 GL.GLfloat)) ul)

    where
    al = GL.AttribLocation 0

bindAttrib :: GL.Program -> AttribInfo -> IO ()
bindAttrib program vai = do
    GL.vertexAttribArray attribLocation GL.$= GL.Enabled
    GL.vertexAttribPointer attribLocation GL.$= (GL.ToFloat, vad)
    where
    AttribInfo attribLocation dataType num stride offset = vai
    GL.AttribLocation location = attribLocation
    ptrOffset = Ptr.nullPtr `Ptr.plusPtr` offset
    vad = GL.VertexArrayDescriptor num dataType stride ptrOffset


bindUniform :: GL.Program -> UniformInfo -> IO ()
bindUniform program (UniformInfo (UniformLocation _ l) u) =
    GL.uniform l GL.$= u

mkShader :: GL.ShaderType -> BS.ByteString -> IO GL.Shader
mkShader shaderType src = do
    shader <- GL.createShader shaderType
    GL.shaderSourceBS shader GL.$= src
    GL.compileShader shader
    addFinalizer shader $ GL.deleteObjectName shader
    checkStatus GL.compileStatus GL.shaderInfoLog "shader compile error" shader
    return shader

mkProgram :: [GL.Shader] -> IO GL.Program
mkProgram shaders = do
    program <- GL.createProgram
    mapM_ (GL.attachShader program) shaders
    GL.linkProgram program
    addFinalizer program $ do
        mapM_ (GL.detachShader program) shaders
        GL.deleteObjectName program
    checkStatus GL.linkStatus GL.programInfoLog "program link error" program
    return program


checkStatus
    :: (a -> GL.GettableStateVar Bool)
    -> (a -> GL.GettableStateVar String)
    -> String
    -> a
    -> IO ()
checkStatus getStatus getInfoLog message object = do
    ok <- GL.get . getStatus $ object
    unless ok $ do
        log <- GL.get . getInfoLog $ object
        throwIO . userError $ message ++ ": " ++ log


sizeOfVertex2D :: Int
sizeOfVertex2D = sizeOf (undefined :: GL.Vertex2 Double)
