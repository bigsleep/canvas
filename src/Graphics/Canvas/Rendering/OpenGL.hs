{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Graphics.Canvas.Rendering.OpenGL
    ( UniformInfo(..)
    , AttribInfo(..)
    , RenderInfo(..)
    , Component(..)
    , RenderResource(..)
    , render
    , renderInternal
    , allocateShader
    , allocateProgram
    , allocateRenderResource
    ) where

import Control.Exception (throwIO)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (ResourceT)
import qualified Control.Monad.Trans.Resource as Resource (allocate, runResourceT)
import qualified Data.ByteString as BS
import Data.FileEmbed (embedFile)
import Data.Proxy (Proxy(..))
import Foreign.Marshal.Array (withArray)
import qualified Foreign.Ptr as Ptr
import Foreign.Storable (sizeOf)
import Graphics.Canvas.Types
import qualified Graphics.Rendering.OpenGL as GL
import Linear (V2(..), V4(..))

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

data RenderResource = RenderResource
    { rrSimpleProgram :: SimpleProgram
    } deriving (Show)

render :: RenderResource -> Canvas -> IO ()
render resource (Canvas o w h drawings) =
    Resource.runResourceT $
        allocateRenderInfos resource drawings >>=
        liftIO . mapM_ renderInternal

convertDrawing :: RenderResource -> Int -> Drawing -> ([GL.Vertex2 GL.GLfloat], [Component])
convertDrawing resource index (ShapeDrawing shapeStyle trans (Triangle p0 p1 p2)) =
    (vs, [component])
    where
    FillStyle (V4 r g b a) = shapeStyleFillStyle shapeStyle
    c0 = GL.Vertex2 r g
    c1 = GL.Vertex2 b a
    toV (V2 x y) = GL.Vertex2 x y
    p0' = toV p0
    p1' = toV p1
    p2' = toV p2
    vs = [p0', c0, c1, p1', c0, c1, p2', c0, c1]
    RenderResource (SimpleProgram program positionAttrib colorAttrib) = resource
    component = Component program GL.Triangles [positionAttrib, colorAttrib] [] (fromIntegral index) (fromIntegral $ length vs)

appendDrawing :: RenderResource -> Drawing -> (Int, [GL.Vertex2 GL.GLfloat], [Component]) -> (Int, [GL.Vertex2 GL.GLfloat], [Component])
appendDrawing resource drawing (index, vs, cs) =
    (index', vs ++ vertices, cs ++ components)
    where
    (vertices, components) = convertDrawing resource index drawing
    index' = index + length vertices

allocateRenderInfos
    :: RenderResource
    -> [Drawing]
    -> ResourceT IO [RenderInfo]
allocateRenderInfos resource drawings = do
    let (_, vertices, components) = foldr (appendDrawing resource) (0, [], []) drawings
        vnum = length vertices
    (_, buffer) <- Resource.allocate (mkBuffer vertices) GL.deleteObjectName
    return . map (RenderInfo buffer) $ components
    where
    mkBuffer vs = do
        let n = length vs
            size = fromIntegral $ n * sizeOf (head vs)
        buffer <- GL.genObjectName
        GL.bindBuffer GL.ArrayBuffer GL.$= Just buffer
        withArray vs $ \ptr -> GL.bufferData GL.ArrayBuffer GL.$= (size, ptr, GL.StaticDraw)
        return buffer

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
    , spColorAttrib :: !AttribInfo
    } deriving (Show, Eq)

allocateSimpleProgram :: ResourceT IO SimpleProgram
allocateSimpleProgram = do
    vertexShader <- allocateShader GL.VertexShader $(embedFile "shader/vertex.glsl")
    fragmentShader <- allocateShader GL.FragmentShader $(embedFile "shader/fragment.glsl")
    program <- allocateProgram [vertexShader, fragmentShader]

    liftIO $ do
        GL.attribLocation program "position" GL.$= positionAttribLocation
        let positionAttrib = AttribInfo positionAttribLocation GL.Float 2 stride 0

        GL.attribLocation program "color" GL.$= colorAttribLocation
        let colorAttrib = AttribInfo colorAttribLocation GL.Float 4 stride colorOffset

        return $ SimpleProgram program positionAttrib colorAttrib

    where
    positionAttribLocation = GL.AttribLocation 0
    colorAttribLocation = GL.AttribLocation 1
    stride = fromIntegral $ 6 * sizeOf (undefined :: GL.GLfloat)
    colorOffset = 2 * sizeOf (undefined :: GL.GLfloat)

allocateRenderResource :: ResourceT IO RenderResource
allocateRenderResource = fmap RenderResource allocateSimpleProgram

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

allocateShader :: GL.ShaderType -> BS.ByteString -> ResourceT IO GL.Shader
allocateShader shaderType src = do
    (_, shader) <- Resource.allocate allocateShader GL.deleteObjectName
    return shader
    where
    allocateShader = do
        shader <- GL.createShader shaderType
        GL.shaderSourceBS shader GL.$= src
        GL.compileShader shader
        checkStatus GL.compileStatus GL.shaderInfoLog "shader compile error" shader
        return shader

allocateProgram :: [GL.Shader] -> ResourceT IO GL.Program
allocateProgram shaders = do
    (_, program) <- Resource.allocate mkProgram finalizeProgram
    return program
    where
    mkProgram = do
        program <- GL.createProgram
        mapM_ (GL.attachShader program) shaders
        GL.linkProgram program
        checkStatus GL.linkStatus GL.programInfoLog "program link error" program
        return program
    finalizeProgram program = do
        mapM_ (GL.detachShader program) shaders
        GL.deleteObjectName program


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
