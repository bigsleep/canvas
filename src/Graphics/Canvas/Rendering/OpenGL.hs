{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Graphics.Canvas.Rendering.OpenGL
    ( UniformInfo(..)
    , AttribInfo(..)
    , RenderInfo(..)
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
import Linear (V2(..), V3(..), V4(..), (!*))

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
    { riProgram :: !GL.Program
    , riAttribs :: ![AttribInfo]
    , riUniforms :: ![UniformInfo]
    , riMode :: !GL.PrimitiveMode
    , riVertexBuffer :: !GL.BufferObject
    , riIndexBuffer :: !GL.BufferObject
    , riIndex :: !GL.ArrayIndex
    , riNum :: !GL.NumArrayIndices
    } deriving (Show)

data RenderResource = RenderResource
    { rrSimpleProgram :: SimpleProgram
    } deriving (Show)

render :: RenderResource -> Canvas -> IO ()
render resource (Canvas o w h drawings) =
    Resource.runResourceT $
        allocateRenderInfo resource drawings >>=
        liftIO . renderInternal

convertDrawing :: GL.GLuint -> Drawing -> ([GL.Vertex2 GL.GLfloat], [GL.GLuint])
convertDrawing start (ShapeDrawing shapeStyle trans (Triangle p0 p1 p2)) = (vertices, indices)
    where
    FillStyle (V4 r g b a) = shapeStyleFillStyle shapeStyle
    f (V2 x y) = [GL.Vertex2 x y, GL.Vertex2 r g, GL.Vertex2 b a]
    vertices = concat . map f $ [p0, p1, p2]
    indices = map (+ start) [0..2]

convertDrawing start (ShapeDrawing shapeStyle trans (Rectangle p0 width height)) = (vertices, indices)
    where
    V2 x y = p0
    p1 = V2 (x + width) y
    p2 = V2 (x + width) (y + height)
    p3 = V2 x (y + height)
    ps = [p0, p1, p2, p3]
    FillStyle (V4 r g b a) = shapeStyleFillStyle shapeStyle
    f (V2 x y) = [GL.Vertex2 x y, GL.Vertex2 r g, GL.Vertex2 b a]
    vertices = concat . map f $ ps
    indices = map (+ start) [0, 1, 3, 1, 2, 3]

convertDrawing start (ShapeDrawing shapeStyle trans (Circle p0 radius)) = (vertices, indices)
    where
    V2 x y = p0
    m = V2 (V3 radius 0 x)
           (V3 0 radius y)
    vs = map (\(V2 px py) -> m !* V3 px py 1)$ circleVertices
    vertices = map toV . concat . map (\p -> [p, c0, c1]) $ vs
    indices = concat . take circleDivision $ zipWith (\i j -> [start, i, j]) [(start + 1)..] [(start + 2)..]
    FillStyle (V4 r g b a) = shapeStyleFillStyle shapeStyle
    c0 = V2 r g
    c1 = V2 b a
    toV (V2 x y) = GL.Vertex2 x y

circleDivision :: Int
circleDivision = 20

circleVertices :: [V2 Float]
circleVertices = vertices
    where
    division = circleDivision
    da = 2 * pi / fromIntegral division :: Float
    cosa = cos da
    sina = sin da
    rmat = V2 (V2 cosa (-sina)) (V2 sina cosa)
    vertices = (V2 0 0 :) . take division $ iterate (rmat !*) (V2 0 1)

appendDrawing :: Drawing -> (GL.GLuint, [GL.Vertex2 GL.GLfloat], [GL.GLuint]) -> (GL.GLuint, [GL.Vertex2 GL.GLfloat], [GL.GLuint])
appendDrawing drawing (i, vs0, is0) = (i + fromIntegral (length is), vs0 ++ vs, is0 ++ is)
    where
    (vs, is) = convertDrawing i drawing

allocateRenderInfo
    :: RenderResource
    -> [Drawing]
    -> ResourceT IO RenderInfo
allocateRenderInfo resource drawings = do
    (_, vertexBuffer) <- Resource.allocate (mkBuffer GL.ArrayBuffer vs) GL.deleteObjectName
    (_, indexBuffer) <- Resource.allocate (mkBuffer GL.ElementArrayBuffer is) GL.deleteObjectName
    return $ RenderInfo program [positionAttrib, colorAttrib] [] GL.Triangles vertexBuffer indexBuffer 0 (fromIntegral num)
    where
    SimpleProgram program positionAttrib colorAttrib = rrSimpleProgram resource
    (num, vs, is) = foldr appendDrawing (0, [], []) $ drawings
    mkBuffer bufferType xs = do
        let n = length xs
            size = fromIntegral $ n * sizeOf (head xs)
        buffer <- GL.genObjectName
        GL.bindBuffer bufferType GL.$= Just buffer
        withArray xs $ \ptr -> GL.bufferData bufferType GL.$= (size, ptr, GL.StreamDraw)
        return buffer

renderInternal
    :: RenderInfo
    -> IO ()
renderInternal info = do
    GL.currentProgram GL.$= Just program
    mapM_ (bindAttrib program) attribs
    mapM_ (bindUniform program) uniforms
    GL.drawElements mode num GL.UnsignedInt Ptr.nullPtr

    where
    RenderInfo program attribs uniforms mode vertexBuffer indexBuffer index num = info

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
