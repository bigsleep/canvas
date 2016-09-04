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
import Control.Monad.Trans.Resource (ResourceT(..))
import qualified Control.Monad.Trans.Resource as Resource (allocate, runResourceT)
import qualified Data.ByteString as BS
import Data.FileEmbed (embedFile)
import Data.Fixed (div', mod')
import Data.Foldable (Foldable(..), foldrM)
import Data.Proxy (Proxy(..))
import Foreign.Marshal.Array (withArray)
import qualified Foreign.Ptr as Ptr
import Foreign.Storable (sizeOf)
import Graphics.Canvas.Types
import qualified Graphics.Rendering.OpenGL as GL
import Linear (V2(..), V3(..), V4(..), M22(..), (!*), ortho, lookAt)

data UniformInfo = forall a. (GL.Uniform a, Show a) => UniformInfo !GL.UniformLocation !a
deriving instance Show UniformInfo

data AttribInfo = AttribInfo
    { aiAttribLocation :: !GL.AttribLocation
    , aiDataType :: !GL.DataType
    , aiNumArrayIndices :: !Int
    , aiStride :: !Int
    , aiOffset :: !Int
    } deriving (Show, Eq)

data ProgramInfo = ProgramInfo
    { piProgram :: !GL.Program
    , piAttribs :: ![AttribInfo]
    , piUniforms :: ![GL.UniformLocation]
    } deriving (Show)

data RenderInfo = RenderInfo
    { riProgram :: !ProgramInfo
    , riMode :: !GL.PrimitiveMode
    , riVertexBuffer :: !GL.BufferObject
    , riIndex :: !GL.ArrayIndex
    , riNum :: !GL.NumArrayIndices
    } deriving (Show)

data RenderResource = RenderResource
    { rrTriangleProgramInfo :: !ProgramInfo
    , rrCircleProgramInfo :: !ProgramInfo
    , rrArcProgramInfo :: !ProgramInfo
    } deriving (Show)

data VertexGroups = VertexGroups
    { vgTriangleVertices :: ![GL.GLfloat]
    , vgCircleVertices :: ![GL.GLfloat]
    , vgArcVertices :: ![GL.GLfloat]
    } deriving (Show)

data AttribParams = AttribParams
    { apName :: !String
    , apSize :: !Int
    } deriving (Show)

instance Monoid VertexGroups where
    mappend (VertexGroups as bs cs) (VertexGroups as' bs' cs') = VertexGroups (as ++ as') (bs ++ bs') (cs ++ cs')
    mempty = VertexGroups [] [] []

rotate :: (a, a, a) -> (a, a, a)
rotate (q0, q1, q2) = (q1, q2, q0)

convertDrawing :: Drawing -> VertexGroups
convertDrawing (ShapeDrawing shapeStyle trans (Triangle p0 p1 p2)) = VertexGroups vertices [] []
    where
    LineStyle lineColor lineWidth = shapeStyleLineStyle shapeStyle
    FillStyle fillColor = shapeStyleFillStyle shapeStyle
    vs = take 3 $ iterate rotate (toList p0, toList p1, toList p2)
    otherVals = toList fillColor ++ toList lineColor ++ [lineWidth, lineWidth, lineWidth]
    format (q0, q1, q2) = concat [q0, q1, q2, otherVals]
    vertices = concatMap format $ vs

convertDrawing (ShapeDrawing shapeStyle trans (Rectangle p0 width height)) = VertexGroups vertices [] []
    where
    LineStyle lineColor lineWidth = shapeStyleLineStyle shapeStyle
    FillStyle fillColor = shapeStyleFillStyle shapeStyle
    vals = toList fillColor ++ toList lineColor
    V2 x y = p0
    p1 = V2 (x + width) y
    p2 = V2 (x + width) (y + height)
    p3 = V2 x (y + height)
    format ((q0, w0), (q1, w1), (q2, w2)) = concat [q0, q1, q2, vals, [w0, w1, w2]]
    gen (q0, q1, q2)= take 3 (iterate rotate ((toList q0, lineWidth), (toList q1, lineWidth), (toList q2, 0)))
    vs = gen (p2, p0, p1) ++ gen (p0, p2, p3)
    vertices = concatMap format $ vs

convertDrawing (ShapeDrawing shapeStyle trans (Circle p0 radius)) = VertexGroups [] vertices []
    where
    LineStyle lineColor lineWidth = shapeStyleLineStyle shapeStyle
    FillStyle fillColor = shapeStyleFillStyle shapeStyle
    vals = toList p0 ++ [radius] ++ toList fillColor ++ toList lineColor ++ [lineWidth]
    V2 x y = p0
    r' = radius / sin (pi / 3)
    m = V2 (V3 r' 0 x)
           (V3 0 r' y)
    vs = map (\(V2 px py) -> m !* V3 px py 1) circleVertices
    format (V2 qx qy) = qx : qy : vals
    xs = zipWith (\p1 p2 -> [p2, p0, p1]) vs (tail . cycle $ vs)
    vertices = concatMap (concatMap format) $ xs

convertDrawing (PathDrawing lineStyle trans (Arc p0 radius startAngle endAngle)) = VertexGroups [] [] vertices
    where
    LineStyle lineColor lineWidth = lineStyle
    vals = toList p0 ++ [radius] ++ toList lineColor ++ [lineWidth, startAngle, endAngle]
    V2 x y = p0
    r' = radius / sin (pi / 3)
    m = V2 (V3 r' 0 x)
           (V3 0 r' y)
    vs = map (\(V2 px py) -> m !* V3 px py 1) circleVertices
    format (V2 qx qy) = qx : qy : vals
    xs = zipWith (\p1 p2 -> [p2, p0, p1]) vs (tail . cycle $ vs)
    vertices = concatMap (concatMap format) $ xs

circleDivision :: Int
circleDivision = 6

circleVertices :: [V2 Float]
circleVertices = vertices
    where
    division = circleDivision
    da = 2 * pi / fromIntegral division :: Float
    rmat = rotateMatrix da
    vertices = take division $ iterate (rmat !*) (V2 0 1)

allocateRenderInfo
    :: RenderResource
    -> [Drawing]
    -> ResourceT IO [RenderInfo]
allocateRenderInfo resource drawings = do
    (_, triangleVertexBuffer) <- Resource.allocate (mkBuffer GL.ArrayBuffer tvs) GL.deleteObjectName
    (_, circleVertexBuffer) <- Resource.allocate (mkBuffer GL.ArrayBuffer cvs) GL.deleteObjectName
    (_, arcVertexBuffer) <- Resource.allocate (mkBuffer GL.ArrayBuffer avs) GL.deleteObjectName
    return [ RenderInfo triangleSource GL.Triangles triangleVertexBuffer 0 tnum
           , RenderInfo circleSource GL.Triangles circleVertexBuffer 0 cnum
           , RenderInfo arcSource GL.Triangles arcVertexBuffer 0 anum
           ]
    where
    RenderResource triangleSource circleSource arcSource = resource
    VertexGroups tvs cvs avs = fold . map convertDrawing $ drawings
    tnum = fromIntegral $ length tvs `div` attribNum (piAttribs triangleSource)
    cnum = fromIntegral $ length cvs `div` attribNum (piAttribs circleSource)
    anum = fromIntegral $ length avs `div` attribNum (piAttribs arcSource)
    mkBuffer bufferTarget xs = do
        let n = length xs
            size = fromIntegral $ n * sizeOf (head xs)
        buffer <- GL.genObjectName
        GL.bindBuffer bufferTarget GL.$= Just buffer
        withArray xs $ \ptr -> GL.bufferData bufferTarget GL.$= (size, ptr, GL.StreamDraw)
        GL.bindBuffer bufferTarget GL.$= Nothing
        return buffer
    attribNum = fromIntegral . sum . map aiNumArrayIndices

render :: RenderResource -> Canvas -> IO ()
render resource (Canvas (V2 ox oy) w h drawings) =
    Resource.runResourceT $ do
        rs <- allocateRenderInfo resource drawings
        liftIO $ do
            pm <- GL.newMatrix GL.RowMajor . concatMap toList . toList $ projectionMatrix
            mvm <- GL.newMatrix GL.RowMajor . concatMap toList . toList $ modelViewMatrix
            GL.blend GL.$= GL.Enabled
            GL.blendFunc GL.$= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
            mapM_ (renderInternal pm mvm) rs
    where
    projectionMatrix = ortho ox (ox + w) oy (oy + h) 1 (-1)
    modelViewMatrix = lookAt (V3 0 0 1) (V3 0 0 0) (V3 0 1 0)

renderInternal
    :: GL.GLmatrix GL.GLfloat
    -> GL.GLmatrix GL.GLfloat
    -> RenderInfo
    -> IO ()
renderInternal pm mvm info = do
    GL.currentProgram GL.$= Just program
    GL.bindBuffer GL.ArrayBuffer GL.$= Just vertexBuffer
    mapM_ (bindAttrib program) attribs
    mapM_ (bindUniform program) uniforms
    GL.drawArrays mode (fromIntegral index) (fromIntegral num)
    GL.bindBuffer GL.ArrayBuffer GL.$= Nothing
    mapM_ (unbindAttrib program) attribs

    where
    RenderInfo programInfo mode vertexBuffer index num = info
    ProgramInfo program attribs uniformLocations = programInfo
    uniforms = zipWith UniformInfo uniformLocations [pm, mvm]

allocateRenderResource :: ResourceT IO RenderResource
allocateRenderResource = do
    triangleProgram <- allocateTriangleProgram
    circleProgram <- allocateCircleProgram
    arcProgram <- allocateArcProgram
    return (RenderResource triangleProgram circleProgram arcProgram)

allocateProgramInfo
    :: BS.ByteString
    -> BS.ByteString
    -> [AttribParams]
    -> [String]
    -> ResourceT IO ProgramInfo
allocateProgramInfo vertexShaderCode fragmentShaderCode attribParams uniformNames = do
    vertexShader <- allocateShader GL.VertexShader vertexShaderCode
    fragmentShader <- allocateShader GL.FragmentShader fragmentShaderCode
    program <- allocateProgram [vertexShader, fragmentShader]

    liftIO $ do
        (_, attribs) <- foldrM (allocateAttrib program stride) (stride, []) ([0..] `zip` attribParams)
        uniforms <- mapM (GL.uniformLocation program) uniformNames
        return $ ProgramInfo program attribs uniforms
    where
    stride = floatSize * sum (map apSize attribParams)
    allocateAttrib program stride (i, AttribParams attribName size) (offset, xs) = do
        let offset' = offset - size * floatSize
        GL.attribLocation program attribName GL.$= GL.AttribLocation i
        return $ (offset', AttribInfo (GL.AttribLocation i) GL.Float size stride offset' : xs)
    floatSize = sizeOf (undefined :: Float)

allocateTriangleProgram :: ResourceT IO ProgramInfo
allocateTriangleProgram = allocateProgramInfo
    $(embedFile "shader/triangle-vertex.glsl")
    $(embedFile "shader/triangle-fragment.glsl")
    attribParams
    uniformNames

    where
    attribParams =
        [ AttribParams "prevPosition" 2
        , AttribParams "position" 2
        , AttribParams "nextPosition" 2
        , AttribParams "color" 4
        , AttribParams "lineColor" 4
        , AttribParams "lineWidth" 3
        ]
    uniformNames =
        [ "projectionMatrix"
        , "modelViewMatrix"
        ]

allocateCircleProgram :: ResourceT IO ProgramInfo
allocateCircleProgram = allocateProgramInfo
    $(embedFile "shader/circle-vertex.glsl")
    $(embedFile "shader/circle-fragment.glsl")
    attribParams
    uniformNames

    where
    attribParams =
        [ AttribParams "position" 2
        , AttribParams "center" 2
        , AttribParams "radius" 1
        , AttribParams "color" 4
        , AttribParams "lineColor" 4
        , AttribParams "lineWidth" 1
        ]
    uniformNames =
        [ "projectionMatrix"
        , "modelViewMatrix"
        ]

allocateArcProgram :: ResourceT IO ProgramInfo
allocateArcProgram = allocateProgramInfo
    $(embedFile "shader/arc-vertex.glsl")
    $(embedFile "shader/arc-fragment.glsl")
    attribParams
    uniformNames

    where
    attribParams =
        [ AttribParams "position" 2
        , AttribParams "center" 2
        , AttribParams "radius" 1
        , AttribParams "lineColor" 4
        , AttribParams "lineWidth" 1
        , AttribParams "startAngle" 1
        , AttribParams "endAngle" 1
        ]
    uniformNames =
        [ "projectionMatrix"
        , "modelViewMatrix"
        ]

bindAttrib :: GL.Program -> AttribInfo -> IO ()
bindAttrib program vai = do
    GL.vertexAttribArray attribLocation GL.$= GL.Enabled
    GL.vertexAttribPointer attribLocation GL.$= (GL.ToFloat, vad)
    where
    AttribInfo attribLocation dataType num stride offset = vai
    GL.AttribLocation location = attribLocation
    ptrOffset = Ptr.nullPtr `Ptr.plusPtr` offset
    vad = GL.VertexArrayDescriptor (fromIntegral num) dataType (fromIntegral stride) ptrOffset

unbindAttrib :: GL.Program -> AttribInfo -> IO ()
unbindAttrib program vai =
    GL.vertexAttribArray attribLocation GL.$= GL.Disabled
    where
    AttribInfo attribLocation _ _ _ _  = vai

bindUniform :: GL.Program -> UniformInfo -> IO ()
bindUniform program (UniformInfo l u) =
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


rotateMatrix :: Float -> M22 Float
rotateMatrix a = V2 (V2 cosa (-sina)) (V2 sina cosa)
    where
    cosa = cos a
    sina = sin a
