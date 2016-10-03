{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Bits hiding (rotate)
import qualified Data.ByteString as BS
import Data.FileEmbed (embedFile)
import Data.Foldable (Foldable(..), foldrM)
import Data.Proxy (Proxy(..))
import Foreign.Marshal.Array (withArray)
import qualified Foreign.Ptr as Ptr
import Foreign.Storable (Storable(..), sizeOf)
import Graphics.Canvas.Types
import Graphics.Canvas.Rendering.OpenGL.Vertex
import qualified Graphics.Rendering.OpenGL as GL
import Linear (V2(..), V3(..), V4(..), M22, (!*), ortho, lookAt)

data UniformInfo = forall a. (GL.Uniform a, Show a) => UniformInfo !GL.UniformLocation !a
deriving instance Show UniformInfo

data AttribInfo = AttribInfo
    { aiAttribLocation :: !GL.AttribLocation
    , aiDataType :: !GL.DataType
    , aiNumArrayIndices :: !Int
    , aiIntegerHandling :: !GL.IntegerHandling
    , aiStride :: !Int
    , aiOffset :: !Int
    } deriving (Show, Eq)

data ProgramInfo = ProgramInfo !GL.Program ![AttribInfo] ![GL.UniformLocation]
    deriving (Show)

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
    , rrLineProgramInfo :: !ProgramInfo
    } deriving (Show)

data VertexGroups = VertexGroups ![TriangleVertex] ![CircleVertex] ![ArcVertex] ![LineVertex]
    deriving (Show)

instance Monoid VertexGroups where
    mappend (VertexGroups as bs cs ds) (VertexGroups as' bs' cs' ds') = VertexGroups (as ++ as') (bs ++ bs') (cs ++ cs') (ds ++ ds')
    mempty = VertexGroups [] [] [] []

convertDrawing :: Drawing -> VertexGroups
convertDrawing (ShapeDrawing shapeStyle trans (Triangle p0 p1 p2)) = VertexGroups vertices [] [] []
    where
    lineStyle = shapeStyleLineStyle shapeStyle
    (lineColor, lineWidth, lineFlags) = case lineStyle of
        Nothing -> (V4 0 0 0 0, 0, 0)
        Just (LineStyle c w) -> (c, w, triangleBottomLine0 .|. triangleBottomLine1 .|. triangleBottomLine2)
    FillStyle fillColor = shapeStyleFillStyle shapeStyle
    vs = take 3 $ iterate rotate (p0, p1, p2)
    format (q0, q1, q2) = triangleVertex q0 q1 q2 fillColor lineColor lineWidth 0 lineFlags
    vertices = map format $ vs

convertDrawing (ShapeDrawing shapeStyle trans (Rectangle p0 width height)) = VertexGroups vertices [] [] []
    where
    lineStyle = shapeStyleLineStyle shapeStyle
    (lineColor, lineWidth, lineFlags) = case lineStyle of
        Nothing -> (V4 0 0 0 0, 0, 0)
        Just (LineStyle c w) -> (c, w, triangleBottomLine0 .|. triangleBottomLine1)
    FillStyle fillColor = shapeStyleFillStyle shapeStyle
    V2 x y = p0
    p1 = V2 (x + width) y
    p2 = V2 (x + width) (y + height)
    p3 = V2 x (y + height)
    format (q0, q1, q2) = triangleVertex q0 q1 q2 fillColor lineColor lineWidth 0 lineFlags
    gen = take 3 . iterate rotate
    vs = gen (p1, p0, p2) ++ gen (p3, p2, p0)
    vertices = map format $ vs

convertDrawing (ShapeDrawing shapeStyle trans (Circle p0 radius)) = VertexGroups [] vertices [] []
    where
    lineStyle = shapeStyleLineStyle shapeStyle
    (lineColor, lineWidth) = case lineStyle of
        Nothing -> (V4 0 0 0 0, 0)
        Just (LineStyle c w) -> (c, w)
    FillStyle fillColor = shapeStyleFillStyle shapeStyle
    V2 x y = p0
    r' = radius / sin (pi / 3)
    m = V2 (V3 r' 0 x)
           (V3 0 r' y)
    vs = map (\(V2 px py) -> m !* V3 px py 1) circleVertices
    format q = circleVertex q p0 radius fillColor lineColor lineWidth
    xs = zipWith (\p1 p2 -> [p2, p0, p1]) vs (tail . cycle $ vs)
    vertices = concatMap (map format) $ xs

convertDrawing (PathDrawing lineStyle trans (Arc p0 radius startAngle endAngle)) = VertexGroups [] [] vertices []
    where
    LineStyle lineColor lineWidth = lineStyle
    V2 x y = p0
    r' = radius / sin (pi / 3)
    m = V2 (V3 r' 0 x)
           (V3 0 r' y)
    vs = map (\(V2 px py) -> m !* V3 px py 1) circleVertices
    format q = arcVertex q p0 radius lineColor lineWidth startAngle endAngle
    xs = zipWith (\p1 p2 -> [p2, p0, p1]) vs (tail . cycle $ vs)
    vertices = concatMap (map format) $ xs

convertDrawing (PathDrawing _ _ (StripPath [])) = VertexGroups [] [] [] []

convertDrawing (PathDrawing _ _ (StripPath (_ : []))) = VertexGroups [] [] [] []

convertDrawing (PathDrawing lineStyle trans (StripPath (p0 : p1 : ps))) = VertexGroups [] [] [] vertices
    where
    LineStyle lineColor lineWidth = lineStyle
    segs = zip (p0 : p1 : ps) (p1 : ps)
    segs' = zip3 (Nothing : map Just segs) segs (map Just (tail segs) ++ [Nothing])
    triangulate (Nothing, (q0, q1), Nothing) =
        let a0 = lineVertex q0 q1 q0 lineWidth 4 3 lineColor
            a1 = lineVertex q0 q1 q0 lineWidth 4 4 lineColor
            a2 = lineVertex q1 q0 q1 lineWidth 4 3 lineColor
            a3 = lineVertex q1 q0 q1 lineWidth 4 4 lineColor
        in [a0, a1, a2, a2, a3, a0]
    triangulate (Nothing, (q0, q1), Just (r0, r1)) =
        let a0 = lineVertex q0 q1 q0 lineWidth 4 3 lineColor
            a1 = lineVertex q0 q1 q0 lineWidth 4 4 lineColor
            a2 = lineVertex q1 q0 r1 lineWidth 4 1 lineColor
            a3 = lineVertex q1 q0 r1 lineWidth 4 2 lineColor
            b0 = lineVertex r0 r1 q0 lineWidth 4 1 lineColor
            b1 = lineVertex r0 r1 q0 lineWidth 4 2 lineColor
        in [a0, a1, a2, a2, a3, a0, a3, a2, b0, b1, b0, a2]
    triangulate (Just (s0, _), (q0, q1), Just (r0, r1)) =
        let a0 = lineVertex q0 q1 s0 lineWidth 4 1 lineColor
            a1 = lineVertex q0 q1 s0 lineWidth 4 2 lineColor
            a2 = lineVertex q1 q0 r1 lineWidth 4 1 lineColor
            a3 = lineVertex q1 q0 r1 lineWidth 4 2 lineColor
            b0 = lineVertex r0 r1 q0 lineWidth 4 1 lineColor
            b1 = lineVertex r0 r1 q0 lineWidth 4 2 lineColor
        in [a0, a1, a2, a2, a3, a0, a3, a2, b0, b1, b0, a2]
    triangulate (Just (a, _), (q0, q1), Nothing) =
        let a0 = lineVertex q0 q1 a lineWidth 4 1 lineColor
            a1 = lineVertex q0 q1 a lineWidth 4 2 lineColor
            a2 = lineVertex q1 q0 q1 lineWidth 4 3 lineColor
            a3 = lineVertex q1 q0 q1 lineWidth 4 4 lineColor
        in [a0, a1, a2, a2, a3, a0]
    vertices = concatMap triangulate $ segs'

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
    (_, lineVertexBuffer) <- Resource.allocate (mkBuffer GL.ArrayBuffer lvs) GL.deleteObjectName
    return [ RenderInfo triangleSource GL.Triangles triangleVertexBuffer 0 tnum
           , RenderInfo circleSource GL.Triangles circleVertexBuffer 0 cnum
           , RenderInfo arcSource GL.Triangles arcVertexBuffer 0 anum
           , RenderInfo lineSource GL.Triangles lineVertexBuffer 0 lnum
           ]
    where
    RenderResource triangleSource circleSource arcSource lineSource = resource
    VertexGroups tvs cvs avs lvs = fold . map convertDrawing $ drawings
    tnum = fromIntegral $ length tvs
    cnum = fromIntegral $ length cvs
    anum = fromIntegral $ length avs
    lnum = fromIntegral $ length lvs
    mkBuffer bufferTarget xs = do
        let n = length xs
            size = fromIntegral $ n * sizeOf (head xs)
        buffer <- GL.genObjectName
        GL.bindBuffer bufferTarget GL.$= Just buffer
        withArray xs $ \ptr -> GL.bufferData bufferTarget GL.$= (size, ptr, GL.StreamDraw)
        GL.bindBuffer bufferTarget GL.$= Nothing
        return buffer

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
    mapM_ bindAttrib attribs
    mapM_ bindUniform uniforms
    GL.drawArrays mode (fromIntegral index) (fromIntegral num)
    GL.bindBuffer GL.ArrayBuffer GL.$= Nothing
    mapM_ unbindAttrib attribs

    where
    RenderInfo programInfo mode vertexBuffer index num = info
    ProgramInfo program attribs uniformLocations = programInfo
    uniforms = zipWith UniformInfo uniformLocations [pm, mvm]

allocateRenderResource :: ResourceT IO RenderResource
allocateRenderResource = do
    triangleProgram <- allocateTriangleProgram
    circleProgram <- allocateCircleProgram
    arcProgram <- allocateArcProgram
    lineProgram <- allocateLineProgram
    return (RenderResource triangleProgram circleProgram arcProgram lineProgram)

allocateProgramInfo
    :: BS.ByteString
    -> BS.ByteString
    -> [VertexField]
    -> [String]
    -> ResourceT IO ProgramInfo
allocateProgramInfo vertexShaderCode fragmentShaderCode attribParams uniformNames = do
    vertexShader <- allocateShader GL.VertexShader vertexShaderCode
    fragmentShader <- allocateShader GL.FragmentShader fragmentShaderCode
    program <- allocateProgram [vertexShader, fragmentShader]

    liftIO $ do
        (_, attribs) <- foldrM (allocateAttrib program) (stride, []) attribParams
        uniforms <- mapM (GL.uniformLocation program) uniformNames
        return $ ProgramInfo program attribs uniforms
    where
    stride = sum (map vfByteSize attribParams)
    allocateAttrib program (VertexField location attribName dataType num byteSize ihandling) (offset, xs) = do
        let offset' = offset - byteSize
        GL.attribLocation program attribName GL.$= location
        return $ (offset', AttribInfo location dataType num ihandling stride offset' : xs)

allocateTriangleProgram :: ResourceT IO ProgramInfo
allocateTriangleProgram = allocateProgramInfo
    $(embedFile "shader/triangle-vertex.glsl")
    $(embedFile "shader/triangle-fragment.glsl")
    vfs
    uniformNames

    where
    vfs = vertexFields . vertexSpec $ (Proxy :: Proxy TriangleVertex)
    uniformNames =
        [ "projectionMatrix"
        , "modelViewMatrix"
        ]

allocateCircleProgram :: ResourceT IO ProgramInfo
allocateCircleProgram = allocateProgramInfo
    $(embedFile "shader/circle-vertex.glsl")
    $(embedFile "shader/circle-fragment.glsl")
    vfs
    uniformNames

    where
    vfs = vertexFields . vertexSpec $ (Proxy :: Proxy CircleVertex)
    uniformNames =
        [ "projectionMatrix"
        , "modelViewMatrix"
        ]

allocateArcProgram :: ResourceT IO ProgramInfo
allocateArcProgram = allocateProgramInfo
    $(embedFile "shader/arc-vertex.glsl")
    $(embedFile "shader/arc-fragment.glsl")
    vfs
    uniformNames

    where
    vfs = vertexFields $ vertexSpec (Proxy :: Proxy ArcVertex)
    uniformNames =
        [ "projectionMatrix"
        , "modelViewMatrix"
        ]

allocateLineProgram :: ResourceT IO ProgramInfo
allocateLineProgram = allocateProgramInfo
    $(embedFile "shader/line-vertex.glsl")
    $(embedFile "shader/line-fragment.glsl")
    vfs
    uniformNames

    where
    vfs = vertexFields $ vertexSpec (Proxy :: Proxy LineVertex)
    uniformNames =
        [ "projectionMatrix"
        , "modelViewMatrix"
        ]

bindAttrib :: AttribInfo -> IO ()
bindAttrib vai = do
    GL.vertexAttribArray attribLocation GL.$= GL.Enabled
    GL.vertexAttribPointer attribLocation GL.$= (ihandling, vad)
    where
    AttribInfo attribLocation dataType num ihandling stride offset = vai
    ptrOffset = Ptr.nullPtr `Ptr.plusPtr` offset
    vad = GL.VertexArrayDescriptor (fromIntegral num) dataType (fromIntegral stride) ptrOffset

unbindAttrib :: AttribInfo -> IO ()
unbindAttrib vai =
    GL.vertexAttribArray (aiAttribLocation vai) GL.$= GL.Disabled

bindUniform :: UniformInfo -> IO ()
bindUniform (UniformInfo l u) =
    GL.uniform l GL.$= u

allocateShader :: GL.ShaderType -> BS.ByteString -> ResourceT IO GL.Shader
allocateShader shaderType src = do
    (_, shader) <- Resource.allocate mkShader GL.deleteObjectName
    return shader
    where
    mkShader = do
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
        log' <- GL.get . getInfoLog $ object
        throwIO . userError $ message ++ ": " ++ log'


rotateMatrix :: Float -> M22 Float
rotateMatrix a = V2 (V2 cosa (-sina)) (V2 sina cosa)
    where
    cosa = cos a
    sina = sin a

rotate :: (a, a, a) -> (a, a, a)
rotate (q0, q1, q2) = (q1, q2, q0)


triangleBottomLine0 :: GL.GLuint
triangleBottomLine0 = 1

triangleBottomLine1 :: GL.GLuint
triangleBottomLine1 = 1 `shiftL` 1

triangleBottomLine2 :: GL.GLuint
triangleBottomLine2 = 1 `shiftL` 2
