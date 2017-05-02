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
    , addTextureResource
    ) where

import Control.Exception (throwIO)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (ResourceT)
import qualified Control.Monad.Trans.Resource as Resource (allocate, runResourceT)
import Data.Bits hiding (rotate)
import qualified Data.ByteString as BS
import Data.FileEmbed (embedFile)
import Data.Foldable (Foldable(..), for_, foldrM)
import qualified Data.List as List (groupBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, catMaybes, isJust)
import Data.Proxy (Proxy(..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector
import Data.Word (Word8)
import Debug.Trace (trace)
import qualified Foreign.Marshal as Ptr
import Foreign.Marshal.Array (withArray, copyArray, peekArray)
import qualified Foreign.Ptr as Ptr
import Foreign.Storable (Storable(..), sizeOf)
import Graphics.Canvas.Types
import Graphics.Canvas.Rendering.OpenGL.Vertex
import qualified Graphics.Rendering.OpenGL as GL
import Linear (V2(..), V3(..), V4(..), M22, M44, (!*), ortho, lookAt, nearZero)

data Uniform = forall a. (GL.Uniform a, Show a) => Uniform !a
deriving instance Show Uniform

data UniformInfo = UniformInfo !GL.UniformLocation !Uniform
    deriving (Show)

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
    , riUniformInfos :: ![UniformInfo]
    , riTexture :: !GL.TextureObject
    }

data RenderProgramInfos = RenderProgramInfos
    { rpiTriangleProgramInfo :: !ProgramInfo
    , rpiArcProgramInfo :: !ProgramInfo
    , rpiLineProgramInfo :: !ProgramInfo
    } deriving (Show)

data RenderResource = RenderResource
    { rrRenderProgramInfos :: !RenderProgramInfos
    , rrTextures :: !(Map Text GL.TextureObject)
    , rrPalette :: !Palette
    } deriving (Show)

data VertexGroups = VertexGroups
    { vgTriangleVertex :: ![TriangleVertexUnit]
    , vgArcVertex :: ![ArcVertexUnit]
    , vgLineVertex :: ![LineVertexUnit]
    } deriving (Show)

type TriangleVertexUnit = (GL.TextureObject, [TriangleVertex])

type ArcVertexUnit = (GL.TextureObject, [ArcVertex])

type LineVertexUnit = (GL.TextureObject, [LineVertex])

data Palette = Palette
    { paletteColors :: !(Set Color)
    , paletteColorCoords :: !(Map Color Coord)
    , paletteStorage :: !(Vector Color)
    , paletteTexture :: !GL.TextureObject
    , palettePixelBufferObject :: !GL.BufferObject
    } deriving (Show)

instance Monoid VertexGroups where
    mappend (VertexGroups as bs cs) (VertexGroups as' bs' cs') = VertexGroups (as ++ as') (bs ++ bs') (cs ++ cs')
    mempty = VertexGroups [] [] []

convertDrawing :: RenderResource ->  Drawing -> VertexGroups
convertDrawing resource (ShapeDrawing (ShapeStyle lineStyle (PlainColorFillStyle fillColor)) (Triangle p0 p1 p2)) = mempty { vgTriangleVertex = vertices }
    where
    (lineColor, lineWidth, lineFlags) = case lineStyle of
        Nothing -> (V4 0 0 0 0, 0, 0)
        Just (LineStyle c w) -> (c, w, triangleBottomLine0 .|. triangleBottomLine1 .|. triangleBottomLine2)
    colorCoords = paletteColorCoords . rrPalette $ resource
    texture = paletteTexture . rrPalette $ resource
    fillColorCoord = fromMaybe (V2 0 0) . Map.lookup fillColor $ colorCoords
    lineColorCoord = fromMaybe (V2 0 0) . Map.lookup lineColor $ colorCoords
    vs = take 3 $ iterate rotate (p0, p1, p2)
    format (q0, q1, q2) = triangleVertex q0 q1 q2 fillColorCoord lineColorCoord lineWidth 0 lineFlags
    vs' = map format vs
    vertices = [(texture, vs')]

convertDrawing resource (ShapeDrawing (ShapeStyle _ (TexturedFillStyle textureRange)) (Triangle p0 p1 p2)) = mempty { vgTriangleVertex = vertices }
    where
    TextureRange textureName (V2 x0 y0) (V2 x1 y1) = textureRange
    texture = fromMaybe (GL.TextureObject 0) . Map.lookup textureName . rrTextures $ resource
    ps = take 3 $ iterate rotate (p0, p1, p2)
    tps = [(V2 x0 y0), (V2 x0 y1), (V2 x1 y0)]
    format ((q0, q1, q2), tp) = triangleVertex q0 q1 q2 (V2 0 0) tp 0 0 0
    vs = map format $ zip ps tps
    vertices = [(texture, vs)]

convertDrawing _ (ShapeDrawing _ (Rectangle _ width height)) | width <= 0 || height <= 0 || nearZero width || nearZero height = mempty

convertDrawing resource (ShapeDrawing (ShapeStyle lineStyle (PlainColorFillStyle fillColor)) (Rectangle p0 width height)) = mempty { vgTriangleVertex = vertices }
    where
    (lineColor, lineWidth, lineFlags) = case lineStyle of
        Nothing -> (V4 0 0 0 0, 0, 0)
        Just (LineStyle c w) -> (c, w, triangleBottomLine0 .|. triangleBottomLine2 .|. triangleTopLine0 .|. triangleTopLine2)
    colorCoords = paletteColorCoords . rrPalette $ resource
    texture = paletteTexture . rrPalette $ resource
    fillColorCoord = fromMaybe (V2 0 0) . Map.lookup fillColor $ colorCoords
    lineColorCoord = fromMaybe (V2 0 0) . Map.lookup lineColor $ colorCoords
    vs = genRectVertices fillColorCoord lineColorCoord lineWidth lineWidth lineFlags lineFlags p0 width height
    vertices = [(texture, vs)]

convertDrawing _ (ShapeDrawing _ (Circle _ radius)) | radius <= 0 = mempty

convertDrawing resource (ShapeDrawing (ShapeStyle lineStyle (PlainColorFillStyle fillColor)) (Circle p0 radius)) = mempty { vgArcVertex = vertices }
    where
    (lineColor, lineWidth) = case lineStyle of
        Nothing -> (V4 0 0 0 0, 0)
        Just (LineStyle c w) -> (c, w)
    colorCoords = paletteColorCoords . rrPalette $ resource
    texture = paletteTexture . rrPalette $ resource
    fillColorCoord = fromMaybe (V2 0 0) . Map.lookup fillColor $ colorCoords
    lineColorCoord = fromMaybe (V2 0 0) . Map.lookup lineColor $ colorCoords
    V2 x y = p0
    r' = radius * 1.16
    m = V2 (V3 r' 0 x)
           (V3 0 r' y)
    vs = map (\(V2 px py) -> m !* V3 px py 1) circleVertices
    format q = arcVertex q p0 radius fillColorCoord lineColorCoord lineWidth 0 (pi * 2)
    xs = zipWith (\p1 p2 -> [p2, p0, p1]) vs (tail . cycle $ vs)
    vertices = [(texture, concatMap (map format) xs)]

convertDrawing _ (ShapeDrawing _ (RoundRect _ width height _)) | width <= 0 || height <= 0 || nearZero width || nearZero height = mempty

convertDrawing resource (ShapeDrawing shapeStyle (RoundRect p0 width height radius)) | nearZero radius = convertDrawing resource (ShapeDrawing shapeStyle (Rectangle p0 width height))

convertDrawing resource (ShapeDrawing shapeStyle (RoundRect p0 width height radius')) = mempty { vgTriangleVertex = [(texture, tvs)], vgArcVertex = [(texture, avs)] }
    where
    radius = radius' `min` (height * 0.5) `min` (width * 0.5)
    V2 x y = p0
    lineStyle = shapeStyleLineStyle shapeStyle
    LineStyle lineColor lineWidth = fromMaybe (LineStyle (V4 0 0 0 0) 0) lineStyle
    PlainColorFillStyle fillColor = shapeStyleFillStyle shapeStyle
    colorCoords = paletteColorCoords . rrPalette $ resource
    texture = paletteTexture . rrPalette $ resource
    fillColorCoord = fromMaybe (V2 0 0) . Map.lookup fillColor $ colorCoords
    lineColorCoord = fromMaybe (V2 0 0) . Map.lookup lineColor $ colorCoords
    rectLineFlag = if lineWidth > radius
        then triangleBottomLine0 .|. triangleBottomLine2 .|. triangleTopLine0 .|. triangleTopLine2
        else 0
    genRect f0 f1 lw = genRectVertices fillColorCoord lineColorCoord lw lw f0 f1
    tvs = genRect triangleBottomLine2 triangleTopLine2 lineWidth (V2 (x + radius) y) (width - radius * 2) radius
        ++ genRect triangleBottomLine0 triangleTopLine0 lineWidth (V2 (x + width - radius) (y + radius)) radius (height - radius * 2)
        ++ genRect triangleTopLine2 triangleBottomLine2 lineWidth (V2 (x + radius) (y + height - radius)) (width - radius * 2) radius
        ++ genRect triangleTopLine0 triangleBottomLine0 lineWidth (V2 x (y + radius)) radius (height - radius * 2)
        ++ genRect rectLineFlag rectLineFlag (max 0 (lineWidth - radius)) (V2 (x + radius) (y + radius)) (width - radius * 2) (height - radius * 2)
    vs = take 4 . iterate (\(V2 vx vy) -> (V2 (-vy) vx)) $ V2 (-radius * 1.5) 0
    centers = genRectCoords (V2 (x + radius) (y + radius)) (width - radius * 2) (height - radius * 2)
    formatArcVertices q r = arcVertex r q radius fillColorCoord lineColorCoord lineWidth 0 (pi * 2)
    avs = concatMap (\(q, v) -> map (formatArcVertices q) $ genCornerCoords q v) $ zip centers vs
    genCornerCoords q v @ (V2 vx vy) = [q, q + v, q + V2 (-vy) vx]

convertDrawing resource (PathDrawing lineStyle (Arc p0 radius startAngle endAngle)) = mempty { vgArcVertex = vertices }
    where
    LineStyle lineColor lineWidth = lineStyle
    colorCoords = paletteColorCoords . rrPalette $ resource
    texture = paletteTexture . rrPalette $ resource
    fillColorCoord = V2 0 0
    lineColorCoord = fromMaybe (V2 0 0) . Map.lookup lineColor $ colorCoords
    V2 x y = p0
    r' = radius / sin (pi / 3)
    m = V2 (V3 r' 0 x)
           (V3 0 r' y)
    vs = map (\(V2 px py) -> m !* V3 px py 1) circleVertices
    format q = arcVertex q p0 radius fillColorCoord lineColorCoord lineWidth startAngle endAngle
    xs = zipWith (\p1 p2 -> [p2, p0, p1]) vs (tail . cycle $ vs)
    vertices = [(texture, concatMap (map format) xs)]

convertDrawing _ (PathDrawing _ (StripPath [])) = mempty

convertDrawing _ (PathDrawing _ (StripPath [_])) = mempty

convertDrawing resource (PathDrawing lineStyle (StripPath (p0 : p1 : ps))) = mempty { vgLineVertex = vertices }
    where
    LineStyle lineColor lineWidth = lineStyle
    colorCoords = paletteColorCoords . rrPalette $ resource
    texture = paletteTexture . rrPalette $ resource
    lineColorCoord = fromMaybe (V2 0 0) . Map.lookup lineColor $ colorCoords
    segs = zip (p0 : p1 : ps) (p1 : ps)
    segs' = zip3 (Nothing : map Just segs) segs (map Just (tail segs) ++ [Nothing])
    triangulate (Nothing, (q0, q1), Nothing) =
        let a0 = lineVertex q0 q1 q0 lineWidth 4 3 lineColorCoord
            a1 = lineVertex q0 q1 q0 lineWidth 4 4 lineColorCoord
            a2 = lineVertex q1 q0 q1 lineWidth 4 3 lineColorCoord
            a3 = lineVertex q1 q0 q1 lineWidth 4 4 lineColorCoord
        in [a0, a1, a2, a2, a3, a0]
    triangulate (Nothing, (q0, q1), Just (r0, r1)) =
        let a0 = lineVertex q0 q1 q0 lineWidth 4 3 lineColorCoord
            a1 = lineVertex q0 q1 q0 lineWidth 4 4 lineColorCoord
            a2 = lineVertex q1 q0 r1 lineWidth 4 1 lineColorCoord
            a3 = lineVertex q1 q0 r1 lineWidth 4 2 lineColorCoord
            b0 = lineVertex r0 r1 q0 lineWidth 4 1 lineColorCoord
            b1 = lineVertex r0 r1 q0 lineWidth 4 2 lineColorCoord
        in [a0, a1, a2, a2, a3, a0, a3, a2, b0, b1, b0, a2]
    triangulate (Just (s0, _), (q0, q1), Just (r0, r1)) =
        let a0 = lineVertex q0 q1 s0 lineWidth 4 1 lineColorCoord
            a1 = lineVertex q0 q1 s0 lineWidth 4 2 lineColorCoord
            a2 = lineVertex q1 q0 r1 lineWidth 4 1 lineColorCoord
            a3 = lineVertex q1 q0 r1 lineWidth 4 2 lineColorCoord
            b0 = lineVertex r0 r1 q0 lineWidth 4 1 lineColorCoord
            b1 = lineVertex r0 r1 q0 lineWidth 4 2 lineColorCoord
        in [a0, a1, a2, a2, a3, a0, a3, a2, b0, b1, b0, a2]
    triangulate (Just (a, _), (q0, q1), Nothing) =
        let a0 = lineVertex q0 q1 a lineWidth 4 1 lineColorCoord
            a1 = lineVertex q0 q1 a lineWidth 4 2 lineColorCoord
            a2 = lineVertex q1 q0 q1 lineWidth 4 3 lineColorCoord
            a3 = lineVertex q1 q0 q1 lineWidth 4 4 lineColorCoord
        in [a0, a1, a2, a2, a3, a0]
    vs = concatMap triangulate $ segs'
    vertices = [(texture, vs)]

circleDivision :: Int
circleDivision = 6

circleVertices :: [Coord]
circleVertices = vertices
    where
    division = circleDivision
    da = 2 * pi / fromIntegral division :: Float
    rmat = rotateMatrix da
    vertices = take division $ iterate (rmat !*) (V2 0 1)

genRectCoords :: Coord -> Float -> Float -> [Coord]
genRectCoords p0 width height = [p0, p1, p2, p3]
    where
    V2 x y = p0
    p1 = V2 (x + width) y
    p2 = V2 (x + width) (y + height)
    p3 = V2 x (y + height)

genRectVertices :: Coord -> Coord -> Float -> Float -> GL.GLuint -> GL.GLuint -> Coord -> Float -> Float -> [TriangleVertex]
genRectVertices fillColor lineColor bottomLineWidth topLineWidth lineFlags0 lineFlags1 p0 width height = vertices
    where
    _ : p1 : p2 : p3 : _ = genRectCoords p0 width height
    format (flag, (q0, q1, q2)) = triangleVertex q0 q1 q2 fillColor lineColor bottomLineWidth topLineWidth flag
    gen flag = zip (repeat flag) . take 3 . iterate rotate
    vs = gen lineFlags0 (p2, p0, p1) ++ gen lineFlags1 (p0, p2, p3)
    vertices = map format $ vs

collectColors :: [Drawing] -> Set Color
collectColors = Set.fromList . concatMap collectColorsOne

collectColorsOne :: Drawing -> [Color]
collectColorsOne (ShapeDrawing (ShapeStyle lineStyle fillStyle) _) = catMaybes $ fmap (lineStyleColor) lineStyle : getFillColor fillStyle : []
    where
    getFillColor (PlainColorFillStyle fillColor) = Just fillColor
    getFillColor _ = Nothing

collectColorsOne (PathDrawing lineStyle _) = [lineStyleColor lineStyle]

paletteSize :: (Int, Int)
paletteSize = (1024, 1024)

calcPaletteCoord :: Int -> Coord
calcPaletteCoord i = V2 x y
    where
    (w, h) = paletteSize
    x = fromIntegral (i `mod` w) / fromIntegral w
    y = fromIntegral (i `div` w) / fromIntegral h

allocatePalette :: ResourceT IO Palette
allocatePalette = do
    pbo <- allocatePixelBufferObject (w * h * colorByteSize)
    texture <- allocateTexImage2D GL.NoProxy 0 GL.RGBA' size 0 pdata
    return $ Palette Set.empty Map.empty Vector.empty texture pbo
    where
    (w, h) = paletteSize
    size = GL.TextureSize2D (fromIntegral w) (fromIntegral h)
    pdata = GL.PixelData GL.RGBA GL.UnsignedByte (Ptr.nullPtr :: Ptr.Ptr Word8)

updatePalette :: Palette -> Set Color -> IO Palette
updatePalette prev nextColors =
    if not (Set.null newColors)
    then do
        updatePaletteTexture prev nextStorage
        return (trace (show nextPalette) nextPalette)
    else return prev
    where
    Palette prevColors prevColorCoords prevStorage texture pbo = prev
    prevLength = Vector.length prevStorage
    newColors = Set.difference nextColors prevColors
    nextStorage = (prevStorage Vector.++) . Vector.fromList . Set.elems $ newColors
    unionColors = Set.union prevColors newColors
    newColorCoords = Map.fromList $ zip (Set.toList newColors) (map calcPaletteCoord [prevLength..])
    nextColorCoords = Map.union prevColorCoords newColorCoords
    nextPalette = Palette unionColors nextColorCoords nextStorage texture pbo

updatePaletteTexture :: Palette -> Vector Color -> IO ()
updatePaletteTexture prev storage = do
    GL.bindBuffer GL.PixelUnpackBuffer GL.$= Just pbo
    GL.textureBinding GL.Texture2D GL.$= Just texture
    GL.withMappedBuffer GL.PixelUnpackBuffer GL.ReadWrite writeAddedColors onUpdateFailure

    GL.textureFilter GL.Texture2D GL.$= ((GL.Nearest, Nothing), GL.Nearest)
    GL.texSubImage2D GL.Texture2D 0 pos size pdata
    GL.textureBinding GL.Texture2D GL.$= Nothing
    GL.bindBuffer GL.PixelUnpackBuffer GL.$= Nothing
    where
    Palette _ _ prevStorage texture pbo = prev
    offset = Vector.length prevStorage * colorByteSize
    writeAddedColors :: Ptr.Ptr Color -> IO ()
    writeAddedColors ptr =
        let dest = Ptr.plusPtr ptr offset
            len = Vector.length storage
        in do
            Vector.unsafeWith storage $
                \source -> copyArray dest source len
            content <- peekArray 20 ptr
            trace ("pbo content: " ++ show content) (putStrLn "")
    onUpdateFailure failure =
        throwIO . userError $ "mapBuffer failure: " ++ show failure
    (w, h) = paletteSize
    pos = GL.TexturePosition2D 0 0
    size = GL.TextureSize2D 2 1
    pdata = GL.PixelData GL.RGBA GL.UnsignedByte (Ptr.nullPtr :: Ptr.Ptr Word8)

allocateRenderInfo
    :: RenderResource
    -> [Uniform]
    -> [Drawing]
    -> ResourceT IO [RenderInfo]
allocateRenderInfo resource uniforms drawings = do
    rs0 <- mkRenderInfos p0 vs0
    rs1 <- mkRenderInfos p1 vs1
    rs2 <- mkRenderInfos p2 vs2
    return $ concat [rs0, rs1, rs2]
    where
    RenderResource (RenderProgramInfos p0 p1 p2) _ _ = resource
    VertexGroups vs0 vs1 vs2 = fold . map (convertDrawing resource) $ drawings
    group = List.groupBy (\(GL.TextureObject a, _) (GL.TextureObject b, _) -> a == b)
    mkBuffer bufferTarget xs = do
        let n = length xs
            size = fromIntegral $ n * sizeOf (head xs)
        buffer <- GL.genObjectName
        GL.bindBuffer bufferTarget GL.$= Just buffer
        withArray xs $ \ptr -> GL.bufferData bufferTarget GL.$= (size, ptr, GL.StreamDraw)
        GL.bindBuffer bufferTarget GL.$= Nothing
        return buffer
    mkRenderInfos program xs = do
        let ProgramInfo _ _ uniformLocations = program
            uniformInfos = zipWith UniformInfo uniformLocations uniforms
            gs = group xs
            gs' = zip (map (fst . head) gs) (map (concat . map snd) gs)
        mapM (mkRenderInfo program uniformInfos) gs'
    mkRenderInfo program uniformInfos (texture, vs) = do
        (_, buffer) <- Resource.allocate (mkBuffer GL.ArrayBuffer vs) GL.deleteObjectName
        return $ RenderInfo program GL.Triangles buffer 0 (fromIntegral $ length vs) uniformInfos texture

render :: RenderResource -> Canvas -> IO RenderResource
render resource (Canvas (V2 ox oy) w h drawings) =
    Resource.runResourceT $ do
        ms <- liftIO . mapM mkMat $ [projectionMatrix, modelViewMatrix]
        let us = map Uniform ms ++ [Uniform (GL.TextureUnit 0)]
        resource' <- liftIO $ do
            palette' <- updatePalette palette colors
            return $ resource { rrPalette = palette' }
        rs <- allocateRenderInfo resource' us drawings
        liftIO $ do
            GL.blend GL.$= GL.Enabled
            GL.blendFunc GL.$= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
            mapM_ renderInternal rs
        return resource'
    where
    projectionMatrix = ortho ox (ox + w) oy (oy + h) 1 (-1)
    modelViewMatrix = lookAt (V3 0 0 1) (V3 0 0 0) (V3 0 1 0)
    mkMat :: M44 Float -> IO (GL.GLmatrix GL.GLfloat)
    mkMat = GL.newMatrix GL.RowMajor . concatMap toList . toList
    palette = rrPalette resource
    colors = collectColors drawings

renderInternal
    :: RenderInfo
    -> IO ()
renderInternal info = do
    GL.currentProgram GL.$= Just program
    GL.bindBuffer GL.ArrayBuffer GL.$= Just vertexBuffer
    GL.activeTexture GL.$= (GL.TextureUnit 0)
    GL.textureBinding GL.Texture2D GL.$= Just texture
    mapM_ bindAttrib attribs
    mapM_ bindUniform uniforms
    GL.drawArrays mode (fromIntegral index) (fromIntegral num)
    GL.textureBinding GL.Texture2D GL.$= Nothing
    GL.bindBuffer GL.ArrayBuffer GL.$= Nothing
    GL.textureBinding GL.Texture2D GL.$= Nothing
    mapM_ unbindAttrib attribs

    where
    RenderInfo programInfo mode vertexBuffer index num uniforms texture = info
    ProgramInfo program attribs _ = programInfo

allocateRenderResource :: ResourceT IO RenderResource
allocateRenderResource = do
    triangleProgram <- allocateTriangleProgram
    arcProgram <- allocateArcProgram
    lineProgram <- allocateLineProgram
    palette <- allocatePalette
    return (RenderResource (RenderProgramInfos triangleProgram arcProgram lineProgram) Map.empty palette)

allocateProgramInfo
    :: String
    -> BS.ByteString
    -> BS.ByteString
    -> [VertexField]
    -> [String]
    -> ResourceT IO ProgramInfo
allocateProgramInfo name vertexShaderCode fragmentShaderCode attribParams uniformNames = do
    vertexShader <- allocateShader name GL.VertexShader vertexShaderCode
    fragmentShader <- allocateShader name GL.FragmentShader fragmentShaderCode
    program <- allocateProgram [vertexShader, fragmentShader]

    liftIO $ do
        attribs <- foldrM (allocateAttrib program) [] attribParams
        uniforms <- mapM (GL.uniformLocation program) uniformNames
        return $ ProgramInfo program attribs uniforms
    where
    stride = sum (map vfByteSize attribParams)
    allocateAttrib program (VertexField location attribName dataType num _ byteOffset ihandling) xs = do
        GL.attribLocation program attribName GL.$= location
        return $ AttribInfo location dataType num ihandling stride byteOffset : xs

allocateTriangleProgram :: ResourceT IO ProgramInfo
allocateTriangleProgram = allocateProgramInfo
    "triangle"
    $(embedFile "shader/triangle-vertex.glsl")
    $(embedFile "shader/triangle-fragment.glsl")
    vfs
    uniformNames

    where
    vfs = vertexFields . vertexSpec $ (Proxy :: Proxy TriangleVertex)
    uniformNames =
        [ "projectionMatrix"
        , "modelViewMatrix"
        , "texture"
        ]

allocateArcProgram :: ResourceT IO ProgramInfo
allocateArcProgram = allocateProgramInfo
    "arc"
    $(embedFile "shader/arc-vertex.glsl")
    $(embedFile "shader/arc-fragment.glsl")
    vfs
    uniformNames

    where
    vfs = vertexFields $ vertexSpec (Proxy :: Proxy ArcVertex)
    uniformNames =
        [ "projectionMatrix"
        , "modelViewMatrix"
        , "texture"
        ]

allocateLineProgram :: ResourceT IO ProgramInfo
allocateLineProgram = allocateProgramInfo
    "line"
    $(embedFile "shader/line-vertex.glsl")
    $(embedFile "shader/line-fragment.glsl")

    vfs
    uniformNames

    where
    vfs = vertexFields $ vertexSpec (Proxy :: Proxy LineVertex)
    uniformNames =
        [ "projectionMatrix"
        , "modelViewMatrix"
        , "texture"
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
bindUniform (UniformInfo l (Uniform u)) =
    GL.uniform l GL.$= u

allocateShader :: String -> GL.ShaderType -> BS.ByteString -> ResourceT IO GL.Shader
allocateShader name shaderType src = do
    (_, shader) <- Resource.allocate mkShader GL.deleteObjectName
    return shader
    where
    mkShader = do
        shader <- GL.createShader shaderType
        GL.shaderSourceBS shader GL.$= src
        GL.compileShader shader
        checkStatus GL.compileStatus GL.shaderInfoLog ("shader " ++ name ++ " compile error") shader
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

allocateTexImage2D
    :: GL.Proxy
    -> GL.Level
    -> GL.PixelInternalFormat
    -> GL.TextureSize2D
    -> GL.Border
    -> GL.PixelData a
    -> ResourceT IO GL.TextureObject
allocateTexImage2D proxy level format size boader texData = do
    (_, texture) <- Resource.allocate mkTexture GL.deleteObjectName
    return texture

    where
    mkTexture = do
        texture <- GL.genObjectName
        GL.activeTexture GL.$= (GL.TextureUnit 0)
        GL.textureBinding GL.Texture2D GL.$= Just texture
        GL.textureFilter GL.Texture2D GL.$= ((GL.Linear', Nothing), GL.Linear')
        GL.texImage2D GL.Texture2D proxy level format size boader texData
        return texture

addTextureResource
    :: Text
    -> GL.Proxy
    -> GL.Level
    -> GL.PixelInternalFormat
    -> GL.TextureSize2D
    -> GL.Border
    -> GL.PixelData a
    -> RenderResource
    -> ResourceT IO RenderResource
addTextureResource textureName proxy level format size boader texData rr @ (RenderResource _ ts _) = do
    texture <- allocateTexImage2D proxy level format size boader texData
    return $ rr { rrTextures = Map.insert textureName texture ts }

allocatePixelBufferObject :: Int -> ResourceT IO GL.BufferObject
allocatePixelBufferObject byteSize = do
    (_, pbo) <- Resource.allocate mkPbo GL.deleteObjectName
    return pbo
    where
    mkPbo = do
        pbo <- GL.genObjectName
        GL.bindBuffer GL.PixelUnpackBuffer GL.$= Just pbo
        GL.bufferData GL.PixelUnpackBuffer GL.$= (fromIntegral byteSize, (Ptr.nullPtr :: Ptr.Ptr Word8), GL.StreamDraw)
        GL.bindBuffer GL.PixelUnpackBuffer GL.$= Nothing
        return pbo

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

triangleTopLine0 :: GL.GLuint
triangleTopLine0 = 1 `shiftL` 3

triangleTopLine1 :: GL.GLuint
triangleTopLine1 = 1 `shiftL` 4

triangleTopLine2 :: GL.GLuint
triangleTopLine2 = 1 `shiftL` 5

colorByteSize :: Int
colorByteSize = sizeOf (undefined :: Color)

traceTexture = do
    tptr <- Ptr.mallocBytes (1024 * 1024 * 4)
    let pd = GL.PixelData GL.RGBA GL.UnsignedByte (tptr :: Ptr.Ptr Word8)
    GL.getTexImage GL.Texture2D 0 pd
    content <- peekArray 20 (Ptr.castPtr tptr :: Ptr.Ptr Color)
    trace ("texture content:" ++ show content) (putStrLn "")
    Ptr.free tptr
