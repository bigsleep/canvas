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
import Foreign.Marshal.Array (withArray, copyArray)
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
    , riTexture :: !(Maybe GL.TextureObject)
    }

data RenderProgramInfos = RenderProgramInfos
    { rpiTriangleProgramInfo :: !ProgramInfo
    , rpiArcProgramInfo :: !ProgramInfo
    , rpiLineProgramInfo :: !ProgramInfo
    , rpiTexturedProgramInfo :: !ProgramInfo
    } deriving (Show)

data RenderResource = RenderResource
    { rrRenderProgramInfos :: !RenderProgramInfos
    , rrTextures :: !(Map Text GL.TextureObject)
    , rrPalette :: !Palette
    } deriving (Show)

data TexturedVertexUnit = TexturedVertexUnit
    { tvuTextureName :: !Text
    , tvuVertex :: ![TexturedVertex]
    } deriving (Show)

data VertexGroups = VertexGroups
    { vgTriangleVertex :: ![TriangleVertex]
    , vgArcVertex :: ![ArcVertex]
    , vgLineVertex :: ![LineVertex]
    , vgTexturedVertex :: ![TexturedVertexUnit]
    } deriving (Show)

data Palette = Palette
    { paletteColors :: !(Set Color)
    , paletteColorCoords :: !(Map Color (V2 Float))
    , paletteStorage :: !(Vector Color)
    , paletteTexture :: !GL.TextureObject
    , palettePixelBufferObject :: !GL.BufferObject
    } deriving (Show)

instance Monoid VertexGroups where
    mappend (VertexGroups as bs cs ds) (VertexGroups as' bs' cs' ds') = VertexGroups (as ++ as') (bs ++ bs') (cs ++ cs') (ds ++ ds')
    mempty = VertexGroups [] [] [] []

convertDrawing :: Drawing -> VertexGroups
convertDrawing (ShapeDrawing (ShapeStyle lineStyle (PlainColorFillStyle fillColor)) (Triangle p0 p1 p2)) = mempty { vgTriangleVertex = vertices }
    where
    (lineColor, lineWidth, lineFlags) = case lineStyle of
        Nothing -> (V4 0 0 0 0, 0, 0)
        Just (LineStyle c w) -> (c, w, triangleBottomLine0 .|. triangleBottomLine1 .|. triangleBottomLine2)
    vs = take 3 $ iterate rotate (p0, p1, p2)
    format (q0, q1, q2) = triangleVertex q0 q1 q2 fillColor lineColor lineWidth 0 lineFlags
    vertices = map format vs

convertDrawing (ShapeDrawing (ShapeStyle _ (TexturedFillStyle textureRange)) (Triangle p0 p1 p2)) = mempty { vgTexturedVertex = [vertices] }
    where
    TextureRange textureName (V2 x0 y0) (V2 x1 y1) = textureRange
    vertices = TexturedVertexUnit textureName [texturedVertex p0 (V2 x0 y0), texturedVertex p1 (V2 x0 y1), texturedVertex p2 (V2 x1 y0)]

convertDrawing (ShapeDrawing _ (Rectangle _ width height)) | width <= 0 || height <= 0 || nearZero width || nearZero height = mempty

convertDrawing (ShapeDrawing (ShapeStyle lineStyle (PlainColorFillStyle fillColor)) (Rectangle p0 width height)) = mempty { vgTriangleVertex = vertices }
    where
    (lineColor, lineWidth, lineFlags) = case lineStyle of
        Nothing -> (V4 0 0 0 0, 0, 0)
        Just (LineStyle c w) -> (c, w, triangleBottomLine0 .|. triangleBottomLine2 .|. triangleTopLine0 .|. triangleTopLine2)
    vertices = genRectVertices fillColor lineColor lineWidth lineWidth lineFlags lineFlags p0 width height

convertDrawing (ShapeDrawing _ (Circle _ radius)) | radius <= 0 = mempty

convertDrawing (ShapeDrawing (ShapeStyle lineStyle (PlainColorFillStyle fillColor)) (Circle p0 radius)) = mempty { vgArcVertex = vertices }
    where
    (lineColor, lineWidth) = case lineStyle of
        Nothing -> (V4 0 0 0 0, 0)
        Just (LineStyle c w) -> (c, w)
    V2 x y = p0
    r' = radius * 1.16
    m = V2 (V3 r' 0 x)
           (V3 0 r' y)
    vs = map (\(V2 px py) -> m !* V3 px py 1) circleVertices
    format q = arcVertex q p0 radius fillColor lineColor lineWidth 0 (pi * 2)
    xs = zipWith (\p1 p2 -> [p2, p0, p1]) vs (tail . cycle $ vs)
    vertices = concatMap (map format) xs

convertDrawing (ShapeDrawing _ (RoundRect _ width height _)) | width <= 0 || height <= 0 || nearZero width || nearZero height = mempty

convertDrawing (ShapeDrawing shapeStyle (RoundRect p0 width height radius)) | nearZero radius = convertDrawing (ShapeDrawing shapeStyle (Rectangle p0 width height))

convertDrawing (ShapeDrawing shapeStyle (RoundRect p0 width height radius')) = mempty { vgTriangleVertex = tvs, vgArcVertex = avs }
    where
    radius = radius' `min` (height * 0.5) `min` (width * 0.5)
    V2 x y = p0
    lineStyle = shapeStyleLineStyle shapeStyle
    LineStyle lineColor lineWidth = fromMaybe (LineStyle (V4 0 0 0 0) 0) lineStyle
    PlainColorFillStyle fillColor = shapeStyleFillStyle shapeStyle
    rectLineFlag = if lineWidth > radius
        then triangleBottomLine0 .|. triangleBottomLine2 .|. triangleTopLine0 .|. triangleTopLine2
        else 0
    genRect f0 f1 lw = genRectVertices fillColor lineColor lw lw f0 f1
    tvs = genRect triangleBottomLine2 triangleTopLine2 lineWidth (V2 (x + radius) y) (width - radius * 2) radius
        ++ genRect triangleBottomLine0 triangleTopLine0 lineWidth (V2 (x + width - radius) (y + radius)) radius (height - radius * 2)
        ++ genRect triangleTopLine2 triangleBottomLine2 lineWidth (V2 (x + radius) (y + height - radius)) (width - radius * 2) radius
        ++ genRect triangleTopLine0 triangleBottomLine0 lineWidth (V2 x (y + radius)) radius (height - radius * 2)
        ++ genRect rectLineFlag rectLineFlag (max 0 (lineWidth - radius)) (V2 (x + radius) (y + radius)) (width - radius * 2) (height - radius * 2)
    vs = take 4 . iterate (\(V2 vx vy) -> (V2 (-vy) vx)) $ V2 (-radius * 1.5) 0
    centers = genRectCoords (V2 (x + radius) (y + radius)) (width - radius * 2) (height - radius * 2)
    formatArcVertices q r = arcVertex r q radius fillColor lineColor lineWidth 0 (pi * 2)
    avs = concatMap (\(q, v) -> map (formatArcVertices q) $ genCornerCoords q v) $ zip centers vs
    genCornerCoords q v @ (V2 vx vy) = [q, q + v, q + V2 (-vy) vx]

convertDrawing (PathDrawing lineStyle (Arc p0 radius startAngle endAngle)) = mempty { vgArcVertex = vertices }
    where
    LineStyle lineColor lineWidth = lineStyle
    fillColor = V4 0 0 0 0
    V2 x y = p0
    r' = radius / sin (pi / 3)
    m = V2 (V3 r' 0 x)
           (V3 0 r' y)
    vs = map (\(V2 px py) -> m !* V3 px py 1) circleVertices
    format q = arcVertex q p0 radius fillColor lineColor lineWidth startAngle endAngle
    xs = zipWith (\p1 p2 -> [p2, p0, p1]) vs (tail . cycle $ vs)
    vertices = concatMap (map format) xs

convertDrawing (PathDrawing _ (StripPath [])) = mempty

convertDrawing (PathDrawing _ (StripPath [_])) = mempty

convertDrawing (PathDrawing lineStyle (StripPath (p0 : p1 : ps))) = mempty { vgLineVertex = vertices }
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

collectColors :: [Drawing] -> Set Color
collectColors = Set.fromList . concatMap collectColorsOne

collectColorsOne :: Drawing -> [Color]
collectColorsOne (ShapeDrawing (ShapeStyle lineStyle fillStyle) _) = catMaybes $ fmap lineStyleColor lineStyle : getFillColor fillStyle : []
    where
    getFillColor (PlainColorFillStyle fillColor) = Just fillColor
    getFillColor _ = Nothing

collectColorsOne (PathDrawing lineStyle _) = [lineStyleColor lineStyle]

paletteSize :: (Int, Int)
paletteSize = (1024, 1024)

calcPaletteCoord :: Int -> V2 Float
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
updatePalette prev nextColors = do
    updatePaletteTexture prev nextStorage
    return nextPalette
    where
    Palette prevColors prevColorCoords prevStorage texture pbo = prev
    prevLength = Vector.length prevStorage
    newColors = Set.difference nextColors prevColors
    nextStorage = (prevStorage Vector.++) . Vector.fromList . Set.elems $ newColors
    nextColors = Set.union prevColors newColors
    newColorCoords = Map.fromList $ zip (Set.toList newColors) (map calcPaletteCoord [prevLength..])
    nextColorCoords = Map.union prevColorCoords newColorCoords
    nextPalette = Palette nextColors nextColorCoords nextStorage texture pbo

updatePaletteTexture :: Palette -> Vector Color -> IO ()
updatePaletteTexture prev storage = do
    GL.bindBuffer GL.PixelPackBuffer GL.$= Just pbo
    GL.withMappedBuffer GL.PixelPackBuffer GL.ReadWrite writeAddedColors onUpdateFailure
    GL.bindBuffer GL.PixelPackBuffer GL.$= Nothing

    GL.bindBuffer GL.PixelUnpackBuffer GL.$= Just pbo
    GL.activeTexture GL.$= (GL.TextureUnit 0)
    GL.textureBinding GL.Texture2D GL.$= Just texture
    GL.texSubImage2D GL.Texture2D 0 pos size pdata
    GL.bindBuffer GL.PixelUnpackBuffer GL.$= Nothing
    where
    Palette _ _ prevStorage texture pbo = prev
    offset = Vector.length prevStorage * colorByteSize
    writeAddedColors ptr =
        let dest = Ptr.plusPtr ptr offset
            len = Vector.length storage
        in Vector.unsafeWith storage $
            \source -> copyArray dest source len
    onUpdateFailure failure =
        throwIO . userError $ "mapBuffer failure: " ++ show failure
    (w, h) = paletteSize
    pos = GL.TexturePosition2D 0 0
    size = GL.TextureSize2D (fromIntegral w) (fromIntegral h)
    pdata = GL.PixelData GL.RGBA GL.UnsignedByte (Ptr.nullPtr :: Ptr.Ptr Word8)

genRectCoords :: Coord -> Float -> Float -> [Coord]
genRectCoords p0 width height = [p0, p1, p2, p3]
    where
    V2 x y = p0
    p1 = V2 (x + width) y
    p2 = V2 (x + width) (y + height)
    p3 = V2 x (y + height)

genRectVertices :: Color -> Color -> Float -> Float -> GL.GLuint -> GL.GLuint -> Coord -> Float -> Float -> [TriangleVertex]
genRectVertices fillColor lineColor bottomLineWidth topLineWidth lineFlags0 lineFlags1 p0 width height = vertices
    where
    _ : p1 : p2 : p3 : _ = genRectCoords p0 width height
    format (flag, (q0, q1, q2)) = triangleVertex q0 q1 q2 fillColor lineColor bottomLineWidth topLineWidth flag
    gen flag = zip (repeat flag) . take 3 . iterate rotate
    vs = gen lineFlags0 (p2, p0, p1) ++ gen lineFlags1 (p0, p2, p3)
    vertices = map format $ vs

allocateRenderInfo
    :: RenderResource
    -> [Uniform]
    -> [Drawing]
    -> ResourceT IO [RenderInfo]
allocateRenderInfo resource uniforms drawings = sequence $
    [ mkRenderInfo p1 vs1
    , mkRenderInfo p2 vs2
    , mkRenderInfo p3 vs3
    ]
    ++
    catMaybes (map (mkRenderInfoTvu p4) tvuGroups)
    where
    RenderResource (RenderProgramInfos p1 p2 p3 p4) ts _ = resource
    VertexGroups vs1 vs2 vs3 vs4 = fold . map convertDrawing $ drawings
    mkBuffer bufferTarget xs = do
        let n = length xs
            size = fromIntegral $ n * sizeOf (head xs)
        buffer <- GL.genObjectName
        GL.bindBuffer bufferTarget GL.$= Just buffer
        withArray xs $ \ptr -> GL.bufferData bufferTarget GL.$= (size, ptr, GL.StreamDraw)
        GL.bindBuffer bufferTarget GL.$= Nothing
        return buffer
    mkRenderInfo program vs = do
        (_, buffer) <- Resource.allocate (mkBuffer GL.ArrayBuffer vs) GL.deleteObjectName
        let ProgramInfo _ _ uniformLocations = program
            uniformInfos = zipWith UniformInfo uniformLocations uniforms
        return $ RenderInfo program GL.Triangles buffer 0 (fromIntegral $ length vs) uniformInfos Nothing
    tvuGroups = List.groupBy (\a b -> tvuTextureName a == tvuTextureName b) vs4
    mkRenderInfoTvu _ [] = Nothing
    mkRenderInfoTvu program xs @ (x : _) = Just $ do
        let tname = tvuTextureName x
            texture = Map.lookup tname ts
            vs = concatMap tvuVertex xs
            ProgramInfo _ _ uniformLocations = program
            uniformInfos = zipWith UniformInfo uniformLocations (uniforms ++ [Uniform (GL.TextureUnit 0)])
        (_, buffer) <- Resource.allocate (mkBuffer GL.ArrayBuffer vs) GL.deleteObjectName
        return $ RenderInfo program GL.Triangles buffer 0 (fromIntegral $ length vs) uniformInfos texture

render :: RenderResource -> Canvas -> IO RenderResource
render resource (Canvas (V2 ox oy) w h drawings) =
    Resource.runResourceT $ do
        ms <- liftIO . mapM mkMat $ [projectionMatrix, modelViewMatrix]
        let us = map Uniform ms
        rs <- allocateRenderInfo resource us drawings
        liftIO $ do
            GL.blend GL.$= GL.Enabled
            GL.blendFunc GL.$= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
            mapM_ renderInternal rs
        return resource
    where
    projectionMatrix = ortho ox (ox + w) oy (oy + h) 1 (-1)
    modelViewMatrix = lookAt (V3 0 0 1) (V3 0 0 0) (V3 0 1 0)
    mkMat :: M44 Float -> IO (GL.GLmatrix GL.GLfloat)
    mkMat = GL.newMatrix GL.RowMajor . concatMap toList . toList

renderInternal
    :: RenderInfo
    -> IO ()
renderInternal info = do
    GL.currentProgram GL.$= Just program
    GL.bindBuffer GL.ArrayBuffer GL.$= Just vertexBuffer
    when (isJust texture) $ do
        GL.activeTexture GL.$= (GL.TextureUnit 0)
        GL.textureBinding GL.Texture2D GL.$= texture
    mapM_ bindAttrib attribs
    mapM_ bindUniform uniforms
    GL.drawArrays mode (fromIntegral index) (fromIntegral num)
    GL.textureBinding GL.Texture2D GL.$= Nothing
    GL.bindBuffer GL.ArrayBuffer GL.$= Nothing
    when (isJust texture) $ do
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
    texturedProgram <- allocateTexturedProgram
    palette <- allocatePalette
    return (RenderResource (RenderProgramInfos triangleProgram arcProgram lineProgram texturedProgram) Map.empty palette)

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

allocateTexturedProgram :: ResourceT IO ProgramInfo
allocateTexturedProgram = allocateProgramInfo
    $(embedFile "shader/textured-vertex.glsl")
    $(embedFile "shader/textured-fragment.glsl")
    vfs
    uniformNames

    where
    vfs = vertexFields $ vertexSpec (Proxy :: Proxy TexturedVertex)
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
bindUniform (UniformInfo l (Uniform u)) =
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
        GL.bufferData GL.PixelUnpackBuffer GL.$= (fromIntegral byteSize, (Ptr.nullPtr :: Ptr.Ptr Word8), GL.DynamicDraw)
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
