{-# LANGUAGE TypeOperators, DataKinds, FlexibleContexts, FlexibleInstances, UndecidableInstances, PolyKinds, TemplateHaskell, RankNTypes, ConstraintKinds, ScopedTypeVariables, MultiParamTypeClasses, GADTs #-}
module Graphics.Canvas.Rendering.OpenGL.Vertex
    ( VertexField(..)
    , VertexSpec(..)
    , Vertex(..)
    , TriangleVertex
    , triangleVertex
    , CircleVertex
    , circleVertex
    , ArcVertex
    , arcVertex
    , LineVertex
    , lineVertex
    , TexturedVertex
    , texturedVertex
    , alignOffset
    ) where

import Control.Monad (void)
import qualified Control.Monad.Trans.State as State
import Control.Monad.Trans.Class (lift)
import Data.Constraint (Dict(..))
import Data.Extensible ((:*)(..), (<:), (@=), Assoc(..), AssocKey, AssocValue, Comp(..), Const'(..), Field(..), Forall(..), KeyValue, Membership, Record, hfoldMap, hzipWith, hsequence, library, mkField)
import Data.Extensible.Internal (getMemberId)
import Data.Functor.Identity (Identity(..))
import Data.Proxy (Proxy(..))
import Foreign.Ptr (plusPtr, castPtr)
import Foreign.Storable (Storable(..))
import GHC.TypeLits (KnownSymbol, symbolVal)
import qualified Graphics.Rendering.OpenGL as GL
import Linear (V1(..), V2(..), V3(..), V4(..))

data VertexField = VertexField
    { vfAttribLocation :: !GL.AttribLocation
    , vfName :: !String
    , vfDataType :: !GL.DataType
    , vfNumberOfValue :: !Int
    , vfByteSize :: !Int
    , vfByteOffset :: !Int
    , vfIntegerlHandling :: !GL.IntegerHandling
    } deriving (Show, Eq)

data VertexSpec = VertexSpec
    { vertexFields :: ![VertexField]
    } deriving (Show, Eq)

class HasDataType a where
    dataType :: Proxy a -> GL.DataType
    integerHandling :: Proxy a -> GL.IntegerHandling

instance HasDataType GL.GLubyte where
    dataType _ = GL.UnsignedByte
    integerHandling _ = GL.KeepIntegral
instance HasDataType GL.GLbyte where
    dataType _ = GL.Byte
    integerHandling _ = GL.KeepIntegral
instance HasDataType GL.GLushort where
    dataType _ = GL.UnsignedShort
    integerHandling _ = GL.KeepIntegral
instance HasDataType GL.GLshort where
    dataType _ = GL.Short
    integerHandling _ = GL.KeepIntegral
instance HasDataType GL.GLuint where
    dataType _ = GL.UnsignedInt
    integerHandling _ = GL.KeepIntegral
instance HasDataType GL.GLint where
    dataType _ = GL.Int
    integerHandling _ = GL.KeepIntegral
instance HasDataType GL.GLhalf where
    dataType _ = GL.HalfFloat
    integerHandling _ = GL.ToFloat
instance HasDataType GL.GLfloat where
    dataType _ = GL.Float
    integerHandling _ = GL.ToFloat
instance HasDataType GL.GLdouble where
    dataType _ = GL.Double
    integerHandling _ = GL.ToFloat

class (Storable a) => IsVertexAttrib a where
    valueDataType :: Proxy a -> GL.DataType
    numberOfValue :: Proxy a -> Int
    byteSize :: Proxy a -> Int
    integerHandling' :: Proxy a -> GL.IntegerHandling

instance {-# OVERLAPPABLE #-} (Storable a, HasDataType a) => IsVertexAttrib a where
    valueDataType = dataType
    numberOfValue _ = 1
    byteSize _ = sizeOf (undefined :: a)
    integerHandling' _ = integerHandling (Proxy :: Proxy a)

instance (Storable a, HasDataType a) => IsVertexAttrib (V1 a) where
    valueDataType _ = dataType (Proxy :: Proxy a)
    numberOfValue _ = 1
    byteSize _ = sizeOf (undefined :: V1 a)
    integerHandling' _ = integerHandling (Proxy :: Proxy a)

instance (Storable a, HasDataType a) => IsVertexAttrib (V2 a) where
    valueDataType _ = dataType (Proxy :: Proxy a)
    numberOfValue _ = 2
    byteSize _ = sizeOf (undefined :: V2 a)
    integerHandling' _ = integerHandling (Proxy :: Proxy a)

instance (Storable a, HasDataType a) => IsVertexAttrib (V3 a) where
    valueDataType _ = dataType (Proxy :: Proxy a)
    numberOfValue _ = 3
    byteSize _ = sizeOf (undefined :: V3 a)
    integerHandling' _ = integerHandling (Proxy :: Proxy a)

instance (Storable a, HasDataType a) => IsVertexAttrib (V4 a) where
    valueDataType _ = dataType (Proxy :: Proxy a)
    numberOfValue _ = 4
    byteSize _ = sizeOf (undefined :: V4 a)
    integerHandling' _ = integerHandling (Proxy :: Proxy a)

htraverseFor :: forall c f g h proxy xs. (Forall c xs, Applicative f) => proxy c -> (forall x. (c x) => g x -> f (h x)) -> g :* xs -> f (h :* xs)
htraverseFor _ f =
    hsequence . hzipWith (\(Comp Dict) a -> Comp (f a)) (library :: Comp Dict c :* xs)

newtype Lcm = Lcm { getLcm :: Int } deriving (Show, Eq)

instance Monoid Lcm where
    mempty = Lcm 1
    mappend (Lcm a) (Lcm b) = (Lcm (lcm a b))

alignOffset :: forall v. Storable v => v -> Int -> Int
alignOffset a offset = offset + ((alignment a - offset) `mod` alignment a)

newtype WrapRecord xs = WrapRecord { unWrapRecord :: (Record xs) }

instance (Forall (KeyValue KnownSymbol Storable) xs) => Storable (WrapRecord xs) where
    sizeOf = flip alignOffset size'
        where
        f :: forall v kv. (v ~ AssocValue kv, KeyValue KnownSymbol Storable kv) => Membership xs kv -> State.State Int (Proxy kv)
        f _ = State.modify ((+) (sizeOf (undefined :: v)) . alignOffset (undefined :: v)) >> return (Proxy :: Proxy kv)
        r = hgenerateFor (Proxy :: Proxy (KeyValue KnownSymbol Storable)) $ f
        size' = State.execState r 0
    alignment _ = getLcm . hfoldMap (Lcm . getConst') . runIdentity $ r
        where
        f :: forall v kv. (v ~ AssocValue kv, KeyValue KnownSymbol Storable kv) => Membership xs kv -> Identity (Const' Int kv)
        f _ = return . Const' . alignment $ (undefined :: v)
        r = hgenerateFor (Proxy :: Proxy (KeyValue KnownSymbol Storable)) $ f
    peek ptr = fmap WrapRecord . flip State.evalStateT 0 $ hgenerateFor (Proxy :: Proxy (KeyValue KnownSymbol Storable)) f
        where
        f :: forall kv v. (v ~ AssocValue kv, Storable v) => Membership xs kv -> State.StateT Int IO (Field Identity kv)
        f _ = do
              offset <- State.get
              let offset' = alignOffset (undefined :: v) offset
              State.put (offset' + sizeOf (undefined :: v))
              a <- lift . peek $ castPtr ptr `plusPtr` offset'
              return (Field (Identity (a)))
    poke ptr =
        void . flip State.evalStateT 0 . htraverseFor proxy f . unWrapRecord
        where
            proxy = Proxy :: Proxy (KeyValue KnownSymbol Storable)
            f :: (KeyValue KnownSymbol Storable kv) => Field Identity kv -> State.StateT Int IO (Field Identity kv)
            f kv @ (Field (Identity x)) = do
                offset <- State.get
                let offset' = alignOffset x offset
                State.put (offset' + sizeOf x)
                lift $ poke (castPtr ptr `plusPtr` offset') x
                return kv

class Vertex a where
    vertexSpec :: Proxy a -> VertexSpec

instance (Storable (WrapRecord xs), Forall (KeyValue KnownSymbol IsVertexAttrib) xs) => Vertex (Record xs) where
    vertexSpec _ = VertexSpec . hfoldMap (return . getConst' . getComp) . flip State.evalState 0 $ r
        where
        f :: forall k v kv. (k ~ AssocKey kv, v ~ AssocValue kv, KeyValue KnownSymbol IsVertexAttrib kv)
          => Membership xs kv -> State.State Int (Comp (Const' VertexField) (Field Identity) kv)
        f m = do
              offset <- State.get
              let location = GL.AttribLocation . fromIntegral . getMemberId $ m
                  vproxy = Proxy :: Proxy v
                  name = symbolVal (Proxy :: Proxy k)
                  glDataType = valueDataType vproxy
                  num = numberOfValue vproxy
                  size = byteSize vproxy
                  ihandling = integerHandling' vproxy
                  offset' = offset + (offset `mod` alignment (undefined :: v))
              State.put $ offset' + size
              return . Comp . Const' $ VertexField location name glDataType num size offset' ihandling
        r = hgenerateFor (Proxy :: Proxy (KeyValue KnownSymbol IsVertexAttrib)) f

mkField "prevPosition position nextPosition color lineColor bottomLineWidth topLineWidth lineFlags lineWidth center radius startAngle endAngle otherEndPosition jointEndPosition miterLimit positionType textureCoord"

type TriangleVertexFields =
    '[ "prevPosition" ':> V2 Float
    , "position" ':> V2 Float
    , "nextPosition" ':> V2 Float
    , "color" ':> V4 Float
    , "lineColor" ':> V4 Float
    , "bottomLineWidth" ':> Float
    , "topLineWidth" ':> Float
    , "lineFlags" ':> GL.GLuint
    ]

type TriangleVertexRecord = Record TriangleVertexFields

newtype TriangleVertex = TriangleVertex TriangleVertexRecord deriving (Show)

triangleVertex :: V2 Float -> V2 Float -> V2 Float -> V4 Float -> V4 Float -> Float -> Float -> GL.GLuint -> TriangleVertex
triangleVertex prevPosition' position' nextPosition' color' lineColor' bottomLineWidth' topLineWidth' lineFlags' = TriangleVertex
    $ prevPosition @= prevPosition'
    <: position @= position'
    <: nextPosition @= nextPosition'
    <: color @= color'
    <: lineColor @= lineColor'
    <: bottomLineWidth @= bottomLineWidth'
    <: topLineWidth @= topLineWidth'
    <: lineFlags @= lineFlags'
    <: Nil

sizeOfTriangleVertex :: Int
sizeOfTriangleVertex = sizeOf (undefined :: WrapRecord TriangleVertexFields)

alignmentOfTriangleVertex :: Int
alignmentOfTriangleVertex = alignment (undefined :: WrapRecord TriangleVertexFields)

triangleVertexSpec :: VertexSpec
triangleVertexSpec = vertexSpec (Proxy :: Proxy TriangleVertexRecord)

instance Storable TriangleVertex where
    sizeOf _ = sizeOfTriangleVertex
    alignment _ = alignmentOfTriangleVertex
    peek ptr = (TriangleVertex . unWrapRecord) `fmap` (peek . castPtr $ ptr)
    poke ptr (TriangleVertex v) = poke (castPtr ptr) (WrapRecord v)

instance Vertex TriangleVertex where
    vertexSpec _ = triangleVertexSpec


type CircleVertexFields =
    '[ "position" ':> V2 Float
    , "center" ':> V2 Float
    , "radius" ':> Float
    , "color" ':> V4 Float
    , "lineColor" ':> V4 Float
    , "lineWidth" ':> Float
    ]

type CircleVertexRecord = Record CircleVertexFields

newtype CircleVertex = CircleVertex CircleVertexRecord deriving (Show)

circleVertex :: V2 Float -> V2 Float -> Float -> V4 Float -> V4 Float -> Float -> CircleVertex
circleVertex position' center' radius' color' lineColor' lineWidth' = CircleVertex
    $ position @= position'
    <: center @= center'
    <: radius @= radius'
    <: color @= color'
    <: lineColor @= lineColor'
    <: lineWidth @= lineWidth'
    <: Nil

sizeOfCircleVertex :: Int
sizeOfCircleVertex = sizeOf (undefined :: WrapRecord CircleVertexFields)

alignmentOfCircleVertex :: Int
alignmentOfCircleVertex = alignment (undefined :: WrapRecord CircleVertexFields)

circleVertexSpec :: VertexSpec
circleVertexSpec = vertexSpec (Proxy :: Proxy CircleVertexRecord)

instance Storable CircleVertex where
    sizeOf _ = sizeOfCircleVertex
    alignment _ = alignmentOfCircleVertex
    peek ptr = (CircleVertex . unWrapRecord) `fmap` (peek . castPtr $ ptr)
    poke ptr (CircleVertex v) = poke (castPtr ptr) (WrapRecord v)

instance Vertex CircleVertex where
    vertexSpec _ = circleVertexSpec


type ArcVertexFields =
    '[ "position" ':> V2 Float
    , "center" ':> V2 Float
    , "radius" ':> Float
    , "lineColor" ':> V4 Float
    , "lineWidth" ':> Float
    , "startAngle" ':> Float
    , "endAngle" ':> Float
    ]

type ArcVertexRecord = Record ArcVertexFields

newtype ArcVertex = ArcVertex ArcVertexRecord deriving (Show)

arcVertex :: V2 Float -> V2 Float -> Float -> V4 Float -> Float -> Float -> Float -> ArcVertex
arcVertex position' center' radius' lineColor' lineWidth' startAngle' endAngle' = ArcVertex
    $ position @= position'
    <: center @= center'
    <: radius @= radius'
    <: lineColor @= lineColor'
    <: lineWidth @= lineWidth'
    <: startAngle @= startAngle'
    <: endAngle @= endAngle'
    <: Nil

sizeOfArcVertex :: Int
sizeOfArcVertex = sizeOf (undefined :: WrapRecord ArcVertexFields)

alignmentOfArcVertex :: Int
alignmentOfArcVertex = alignment (undefined :: WrapRecord ArcVertexFields)

arcVertexSpec :: VertexSpec
arcVertexSpec = vertexSpec (Proxy :: Proxy ArcVertexRecord)

instance Storable ArcVertex where
    sizeOf _ = sizeOfArcVertex
    alignment _ = alignmentOfArcVertex
    peek ptr = (ArcVertex . unWrapRecord) `fmap` (peek . castPtr $ ptr)
    poke ptr (ArcVertex v) = poke (castPtr ptr) (WrapRecord v)

instance Vertex ArcVertex where
    vertexSpec _ = arcVertexSpec


type LineVertexFields =
    '[ "position" ':> V2 Float
    , "otherEndPosition" ':> V2 Float
    , "jointEndPosition" ':> V2 Float
    , "lineWidth" ':> Float
    , "miterLimit" ':> Float
    , "positionType" ':> GL.GLint
    , "lineColor" ':> V4 Float
    ]

type LineVertexRecord = Record LineVertexFields

newtype LineVertex = LineVertex LineVertexRecord deriving (Show)

lineVertex :: V2 Float -> V2 Float -> V2 Float -> Float -> Float -> GL.GLint -> V4 Float -> LineVertex
lineVertex position' otherEndPosition' jointEndPosition' lineWidth' miterLimit' positionType' lineColor' = LineVertex
    $ position @= position'
    <: otherEndPosition @= otherEndPosition'
    <: jointEndPosition @= jointEndPosition'
    <: lineWidth @= lineWidth'
    <: miterLimit @= miterLimit'
    <: positionType @= positionType'
    <: lineColor @= lineColor'
    <: Nil

sizeOfLineVertex :: Int
sizeOfLineVertex = sizeOf (undefined :: WrapRecord LineVertexFields)

alignmentOfLineVertex :: Int
alignmentOfLineVertex = alignment (undefined :: WrapRecord LineVertexFields)

lineVertexSpec :: VertexSpec
lineVertexSpec = vertexSpec (Proxy :: Proxy LineVertexRecord)

instance Storable LineVertex where
    sizeOf _ = sizeOfLineVertex
    alignment _ = alignmentOfLineVertex
    peek ptr = (LineVertex . unWrapRecord) `fmap` (peek . castPtr $ ptr)
    poke ptr (LineVertex v) = poke (castPtr ptr) (WrapRecord v)

instance Vertex LineVertex where
    vertexSpec _ = lineVertexSpec


type TexturedVertexFields =
    '[ "position" ':> V2 Float
    , "textureCoord" ':> V2 Float
    ]

type TexturedVertexRecord = Record TexturedVertexFields

newtype TexturedVertex = TexturedVertex TexturedVertexRecord deriving (Show)

texturedVertex :: V2 Float -> V2 Float -> TexturedVertex
texturedVertex position' textureCoord' = TexturedVertex
    $ position @= position'
    <: textureCoord @= textureCoord'
    <: Nil

sizeOfTexturedVertex :: Int
sizeOfTexturedVertex = sizeOf (undefined :: WrapRecord TexturedVertexFields)

alignmentOfTexturedVertex :: Int
alignmentOfTexturedVertex = alignment (undefined :: WrapRecord TexturedVertexFields)

texturedVertexSpec :: VertexSpec
texturedVertexSpec = vertexSpec (Proxy :: Proxy TexturedVertexRecord)

instance Storable TexturedVertex where
    sizeOf _ = sizeOfTexturedVertex
    alignment _ = alignmentOfTexturedVertex
    peek ptr = (TexturedVertex . unWrapRecord) `fmap` (peek . castPtr $ ptr)
    poke ptr (TexturedVertex v) = poke (castPtr ptr) (WrapRecord v)

instance Vertex TexturedVertex where
    vertexSpec _ = texturedVertexSpec
