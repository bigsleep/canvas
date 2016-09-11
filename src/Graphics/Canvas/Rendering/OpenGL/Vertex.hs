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
    ) where

import Control.Monad (void)
import qualified Control.Monad.Trans.State as State
import Control.Monad.Trans.Class (lift)
import Data.Constraint (Dict(..))
import Data.Extensible ((:*)(..), (<:), (@=), Assoc(..), AssocValue, Comp(..), Const'(..), Record(..), Field(..), KeyValue, AssocKey, Forall(..), Membership, hfoldMap, hzipWith, hsequence, library, mkField)
import Data.Extensible.Internal (getMemberId)
import Data.Functor.Identity (Identity(..))
import Data.Monoid ((<>), Monoid(..), Sum(..))
import Data.Proxy (Proxy(..))
import Data.Foldable (foldrM)
import Foreign.Ptr (plusPtr, castPtr)
import Foreign.Storable (Storable(..))
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import qualified Graphics.Rendering.OpenGL as GL
import Linear (V1(..), V2(..), V3(..), V4(..))

data VertexField = VertexField
    { vfAttribLocation :: !GL.AttribLocation
    , vfName :: !String
    , vfDataType :: !GL.DataType
    , vfNumberOfValue :: !Int
    , vfByteSize :: !Int
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

class IsVertexAttrib a where
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

sizeOfField :: forall proxy kv z. (Storable z, AssocValue kv ~ z) => proxy kv -> Int
sizeOfField _ = sizeOf (undefined :: z)

instance (Forall (KeyValue KnownSymbol Storable) xs) => Storable (Record xs) where
    sizeOf _ = getSum . hfoldMap (Sum . getConst') . runIdentity $ r
        where
        f :: forall v kv. (v ~ AssocValue kv, KeyValue KnownSymbol Storable kv) => Membership xs kv -> Identity (Const' Int kv)
        f _ = return . Const' . sizeOf $ (undefined :: v)
        r = hgenerateFor (Proxy :: Proxy (KeyValue KnownSymbol Storable)) $ f
    alignment _ = 0
    peek ptr = flip State.evalStateT 0 $ hgenerateFor (Proxy :: Proxy (KeyValue KnownSymbol Storable)) f
        where
        f :: (Storable (AssocValue kv)) => Membership xs kv -> State.StateT Int IO (Field Identity kv)
        f m = do
              offset <- State.get
              State.put (offset + sizeOfField m)
              a <- lift . peek $ castPtr ptr `plusPtr` offset
              return (Field (Identity (a)))
    poke ptr =
        void . flip State.evalStateT 0 . htraverseFor proxy f
        where
            proxy = Proxy :: Proxy (KeyValue KnownSymbol Storable)
            f :: (KeyValue KnownSymbol Storable kv) => Field Identity kv -> State.StateT Int IO (Field Identity kv)
            f kv @ (Field (Identity x)) = do
                offset <- State.get
                State.put (offset + sizeOf x)
                lift $ poke (castPtr ptr `plusPtr` offset) x
                return kv

class Vertex a where
    vertexSpec :: Proxy a -> VertexSpec

instance (Storable (Record xs), Forall (KeyValue KnownSymbol IsVertexAttrib) xs) => Vertex (Record xs) where
    vertexSpec _ = VertexSpec . hfoldMap (return . getConst' . getComp) . flip State.evalState 0 $ r
        where
        f :: forall k v kv. (k ~ AssocKey kv, v ~ AssocValue kv, KeyValue KnownSymbol IsVertexAttrib kv)
          => Membership xs kv -> State.State Int (Comp (Const' VertexField) (Field Identity) kv)
        f m = do
              offset <- State.get
              let location = GL.AttribLocation . fromIntegral . getMemberId $ m
                  name = symbolVal (Proxy :: Proxy k)
                  dataType = valueDataType (Proxy :: Proxy v)
                  num = numberOfValue (Proxy :: Proxy v)
                  size = byteSize (Proxy :: Proxy v)
                  ihandling = integerHandling' (Proxy :: Proxy v)
              State.put $ offset + size
              return . Comp . Const' $ VertexField location name dataType num size ihandling
        r = hgenerateFor (Proxy :: Proxy (KeyValue KnownSymbol IsVertexAttrib)) f

mkField "prevPosition position nextPosition color lineColor lineWidth center radius startAngle endAngle otherEndPosition jointEndPosition miterLimit positionType"

type TriangleVertexRecord = Record
    '[ "prevPosition" :> V2 Float
    , "position" :> V2 Float
    , "nextPosition" :> V2 Float
    , "color" :> V4 Float
    , "lineColor" :> V4 Float
    , "lineWidth" :> V3 Float
    ]

newtype TriangleVertex = TriangleVertex
    { unTriangleVertex :: TriangleVertexRecord
    } deriving (Show)

triangleVertex :: V2 Float -> V2 Float -> V2 Float -> V4 Float -> V4 Float -> V3 Float -> TriangleVertex
triangleVertex prevPosition' position' nextPosition' color' lineColor' lineWidth' = TriangleVertex
    $ prevPosition @= prevPosition'
    <: position @= position'
    <: nextPosition @= nextPosition'
    <: color @= color'
    <: lineColor @= lineColor'
    <: lineWidth @= lineWidth'
    <: Nil

sizeOfTriangleVertex :: Int
sizeOfTriangleVertex = sizeOf (undefined :: TriangleVertexRecord)

triangleVertexSpec :: VertexSpec
triangleVertexSpec = vertexSpec (Proxy :: Proxy TriangleVertexRecord)

instance Storable TriangleVertex where
    sizeOf _ = sizeOfTriangleVertex
    alignment _ = alignment (undefined :: TriangleVertexRecord)
    peek ptr = TriangleVertex `fmap` (peek . castPtr $ ptr)
    poke ptr (TriangleVertex v) = poke (castPtr ptr) v

instance Vertex TriangleVertex where
    vertexSpec _ = triangleVertexSpec


type CircleVertexRecord = Record
    '[ "position" :> V2 Float
    , "center" :> V2 Float
    , "radius" :> Float
    , "color" :> V4 Float
    , "lineColor" :> V4 Float
    , "lineWidth" :> Float
    ]

newtype CircleVertex = CircleVertex
    { unCircleVertex :: CircleVertexRecord
    } deriving (Show)

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
sizeOfCircleVertex = sizeOf (undefined :: CircleVertexRecord)

circleVertexSpec :: VertexSpec
circleVertexSpec = vertexSpec (Proxy :: Proxy CircleVertexRecord)

instance Storable CircleVertex where
    sizeOf _ = sizeOfCircleVertex
    alignment _ = alignment (undefined :: CircleVertexRecord)
    peek ptr = CircleVertex `fmap` (peek . castPtr $ ptr)
    poke ptr (CircleVertex v) = poke (castPtr ptr) v

instance Vertex CircleVertex where
    vertexSpec _ = circleVertexSpec


type ArcVertexRecord = Record
    '[ "position" :> V2 Float
    , "center" :> V2 Float
    , "radius" :> Float
    , "lineColor" :> V4 Float
    , "lineWidth" :> Float
    , "startAngle" :> Float
    , "endAngle" :> Float
    ]

newtype ArcVertex = ArcVertex
    { unArcVertex :: ArcVertexRecord
    } deriving (Show)

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
sizeOfArcVertex = sizeOf (undefined :: ArcVertexRecord)

arcVertexSpec :: VertexSpec
arcVertexSpec = vertexSpec (Proxy :: Proxy ArcVertexRecord)

instance Storable ArcVertex where
    sizeOf _ = sizeOfArcVertex
    alignment _ = alignment (undefined :: ArcVertexRecord)
    peek ptr = ArcVertex `fmap` (peek . castPtr $ ptr)
    poke ptr (ArcVertex v) = poke (castPtr ptr) v

instance Vertex ArcVertex where
    vertexSpec _ = arcVertexSpec


type LineVertexRecord = Record
    '[ "position" :> V2 Float
    , "otherEndPosition" :> V2 Float
    , "jointEndPosition" :> V2 Float
    , "lineWidth" :> Float
    , "miterLimit" :> Float
    , "positionType" :> GL.GLint
    , "lineColor" :> V4 Float
    ]

newtype LineVertex = LineVertex
    { unLineVertex :: LineVertexRecord
    } deriving (Show)

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
sizeOfLineVertex = sizeOf (undefined :: LineVertexRecord)

lineVertexSpec :: VertexSpec
lineVertexSpec = vertexSpec (Proxy :: Proxy LineVertexRecord)

instance Storable LineVertex where
    sizeOf _ = sizeOfLineVertex
    alignment _ = alignment (undefined :: LineVertexRecord)
    peek ptr = LineVertex `fmap` (peek . castPtr $ ptr)
    poke ptr (LineVertex v) = poke (castPtr ptr) v

instance Vertex LineVertex where
    vertexSpec _ = lineVertexSpec
