{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception (throw)
import Control.Monad.IO.Class (liftIO)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector
import Data.Word
import qualified Foreign.Ptr as Ptr (Ptr, castPtr)
import qualified Foreign.Marshal.Array as Ptr (newArray)
import Linear (V2(..), V3(..), V4(..))
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Canvas.Types
import Graphics.Canvas.Rendering.OpenGL (render, allocateRenderResource, RenderResource, addTextureResource)
import Sample

mkCircleImage :: Int -> Vector Word8
mkCircleImage size = Vector.generate (size * size) genMask
    where
    genMask i =
        let (y, x) = divMod i size
            x' = fromIntegral x + 1
            y' = fromIntegral y + 1
            size' = fromIntegral size
        in if (x' * x' + y' * y') <= (size' * size')
            then 255
            else 0

mkColoredImage :: Int -> Vector (V4 Word8)
mkColoredImage size = Vector.generate (size * size) gen
    where
    gen i =
        let (y, x) = divMod i size
            r = fromIntegral $ 255 * x `div` size
            g = fromIntegral $ 255 * y `div` size
            b = 0
            a = 255
        in V4 r g b a


main :: IO ()
main = do
    let width  = 640
        height = 480
        w = fromIntegral width
        h = fromIntegral height
        tsize = 1024
        image = mkColoredImage tsize

        shapeStyle1 = ShapeStyle Nothing $ TexturedFillStyle (TextureRange "t1" (V2 1 1) (V2 0 0))
        shapeStyle2 = ShapeStyle Nothing $ TexturedFillStyle (TextureRange "t1" (V2 0 0) (V2 1 1))

        drawings =
            [ ShapeDrawing shapeStyle1 $ Triangle (V2 w h) (V2 0 h) (V2 w 0)
            , ShapeDrawing shapeStyle2 $ Triangle (V2 0 0) (V2 w 0) (V2 0 h)
            ]

        canvas = Canvas (V2 0 0) (fromIntegral width) (fromIntegral height) drawings
        init = do
            ptr1 <- liftIO $ Vector.unsafeWith image return
            let pdata1 = GL.PixelData GL.RGBA GL.UnsignedByte ptr1
                size1 = GL.TextureSize2D (fromIntegral tsize) (fromIntegral tsize)
            resource <- addTextureResource "t1" GL.NoProxy 0 GL.RGBA' size1 0 pdata1
                =<< allocateRenderResource
            return (resource, canvas)

    withWindow width height "canvas-sample" init onDisplay

    putStrLn "ended!"
