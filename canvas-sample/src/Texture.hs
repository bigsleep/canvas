{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception (throw)
import Control.Monad.IO.Class (liftIO)
import Data.Word
import qualified Foreign.Ptr as Ptr (Ptr, castPtr)
import qualified Foreign.Marshal.Array as Ptr (newArray)
import Linear (V2(..), V3(..), V4(..))
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Canvas.Types
import Graphics.Canvas.Rendering.OpenGL (render, allocateRenderResource, RenderResource, addTextureResource)
import Sample

main :: IO ()
main = do
    let width  = 640
        height = 480
        w = fromIntegral width
        h = fromIntegral height

        shapeStyle1 = ShapeStyle Nothing $ TexturedFillStyle (TextureRange "t1" (V2 0 0) (V2 1 1))
        shapeStyle2 = ShapeStyle Nothing $ TexturedFillStyle (TextureRange "t2" (V2 0 0) (V2 1 1))
        shapeStyle3 = ShapeStyle Nothing $ TexturedFillStyle (TextureRange "t3" (V2 0 0) (V2 1 1))

        drawings =
            [ ShapeDrawing shapeStyle1 $ Triangle (V2 0 0) (V2 w 0) (V2 w h)
            , ShapeDrawing shapeStyle2 $ Triangle (V2 0 0) (V2 w h) (V2 0 h)
            , ShapeDrawing shapeStyle3 $ Triangle (V2 0 h) (V2 w 0) (V2 0 0)
            ]

        canvas = Canvas (V2 0 0) (fromIntegral width) (fromIntegral height) drawings
        init = do
            ptr1 <- liftIO $ Ptr.newArray (replicate 1 (V4 0 255 0 255 :: V4 Word8))
            ptr2 <- liftIO $ Ptr.newArray (replicate 1 (V4 255 0 0 255 :: V4 Word8))
            ptr3 <- liftIO $ Ptr.newArray (replicate 1 (V4 0 0 255 255 :: V4 Word8))
            let pdata1 = GL.PixelData GL.RGBA GL.UnsignedByte ptr1
                size1 = GL.TextureSize2D 1 1
                pdata2 = GL.PixelData GL.RGBA GL.UnsignedByte ptr2
                size2 = GL.TextureSize2D 1 1
                pdata3 = GL.PixelData GL.RGBA GL.UnsignedByte ptr3
                size3 = GL.TextureSize2D 1 1
            resource <- addTextureResource "t1" GL.NoProxy 0 GL.RGBA' size1 0 pdata1
                =<< addTextureResource "t2" GL.NoProxy 0 GL.RGBA' size2 0 pdata2
                =<< addTextureResource "t3" GL.NoProxy 0 GL.RGBA' size3 0 pdata3
                =<< allocateRenderResource
            return (resource, canvas)

    withWindow width height "canvas-sample" init onDisplay

    putStrLn "ended!"
