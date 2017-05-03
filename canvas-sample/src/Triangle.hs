module Main where

import Linear (V2(..), V4(..))
import Graphics.Canvas.Types
import Graphics.Canvas.Rendering.OpenGL (render, allocateRenderResource, RenderResource)
import Sample

main :: IO ()
main = do
    let width  = 640
        height = 480

        lineColor = V4 0 178 153 255
        lineWidth = 0.8
        fillColor = V4 255 0 0 255

        lineStyle = LineStyle lineColor lineWidth
        fillStyle = PlainColorFillStyle fillColor
        style = (ShapeStyle (Just lineStyle) fillStyle)

        divCount = 1
        dx = fromIntegral width / fromIntegral divCount
        dy = fromIntegral height / fromIntegral divCount

        drawings = do
            i <- [0..(divCount - 1)]
            j <- [0..(divCount - 1)]
            let x0 = fromIntegral i * dx
                y0 = fromIntegral j * dy
                triangle = Triangle (V2 x0 y0) (V2 (x0 + dx) y0) (V2 x0 (y0 + dy))
            return $ ShapeDrawing style triangle

        canvas = Canvas (V2 0 0) (fromIntegral width) (fromIntegral height) drawings
        init = do
            resource <- allocateRenderResource
            return (resource, canvas)

    withWindow width height "canvas-sample" init onDisplay

    putStrLn "ended!"
