module Main where

import Linear (V2(..), V4(..))
import Graphics.Canvas.Types
import Graphics.Canvas.Rendering.OpenGL (render, allocateRenderResource, RenderResource)
import Sample

main :: IO ()
main = do
    let width  = 640
        height = 480

        lineColor = V4 0 0 0 255
        lineStyle = LineStyle lineColor 20
        startAngle = pi / 4
        endAngle = 3 * pi / 2

        divCount = 4
        dx = fromIntegral width / fromIntegral divCount
        dy = fromIntegral height / fromIntegral divCount
        radius = dx * 0.2

        ps = [V2 50 150, V2 100 220, V2 150 110, V2 200 240, V2 180 50]
        --ps = [V2 50 50, V2 50 400, V2 150 50, V2 150 400]
        path = PathDrawing lineStyle (StripPath ps)

        canvas = Canvas (V2 0 0) (fromIntegral width) (fromIntegral height) [path]
        init = do
            resource <- allocateRenderResource
            return (resource, canvas)

    withWindow width height "canvas-sample" init onDisplay

    putStrLn "ended!"
