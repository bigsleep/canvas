#version 130

in vec2 position;
in vec2 center;
in float radius;
in vec4 color;
in vec4 lineColor;
in float lineWidth;
varying vec2 fragPosition;
out vec2 fragCenter;
out float fragRadius;
out vec4 fragColor;
out vec4 fragLineColor;
out float fragLineWidth;

uniform mat4 projectionMatrix;
uniform mat4 modelViewMatrix;

void main()
{
    gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 0.0, 1.0);
    fragPosition = position;
    fragCenter = center;
    fragRadius = radius;
    fragColor = color;
    fragLineColor = lineColor;
    fragLineWidth = lineWidth;
}
