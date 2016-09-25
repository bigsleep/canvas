#version 130

in vec2 position;
in vec2 center;
in float radius;
in vec4 lineColor;
in float lineWidth;
in float startAngle;
in float endAngle;
varying vec2 fragPosition;
out vec2 fragCenter;
out float fragRadius;
out vec4 fragLineColor;
out float fragLineWidth;
out float fragStartAngle;
out float fragEndAngle;

uniform mat4 projectionMatrix;
uniform mat4 modelViewMatrix;

void main()
{
    gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 0.0, 1.0);
    fragPosition = position;
    fragCenter = center;
    fragRadius = radius;
    fragLineColor = lineColor;
    fragLineWidth = lineWidth;
    fragStartAngle = startAngle;
    fragEndAngle = endAngle;
}
