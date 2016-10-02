#version 130

in vec2 prevPosition;
in vec2 position;
in vec2 nextPosition;
in vec4 color;
in vec4 lineColor;
in float bottomLineWidth;
in float topLineWidth;
in int lineFlags;
out vec4 fragmentColor;
out vec4 fragmentLineColor;
flat out int fragmentLineFlags;
out vec3 bottomLineAttrib;
out vec3 topLineAttrib;

uniform mat4 projectionMatrix;
uniform mat4 modelViewMatrix;

const float epsilon = 1.0E-6;
const float big = 1.0E+8;

float distanceOfPointToLine(vec2 p, vec2 la, vec2 lb)
{
    vec2 pv = p - la;
    vec2 lv = lb - la;
    float len = length(lv);
    if (len < epsilon) {
        return length(pv);
    }
    return abs(dot(pv, vec2(lv.y, -lv.x)) / len);
}

void main()
{
    gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 0.0, 1.0);
    fragmentColor = color;
    fragmentLineColor = lineColor;
    fragmentLineFlags = lineFlags;

    bottomLineAttrib = vec3(0.0, 0.0, 0.0);
    float sideDistance = distanceOfPointToLine(position, nextPosition, prevPosition);
    if (bottomLineWidth >= epsilon) {
        if (sideDistance <= bottomLineWidth) {
            bottomLineAttrib[gl_VertexID % 3] = big;
        } else {
            bottomLineAttrib[gl_VertexID % 3] = sideDistance / bottomLineWidth;
        }
    }

    topLineAttrib = vec3(0.0, 0.0, 0.0);
    if (topLineWidth >= epsilon) {
        if (sideDistance <= topLineWidth) {
            topLineAttrib[gl_VertexID % 3] = big;
        } else {
            topLineAttrib[gl_VertexID % 3] = sideDistance / (sideDistance - topLineWidth);
        }
    }
}
