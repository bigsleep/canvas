#version 130

in vec2 prevPosition;
in vec2 position;
in vec2 nextPosition;
in vec4 color;
in vec4 lineColor;
in float lineWidth;
out vec4 fragmentColor;
out vec4 fragmentLineColor;
varying vec3 sideAttrib;

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
    gl_Position = vec4(position, 0.0, 1.0);
    fragmentColor = color;
    fragmentLineColor = lineColor;

    if (lineWidth < epsilon) {
        sideAttrib = vec3(0.0, 0.0, 0.0);
        sideAttrib[gl_VertexID % 3] = -big;
    } else {
        float sideDistance = distanceOfPointToLine(position, nextPosition, prevPosition);
        if (sideDistance <= lineWidth) {
            sideAttrib = vec3(0.0, 0.0, 0.0);
            sideAttrib[gl_VertexID % 3] = big;
        } else {
            float a = sideDistance / lineWidth;
            sideAttrib = vec3(0.0, 0.0, 0.0);
            sideAttrib[gl_VertexID % 3] = a;
        }
    }
}
