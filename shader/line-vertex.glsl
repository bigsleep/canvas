#version 130

in vec2 prevPosition;
in vec2 position;
in vec2 nextPosition;
in vec4 lineColor;
in float lineWidth;
out vec4 fragLineColor;

uniform mat4 projectionMatrix;
uniform mat4 modelViewMatrix;

const float epsilon = 1.0E-6;

void main()
{
    vec2 va = position - prevPosition;
    vec2 vb = position - nextPosition;
    float la = length(va);
    float lb = length(vb);
    vec2 n = vec2(0.0, 0.0);
    if ((la < epsilon) && (lb < epsilon)) {
        n = vec2(0.0, 0.0);
    } else if ((la < epsilon) && (lb >= epsilon)) {
        vec2 nb = normalize(vec2(vb.y, -vb.x));
        n = 0.5 * lineWidth * nb;
    } else if ((la >= epsilon) && (lb < epsilon)) {
        vec2 na = normalize(vec2(-va.y, va.x));
        n = 0.5 * lineWidth * na;
    } else {
        vec2 na = normalize(vec2(-va.y, va.x));
        vec2 nb = normalize(vec2(vb.y, -vb.x));
        n = normalize(na + nb);
        float f = va.x * n.y - va.y * n.x;
        float g = 0.5 * lineWidth * la / f;
        n = g * n;
    }
    gl_Position = projectionMatrix * modelViewMatrix * vec4(position + n, 0.0, 1.0);
    fragLineColor = lineColor;
}
