#version 130

in vec2 position;
in vec2 otherEndPosition;
in vec2 jointEndPosition;
in float lineWidth;
in float miterLimit;
in int positionType;
in vec2 lineColor;
out vec2 fragLineColor;

uniform mat4 projectionMatrix;
uniform mat4 modelViewMatrix;

const float epsilon = 1.0E-6;
const float maxMiterLimit = 10.0;
const int positionTypeRight = 1;
const int positionTypeLeft = 2;
const int positionTypeRightEnd = 3;
const int positionTypeLeftEnd = 4;

void main()
{
    fragLineColor = lineColor;
    vec2 d = vec2(0.0, 0.0);
    vec2 va = otherEndPosition - position;
    vec2 vb = jointEndPosition - position;
    float la = length(va);
    float lb = length(vb);
    float miter = min(miterLimit, maxMiterLimit);

    if (la >= epsilon) {
        if (positionType == positionTypeLeftEnd || (lb < epsilon && positionType == positionTypeLeft)) {
            d = 0.5 * lineWidth * normalize(vec2(va.y, -va.x));
        } else if (positionType == positionTypeRightEnd || (lb < epsilon && positionType == positionTypeRight)) {
            d = 0.5 * lineWidth * normalize(vec2(-va.y, va.x));
        } else if (positionType == positionTypeLeft || positionType == positionTypeRight) {
            float det = va.x * vb.y - va.y * vb.x;
            float innerProd = dot(va, vb);
            float lprod = la * lb;

            float alpha = sqrt(0.5 * (lprod - innerProd) / lprod);
            float beta = sqrt(0.5 * (lprod + innerProd) / lprod);
            float gamma = alpha / beta;

            bool overMiterLimit = false;
            if (alpha < epsilon || (1.0 / alpha) > miter) {
                overMiterLimit = true;
            }

            bool isInner = false;
            if ((positionType == positionTypeLeft) && (det < 0.0) || (positionType == positionTypeRight) && (det >= 0.0)) {
                isInner = true;
            }

            vec2 na = vec2(0.0, 0.0);
            vec2 nb = vec2(0.0, 0.0);
            vec2 n = vec2(0.0, 0.0);
            vec2 ua = normalize(va);
            vec2 ub = normalize(vb);
            vec2 m = normalize(ua + ub);
            if (positionType == positionTypeLeft) {
                na = vec2(ua.y, -ua.x);
                nb = vec2(-ub.y, ub.x);
                n = vec2(m.y, -m.x);
            } else {
                na = vec2(-ua.y, ua.x);
                nb = vec2(ub.y, -ub.x);
                n = vec2(-m.y, m.x);
            }

            if (isInner || !overMiterLimit) {
                d = 0.5 * lineWidth / alpha * normalize(na + nb);
            } else {
                if (alpha < epsilon || (0.5 / alpha) > miter) {
                    d = 0.5 * lineWidth / beta * n;
                } else {
                    float f =  lineWidth * (miter - 0.5 / alpha);
                    float g = lineWidth * gamma * (1.0 / alpha - miter);
                    d = -f * m + g * n;
                }
            }
        }
    }
    gl_Position = projectionMatrix * modelViewMatrix * vec4(position + d, 0.0, 1.0);
}
