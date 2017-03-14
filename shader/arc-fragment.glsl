#version 130

varying vec2 fragPosition;
in vec2 fragCenter;
in float fragRadius;
in vec4 fragColor;
in vec4 fragLineColor;
in float fragLineWidth;
in float fragStartAngle;
in float fragEndAngle;
out vec4 outColor;

const float pi2 = 6.283185307179586;

void main()
{
    float distance = length(fragPosition - fragCenter);
    float innerRadius = fragRadius - fragLineWidth;
    vec2 v = fragPosition - fragCenter;
    float angle = atan(v.y, v.x);

    if (angle < 0.0) {
        angle += pi2;
    }

    if ((fragStartAngle <= fragEndAngle && fragStartAngle <= angle && angle <= fragEndAngle) ||
        (fragStartAngle > fragEndAngle) && (angle <= fragEndAngle || fragStartAngle <= angle)) {

        if (distance <= innerRadius) {
            outColor = fragColor;
        } else if (innerRadius < distance && distance <= fragRadius) {
            outColor = fragLineColor;
        } else {
            discard;
        }
    } else {
        discard;
    }
}
