#version 130

in vec2 fragPosition;
in vec2 fragCenter;
in float fragRadius;
in vec2 fragColor;
in vec2 fragLineColor;
in float fragLineWidth;
in float fragStartAngle;
in float fragEndAngle;

out vec4 outColor;

uniform sampler2D texture;

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
            outColor = texture2D(texture, fragColor);
        } else if (innerRadius < distance && distance <= fragRadius) {
            outColor = texture2D(texture, fragLineColor);
        } else {
            discard;
        }
    } else {
        discard;
    }
}
