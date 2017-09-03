#version 130

in vec2 fragPosition;
in vec2 fragCenter;
in float fragRadius;
in vec2 fragColor;
in float fragStartAngle;
in float fragEndAngle;

out vec4 outColor;

uniform sampler2D texture;

const float pi2 = 6.283185307179586;
const vec4 empty = vec4(0.0, 0.0, 0.0, 0.0);

void main()
{
    float distance = length(fragPosition - fragCenter);
    vec2 v = fragPosition - fragCenter;
    float angle = atan(v.y, v.x);

    angle = (angle < 0.0) ? angle + pi2 : angle;

    bool isAngleInRange = (fragStartAngle <= fragEndAngle && fragStartAngle <= angle && angle <= fragEndAngle) ||
        (fragStartAngle > fragEndAngle) && (angle <= fragEndAngle || fragStartAngle <= angle);
    bool isDistanceInRange = distance <= fragRadius;

    outColor = isAngleInRange ? (isDistanceInRange ? texture2D(texture, fragColor) : empty) : empty;
}
