#version 130

varying vec2 fragPosition;
in vec2 fragCenter;
in float fragRadius;
in vec4 fragColor;
in vec4 fragLineColor;
in float fragLineWidth;
out vec4 outColor;

void main()
{
    float distance = length(fragPosition - fragCenter);
    float innerRadius = fragRadius - fragLineWidth;

    if (distance <= innerRadius) {
        outColor = fragColor;
    } else if (distance <= fragRadius) {
        outColor = fragLineColor;
    } else {
        discard;
    }
}
