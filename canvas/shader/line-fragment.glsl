#version 130

in vec2 fragLineColor;

out vec4 outColor;

uniform sampler2D texture;

void main()
{
    outColor = texture2D(texture, fragLineColor);
}
