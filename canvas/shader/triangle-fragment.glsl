#version 130

in vec2 fragmentColor;
out vec4 outColor;
uniform sampler2D texture;

void main()
{
    outColor = texture2D(texture, fragmentColor);
}
