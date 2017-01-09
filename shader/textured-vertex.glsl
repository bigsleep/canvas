#version 130

in vec2 position;
in vec2 textureCoord;
out vec2 fragmentTextureCoord;

uniform mat4 projectionMatrix;
uniform mat4 modelViewMatrix;

void main()
{
    gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 0.0, 1.0);
    fragmentTextureCoord = textureCoord;
}
