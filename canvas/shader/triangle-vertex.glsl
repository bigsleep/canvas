#version 130

in vec2 position;
in vec2 color;
out vec2 fragmentColor;

uniform mat4 projectionMatrix;
uniform mat4 modelViewMatrix;

void main()
{
    gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 0.0, 1.0);
    fragmentColor = color;
}
