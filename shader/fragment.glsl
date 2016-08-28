#version 130

in vec4 fragmentColor;
in vec4 fragmentLineColor;
out vec4 outColor;
varying vec3 sideAttrib;

void main()
{
    if ((sideAttrib[0] > 0.0 && sideAttrib[0] <= 1.0) || (sideAttrib[1] > 0.0 && sideAttrib[1] <= 1.0) || (sideAttrib[2] > 0.0 && sideAttrib[2] <= 1.0)) {
        outColor = fragmentLineColor;
    } else {
        outColor = fragmentColor;
    }
}
