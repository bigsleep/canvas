#version 130

in vec4 fragmentColor;
in vec4 fragmentLineColor;
flat in int fragmentLineFlags;
out vec4 outColor;
varying vec3 bottomLineAttrib;
varying vec3 topLineAttrib;

bool bottomLineEnabled(int i, int flags)
{
    return 0 != (flags & (1 << i));
}

bool topLineEnabled(int i, int flags)
{
    return 0 != (flags & (1 << (i + 3)));
}

void main()
{
    outColor = fragmentColor;

    for (int i = 0; i < 3; ++i) {
        if ((bottomLineEnabled(i, fragmentLineFlags) && bottomLineAttrib[i] <= 1.0) || (topLineEnabled(i, fragmentLineFlags) && topLineAttrib[i] >= 1.0)) {
            outColor = fragmentLineColor;
            break;
        }
    }
}
