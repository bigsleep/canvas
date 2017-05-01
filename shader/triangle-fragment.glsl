#version 130

in vec2 fragmentColor;
in vec2 fragmentLineColor;
flat in int fragmentLineFlags;
in vec3 bottomLineAttrib;
in vec3 topLineAttrib;

out vec4 outColor;
uniform sampler2D texture;

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
    outColor = texture2D(texture, fragmentColor);

    for (int i = 0; i < 3; ++i) {
        if ((bottomLineEnabled(i, fragmentLineFlags) && bottomLineAttrib[i] <= 1.0) || (topLineEnabled(i, fragmentLineFlags) && topLineAttrib[i] >= 1.0)) {
            outColor = texture2D(texture, fragmentLineColor);
            break;
        }
    }
}
