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

bool onLine(int i, float bottom, float top, int flag)
{
    return (bottomLineEnabled(i, fragmentLineFlags) && (bottom <= 1.0)) || (topLineEnabled(i, fragmentLineFlags) && (top >= 1.0));
}

void main()
{
    bool line = false;
    vec4 color = texture2D(texture, fragmentColor);

    for (int i = 0; (i < 3) && !line; ++i) {
        line = (line || onLine(i, bottomLineAttrib[i], topLineAttrib[i], fragmentLineFlags));
        color = line ? texture2D(texture, fragmentLineColor) : color;
    }

    outColor = color;
}
