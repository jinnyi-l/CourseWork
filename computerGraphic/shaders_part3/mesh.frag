#version 330
#extension GL_ARB_explicit_attrib_location : require

// Fragment shader input
in vec3 v_color;

// Fragment shader output
out vec4 frag_color;

void main()
{
    frag_color = vec4(v_color, 1.0);
}
