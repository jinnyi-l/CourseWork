#version 330
#extension GL_ARB_explicit_attrib_location : require

// Uniforms
uniform float u_time;
uniform mat4 u_view;       // Rotation matrix from trackball
uniform mat4 u_projection; // Perspective projection matrix

// Vertex inputs
layout(location = 0) in vec4 a_position;
layout(location = 2) in vec3 a_normal; // Vertex normals

// Vertex shader output: we use v_color to visualize normals
out vec3 v_color;

void main()
{
    // Apply projection and view transformation
    gl_Position = u_projection * u_view * a_position;

    // Map normals from [-1,1] to [0,1] to get an RGB color
    v_color = 0.5 * a_normal + 0.5;
}
