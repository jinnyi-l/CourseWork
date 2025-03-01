//declare three uniform mat4 variables
//the mvp is ultiplication of three variables




#version 330
#extension GL_ARB_explicit_attrib_location : require

// Uniform variables
uniform float u_time;
uniform mat4 u_view;
uniform mat4 u_projection;
uniform mat4 u_model;

// Vertex inputs
layout(location = 0) in vec4 a_position;
layout(location = 2) in vec3 a_normal; // Using normals to compute color

// Vertex shader output
out vec3 v_color;

void main()
{
    // Compute the Model-View-Projection (MVP) matrix.
    // Order: First apply model transform, then view transform, then projection.
    mat4 mvp = u_projection * u_view * u_model;
    gl_Position = mvp * a_position;

    // Map normals from [-1,1] to [0,1] for visualization.
    v_color = 0.5 * a_normal + 0.5;
}
