/*
added variable:
uniform vec3 u_diffuseColor;
uniform vec3 u_lightPosition;
mv = view * model 
mvp = mv * projection 

change vertice position: gl_Position = mvp * a_position;

for space normal, light direction and diffuse Lambertian term
    vec3 N = normalize(mat3(mv) * a_normal);
    vec3 L = normalize(u_lightPosition - positionEye);
    float diffuse = max(0.0, dot(N, L));
    v_color = diffuse * u_diffuseColor;
*/



#version 330
#extension GL_ARB_explicit_attrib_location : require

// Uniform variables
uniform float u_time;
uniform mat4 u_view;
uniform mat4 u_projection;
uniform mat4 u_model;
uniform vec3 u_diffuseColor; // The diffuse surface color of the model
uniform vec3 u_lightPosition; // The position of the light source

// Vertex inputs
layout(location = 0) in vec4 a_position;
layout(location = 2) in vec3 a_normal;

out vec3 v_color;

void main()
{
    
    mat4 mv = u_view * u_model;
    mat4 mvp = u_projection * mv;
    gl_Position = mvp * a_position;
    vec3 positionEye = vec3(mv * a_position);
    vec3 N = normalize(mat3(mv) * a_normal);
    vec3 L = normalize(u_lightPosition - positionEye);
    float diffuse = max(0.0, dot(N, L));
    v_color = diffuse * u_diffuseColor;
}
