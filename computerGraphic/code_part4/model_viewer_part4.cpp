#include "gltf_io.h"
#include "gltf_scene.h"
#include "gltf_render.h"
#include "cg_utils.h"
#include "cg_trackball.h"

#include <GL/gl3w.h>
#include <GLFW/glfw3.h>

#include <imgui.h>
#include <imgui_impl_glfw.h>
#include <imgui_impl_opengl3.h>

#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/quaternion.hpp>

#include <cstdlib>
#include <iostream>


struct Context {
    int width = 512; 
    int height = 512; 
    GLFWwindow *window; 

    gltf::GLTFAsset asset; 
    gltf::DrawableList drawables;

    cg::Trackball trackball;     //add mouse control

    GLuint program; 
    GLuint emptyVAO;  
    float elapsedTime;

    std::string gltfFilename = "armadillo.gltf";
    glm::vec3 clearColor = glm::vec3(0.2f, 0.2f, 0.2f);

    //============================
    //  Blinn-Phong
    //============================

    glm::vec3 lightPosition = glm::vec3(0.0f, 0.0f, 2.0f); //light position
    glm::vec3 lightColor    = glm::vec3(1.0f, 1.0f, 1.0f); //light color


    //default value 

    glm::vec3 ambientColor  = glm::vec3(0.1f, 0.1f, 0.1f); //ambient light color 
    glm::vec3 diffuseColor  = glm::vec3(1.0f, 1.0f, 1.0f); //light color
    glm::vec3 specularColor = glm::vec3(1.0f, 1.0f, 1.0f); //specular color
    float specularPower     = 64.0f;                      // specular power

    
    bool showNormals        = false; 
    bool useOrthographic    = false; 
    float cameraZoom        = 45.0f; 
};

std::string shader_dir(void)
{
    std::string rootDir = cg::get_env_var("MODEL_VIEWER_ROOT");
    if (rootDir.empty()) {
        std::cout << "Error: MODEL_VIEWER_ROOT is not set." << std::endl;
        std::exit(EXIT_FAILURE);
    }
    return rootDir + "/src/shaders/";
}

std::string gltf_dir(void)
{
    std::string rootDir = cg::get_env_var("MODEL_VIEWER_ROOT");
    if (rootDir.empty()) {
        std::cout << "Error: MODEL_VIEWER_ROOT is not set." << std::endl;
        std::exit(EXIT_FAILURE);
    }
    return rootDir + "/assets/gltf/";
}

void do_initialization(Context &ctx)
{
    ctx.program = cg::load_shader_program(shader_dir() + "mesh.vert",
                                          shader_dir() + "mesh.frag");
    gltf::load_gltf_asset(ctx.gltfFilename, gltf_dir(), ctx.asset);
    gltf::create_drawables_from_gltf_asset(ctx.drawables, ctx.asset);
}

//============================
// rendering function
//============================
void draw_scene(Context &ctx)
{
   
    glUseProgram(ctx.program);
    glEnable(GL_DEPTH_TEST);

    float aspect = static_cast<float>(ctx.width) / static_cast<float>(ctx.height);
    glm::mat4 projection;

    
    if (ctx.useOrthographic) {

        //use fixed-size orthographic projection
        float orthoSize = 2.0f;
        projection = glm::ortho(-orthoSize * aspect, orthoSize * aspect,
                                -orthoSize, orthoSize,
                                0.1f, 100.0f);
    } else {
        //perspective projection
        float fov = glm::radians(ctx.cameraZoom);
        projection = glm::perspective(fov, aspect, 0.1f, 100.0f);
    }

    glUniformMatrix4fv(glGetUniformLocation(ctx.program, "u_projection"),
                       1, GL_FALSE, &projection[0][0]);

    //============================
    //set up view matrix
    //============================

    //to deicide the location of cam
    //ctx.useOrthographic == false, glm::vec3(0.0f, 0.0f, 3.0f)
    //ctx.useOrthographic == true, glm::vec3(0.0f, 0.0f, 5.0f)

    glm::mat4 lookat = glm::lookAt(glm::vec3(0.0f, 0.0f, ctx.useOrthographic ? 5.0f : 3.0f), 
                                   glm::vec3(0.0f, 0.0f, 0.0f), //always look at origin
                                   glm::vec3(0.0f, 1.0f, 0.0f));

    glm::mat4 view = lookat * glm::mat4(ctx.trackball.orient);
    
    glUniformMatrix4fv(glGetUniformLocation(ctx.program, "u_view"),
                       1, GL_FALSE, &view[0][0]);

    //pass parameters 
    glUniform3fv(glGetUniformLocation(ctx.program, "u_lightPosition"),
                 1, &ctx.lightPosition[0]);
    glUniform3fv(glGetUniformLocation(ctx.program, "u_lightColor"),
                 1, &ctx.lightColor[0]);

    
    glUniform3fv(glGetUniformLocation(ctx.program, "u_ambientColor"),
                 1, &ctx.ambientColor[0]);
    glUniform3fv(glGetUniformLocation(ctx.program, "u_diffuseColor"),
                 1, &ctx.diffuseColor[0]);
    glUniform3fv(glGetUniformLocation(ctx.program, "u_specularColor"),
                 1, &ctx.specularColor[0]);
    glUniform1f(glGetUniformLocation(ctx.program, "u_specularPower"),
                 ctx.specularPower);
    glUniform1i(glGetUniformLocation(ctx.program, "u_showNormals"),
                ctx.showNormals ? 1 : 0);

    //Draw scene            
    for (unsigned i = 0; i < ctx.asset.nodes.size(); ++i) {
        const gltf::Node &node = ctx.asset.nodes[i];
        const gltf::Drawable &drawable = ctx.drawables[node.mesh];

        //pre-object uniform
        glm::mat4 model = glm::mat4(1.0f); //initial state
        model = glm::translate(model, node.translation); //translation 
        model = model * glm::mat4_cast(node.rotation);  //rotation 
        model = glm::scale(model, node.scale); //scale
        glUniformMatrix4fv(glGetUniformLocation(ctx.program, "u_model"),
                           1, GL_FALSE, &model[0][0]);

        //Draw object
        glBindVertexArray(drawable.vao);
        glDrawElements(GL_TRIANGLES, drawable.indexCount,
                       drawable.indexType,
                       (GLvoid*)(intptr_t)drawable.indexByteOffset);
        glBindVertexArray(0);
    }

    // Clean up
    cg::reset_gl_render_state();
    glUseProgram(0);
}

void reload_shaders(Context *ctx)
{
    glDeleteProgram(ctx->program);
    ctx->program = cg::load_shader_program(shader_dir() + "mesh.vert", shader_dir() + "mesh.frag");
}

void error_callback(int /*error*/, const char *description)
{
    std::cerr << description << std::endl;
}

void key_callback(GLFWwindow *window, int key, int scancode, int action, int mods)
{
    // Forward event to ImGui
    ImGui_ImplGlfw_KeyCallback(window, key, scancode, action, mods);
    if (ImGui::GetIO().WantCaptureKeyboard) return;

    Context *ctx = static_cast<Context *>(glfwGetWindowUserPointer(window));
    if (key == GLFW_KEY_R && action == GLFW_PRESS) { reload_shaders(ctx); }
}

void char_callback(GLFWwindow *window, unsigned int codepoint)
{
    // Forward event to ImGui
    ImGui_ImplGlfw_CharCallback(window, codepoint);
    if (ImGui::GetIO().WantTextInput) return;
}

void mouse_button_callback(GLFWwindow *window, int button, int action, int mods)
{
    // Forward event to ImGui
    ImGui_ImplGlfw_MouseButtonCallback(window, button, action, mods);
    if (ImGui::GetIO().WantCaptureMouse) return;

    double x, y;
    glfwGetCursorPos(window, &x, &y);

    Context *ctx = static_cast<Context *>(glfwGetWindowUserPointer(window));
    if (button == GLFW_MOUSE_BUTTON_LEFT) {
        ctx->trackball.center = glm::vec2(x, y);
        ctx->trackball.tracking = (action == GLFW_PRESS);
    }
}

void cursor_pos_callback(GLFWwindow *window, double x, double y)
{
    // Forward event to ImGui
    if (ImGui::GetIO().WantCaptureMouse) return;

    Context *ctx = static_cast<Context *>(glfwGetWindowUserPointer(window));
    cg::trackball_move(ctx->trackball, float(x), float(y));
}

//updated scroll action
void scroll_callback(GLFWwindow *window, double xoffset, double yoffset)
{
    ImGui_ImplGlfw_ScrollCallback(window, xoffset, yoffset);
    if (ImGui::GetIO().WantCaptureMouse) return;

    Context *ctx = static_cast<Context *>(glfwGetWindowUserPointer(window)); //update cam zoom
    ctx->cameraZoom -= float(yoffset);
    if (ctx->cameraZoom < 1.0f)   ctx->cameraZoom = 1.0f;
    if (ctx->cameraZoom > 120.0f) ctx->cameraZoom = 120.0f;
}

void resize_callback(GLFWwindow *window, int width, int height)
{
    // Update window size and viewport rectangle
    Context *ctx = static_cast<Context *>(glfwGetWindowUserPointer(window));
    ctx->width = width;
    ctx->height = height;
    glViewport(0, 0, width, height);
}

//--------------------------------------
//Mai
//--------------------------------------
int main(int argc, char *argv[])
{
    Context ctx;
    if (argc > 1) {ctx.gltfFilename = std::string(argv[1]);}

    // Create a GLFW window
    glfwSetErrorCallback(error_callback);
    glfwInit();
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
    ctx.window = glfwCreateWindow(ctx.width, ctx.height, "Model viewer", nullptr, nullptr);
    glfwMakeContextCurrent(ctx.window);
    glfwSetWindowUserPointer(ctx.window, &ctx);
    glfwSetKeyCallback(ctx.window, key_callback);
    glfwSetCharCallback(ctx.window, char_callback);
    glfwSetMouseButtonCallback(ctx.window, mouse_button_callback);
    glfwSetCursorPosCallback(ctx.window, cursor_pos_callback);
    glfwSetScrollCallback(ctx.window, scroll_callback);
    glfwSetFramebufferSizeCallback(ctx.window, resize_callback);

    // Load OpenGL functions
    if (gl3wInit() || !gl3wIsSupported(3, 3)) {
        std::cerr << "Error: failed to initialize OpenGL" << std::endl;
        std::exit(EXIT_FAILURE);
    }
    std::cout << "OpenGL version: " << glGetString(GL_VERSION) << std::endl;

    // Initialize ImGui
    ImGui::CreateContext();
    ImGui_ImplGlfw_InitForOpenGL(ctx.window, false /*do not install callbacks*/);
    ImGui_ImplOpenGL3_Init("#version 330" /*GLSL version*/);

    glGenVertexArrays(1, &ctx.emptyVAO);
    glBindVertexArray(ctx.emptyVAO);
    glEnable(GL_TEXTURE_CUBE_MAP_SEAMLESS);

    do_initialization(ctx);

    while (!glfwWindowShouldClose(ctx.window)) {

        glfwPollEvents();
        ctx.elapsedTime = glfwGetTime();
        ImGui_ImplOpenGL3_NewFrame();
        ImGui_ImplGlfw_NewFrame();
        ImGui::NewFrame();

        ImGui::Begin("Settings");
        ImGui::ColorEdit3("Background Color", &ctx.clearColor[0]);
        ImGui::Checkbox("Use Orthographic", &ctx.useOrthographic);
        ImGui::Checkbox("Show Normals", &ctx.showNormals);

        ImGui::Text("Blinn-Phong Material:");
        ImGui::ColorEdit3("Light Color", &ctx.lightColor[0]);
        ImGui::DragFloat3("Light Position", &ctx.lightPosition[0], 0.1f);
        ImGui::ColorEdit3("Ambient Color", &ctx.ambientColor[0]);
        ImGui::ColorEdit3("Diffuse Color", &ctx.diffuseColor[0]);
        ImGui::ColorEdit3("Specular Color", &ctx.specularColor[0]);
        ImGui::SliderFloat("Specular Power", &ctx.specularPower, 1.0f, 128.0f);
        ImGui::End();

        cg::reset_gl_render_state();
        glClearColor(ctx.clearColor.r, ctx.clearColor.g, ctx.clearColor.b, 1.0f);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

        draw_scene(ctx);

        ImGui::Render();
        ImGui_ImplOpenGL3_RenderDrawData(ImGui::GetDrawData());

        glfwSwapBuffers(ctx.window);
    }

    ImGui_ImplOpenGL3_Shutdown();
    ImGui_ImplGlfw_Shutdown();
    ImGui::DestroyContext();
    glfwDestroyWindow(ctx.window);
    glfwTerminate();
    std::exit(EXIT_SUCCESS);
}
