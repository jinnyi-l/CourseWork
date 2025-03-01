#version 330
#extension GL_ARB_explicit_attrib_location : require

uniform mat4 u_view;
uniform mat4 u_projection;
uniform mat4 u_model;

//u_ambientColor, u_diffuseColor, u_specularColor, u_specularPower
uniform vec3 u_lightPosition;  
uniform vec3 u_lightColor;     
uniform vec3 u_ambientColor; 
uniform vec3 u_diffuseColor;   
uniform vec3 u_specularColor; 
uniform float u_specularPower;

// 其他控制
uniform bool u_showNormals;     // 是否直接显示法线
uniform float u_time;           // 时间 (可选，如果需要)

// 输入
layout(location = 0) in vec4 a_position;
layout(location = 2) in vec3 a_normal;

// 输出到片段着色器
out vec3 v_color;

void main()
{
    // 1. 计算 MVP 用于顶点位置变换
    mat4 mv  = u_view * u_model;         // ModelView 矩阵
    mat4 mvp = u_projection * mv;        // ModelViewProjection 矩阵
    gl_Position = mvp * a_position;      // 顶点变换

    // 2. 如果要直接显示法线，则在此输出
    //    否则进行 Blinn-Phong 计算
    vec3 positionEye = vec3(mv * a_position);         // 顶点在视空间的坐标
    vec3 N = normalize(mat3(mv) * a_normal);          // 视空间法线
    if (u_showNormals) {
        // 直接显示法线：将 [-1,1] 区间映射到 [0,1]
        v_color = 0.5 * N + 0.5;
        return;
    }

    // 3. Blinn-Phong 模型
    //    3.1 环境光 (Ambient)
    vec3 ambientTerm = u_ambientColor;

    //    3.2 漫反射 (Lambert)
    vec3 L = normalize(u_lightPosition - positionEye); // 光线方向 (视空间)
    float diff = max(dot(N, L), 0.0);
    vec3 diffuseTerm = diff * u_diffuseColor;

    //    3.3 镜面反射 (Specular, Blinn-Phong)
    //    视线方向 V：眼睛在(0,0,0)，顶点在 positionEye，因此 V = -positionEye 的归一化
    vec3 V = normalize(-positionEye);
    //    半程向量 H = normalize(L + V)
    vec3 H = normalize(L + V);
    //    镜面强度
    float specAngle = max(dot(N, H), 0.0);
    float spec = pow(specAngle, u_specularPower);
    vec3 specularTerm = spec * u_specularColor;

    // 4. 将光源颜色与各项相乘并求和
    //    (可根据需要将光源颜色分开乘以 diffuseTerm 和 specularTerm)
    vec3 finalColor = u_lightColor * (ambientTerm + diffuseTerm + specularTerm);

    // 输出给片段着色器
    v_color = finalColor;
}
