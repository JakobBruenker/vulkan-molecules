#version 460
// #extension GL_EXT_debug_printf : enable
// TODO: Use vulkan-utils glsl together with quoteFile to compile shaders

layout(location = 0) in vec3 position;
layout(location = 1) in uint atomType;
layout(location = 0) out vec3 vertColor;
layout(location = 1) out float pointSize;

layout(binding = 0) uniform UniformBufferObject {
    float time;
    int windowWidth;
    int windowHeight;
    float worldWidth;
    float worldHeight;
} ubo;

vec3 atomColor(uint atomType) {
    switch (atomType) {
        case 1: return vec3(.9, .9, .9);
        case 6: return vec3(.3, .3, .3);
        default: return vec3(1, 0, 1);
    }
}

float lj_sigma(uint atomType) {
    switch (atomType) {
        case 1: return 0.5523570;
        case 6: return 1.3541700;
        default: return 1;
    }
}

void main() {
    float floatWidth = float(ubo.windowWidth);
    float floatHeight = float(ubo.windowHeight);
    float angstromPerPixel = ubo.worldWidth / ubo.worldHeight > floatWidth / floatHeight ?
        ubo.worldWidth / floatWidth :
        ubo.worldHeight / floatHeight;

    vec2 pos = position.xy - vec2(ubo.worldWidth / 2, ubo.worldHeight / 2);
    mat2 scaleWorld = mat2(2 / ubo.worldWidth, 0, 0, 2 / ubo.worldHeight);
    mat2 scaleWin = ubo.worldWidth / ubo.worldHeight > floatWidth / floatHeight ?
        mat2(1, 0, 0, (1/((ubo.worldWidth / ubo.worldHeight) * (floatHeight / floatWidth)))) :
        mat2(1/((ubo.worldHeight / ubo.worldWidth) * (floatWidth / floatHeight)), 0, 0, 1);
    gl_Position = vec4((scaleWin * scaleWorld) * pos, 0, 1);
    vertColor = atomColor(atomType);
    pointSize = pow(2, 1/6) * lj_sigma(atomType) / angstromPerPixel;
    gl_PointSize = pointSize;
}