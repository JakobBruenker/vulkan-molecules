#version 460
// #extension GL_EXT_debug_printf : enable

layout(location = 0) in vec3 vertColor;
layout(location = 1) in float pointSize;
layout(location = 0) out vec4 color;

void main() {
    const vec2 pCoord = gl_PointCoord + (gl_SamplePosition - vec2(0.5, 0.5)) / pointSize;
    const vec2 depthVec = pCoord * 2 - vec2(1, 1);
    const float depth = dot(depthVec, depthVec);
    color = vec4((vertColor * (1 - depth)).rgb, 1);
    gl_FragDepth = depth;
}
