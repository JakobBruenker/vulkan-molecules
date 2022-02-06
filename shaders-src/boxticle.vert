#version 450

vec3 atomColor(float type) {
  return type == 0 ? vec3(.0, 0.3, 0.6) : type == 1 ? vec3(0.6, .0, .0) : vec3(1., 1., 1.);
  /* switch (type) { */
  /*   case 0.: return vec3( .0, 0.3, 0.6); */
  /*   case 1.: return vec3(0.6,  .0,  .0); */
  /*   default: return vec3 (1., 1., 1.); */
  /* } */
}

float radius(float type) {
  return type == 0 ? .5 : type == 1 ? 1. : 1.;
  /* switch (type) { */
  /*   case 0.: return 0.5; */
  /*   case 1.: return 1.; */
  /*   default: return 1.; */
  /* } */
}

layout(location = 0) in vec3 position;
layout(location = 1) in float type;
layout(location = 0) out vec4 vertColor;
layout(binding = 0) uniform UniformBufferObject {
  float time;
  int windowWidth;
  int windowHeight;
  float worldWidth;
  float worldHeight;
} ubo;

void main() {
  float floatWidth = ubo.windowWidth;
  float floatHeight = ubo.windowHeight;
  float angstromPerPixel = ubo.worldWidth / ubo.worldHeight > floatWidth / floatHeight ?
    ubo.worldWidth / floatWidth :
    ubo.worldHeight / floatHeight;

  vec2 pos = position.xy - vec2(ubo.worldWidth / 2., ubo.worldHeight / 2.);
  mat2 scaleWorld = mat2(2. / ubo.worldWidth, 0., 0., 2. / ubo.worldHeight);
  mat2 scaleWin = ubo.worldWidth / ubo.worldHeight > floatWidth / floatHeight ?
    mat2(1., 0., 0., 1. / ((ubo.worldWidth / ubo.worldHeight) * (floatHeight / floatWidth))) :
    mat2(1. / ((ubo.worldHeight / ubo.worldWidth) * (floatWidth / floatHeight)), 0., 0., 1.);
  gl_Position = vec4((scaleWin * scaleWorld) * pos, 0., 1.);
  vec3 color = atomColor(type);
  vertColor = vec4(color, 0.9);
  gl_PointSize = 2. * radius(type) / angstromPerPixel;
}
