#version 130
uniform sampler2DArray src;
uniform float speed;

in vec2 texCoord;

out vec4 colour;

void main() {
  vec2 p = texCoord;
  vec2 z = texture(src, vec3(p, 0.0)).xy;
  float n = z.x * speed;
  vec3 yuv;
  if (n < 0.0) {
    yuv = vec3(0.0);
  } else {
    yuv = vec3(clamp(log(1.0 + n / 4.0), 0.0, 1.0), 0.125 * sin(n), -0.125 * cos(n));
  }
  vec3 rgb = yuv * mat3(1.0, 0.0, 1.4, 1.0, -0.395, -0.581, 1.0, 2.03, 0.0);
  colour = vec4(rgb, 1.0);
}
