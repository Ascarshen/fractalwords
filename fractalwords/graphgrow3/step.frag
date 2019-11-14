#version 130
#define MAXCOUNT 16
uniform float er;
uniform int count;
uniform mat3 transform[MAXCOUNT];
uniform float scaleFactor[MAXCOUNT];
uniform float source[MAXCOUNT];
uniform float layer;

uniform sampler2DArray src;

in vec2 texCoord;

out vec4 colour;

void main() {
  vec2 p0 = er * atanh(texCoord * 2.0 - vec2(1.0));
  float escape = -1000.0;
  float factor = 1.0;
  for (int i = 0; i < count && i < MAXCOUNT; ++i) {
    vec3 p = transform[i] * vec3(p0, 1.0);
    vec2 q = p.xy / p.z;
    float l = length(q);
    if (l < er) {
      vec2 z = texture(src, vec3((tanh(clamp(q / er, -vec2(4.0), vec2(4.0))) + vec2(1.0)) / 2.0, source[i])).xy;
      if (escape < z.x) { escape = z.x; factor = 1.0/scaleFactor[i] * z.y; }
    }
  }
  escape += 1.0;
  vec2 z = texture(src, vec3(texCoord, layer)).xy;
  if (escape > z.x) {
    z.x = escape;
    z.y = factor;
  }
  colour = vec4(z, 0.0, 0.0);
}
