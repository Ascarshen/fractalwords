#version 130
uniform float er;
uniform float rho;

in vec2 texCoord;

out vec4 colour;

void main() {
  vec2 p = er * atanh(texCoord * 2.0 - vec2(1.0));
  float l = length(p);
  float n = (log(clamp(l, er * rho, er)) - log(er)) / log(rho);
  colour = vec4(n, 1.0, 0.0, 0.0);
}
