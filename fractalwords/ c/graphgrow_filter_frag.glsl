#version 330 core

const float pi = 3.141592653589793;
const float twopi = 2.0 * pi;
const float SR = 48000.0;

uniform bool u_first_pass;
uniform sampler1D u_spectrum;
uniform float u_bins;
uniform float u_length;
uniform float u_speed;
uniform float u_gain;
uniform float u_scaleA;
uniform float u_scaleB;
uniform float u_offsetA;
uniform float u_offsetB;

in float i_bin;

out vec3 o_response;

vec2 cis(float t) {
  return vec2(cos(t), sin(t));
}

vec2 cmul(vec2 a, vec2 b) {
  return vec2(a.x * b.x - a.y * b.y, a.x * b.y + a.y * b.x);
}

vec2 cpow(vec2 a, float b) {
  return pow(length(a), b) * cis(atan(a.y, a.x) * b);
}

void main() {
  float rms = sqrt(texelFetch(u_spectrum, 0, 11).z);
  float freq = SR * i_bin / u_bins;
  float phase = freq * u_length / u_speed;
  if (u_first_pass) {
    vec2 OUTPUT = pow(u_gain, u_length) * cis(twopi * phase);
    o_response = vec3(OUTPUT, dot(OUTPUT, OUTPUT));
    return;
  }
  vec2 u = pow(u_gain, u_scaleA * u_length) * cis(twopi * u_scaleA * phase);
  vec2 v = pow(u_gain, u_scaleB * u_length) * cis(twopi * u_scaleB * phase);
  {
    float bin2 = i_bin * u_scaleA;
    vec2 u2 = cpow(texture(u_spectrum, bin2/u_bins).xy/rms, u_scaleA);
    if (! ((u2.x == 0 && u2.y == 0) || isnan(u2.x) || isnan(u2.y) || isinf(u2.x) || isinf(u2.y))) { u = u2; }
  }
  {
    float bin2 = i_bin * u_scaleB;
    vec2 v2 = cpow(texture(u_spectrum, bin2/u_bins).xy/rms, u_scaleB);
    if (! ((v2.x == 0 && v2.y == 0) || isnan(v2.x) || isnan(v2.y) || isinf(v2.x) || isinf(v2.y))) { v = v2; }
  }
  vec2 INPUT = vec2(1.0, 0.0);
  // edge losses
  vec2 a = u, b = v, c = v, d = v, e = v, f = u, g = u, h = u;
  // node values
  vec2 z = vec2(0.0, 0.0);
  vec2 A = z, B = z, C = z, D = z, E = z, F = z, G = z, H = z;
  // fixed point iteration
  for (int k = 0; k < 512; ++k) {
    // scale by degree
    A /= 1.0;
    B /= 3.0;
    C /= 3.0;
    D /= 3.0;
    E /= 3.0;
    F /= 1.0;
    G /= 1.0;
    H /= 1.0;
    // accumulate neighbours
    vec2 An = cmul(a, B) + INPUT;
    vec2 Bn = cmul(a, A) + cmul(b, C) + cmul(e, E);
    vec2 Cn = cmul(b, B) + cmul(c, D) + cmul(f, F);
    vec2 Dn = cmul(c, C) + cmul(d, E) + cmul(h, H);
    vec2 En = cmul(e, B) + cmul(d, D) + cmul(g, G);
    vec2 Fn = cmul(f, C);
    vec2 Gn = cmul(g, E);
    vec2 Hn = cmul(h, D);
    // step
    A = An;
    B = Bn;
    C = Cn;
    D = Dn;
    E = En;
    F = Fn;
    G = Gn;
    H = Hn;
  }
  // output
  vec2 OUTPUT = H;
  o_response = vec3(OUTPUT, dot(OUTPUT, OUTPUT));
}
