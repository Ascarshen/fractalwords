#version 330 core

uniform float u_bins;

out float i_bin;

void main() {
  switch (gl_VertexID) {
    default:
    case 0:
      gl_Position = vec4(-1.0, 0.0, 0.0, 1.0);
      i_bin = 0.0;
      break;
    case 1:
      gl_Position = vec4( 1.0, 0.0, 0.0, 1.0);
      i_bin = u_bins / 2.0;
      break;
  }
}
