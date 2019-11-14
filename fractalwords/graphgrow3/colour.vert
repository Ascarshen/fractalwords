#version 130

out vec2 texCoord;

void main() {
  float t = 0.5 * 500.0 / 2048.0;
  switch (gl_VertexID) {
    default:
    case 0:
      gl_Position = vec4(-1.0, -1.0, 0.0, 1.0);
      texCoord = vec2(0.5 - t, 0.5 + t);
      break;
    case 1:
      gl_Position = vec4( 1.0, -1.0, 0.0, 1.0);
      texCoord = vec2(0.5 + t, 0.5 + t);
      break;
    case 2:
      gl_Position = vec4(-1.0,  1.0, 0.0, 1.0);
      texCoord = vec2(0.5 - t, 0.5 - t);
      break;
    case 3:
      gl_Position = vec4( 1.0,  1.0, 0.0, 1.0);
      texCoord = vec2(0.5 + t, 0.5 - t);
      break;
  }
}
