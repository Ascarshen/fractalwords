/* =====================================================================
pool-party -- water simulation
Copyright (C) 2016  Claude Heiland-Allen <claude@mathr.co.uk>
------------------------------------------------------------------------
based on:
rdex -- reaction-diffusion explorer
Copyright (C) 2008  Claude Heiland-Allen <claude@mathr.co.uk>
------------------------------------------------------------------------
Generic Shader
===================================================================== */

#include <stdio.h>
#include <stdlib.h>
#include "shader.h"

//======================================================================
// print a shader object's debug log
void shader_debug(GLuint obj) {
//  return; // FIXME: only dump logs when shader compile/link failed
  int infologLength = 0;
  int maxLength;
  if (glIsShader(obj)) {
    glGetShaderiv(obj, GL_INFO_LOG_LENGTH, &maxLength);
  } else {
    glGetProgramiv(obj, GL_INFO_LOG_LENGTH, &maxLength);
  }
  char *infoLog = malloc(maxLength);
  if (!infoLog) {
    return;
  }
  if (glIsShader(obj)) {
    glGetShaderInfoLog(obj, maxLength, &infologLength, infoLog);
  } else {
    glGetProgramInfoLog(obj, maxLength, &infologLength, infoLog);
  }
  if (infologLength > 0) {
    fprintf(stderr, "%s\n", infoLog);
  }
  free(infoLog);
}

//======================================================================
// generic shader initialization
struct shader *shader_init(
  struct shader *shader, const char *vert, const char *frag
) {
  if (! shader) { return 0; }
  shader->linkStatus     = 0;
  shader->vertexSource   = vert;
  shader->fragmentSource = frag;
  if (shader->vertexSource || shader->fragmentSource) {
    shader->program = glCreateProgram();
    if (shader->vertexSource) {
      shader->vertex =
        glCreateShader(GL_VERTEX_SHADER);
      glShaderSource(shader->vertex,
        1, &shader->vertexSource, 0
      );
      glCompileShader(shader->vertex);
      shader_debug(shader->vertex);
      glAttachShader(shader->program, shader->vertex);
    }
    if (shader->fragmentSource) {
      shader->fragment =
        glCreateShader(GL_FRAGMENT_SHADER);
      glShaderSource(shader->fragment,
        1, &shader->fragmentSource, 0
      );
      glCompileShader(shader->fragment);
      shader_debug(shader->fragment);
      glAttachShader(shader->program, shader->fragment);
    }
    glLinkProgram(shader->program);
    shader_debug(shader->program);
    glGetProgramiv(shader->program,
      GL_LINK_STATUS, &shader->linkStatus
    );
    if (! shader->linkStatus) { return 0; }
  } else { return 0; }
  return shader;
}

// EOF
