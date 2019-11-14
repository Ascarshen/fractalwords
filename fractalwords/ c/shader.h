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

#ifndef SHADER_H
#define SHADER_H 1

#include <GL/glew.h>

//======================================================================
// generic shader data
struct shader {
  GLint linkStatus;
  GLuint program;
  GLuint fragment;
  GLuint vertex;
  const char *fragmentSource;
  const char *vertexSource;
};

//======================================================================
// generic shader uniform location access macro
#define shader_uniform(self,name) \
  (self)->uniform.name = \
     glGetUniformLocation((self)->shader.program, #name)

//======================================================================
// generic shader uniform update access macro (integer)
#define shader_updatei(self,name) \
  glUniform1i((self)->uniform.name, (self)->value.name)

//======================================================================
// generic shader uniform update access macro (integer vector)
#define shader_update2i(self,name) \
  glUniform2iv((self)->uniform.name, 1, (self)->value.name)

//======================================================================
// generic shader uniform update access macro (float)
#define shader_updatef(self,name) \
  glUniform1f((self)->uniform.name, (self)->value.name)

//======================================================================
// generic shader initialization
struct shader *shader_init(
  struct shader *shader, const char *vert, const char *frag
);

#endif

