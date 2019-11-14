#include <complex.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include <sndfile.h>
#include <GL/glew.h>
#include <GLFW/glfw3.h>

#include "graphgrow_filter_vert.glsl.c"
#include "graphgrow_filter_frag.glsl.c"
#include "shader.h"

struct filter {
  struct shader shader;
  struct { GLint u_first_pass, u_spectrum, u_bins, u_length, u_speed, u_gain, u_scaleA, u_scaleB; } uniform;
  struct { GLint u_first_pass, u_spectrum; GLfloat u_bins, u_length, u_speed, u_gain, u_scaleA, u_scaleB; } value;
  GLuint vao;
  GLuint tex[2];
  GLuint fbo[2];
  int which;
  complex float spectrum[4096];
};

void filter_init(struct filter *f) {
  shader_init(&f->shader, graphgrow_filter_vert, graphgrow_filter_frag);
  glUseProgram(f->shader.program);
  shader_uniform(f, u_first_pass);
  shader_uniform(f, u_spectrum);
  shader_uniform(f, u_bins);
  shader_uniform(f, u_length);
  shader_uniform(f, u_speed);
  shader_uniform(f, u_gain);
  shader_uniform(f, u_scaleA);
  shader_uniform(f, u_scaleB);
  f->value.u_bins = 4096;
  f->value.u_length = 1;
  f->value.u_speed = 100;
  f->value.u_gain = 0.99;
  shader_updatef(f, u_bins);
  shader_updatef(f, u_length);
  shader_updatef(f, u_speed);
  shader_updatef(f, u_gain);
  glGenVertexArrays(1, &f->vao);
  glBindVertexArray(f->vao);
  glGenTextures(2, f->tex);
  glActiveTexture(GL_TEXTURE0 + 0);
  glBindTexture(GL_TEXTURE_1D, f->tex[0]);
  glTexImage1D(GL_TEXTURE_1D, 0, GL_RGB32F, f->value.u_bins / 2, 0, GL_RGB, GL_FLOAT, 0);
  glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glActiveTexture(GL_TEXTURE0 + 1);
  glBindTexture(GL_TEXTURE_1D, f->tex[1]);
  glTexImage1D(GL_TEXTURE_1D, 0, GL_RGB32F, f->value.u_bins / 2, 0, GL_RGB, GL_FLOAT, 0);
  glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glGenFramebuffers(2, f->fbo);
  GLenum bufs = GL_COLOR_ATTACHMENT0;
  glBindFramebuffer(GL_FRAMEBUFFER, f->fbo[0]);
  glFramebufferTexture1D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_1D, f->tex[0], 0);
  glDrawBuffers(1, &bufs);
  glBindFramebuffer(GL_FRAMEBUFFER, f->fbo[1]);
  glFramebufferTexture1D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_1D, f->tex[1], 0);
  glDrawBuffers(1, &bufs);
  glViewport(0, 0, f->value.u_bins / 2, 1);
}

void filter_kernel(struct filter *f, double t) {
  f->value.u_scaleA = 1.0/4.0 + t * (1.0/2.0 - 1.0/4.0);
  f->value.u_scaleB = sqrt(2) * (1.0/2.0 - f->value.u_scaleA);
  shader_updatef(f, u_scaleA);
  shader_updatef(f, u_scaleB);
  glClearColor(0, 0, 0, 0);
  glBindFramebuffer(GL_FRAMEBUFFER, f->fbo[0]);
  glClear(GL_COLOR_BUFFER_BIT);
  glBindFramebuffer(GL_FRAMEBUFFER, f->fbo[1]);
  glClear(GL_COLOR_BUFFER_BIT);
  int which = 0;
  for (int pass = 0; pass < 20; ++pass) {
    f->value.u_first_pass = pass == 0;
    shader_updatei(f, u_first_pass);
    f->value.u_spectrum = which;
    shader_updatei(f, u_spectrum);
    glActiveTexture(GL_TEXTURE0 + which);
    glGenerateMipmap(GL_TEXTURE_2D);
    glBindFramebuffer(GL_FRAMEBUFFER, f->fbo[1 - which]);
    glDrawArrays(GL_LINES, 0, 2);
    which = 1 - which;
  }
  glReadPixels(0, 0, f->value.u_bins, 1, GL_RG, GL_FLOAT, f->spectrum);
  f->spectrum[2048] = 0;
  for (int k = 1; k < 2048; ++k) {
    f->spectrum[4096 - k] = conjf(f->spectrum[k]);
  }
}

const double pi = 3.141592653589793;

int main(int argc, char **argv) {
  (void) argc;
  (void) argv;

  glfwInit();
  glfwWindowHint(GLFW_CLIENT_API, GLFW_OPENGL_API);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
  glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
  GLFWwindow *window = glfwCreateWindow(512, 16, "graphgrow-filter", 0, 0);
  glfwMakeContextCurrent(window);
  glewExperimental = GL_TRUE;
  glewInit();
  glGetError(); // discard common error from glew

  struct filter filter;
  filter_init(&filter);

  SF_INFO sf_info = { 0, 48000, 2, SF_FORMAT_WAV | SF_FORMAT_FLOAT, 0, 0 };
  SNDFILE *sf_file = sf_open("graphgrow-filter.wav", SFM_WRITE, &sf_info);

  for (int frame = 0; frame < 750; ++frame) {
    filter_kernel(&filter, 0.5*(1 - cos(pi * frame/750.0)));
    sf_writef_float(sf_file, (float *) &filter.spectrum[0], filter.value.u_bins);
  }

  sf_close(sf_file);
  return 0;
}
