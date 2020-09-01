#include <assert.h>
#include <complex.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <GL/glew.h>
#include <GLFW/glfw3.h>

#include <lo/lo.h>

// 1428 bytes < 1500 MTU
struct control
{
  int32_t active;
  int32_t count    [4];
  int32_t source   [4][8];
  float   scale    [4][8];
  float   transform[4][8][3][3];
};

#define FPS 30
#define WIDTH 1920
#define HEIGHT 1080
#define SKIP 1
#define SCALE 1
#define MIPMAP 3
#define QUALITY 10

static struct {
  int running;
  struct control control;
  // graphics
  int which;
  GLuint fbo[11];
  GLuint program[5];
  GLint p0which;
  GLint p0quality;
  GLint p0anim;
  GLint p0source;
  GLint p0count;
  GLint p1which;
  GLint p1quality;
  GLint p1layer;
  GLint p3count;
  GLint p3p;
  GLint p3q;
  GLint p4icount;
  GLint p4hcount;
  GLuint pbo[3];
} S;

void errorcb(int num, const char *msg, const char *path)
{
  fprintf(stderr, "liblo server error %d in path %s: %s\n", num, path, msg);
}

int videocb(const char *path, const char *types, lo_arg **argv, int argc, void *message, void *user_data)
{
  const int bytes = sizeof(S.control);
  int   size =  argv[0]->blob.size;
  void *data = &argv[0]->blob.data;
  if (size == bytes)
    memcpy(&S.control, data, bytes);
  return 0;
  (void) path;
  (void) types;
  (void) argc;
  (void) message;
  (void) user_data;
}

int quitcb(const char *path, const char *types, lo_arg **argv, int argc, void *message, void *user_data)
{
  S.running = 0;
  return 0;
  (void) path;
  (void) types;
  (void) argv;
  (void) argc;
  (void) message;
  (void) user_data;
}

static const char *step_vert =
"#version 330 core\n"
"layout(location = 0) in vec2 pos;\n"
"layout(location = 1) in vec2 tc;\n"
"out vec2 coord;\n"
"void main() {\n"
"  gl_Position = vec4(pos, 0.0, 1.0);\n"
"  coord = vec2(tc.x, 1.0 - tc.y);\n"
"}\n"
;

static const char *step_frag =
"#version 330 core\n"
"#define MAXCOUNT 8\n"
"uniform sampler2DArray which;\n"
"uniform int quality;\n"
"uniform int layer;\n"
"uniform int count;\n"
"uniform mat3 transform[MAXCOUNT];\n"
"uniform int source[MAXCOUNT];\n"
"in vec2 coord;\n"
"layout(location = 0) out vec3 colour;\n"
"const vec3[4] shade = vec3[4]\n"
"  ( vec3(1.00, 0.95, 0.90)\n"
"  , vec3(0.95, 1.00, 0.90)\n"
"  , vec3(0.90, 0.95, 1.00)\n"
"  , vec3(0.95, 0.90, 1.00)\n"
"  );\n"
"vec2 fromSquare(vec2 z) {\n"
"  return clamp(atanh(2.0 * z - vec2(1.0)), -4.0, 4.0);\n"
"}"
"vec2 toSquare(vec2 z) {\n"
"  return (tanh(clamp(z, -4.0, 4.0)) + vec2(1.0)) / 2.0;\n"
"}\n"
"float gain(vec2 z, vec2 z0) {\n"
"  vec4 d0 = vec4(dFdx(z), dFdy(z));\n"
"  vec4 d1 = vec4(dFdx(z0), dFdy(z0));\n"
"  return dot(d1,d1) / dot(d0, d0);\n"
"}\n"
"vec3 lookup(vec2 z, vec2 z0, float l) {\n"
"  vec2 w = toSquare(z);\n"
"  return gain(z, z0) * texture(which, vec3(w, l)).rgb;\n"
"}\n"
"void main() {\n"
"  vec3[4] levels = vec3[4]\n"
"    ( texelFetch(which, ivec3(0,0,0), quality).rgb\n"
"    , texelFetch(which, ivec3(0,0,1), quality).rgb\n"
"    , texelFetch(which, ivec3(0,0,2), quality).rgb\n"
"    , texelFetch(which, ivec3(0,0,3), quality).rgb\n"
"    );\n"
"  float[4] level = float[4]\n"
"    ( max(max(levels[0].r, levels[0].g), levels[0].b)\n"
"    , max(max(levels[1].r, levels[1].g), levels[1].b)\n"
"    , max(max(levels[2].r, levels[2].g), levels[2].b)\n"
"    , max(max(levels[3].r, levels[3].g), levels[3].b)\n"
"    );\n"
"  vec2 z = fromSquare(coord);\n"
"  vec3 total = vec3(0.001);\n"
"  for (int i = 0; i < count && i < MAXCOUNT; ++i) {\n"
"    vec3 w = transform[i] * vec3(z, 1.0);\n"
"    total += shade[source[i]] * lookup(w.xy / w.z, coord, float(source[i])) / level[source[i]];\n"
"  }\n"
"  colour = clamp(total, 0.0, 16777216.0);\n"
"}\n"
;

static const char *show_vert =
"#version 330 core\n"
"uniform int quality;\n"
"uniform float aspect;\n"
"layout(location = 0) in vec2 pos;\n"
"layout(location = 1) in vec2 tc;\n"
"out vec2 coord;\n"
"void main() {\n"
"  gl_Position = vec4(pos, 0.0, 1.0);\n"
"  coord = 2.0 * vec2((tc.x - 0.5), (0.5 - tc.y) / aspect);\n"
"}\n"
;

static const char *show_frag =
"#version 330 core\n"
"uniform sampler2DArray which;\n"
"uniform float level;\n"
"uniform int layer;\n"
"in vec2 coord;\n"
"layout(location = 0) out vec4 colour;\n"
"vec2 toSquare(vec2 z) {\n"
"  return (tanh(clamp(z, -4.0, 4.0)) + vec2(1.0)) / 2.0;\n"
"}\n"
"void main() {\n"
"  vec3 c = texture(which, vec3(toSquare(coord), float(layer))).rgb;\n"
"  colour = vec4(c, dot(vec3(1.0 / 3.0), c));\n"
"}\n"
;

static const char *tone_vert =
"#version 330 core\n"
"layout(location = 0) in vec2 pos;\n"
"layout(location = 1) in vec2 tc;\n"
"out vec2 coord;\n"
"void main() {\n"
"  gl_Position = vec4(pos, 0.0, 1.0);\n"
"  coord = vec2(tc.x, tc.y);\n"
"}\n"
;

static const char *tone_frag =
"#version 330 core\n"
"uniform sampler2D source;\n"
"in vec2 coord;\n"
"layout(location = 0) out vec4 colour;\n"
"void main() {\n"
"  vec4 v = texture(source, coord);\n"
"  colour = vec4(pow(v.a, 4.0) * v.rgb / max(max(v.r, v.g), v.b), 1.0);\n"
"}\n"
;

static const char *sort_comp =
"#version 440 core\n"
"layout(local_size_x = 1024) in;\n"
"layout(std430, binding = 0) buffer src {\n"
"  restrict readonly float srcdata[];\n"
"};\n"
"layout(std430, binding = 1) buffer dst {\n"
"  restrict writeonly float dstdata[];\n"
"};\n"
"uniform uint count;\n"
"uniform uint p;\n"
"uniform uint q;\n"
"void main() {\n"
"  uint i = gl_GlobalInvocationID.x;\n"
"  uint d = 1u << (p - q);\n"
"  if ((i & d) == 0 && i < count) {\n"
"    float a = srcdata[i];\n"
"    if ((i | d) < count) {\n"
"      bool up = ((i >> p) & 2) == 0;\n"
"      float b = srcdata[i | d];\n"
"      if ((a > b) == up) {\n"
"        float t = a; a = b; b = t;\n"
"      }\n"
"      dstdata[i | d] = b;\n"
"    }\n"
"    dstdata[i] = a;\n"
"  }\n"
"}\n"
;

static const char *look_comp =
"#version 440 core\n"
"layout(local_size_x = 1024) in;\n"
"layout(std430, binding = 0) buffer img {\n"
"  restrict readonly float imgdata[];\n"
"};\n"
"layout(std430, binding = 1) buffer hst {\n"
"  restrict readonly float hstdata[];\n"
"};\n"
"layout(std430, binding = 2) buffer dst {\n"
"  restrict writeonly float dstdata[];\n"
"};\n"
"uniform uint icount;\n"
"uniform uint hcount;\n"
"void main() {\n"
"  uint i = gl_GlobalInvocationID.x;\n"
"  if (i < icount) {\n"
"    float x = imgdata[4 * i + 3];\n"
"    uint l = 0;\n"
"    uint r = hcount;\n"
"    uint m = (l + r) / 2;\n"
"    for (uint p = 0; p < 24; ++p) {\n"
"      if (r < l + 64) {\n"
"        break;\n"
"      }\n"
"      float y = hstdata[m];\n"
"      if (x < y) {\n"
"        r = m;\n"
"        m = (l + r) / 2;\n"
"        continue;\n"
"      } else if (x > y) {\n"
"        l = m;\n"
"        m = (l + r) / 2;\n"
"        continue;\n"
"      } else {\n"
"       break;\n"
"      }\n"
"    }\n"
"    dstdata[4 * i + 0] = imgdata[4 * i + 0];\n"
"    dstdata[4 * i + 1] = imgdata[4 * i + 1];\n"
"    dstdata[4 * i + 2] = imgdata[4 * i + 2];\n"
"    dstdata[4 * i + 3] = float(m) / float(hcount);\n"
"  }\n"
"}\n"
;

static void debug_program(GLuint program, const char *name) {
  if (program) {
    GLint linked = GL_FALSE;
    glGetProgramiv(program, GL_LINK_STATUS, &linked);
    if (linked != GL_TRUE) {
      fprintf(stderr, "%s: OpenGL shader program link failed\n", name);
    }
    GLint length = 0;
    glGetProgramiv(program, GL_INFO_LOG_LENGTH, &length);
    char *buffer = (char *) malloc(length + 1);
    glGetProgramInfoLog(program, length, 0, buffer);
    buffer[length] = 0;
    if (buffer[0]) {
      fprintf(stderr, "%s: OpenGL shader program info log\n", name);
      fprintf(stderr, "%s\n", buffer);
    }
    free(buffer);
    assert(linked == GL_TRUE);
  } else {
    fprintf(stderr, "%s: OpenGL shader program creation failed\n", name);
  }
}

static void debug_shader(GLuint shader, GLenum type, const char *name) {
  const char *tname = 0;
  switch (type) {
    case GL_VERTEX_SHADER:   tname = "vertex";   break;
    case GL_FRAGMENT_SHADER: tname = "fragment"; break;
    case GL_COMPUTE_SHADER:  tname = "compute";  break;
    default:                 tname = "unknown";  break;
  }
  if (shader) {
    GLint compiled = GL_FALSE;
    glGetShaderiv(shader, GL_COMPILE_STATUS, &compiled);
    if (compiled != GL_TRUE) {
      fprintf(stderr, "%s: OpenGL %s shader compile failed\n", name, tname);
    }
    GLint length = 0;
    glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &length);
    char *buffer = (char *) malloc(length + 1);
    glGetShaderInfoLog(shader, length, 0, buffer);
    buffer[length] = 0;
    if (buffer[0]) {
      fprintf(stderr, "%s: OpenGL %s shader info log\n", name, tname);
      fprintf(stderr, "%s\n", buffer);
    }
    free(buffer);
    assert(compiled == GL_TRUE);
  } else {
    fprintf(stderr, "%s: OpenGL %s shader creation failed\n", name, tname);
  }
}

static void compile_shader(GLint program, GLenum type, const char *name, const GLchar *source) {
  GLuint shader = glCreateShader(type);
  glShaderSource(shader, 1, &source, 0);
  glCompileShader(shader);
  debug_shader(shader, type, name);
  glAttachShader(program, shader);
  glDeleteShader(shader);
}

static GLint compile_program(const char *name, const GLchar *vert, const GLchar *frag) {
  GLint program = glCreateProgram();
  if (vert) { compile_shader(program, GL_VERTEX_SHADER  , name, vert); }
  if (frag) { compile_shader(program, GL_FRAGMENT_SHADER, name, frag); }
  glLinkProgram(program);
  debug_program(program, name);
  return program;
}

static GLint compile_compute(const char *name, const GLchar *comp) {
  GLint program = glCreateProgram();
  if (comp) { compile_shader(program, GL_COMPUTE_SHADER, name, comp); }
  glLinkProgram(program);
  debug_program(program, name);
  return program;
}

static void key_press_handler(GLFWwindow *window, int key, int scancode, int action, int mods) {
  (void) key;
  (void) scancode;
  (void) action;
  (void) mods;
  glfwSetWindowShouldClose(window, GL_TRUE);
}

static GLFWwindow *create_window(int major, int minor, int width, int height, const char *title) {
  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, major);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, minor);
  glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
  glfwWindowHint(GLFW_OPENGL_DEBUG_CONTEXT, GL_TRUE);
  glfwWindowHint(GLFW_RESIZABLE, GL_FALSE);
  glfwWindowHint(GLFW_DECORATED, GL_FALSE);
  GLFWwindow *window = glfwCreateWindow(width, height, title, 0, 0);
  if (! window) {
    fprintf(stderr, "couldn't create window with OpenGL core %d.%d context\n", major, minor);
  }
  assert(window);
  return window;
}

static int debug_error_count = 0;
static void debug_callback(GLenum source, GLenum type, GLuint id, GLenum severity, GLsizei length, const GLchar *message, const GLvoid *user) {
  (void) user;
  (void) length;
  const char *source_str = "unknown";
  switch (source) {
    case GL_DEBUG_SOURCE_API:             source_str = "OpenGL";          break;
    case GL_DEBUG_SOURCE_WINDOW_SYSTEM:   source_str = "window system";   break;
    case GL_DEBUG_SOURCE_SHADER_COMPILER: source_str = "shader compiler"; break;
    case GL_DEBUG_SOURCE_THIRD_PARTY:     source_str = "third party";     break;
    case GL_DEBUG_SOURCE_APPLICATION:     source_str = "application";     break;
    case GL_DEBUG_SOURCE_OTHER:           source_str = "application";     break;
  }
  const char *type_str = "unknown";
  switch (type) {
    case GL_DEBUG_TYPE_ERROR:       type_str = "error";       break;
    case GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR: type_str = "deprecated behavior"; break;
    case GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR:  type_str = "undefined behavior";  break;
    case GL_DEBUG_TYPE_PORTABILITY: type_str = "portability"; break;
    case GL_DEBUG_TYPE_PERFORMANCE: type_str = "performance"; break;
    case GL_DEBUG_TYPE_MARKER:      type_str = "marker";      break;
    case GL_DEBUG_TYPE_PUSH_GROUP:  type_str = "push group";  break;
    case GL_DEBUG_TYPE_POP_GROUP:   type_str = "pop group";   break;
    case GL_DEBUG_TYPE_OTHER:       type_str = "other";       break;
  }
  const char *severity_str = "unknown";
  switch (severity) {
    case GL_DEBUG_SEVERITY_HIGH:         severity_str = "high";         break;
    case GL_DEBUG_SEVERITY_MEDIUM:       severity_str = "medium";       break;
    case GL_DEBUG_SEVERITY_LOW:          severity_str = "low";          break;
    case GL_DEBUG_SEVERITY_NOTIFICATION: severity_str = "notification"; break;
  }
  bool should_print = true;
  if (should_print) {
    fprintf(stderr, "graphgrow-video: %s %s %u %s: %s\n", source_str, type_str, id, severity_str, message);
  }
  if (severity == GL_DEBUG_SEVERITY_HIGH && type == GL_DEBUG_TYPE_ERROR) {
    if (++debug_error_count > 10) {
      abort();
    }
  }
}

static GLFWwindow *create_context(int width, int height, const char *title) {
  int glfw_initialized = glfwInit();
  if (! glfw_initialized) {
    fprintf(stderr, "couldn't initialize glfw\n");
    assert(glfw_initialized);
    return 0;
  }
  int major, minor;
  GLFWwindow *window = create_window(major = 4, minor = 4, width, height, title);
  if (! window) {
    fprintf(stderr, "couldn't create window\n");
    assert(window);
    return 0;
  }
  glfwMakeContextCurrent(window);
  glewExperimental = GL_TRUE;
  glewInit();
  glGetError();
  glDebugMessageCallback(debug_callback, 0);
  return window;
}

static void initialize_gl(int tex_width, int tex_height, int win_width, int win_height) {

  // initialize vertex data for full-screen triangle strip
  GLuint vao;
  glGenVertexArrays(1, &vao);
  glBindVertexArray(vao);
  GLuint vbo;
  glGenBuffers(1, &vbo);
  glBindBuffer(GL_ARRAY_BUFFER, vbo);
  GLfloat vbo_data[] =
    { -1, -1, 0, 1
    , -1,  1, 0, 0
    ,  1, -1, 1, 1
    ,  1,  1, 1, 0
    };
  glBufferData(GL_ARRAY_BUFFER, 16 * sizeof(GLfloat), vbo_data, GL_STATIC_DRAW);
  glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 4 * sizeof(GLfloat), 0);
  glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 4 * sizeof(GLfloat), ((char *)0) + 2 * sizeof(GLfloat));
  glEnableVertexAttribArray(0);
  glEnableVertexAttribArray(1);

  // create textures for ping pong square
  GLuint t_buffer[2];
  glGenTextures(2, t_buffer);
  for (int i = 0; i < 2; ++i) {
    glActiveTexture(GL_TEXTURE0 + i);
    glBindTexture(GL_TEXTURE_2D_ARRAY, t_buffer[i]);
    glTexImage3D(GL_TEXTURE_2D_ARRAY, 0, GL_RGB32F, tex_width, tex_height, 4, 0, GL_RGB, GL_FLOAT, 0);
    glGenerateMipmap(GL_TEXTURE_2D_ARRAY);
    glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_WRAP_S, GL_CLAMP);
    glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_WRAP_T, GL_CLAMP);
  }

  // create texture for rectangular view
  GLuint t_screen;
  glGenTextures(1, &t_screen);
  glActiveTexture(GL_TEXTURE0 + 2);
  glBindTexture(GL_TEXTURE_2D, t_screen);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA32F, win_width, win_height, 0, GL_RGBA, GL_FLOAT, 0);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_MIRRORED_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_MIRRORED_REPEAT);

  // create framebuffers for ping pong square
  glViewport(0, 0, tex_width, tex_height);
  glClearColor(1, 1, 1, 1);
  glGenFramebuffers(9, S.fbo);
  for (int i = 0; i < 8; ++i) {
    glBindFramebuffer(GL_FRAMEBUFFER, S.fbo[i]);
    glFramebufferTextureLayer(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, t_buffer[1  - i / 4], 0, i % 4);
    GLenum dbuf = GL_COLOR_ATTACHMENT0;
    glDrawBuffers(1, &dbuf);
    GLenum status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
    if (status != GL_FRAMEBUFFER_COMPLETE) {
      fprintf(stderr, "OpenGL framebuffer not complete\n");
    }
    assert(status == GL_FRAMEBUFFER_COMPLETE);
    glClear(GL_COLOR_BUFFER_BIT);
  }
  glActiveTexture(GL_TEXTURE0 + 0);
  glGenerateMipmap(GL_TEXTURE_2D_ARRAY);
  glActiveTexture(GL_TEXTURE0 + 1);
  glGenerateMipmap(GL_TEXTURE_2D_ARRAY);

  // create framebuffer for rectangular view
  glBindFramebuffer(GL_FRAMEBUFFER, S.fbo[8]);
  glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, t_screen, 0);
  GLenum dbuf = GL_COLOR_ATTACHMENT0;
  glDrawBuffers(1, &dbuf);
  GLenum status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
  if (status != GL_FRAMEBUFFER_COMPLETE) {
    fprintf(stderr, "OpenGL framebuffer not complete\n");
  }
  assert(status == GL_FRAMEBUFFER_COMPLETE);
  glClear(GL_COLOR_BUFFER_BIT);

  // compile shaders
  S.program[0] = compile_program("step", step_vert, step_frag);
  S.p0which = glGetUniformLocation(S.program[0], "which");
  S.p0quality = glGetUniformLocation(S.program[0], "quality");
  S.p0anim  = glGetUniformLocation(S.program[0], "transform");
  S.p0source  = glGetUniformLocation(S.program[0], "source");
  S.p0count = glGetUniformLocation(S.program[0], "count");
  S.program[1] = compile_program("show", show_vert, show_frag);
  glUseProgram(S.program[1]);
  S.p1which = glGetUniformLocation(S.program[1], "which");
  S.p1quality = glGetUniformLocation(S.program[1], "quality");
  S.p1layer = glGetUniformLocation(S.program[1], "layer");
  GLuint aspect = glGetUniformLocation(S.program[1], "aspect");
  glUniform1f(aspect, WIDTH / (float) HEIGHT);
  S.which = 0;
  S.program[2] = compile_program("tone", tone_vert, tone_frag);
  glUseProgram(S.program[2]);
  GLint p2source = glGetUniformLocation(S.program[2], "source");
  glUniform1i(p2source, 2);

  // pixel buffers for ping pong histogram equalisation
  int logsize = ceil(log2(win_width * win_height));
  glGenBuffers(3, &S.pbo[0]);
  for (int i = 0; i < 3; ++i) {
    glBindBuffer(GL_PIXEL_PACK_BUFFER, S.pbo[i]);
    glBufferData(GL_PIXEL_PACK_BUFFER, 4 * (1 << logsize) * sizeof(float), 0, GL_DYNAMIC_COPY);
  }

  // compute shaders
  S.program[3] = compile_compute("sort", sort_comp);
  S.p3count = glGetUniformLocation(S.program[3], "count");
  S.p3p = glGetUniformLocation(S.program[3], "p");
  S.p3q = glGetUniformLocation(S.program[3], "q");
  S.program[4] = compile_compute("look", look_comp);
  S.p4icount = glGetUniformLocation(S.program[4], "icount");
  S.p4hcount = glGetUniformLocation(S.program[4], "hcount");

}

extern int main(int argc, char **argv) {
  memset(&S, 0, sizeof(S));
  int quality = QUALITY;
  int tex_width = 1 << quality, tex_height = 1 << quality;
  int win_width = WIDTH/SCALE, win_height = HEIGHT/SCALE;
  int hist_width = win_width >> MIPMAP;
  int hist_height = win_height >> MIPMAP;
  int logsize = ceil(log2(hist_width * hist_height));
  GLFWwindow *window = create_context(win_width, win_height, "graphgrow");
  if (! window) {
    assert(window);
    return 1;
  }
  glfwSetKeyCallback(window, key_press_handler);
  initialize_gl(tex_width, tex_height, win_width, win_height);

  GLuint src = S.pbo[0];
  GLuint dst = S.pbo[1];
  GLuint img = S.pbo[2];

  GLuint timers[7];
  glGenQueries(7, timers);

  lo_server_thread lo = lo_server_thread_new("6061", errorcb);
  lo_server_thread_add_method(lo, "/video", "b", videocb, 0);
  lo_server_thread_add_method(lo, "/quit", "", quitcb, 0);
  lo_server_thread_start(lo);

  int frame = 0;
  S.running = 1;
  while (S.running && ! glfwWindowShouldClose(window)) {

    if ((frame % 60) == 0 && frame > 0)
    {
      GLuint64 tifs = 0, tflat = 0, tpbo = 0, tsort = 0, tlup = 0, ttex = 0, tdisp = 0;
      glGetQueryObjectui64v(timers[0], GL_QUERY_RESULT, &tifs);
      glGetQueryObjectui64v(timers[1], GL_QUERY_RESULT, &tflat);
      glGetQueryObjectui64v(timers[2], GL_QUERY_RESULT, &tpbo);
      glGetQueryObjectui64v(timers[3], GL_QUERY_RESULT, &tsort);
      glGetQueryObjectui64v(timers[4], GL_QUERY_RESULT, &tlup);
      glGetQueryObjectui64v(timers[5], GL_QUERY_RESULT, &ttex);
      glGetQueryObjectui64v(timers[6], GL_QUERY_RESULT, &tdisp);
      double ifs = tifs / 1000.0;
      double flat = tflat / 1000.0;
      double pbo = tpbo / 1000.0;
      double sort = tsort / 1000.0;
      double lup = tlup / 1000.0;
      double tex = ttex / 1000.0;
      double disp = tdisp / 1000.0;
      fprintf(stderr, " IFS( %f ) FLAT( %f ) PBO( %f ) SORT( %f ) LUP( %f ) TEX( %f ) DISP( %f )\r", ifs, flat, pbo, sort, lup, tex, disp);
      glDeleteQueries(7, timers);
      glGenQueries(7, timers);
    }

    // iterated function system
    if ((frame % 60) == 0) glBeginQuery(GL_TIME_ELAPSED, timers[0]);
    glUseProgram(S.program[0]);
    glViewport(0, 0, tex_width, tex_height);
    for (int layer = 0; layer < 4; ++layer)
    {
      glUniformMatrix3fv(S.p0anim, 8, GL_TRUE, &S.control.transform[layer][0][0][0]);
      glUniform1iv(S.p0source, 8, &S.control.source[layer][0]);
      glUniform1i(S.p0count, S.control.count[layer]);
      glUniform1i(S.p0which, S.which);
      glUniform1i(S.p0quality, quality);
      glBindFramebuffer(GL_FRAMEBUFFER, S.fbo[S.which * 4 + layer]);
      glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
    }
    S.which = 1 - S.which;
    glActiveTexture(GL_TEXTURE0 + S.which);
    glGenerateMipmap(GL_TEXTURE_2D_ARRAY);
    if ((frame % 60) == 0) glEndQuery(GL_TIME_ELAPSED);
  
    // flatten from square texture to rectangular view
    if ((frame % 60) == 0) glBeginQuery(GL_TIME_ELAPSED, timers[1]);
    glViewport(0, 0, win_width, win_height);
    glBindFramebuffer(GL_FRAMEBUFFER, S.fbo[8]);
    glUseProgram(S.program[1]);
    glUniform1i(S.p1which, S.which);
    glUniform1i(S.p1quality, quality);
    glUniform1i(S.p1layer, S.control.active);
    glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
    if ((frame % 60) == 0) glEndQuery(GL_TIME_ELAPSED);

    if ((frame % 60) == 0) glBeginQuery(GL_TIME_ELAPSED, timers[2]);
    glActiveTexture(GL_TEXTURE0 + 2);
    if (frame % SKIP == 0)
    {
      // copy to PBO
      glGenerateMipmap(GL_TEXTURE_2D);
      glBindBuffer(GL_PIXEL_PACK_BUFFER, src);
      float infty = 1.0 / 0.0;
      glClearBufferSubData(GL_PIXEL_PACK_BUFFER, GL_R32F, 0, (1 << logsize) * sizeof(float), GL_RED, GL_FLOAT, &infty);
      glBindBuffer(GL_PIXEL_PACK_BUFFER, src);
      glGetTexImage(GL_TEXTURE_2D, MIPMAP, GL_ALPHA, GL_FLOAT, 0);
    }
    glBindBuffer(GL_PIXEL_PACK_BUFFER, img);
    glGetTexImage(GL_TEXTURE_2D, 0, GL_RGBA, GL_FLOAT, 0);
    glBindBuffer(GL_PIXEL_PACK_BUFFER, 0);
    if ((frame % 60) == 0) glEndQuery(GL_TIME_ELAPSED);

    if ((frame % 60) == 0) glBeginQuery(GL_TIME_ELAPSED, timers[3]);
    if (frame % SKIP == 0)
    {
      // ping pong merge sort
      glUseProgram(S.program[3]);
      glUniform1ui(S.p3count, 1 << logsize);
      for (int i = 0; i < logsize; ++i) {
        for (int j = 0; j <= i; ++j) {
          glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 0, src);
          glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 1, dst);
          glUniform1ui(S.p3p, i);
          glUniform1ui(S.p3q, j);
          glDispatchCompute((1 << logsize) / 1024, 1, 1);
          { GLuint t = src; src = dst; dst = t; }
        }
      }
    }
    if ((frame % 60) == 0) glEndQuery(GL_TIME_ELAPSED);

    // binary search lookup
    if ((frame % 60) == 0) glBeginQuery(GL_TIME_ELAPSED, timers[4]);
    glUseProgram(S.program[4]);
    glUniform1ui(S.p4icount, win_width * win_height);
    glUniform1ui(S.p4hcount, hist_width * hist_height);
    glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 0, img);
    glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 1, src);
    glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 2, dst);
    glDispatchCompute((win_width * win_height + 1024 - 1) / 1024, 1, 1);
    if ((frame % 60) == 0) glEndQuery(GL_TIME_ELAPSED);

    // copy to texture
    if ((frame % 60) == 0) glBeginQuery(GL_TIME_ELAPSED, timers[5]);
    glActiveTexture(GL_TEXTURE0 + 2);
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, dst);
    glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, win_width, win_height, GL_RGBA, GL_FLOAT, 0);
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
    if ((frame % 60) == 0) glEndQuery(GL_TIME_ELAPSED);

    // display final image
    if ((frame % 60) == 0) glBeginQuery(GL_TIME_ELAPSED, timers[6]);
    glUseProgram(S.program[2]);
    glBindFramebuffer(GL_FRAMEBUFFER, 0);
    glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
    if ((frame % 60) == 0) glEndQuery(GL_TIME_ELAPSED);

    glfwSwapBuffers(window);

    // handle UI and quit conditions
    glfwPollEvents();
    int e = glGetError();
    if (e) { fprintf(stderr, "OpenGL Error %d\n", e); }
    frame++;

  }

  glfwTerminate();
  fprintf(stderr, "\n%d\n", frame);
  return 0;
  (void) argc;
  (void) argv;
}
