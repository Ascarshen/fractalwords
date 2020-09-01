#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <lo/lo.h>
#include <glm/glm.hpp>

struct control
{
  int32_t active;
  int32_t count    [4];
  int32_t source   [4][8];
  float   scale    [4][8];
  glm::mat3 transform[4][8];
};

int main()
{
  control C;
  memset(&C, 0, sizeof(C));
  for (int i = 0; i < 4; ++i)
    for (int j = 0; j < 8; ++j)
    {
      float s  = rand() / (double) RAND_MAX; s *= s;
      float r =  rand() / (double) RAND_MAX * 2 * 3.141592653589793;
      float tx = rand() / (double) RAND_MAX - 0.5;
      float ty = rand() / (double) RAND_MAX - 0.5;
      float co = cosf(r) * s;
      float si = sinf(r) * s;
      C.source   [i][j] = rand() % 4;
      C.scale    [i][j] = s;
      C.transform[i][j][0][0] = co;
      C.transform[i][j][0][1] = si;
      C.transform[i][j][0][2] = tx;
      C.transform[i][j][1][0] = -si;
      C.transform[i][j][1][1] = co;
      C.transform[i][j][1][2] = ty;
      C.transform[i][j][2][0] = 0;
      C.transform[i][j][2][1] = 0;
      C.transform[i][j][2][2] = 1;
      C.transform[i][j] = glm::inverse(C.transform[i][j]);
    }

  lo_address t = lo_address_new("255.255.255.255", "6061");

  while (1)
  {
    sleep(1);

    C.active = (C.active + 1) % 4;
    int i = rand() % 4;
    C.count[i] = (rand() % 8) + 1;
    int j = rand() % 8;
    float s  = rand() / (double) RAND_MAX; s *= s;
    float r =  rand() / (double) RAND_MAX * 2 * 3.141592653589793;
    float tx = rand() / (double) RAND_MAX - 0.5;
    float ty = rand() / (double) RAND_MAX - 0.5;
    float co = cosf(r) * s;
    float si = sinf(r) * s;
    C.source   [i][j] = rand() % 4;
    C.scale    [i][j] = s;
    C.transform[i][j][0][0] = co;
    C.transform[i][j][0][1] = si;
    C.transform[i][j][0][2] = tx;
    C.transform[i][j][1][0] = -si;
    C.transform[i][j][1][1] = co;
    C.transform[i][j][1][2] = ty;
    C.transform[i][j][2][0] = 0;
    C.transform[i][j][2][1] = 0;
    C.transform[i][j][2][2] = 1;
    C.transform[i][j] = glm::inverse(C.transform[i][j]);

    lo_blob blob = lo_blob_new(sizeof(C), &C);
    if (blob)
    {
      if (lo_send(t, "/video", "b", blob) == -1)
        fprintf(stderr, "OSC error %d: %s\n", lo_address_errno(t), lo_address_errstr(t));
      lo_blob_free(blob);
    }
    else
      fprintf(stderr, "OSC error: couldn't lo_blob_new\n");

  }
  return 0;
}
