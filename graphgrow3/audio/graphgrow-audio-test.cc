#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <lo/lo.h>

struct control
{
  float   scale[4][4][8];
  float   pan  [4][4][8];
  uint8_t level[4][4][8];
};

int main()
{
  control C;
  memset(&C, 0, sizeof(C));

  lo_address t = lo_address_new("255.255.255.255", "6060");

  while (1)
  {
    sleep(1);

    int i = rand() % 4;
    int j = rand() % 4;
    int k = rand() % 8;
    float   scale =  rand() / (double) RAND_MAX; scale *= scale; scale *= scale;
    float   pan   = (rand() / (double) RAND_MAX - 0.5) * 0.5 + 0.5;
    uint8_t level =  rand() % 2;
    C.scale[i][j][k] = scale;
    C.pan  [i][j][k] = pan;
    C.level[i][j][k] = level;

    int count = 0;
    for (i = 0; i < 4; ++i)
      for (j = 0; j < 4; ++j)
        for (k = 0; k < 8; ++k)
          count += C.level[i][j][k];
    fprintf(stderr, "  %d\r", count);

    lo_blob blob = lo_blob_new(sizeof(C), &C);
    if (blob)
    {
      if (lo_send(t, "/audio", "b", blob) == -1)
        fprintf(stderr, "OSC error %d: %s\n", lo_address_errno(t), lo_address_errstr(t));
      lo_blob_free(blob);
    }
    else
      fprintf(stderr, "OSC error: couldn't lo_blob_new\n");

  }
  return 0;
}
