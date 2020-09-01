#include <assert.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <jack/jack.h>
#include <lo/lo.h>

#define PI 3.141592653589793
#define SR 48000
#define BLOCKSIZE 64
// DELAYSIZE must be a multiple of BLOCKSIZE
#define DELAYSIZE 2048

#define GG_HIP 25.0f
#define GG_LOP 5000.0f

typedef float sample;

// 256 is BLOCKSIZE * sizeof(sample)
typedef float    signal __attribute__ ((vector_size (256)));
typedef int32_t isignal __attribute__ ((vector_size (256)));

// 8192 is DELAYSIZE * sizeof(sample)
typedef sample delaybuffer __attribute__ ((vector_size (8192)));

#define   likely(x) __builtin_expect((x),1)
#define unlikely(x) __builtin_expect((x),0)

// postcondition: 0 <= phase <= 1
static inline float cos_reduce(float phase) {
  float p = fabsf(phase);
  // p >= 0
  if (likely(p < (1 << 24))) {
    int q = p;
    return p - q;
  } else {
    if (unlikely(isnanf(p) || isinff(p))) {
      // return NaN
      return p - p;
    } else {
      // int could overflow, and it will be integral anyway
      return 0.0f;
    }
  }
}

signal cos_reduce(const signal &phase) {
  signal p = phase >= 0.0f ? phase : -phase;
  isignal i = isignal(p);
  signal q = signal(i);
  return p < sample(1 << 24) ? p - q : 0.0f;
}

// precondition: 0 <= phase <= 1
static inline float cos_unsafe(float phase) {
  float p = fabsf(4.0f * phase - 2.0f) - 1.0f;
  // p in -1 .. 1
  float s
    = 1.5707963267948965580e+00f * p
    - 6.4596271553942852250e-01f * p * p * p
    + 7.9685048314861006702e-02f * p * p * p * p * p
    - 4.6672571910271187789e-03f * p * p * p * p * p * p * p
    + 1.4859762069630022552e-04f * p * p * p * p * p * p * p * p * p;
  // compiler figures out optimal simd multiplications
  return s;
}

signal cos_unsafe(const signal &phase) {
  signal p = 4.0f * phase - 2.0f;
  p = p >= 0.0f ? p : -p;
  p -= 1.0f;
  // p in -1 .. 1
  signal p2 = p * p;
  signal p3 = p2 * p;
  signal p5 = p2 * p3;
  signal p7 = p2 * p5;
  signal p9 = p2 * p7;
  signal s
    = 1.5707963267948965580e+00f * p
    - 6.4596271553942852250e-01f * p3
    + 7.9685048314861006702e-02f * p5
    - 4.6672571910271187789e-03f * p7
    + 1.4859762069630022552e-04f * p9;
  return s;
}

inline sample cos_poly(sample phase) {
  return cos_unsafe(cos_reduce(phase));
}

inline sample sin_poly(sample phase) {
  return cos_poly(phase - 0.25f);
}

signal cos(const signal &phase) {
  return cos_unsafe(cos_reduce(phase));
}

signal sin(const signal &phase) {
  return cos(phase - 0.25f);
}

inline sample dbtorms(sample f) {
  return expf(0.5f * 0.23025850929940458f * (fminf(f, 870.0f) - 100.0f));
}

inline sample rmstodb(sample f) {
  return 100.0f + 2.0f * 4.3429448190325175f * logf(f);
}

inline sample wrap(sample a) {
  return a - floorf(a);
}

signal wrap(const signal &a) {
  signal c;
  for (int i = 0; i < BLOCKSIZE; ++i)
    c[i] = wrap(a[i]);
  return c;
#if 0
  isignal i = isignal(a) + (a < 0.0f); // vector compare produces ~0
  signal q = signal(i);
  return a - q;
#endif
}

signal exp(const signal &a)
{
  signal c;
  for (int i = 0; i < BLOCKSIZE; ++i)
    c[i] = expf(a[i]);
  return c;
}

signal log(const signal &a)
{
  signal c;
  for (int i = 0; i < BLOCKSIZE; ++i)
    c[i] = logf(a[i]);
  return c;
}

signal sqrt(const signal &a)
{
  signal c;
  for (int i = 0; i < BLOCKSIZE; ++i)
    c[i] = sqrtf(a[i]);
  return c;
}

signal tanh(const signal &a)
{
  signal c;
  for (int i = 0; i < BLOCKSIZE; ++i)
    c[i] = tanhf(a[i]);
  return c;
}

signal sqr(const signal &a)
{
  return a * a;
}

signal min(const signal &a, const signal &b) {
  return a < b ? a : b;
}

signal min(const signal &a, sample b) {
  return a < b ? a : b;
}

signal min(sample a, const signal &b) {
  return a < b ? a : b;
}

sample min(sample a, sample b) {
  return a < b ? a : b;
}

signal dbtorms(const signal &f)
{
  return exp(0.5f * 0.23025850929940458f * (min(f, 870.0f) - 100.0f));
}

signal rmstodb(const signal &f)
{
  return 100.0f + 2.0f * 4.3429448190325175f * log(f);
}

struct lop
{
  // http://www.arpchord.com/pdf/coeffs_first_order_filters_0p1.pdf
  sample k, k12, x1, y1;
  lop(sample hz) : x1(0.0f), y1(0.0f)
  {
    double a = 2 * PI * hz / double(SR);
    k = (1 - sin(a)) / cos(a);
    k12 = (1 - k) / 2;
  };
  signal operator()(const signal &x)
  {
    signal y;
    sample last_x = x1;
    sample last_y = y1;
    for (int i = 0; i < BLOCKSIZE; ++i)
    {
      sample now_x = x[i];
      last_y = y[i] = (now_x + last_x) * k12 + k * last_y;
      last_x = now_x;
    }
    x1 = last_x;
    y1 = last_y;
    return y;
  };
};

struct hip
{
  // http://www.arpchord.com/pdf/coeffs_first_order_filters_0p1.pdf
  sample k, k12, x1, y1;
  hip(sample hz) : x1(0.0f), y1(0.0f)
  {
    double a = 2 * PI * hz / double(SR);
    k = (1 - sin(a)) / cos(a);
    k12 = (1 + k) / 2;
  };
  signal operator()(const signal &x)
  {
    signal y;
    sample last_x = x1;
    sample last_y = y1;
    for (int i = 0; i < BLOCKSIZE; ++i)
    {
      sample now_x = x[i];
      last_y = y[i] = (now_x - last_x) * k12 + k * last_y;
      last_x = now_x;
    }
    x1 = last_x;
    y1 = last_y;
    return y;
  };
};

struct vsig
{
  sample x1;
  vsig(sample x) : x1(x)
  {
  };
  signal operator()(sample x)
  {
    signal y;
    sample last_x = x1;
    for (int i = 0; i < BLOCKSIZE; ++i)
    {
      sample t = (sample(i) + 0.5f) / sample(BLOCKSIZE);
      y[i] = last_x + (x - last_x) * t;
    }
    x1 = x;
    return y;
  };
};

struct phasor
{
  sample p;
  phasor() : p(0.0f)
  {
  };
  signal operator()(const signal &hz)
  {
    signal y;
    sample last_p = p;
    for (int i = 0; i < BLOCKSIZE; ++i)
      last_p = y[i] = wrap(last_p + hz[i] / sample(SR));
    p = last_p;
    return y;
  };
};

struct noise
{
  // pd-0.47-0/src/d_osc.c
  static int32_t seed;
  int32_t val;
  noise()
  {
    val = (seed *= 1319);
  };
  signal operator()()
  {
    signal y;
    int32_t v = val;
    for (int i = 0; i < BLOCKSIZE; ++i)
    {
      y[i] = sample((v & 0x7fffffff) - 0x40000000) * sample(1.0 / 0x40000000);
      v = v * 435898247 + 382842987;
    }
    val = v;
    return y;
  };
};
int32_t noise::seed = 307;

struct delay
{
  delaybuffer v;
  int w1;
  delay() : w1(0)
  {
    for (int i = 0; i < DELAYSIZE; ++i)
      v[i] = 0.0f;
  };
  void write(const signal &x)
  {
    int w = w1;
    assert(w + BLOCKSIZE <= DELAYSIZE);
    for (int i = 0; i < BLOCKSIZE; ++i)
      v[w + i] = x[i];
    w += BLOCKSIZE;
    if (w == DELAYSIZE)
      w = 0;
    w1 = w;
  };
  signal operator()(sample ms)
  {
    signal y;
    int w = w1;
    int r = roundf(ms * sample(SR / 1000.0f));
    assert(0 <= r);
    assert(r < DELAYSIZE - BLOCKSIZE);
    for (int i = 0; i < BLOCKSIZE; ++i)
    {
      int k = w - r + i;
      k = k < 0 ? k + DELAYSIZE : k;
      assert(0 <= k);
      assert(k < DELAYSIZE);
      y[i] = v[k];
    }
    return y;
  };
  signal operator()(const signal &ms)
  {
    // https://en.wikipedia.org/wiki/Cubic_Hermite_spline#Interpolation_on_the_unit_interval_without_exact_derivatives
    signal y;
    int w = w1;
    for (int i = 0; i < BLOCKSIZE; ++i)
    {
      sample d = ms[i] * sample(SR / 1000.0f);
      int d1 = floorf(d);
      int d0 = d1 - 1;
      int d2 = d1 + 1;
      int d3 = d1 + 2;
      sample t = d - d1;
      assert(0 <= d0);
      assert(d3 < DELAYSIZE - BLOCKSIZE);
      assert(0 <= t);
      assert(t < 1);
      int r0 = w - d0 + i;
      int r1 = w - d1 + i;
      int r2 = w - d2 + i;
      int r3 = w - d3 + i;
      r0 = r0 < 0 ? r0 + DELAYSIZE : r0;
      r1 = r1 < 0 ? r1 + DELAYSIZE : r1;
      r2 = r2 < 0 ? r2 + DELAYSIZE : r2;
      r3 = r3 < 0 ? r3 + DELAYSIZE : r3;
      assert(0 <= r0);
      assert(0 <= r1);
      assert(0 <= r2);
      assert(0 <= r3);
      assert(r0 < DELAYSIZE);
      assert(r1 < DELAYSIZE);
      assert(r2 < DELAYSIZE);
      assert(r3 < DELAYSIZE);
      sample y0 = v[r0];
      sample y1 = v[r1];
      sample y2 = v[r2];
      sample y3 = v[r3];
      sample a0 = -t*t*t + 2.0f*t*t - t;
      sample a1 = 3.0f*t*t*t - 5.0f*t*t + 2.0f;
      sample a2 = -3.0f*t*t*t + 4.0f*t*t + t;
      sample a3 = t*t*t - t*t;
      y[i] = 0.5f * (a0 * y0 + a1 * y1 + a2 * y2 + a3 * y3);
    }
    return y;
  };
};

struct pitchshift
{
  delay del;
  phasor p;
  signal operator()(const signal &audio, const signal &transpose)
  {
    const sample d = 20.0f;
    del.write(audio);
    signal hz = (1.0f - exp(transpose * 0.05776f)) * (1000.0f / d);
    signal phase1 = p(hz);
    signal phase2 = wrap(phase1 + 0.5f);
    return cos((phase1 - 0.5f) * 0.5f) * del(d * phase1 + 2.0f)
         + cos((phase2 - 0.5f) * 0.5f) * del(d * phase2 + 2.0f);
  };
};

struct compress
{
  sample threshold;
  sample factor;
  hip hi;
  lop lo1, lo2;
  compress(sample db)
  : threshold(db), factor(0.25f / dbtorms((100.0f - db) * 0.125f + db)), hi(5.0f), lo1(10.0f), lo2(25.0f)
  {
  };
  signal operator()(const signal &audio)
  {
    signal rms = lo2(0.01f + sqrt(lo1(sqr(hi(audio)))));
    signal db = rmstodb(rms);
    db = db > threshold ? threshold + (db - threshold) * 0.125f : threshold;
    signal gain = factor * dbtorms(db);
    return tanh(audio / rms * gain);
  };
};

struct edge
{
  signal *inl, *inr;
  signal *outl, *outr;
  pitchshift psl, psr;
  vsig scale, pan, level;
  edge(signal *from_l, signal *from_r, signal *to_l, signal *to_r)
  : inl(from_l), inr(from_r), outl(to_l), outr(to_r), scale(0.5), pan(0.5), level(0)
  {
  };
  edge() : edge(0, 0, 0, 0) { };
  void operator()(sample iscale, sample ipan, sample ilevel)
  {
    signal vscale = scale(iscale);
    signal vpan = pan(0.25f * ipan);
    signal vlevel = level(ilevel) * vscale;
    signal transpose = log(vscale * 2.0f) * sample(-12.0f / logf(2.0f));
    *outl += vlevel * cos(vpan) * psl(*inl, transpose);
    *outr += vlevel * sin(vpan) * psr(*inr, transpose);
  };
};

struct rule
{
  signal send_l, send_r, return_l, return_r;
  signal *out_l, *out_r;
  delay delayl, delayr;
  noise noisel, noiser;
  hip hipl, hipr;
  lop lopl, lopr;
  compress compressl, compressr;
  sample delaytime;
  rule(signal *outl, signal *outr, sample delaytime)
  : out_l(outl), out_r(outr), hipl(GG_HIP), hipr(GG_HIP), lopl(GG_LOP), lopr(GG_LOP), compressl(48.0f), compressr(48.0f), delaytime(delaytime)
  {
    for (int i = 0; i < BLOCKSIZE; ++i)
    {
      send_l[i] = 0.0f;
      send_r[i] = 0.0f;
      return_l[i] = 0.0f;
      return_r[i] = 0.0f;
    }
  };
  rule() : rule(0, 0, 2) { };
  void operator()()
  {
    send_l = noisel() * 1.0e-6f + delayl(delaytime);
    send_r = noiser() * 1.0e-6f + delayr(delaytime);
    signal l = compressl(lopl(hipl(return_l)));
    signal r = compressr(lopr(hipr(return_r)));
    // one block delay
    for (int i = 0; i < BLOCKSIZE; ++i)
    {
      return_l[i] = 0.0f;
      return_r[i] = 0.0f;
    }
    delayl.write(l);
    delayr.write(r);
    *out_l += l;
    *out_r += r;
  };
};

struct graphgrow
{
  signal out_l, out_r;
  hip hil, hir;
  compress compressl, compressr;
  rule    rules[4];
  edge    edges[4][4][8];
  /* { osc 1152 bytes { */
  sample  scale[4][4][8];
  sample  pan  [4][4][8];
  uint8_t level[4][4][8];
  /* } osc } */
  uint8_t last_level[4][4][8];
  graphgrow()
  : hil(GG_HIP), hir(GG_HIP), compressl(48.0f), compressr(48.0f)
  {
    for (int i = 0; i < BLOCKSIZE; ++i)
    {
      out_l[i] = 0.0f;
      out_r[i] = 0.0f;
    }
    for (int i = 0; i < 4; ++i)
      rules[i] = rule(&out_l, &out_r, 10.0f + sample(i));
    for (int i = 0; i < 4; ++i)
      for (int j = 0; j < 4; ++j)
        for (int k = 0; k < 8; ++k)
        {
          edges[i][j][k] = edge(&rules[i].send_l, &rules[i].send_r, &rules[j].return_l, &rules[j].return_r);
          scale[i][j][k] = 0.5f;
          pan  [i][j][k] = 0.5f;
          level[i][j][k] = 0;
          last_level[i][j][k] = 0;
        }
  };
  void operator()()
  {
    for (int i = 0; i < BLOCKSIZE; ++i)
    {
      out_l[i] = 0.0f;
      out_r[i] = 0.0f;
    }
    for (int i = 0; i < 4; ++i)
      rules[i]();
    for (int i = 0; i < 4; ++i)
      for (int j = 0; j < 4; ++j)
        for (int k = 0; k < 8; ++k)
        {
          if (level[i][j][k] || last_level[i][j][k])
            edges[i][j][k](scale[i][j][k], pan[i][j][k], level[i][j][k]);
          last_level[i][j][k] = level[i][j][k];
        }
    out_l = compressl(hil(out_l));
    out_r = compressr(hir(out_r));
  };
};

struct control
{
  sample  scale[4][4][8];
  sample  pan  [4][4][8];
  uint8_t level[4][4][8];
};


struct
{
  graphgrow G;
  int count;
  jack_client_t *client;
  jack_port_t *out_port[2];
  volatile int in_processcb;
  volatile int running;
  control C;
} S;

int processcb(jack_nframes_t nframes, void *arg)
{
  S.in_processcb = 1;
  jack_default_audio_sample_t *out_l = (jack_default_audio_sample_t *) jack_port_get_buffer(S.out_port[0], nframes);
  jack_default_audio_sample_t *out_r = (jack_default_audio_sample_t *) jack_port_get_buffer(S.out_port[1], nframes);

  for (int i = 0; i < 4; ++i)
    for (int j = 0; j < 4; ++j)
      for (int k = 0; k < 8; ++k)
      {
        S.G.scale[i][j][k] = S.C.scale[i][j][k];
        S.G.pan  [i][j][k] = S.C.pan  [i][j][k];
        S.G.level[i][j][k] = S.C.level[i][j][k];
      }

  int count = S.count;
  for (jack_nframes_t i = 0; i < nframes; ++i)
  {
    if (count == 0)
      S.G();
    out_l[i] = S.G.out_l[count];
    out_r[i] = S.G.out_r[count];
    count = (count + 1) % BLOCKSIZE;
  }
  S.count = count;

  S.in_processcb = 0;
  return 0;
  (void) arg;
}

void errorcb(int num, const char *msg, const char *path)
{
  fprintf(stderr, "liblo server error %d in path %s: %s\n", num, path, msg);
}

int audiocb(const char *path, const char *types, lo_arg **argv, int argc, void *message, void *user_data)
{
  const int bytes = sizeof(S.C);
  int   size =  argv[0]->blob.size;
  void *data = &argv[0]->blob.data;
  if (size == bytes)
  {
    while (S.in_processcb)
      ;
    memcpy(&S.C, data, bytes);
  }
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

int main()
{

   if (! (S.client = jack_client_open("graphgrow", JackNoStartServer, 0))) {
    fprintf(stderr, "jack server not running?\n");
    return 1;
  }
  jack_set_process_callback(S.client, processcb, 0);
  S.out_port[0] = jack_port_register(S.client, "output_1", JACK_DEFAULT_AUDIO_TYPE, JackPortIsOutput, 0);
  S.out_port[1] = jack_port_register(S.client, "output_2", JACK_DEFAULT_AUDIO_TYPE, JackPortIsOutput, 0);
  if (jack_activate(S.client)) {
    fprintf (stderr, "cannot activate JACK client");
    return 1;
  }
  if (jack_connect(S.client, "graphgrow:output_1", "system:playback_1")) {
    fprintf(stderr, "cannot connect output port\n");
  }
  if (jack_connect(S.client, "graphgrow:output_2", "system:playback_2")) {
    fprintf(stderr, "cannot connect output port\n");
  }

  S.running = 1;

  lo_server_thread lo = lo_server_thread_new("6060", errorcb);
  lo_server_thread_add_method(lo, "/audio", "b", audiocb, 0);
  lo_server_thread_add_method(lo, "/quit", "", quitcb, 0);
  lo_server_thread_start(lo);

  while (S.running)
    sleep(1);

  lo_server_thread_free(lo);

  jack_client_close(S.client);

  return 0;
}
