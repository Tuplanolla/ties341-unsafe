#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "findroot.h"

static double snaf(double const x) {
  return x > 0 ? exp(-1 / x) : 0;
}

static double dsnaf(double const x) {
  if (x > 0) {
    double const y = 1 / x;

    return y * y * exp(-y);
  }

  return (double) 0;
}

static double strans(double const x) {
  double const snafx = snaf(x);

  return snafx / (snafx + snaf(1 - x));
}

static double dstrans(double const x) {
  double const y = 1 - x;
  double const snafx = snaf(x), snafy = snaf(y);
  double const dsnafx = dsnaf(x), dsnafy = dsnaf(y);
  double const snafxy = snafx + snafy;

  return (snafy * dsnafx - snafx * dsnafy) / (snafxy * snafxy);
}

static double f(double const x) {
  return x + (strans((x + 1) / 2) * 2 - 1) * 2;
}

static double df(double const x) {
  return 1 + dstrans((x + 1) / 2) * 2;
}

int main(void) {
  double x;

  if (!findroot(f, -2, 8, 1e-6, 256, &x)) {
    fprintf(stderr, "Numerical error.");

    return EXIT_FAILURE;
  }

  fprintf(stdout, "%g\n", x);

  if (!findroot_d(f, df, -2, 8, 1e-6, 256, &x)) {
    fprintf(stderr, "Numerical error.");

    return EXIT_FAILURE;
  }

  fprintf(stdout, "%g\n", x);

  return EXIT_SUCCESS;
}
