#include <math.h>
#include <stdbool.h>
#include <stddef.h>
#include "findroot.h"

static bool samesign(double const x, double const y) {
  return copysign(1, x) * copysign(1, y) > 0;
}

static double midpoint(double const x, double const y) {
  return x + (y - x) / 2;
}

bool findroot(double (* const f)(double),
    double const a, double const b,
    double const epsilon,
    unsigned int const n,
    double* const p) {
  if (samesign(f(a), f(b)))
    return false;

  double x1 = a, x2 = b;
  double x = midpoint(x1, x2);
  double fx1 = f(x1);

  for (unsigned int i = 0;
      i < n;
      ++i) {
    double const fx = f(x);

    if (fabs(fx) < epsilon) {
      if (p != NULL)
        *p = x;

      return true;
    }

    if (samesign(fx, fx1)) {
      x1 = x;
      fx1 = fx;
    } else
      x2 = x;

    x = midpoint(x1, x2);
  }

  return false;
}

bool findroot_d(double (* const f)(double),
    double (* const df)(double),
    double const a, double const b,
    double const epsilon,
    unsigned int const n,
    double* const p) {
  if (samesign(f(a), f(b)))
    return false;

  double x = midpoint(a, b);

  for (unsigned int i = 0;
      i < n;
      ++i) {
    double const fx = f(x);

    if (fabs(fx) < epsilon) {
      if (p != NULL)
        *p = x;

      return true;;
    }

    x = x - fx / df(x);

    if (!(x >= a && x <= b))
      break;
  }

  return findroot(f, a, b, epsilon, n, p);
}
