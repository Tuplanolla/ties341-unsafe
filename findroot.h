#ifndef FINDROOT_H
#define FINDROOT_H

#include <stdbool.h>

bool findroot(double (* f)(double),
    double a, double b,
    double epsilon,
    unsigned int n,
    double* p);

bool findroot_d(double (* f)(double),
    double (* df)(double),
    double a, double b,
    double epsilon,
    unsigned int n,
    double* p);

#endif
