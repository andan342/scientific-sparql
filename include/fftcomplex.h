/*
 * "fftcomplex.h", Pjotr '87.
 */

#include	<math.h>
#ifndef __fftcomplex__
#define __fftcomplex__
#include "complex.h"
// This is stupid!
#ifndef _USE_MATH_DEFINES
#define _USE_MATH_DEFINES
#endif


void Fourier (COMPLEX *in, unsigned n, COMPLEX *out);
static split (COMPLEX *in, register unsigned r, register unsigned m, COMPLEX *out);
static join (COMPLEX *in, register unsigned m, register unsigned n, COMPLEX *out);
int rft (COMPLEX *in, unsigned n, COMPLEX *out);
int fft (COMPLEX *in, unsigned n, COMPLEX *out);
int fftpart (COMPLEX *in, unsigned n, COMPLEX *out, unsigned NN);

void realfft (double *in, unsigned n, double *out);
void realrft (double *in, unsigned n, double *out);

#endif
