/* ----------------------------------------------------------------- */
/*             The Speech Signal Processing Toolkit (SPTK)           */
/*             developed by SPTK Working Group                       */
/*             http://sp-tk.sourceforge.net/                         */
/* ----------------------------------------------------------------- */
/*                                                                   */
/*  Copyright (c) 1984-2007  Tokyo Institute of Technology           */
/*                           Interdisciplinary Graduate School of    */
/*                           Science and Engineering                 */
/*                                                                   */
/*                1996-2012  Nagoya Institute of Technology          */
/*                           Department of Computer Science          */
/*                                                                   */
/* All rights reserved.                                              */
/*                                                                   */
/* Redistribution and use in source and binary forms, with or        */
/* without modification, are permitted provided that the following   */
/* conditions are met:                                               */
/*                                                                   */
/* - Redistributions of source code must retain the above copyright  */
/*   notice, this list of conditions and the following disclaimer.   */
/* - Redistributions in binary form must reproduce the above         */
/*   copyright notice, this list of conditions and the following     */
/*   disclaimer in the documentation and/or other materials provided */
/*   with the distribution.                                          */
/* - Neither the name of the SPTK working group nor the names of its */
/*   contributors may be used to endorse or promote products derived */
/*   from this software without specific prior written permission.   */
/*                                                                   */
/* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND            */
/* CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,       */
/* INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF          */
/* MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE          */
/* DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS */
/* BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,          */
/* EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED   */
/* TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,     */
/* DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON */
/* ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,   */
/* OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY    */
/* OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE           */
/* POSSIBILITY OF SUCH DAMAGE.                                       */
/* ----------------------------------------------------------------- */

/********************************************************
$Id: _fft2.c,v 1.15 2012/12/21 11:27:32 mataki Exp $

*   fft2 : two dimensional fast Fourier transform       *
*                            for complex sequence       *
*                                                       *
*       int fft2( x, y, n )                             *
*                                                       *
*       double   x[0]...x[n*n-1];                       *
*                       input  : data sequence          *
*                       output : real part of DFT       *
*       double   y[0]...y[n*n-1];                       *
*                       input  : working area           *
*                       output : imaginary part of DFT  *
*       int      n;     size of DFT                     *
*                                                       *
*                       T. Kobayashi   May, 1989.       *
********************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#if defined(WIN32)
#  include "SPTK.h"
#else
#  include <SPTK.h>
#endif

int fft2(double x[], double y[], const int n)
{
   double *xq, *yq;
   static double *xb = NULL, *yb;
   double *xp, *yp;
   int i, j;
   static int size_f;

   if (xb == NULL) {
      size_f = 2 * n;
      xb = dgetmem(size_f);
      yb = xb + n;
   }
   if (2 * n > size_f) {
      free(xb);
      size_f = 2 * n;
      xb = dgetmem(size_f);
      yb = xb + n;
   }

   for (i = 0; i < n; i++) {
      xp = xb;
      xq = x + i;
      yp = yb;
      yq = y + i;
      for (j = n; --j >= 0; xq += n, yq += n) {
         *xp++ = *xq;
         *yp++ = *yq;
      }

      if (fft(xb, yb, n) < 0)
         return (-1);

      xp = xb;
      xq = x + i;
      yp = yb;
      yq = y + i;
      for (j = n; --j >= 0; xq += n, yq += n) {
         *xq = *xp++;
         *yq = *yp++;
      }
   }

   for (i = n, xp = x, yp = y; --i >= 0; xp += n, yp += n) {
      if (fft(xp, yp, n) < 0)
         return (-1);
   }

   return 0;
}
