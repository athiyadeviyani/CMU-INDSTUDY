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

/**************************************************************************
*                                                                         *
*    Digital Filter in Standard Form                                      *
*                                                                         *
*                                       1989.6   K.Tokuda                 *
*                                       1995.12  N.Isshiiki modified      *
*       usage:                                                            *
*               dfs [ options ] [ infile ] > stdout                       *
*       options:                                                          *
*               -a k a1 a2 ... aM  : denominator coefficients      [N/A]  *
*               -b b0 b1 b2 ... bM : numerator coefficients        [N/A]  *
*               -p pfile           : denominator coefficients file [NULL] *
*               -z zfile           : numerator coefficients file   [NULL] *
*       infile:                                                           *
*               input (float)                                             *
*       stdout:                                                           *
*               output (float)                                            *
*       note:                                                             *
*               M, N <= 2047                                              *
**************************************************************************/

static char *rcs_id = "$Id: dfs.c,v 1.23 2012/12/21 11:27:32 mataki Exp $";


/* Standard C Libraries */
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#ifdef HAVE_STRING_H
#  include <string.h>
#else
#  include <strings.h>
#  ifndef HAVE_STRRCHR
#     define strrchr rindex
#  endif
#endif


#if defined(WIN32)
#  include "SPTK.h"
#else
#  include <SPTK.h>
#endif

/* Default Values */
#define SIZE 2048

/* Command Name */
char *cmnd;


void usage(int status)
{
   fprintf(stderr, "\n");
   fprintf(stderr, " %s - digital filter in standard form\n", cmnd);
   fprintf(stderr, "\n");
   fprintf(stderr, "  usage:\n");
   fprintf(stderr, "       %s [ options ] [ infile ] > stdout \n", cmnd);
   fprintf(stderr, "  options:\n");
   fprintf(stderr,
           "       -a K  a1...aM : denominator coefficients      [N/A]\n");
   fprintf(stderr,
           "       -b b0 b1...bN : numerator coefficients        [N/A]\n");
   fprintf(stderr,
           "       -p pfile      : denominator coefficients file [NULL]\n");
   fprintf(stderr,
           "       -z zfile      : numerator coefficients file   [NULL]\n");
   fprintf(stderr, "       -h            : print this message\n");
   fprintf(stderr, "  infile:\n");
   fprintf(stderr,
           "       filter input (%s)                          [stdin]\n",
           FORMAT);
   fprintf(stderr, "  stdout:\n");
   fprintf(stderr, "       filter output (%s)\n", FORMAT);
   fprintf(stderr, "  notice:\n");
   fprintf(stderr, "       M,N <= %d \n", SIZE - 1);
#ifdef PACKAGE_VERSION
   fprintf(stderr, "\n");
   fprintf(stderr, " SPTK: version %s\n", PACKAGE_VERSION);
   fprintf(stderr, " CVS Info: %s", rcs_id);
#endif
   fprintf(stderr, "\n");
   exit(status);
}

int main(int argc, char *argv[])
{
   int i;
   static double a[SIZE], b[SIZE];
   static double d[SIZE];
   int bufp = 0;
   int na = -1, nb = -1;
   double x;
   char *file_z = "", *file_p = "";
   FILE *fp_z, *fp_p;

   if ((cmnd = strrchr(argv[0], '/')) == NULL)
      cmnd = argv[0];
   else
      cmnd++;
   while (--argc)
      if (**++argv == '-')
         switch (*(*argv + 1)) {
         case 'a':
            i = 0;
            while ((argc - 1) && !isalpha(*(*(argv + 1) + 1))) {
               a[i++] = atof(*++argv);
               argc--;
               na++;
            }
            break;
         case 'b':
            i = 0;
            while ((argc - 1) && !isalpha(*(*(argv + 1) + 1))) {
               b[i++] = atof(*++argv);
               argc--;
               nb++;
            }
            break;
         case 'z':
            argc--;
            file_z = *++argv;
            break;
         case 'p':
            argc--;
            file_p = *++argv;
            break;
         case 'h':
            usage(0);
         default:
            fprintf(stderr, "%s : Invalid option '%c'!\n", cmnd, *(*argv + 1));
            usage(1);
      } else {
         fprintf(stderr, "%s : Invalid option!\n", cmnd);
         usage(1);
      }

   if (*file_z != '\0') {
      fp_z = getfp(file_z, "rb");
      nb = freadf(b, sizeof(*b), SIZE, fp_z) - 1;
   }
   if (*file_p != '\0') {
      fp_p = getfp(file_p, "rb");
      na = freadf(a, sizeof(*a), SIZE, fp_p) - 1;
   }

   if (na == -1) {
      na = 0;
      a[0] = 1.0;
   }
   if (nb == -1) {
      nb = 0;
      b[0] = 1.0;
   }

   while (freadf(&x, sizeof(x), 1, stdin) == 1) {
      x = dfs(x, a, na, b, nb, d, &bufp);
      fwritef(&x, sizeof(x), 1, stdout);
   }

   return 0;
}
