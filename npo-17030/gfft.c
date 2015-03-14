

#include "stdio.h"
#include "2dcc.h"
#include "math.h"


bitreversal(X, d)
int X[];
int d;
{
   int nv2, nm1, i, j, t, k;

   for (i = 0; i < d; ++i)
      X[i] = i;
   nv2 = d / 2;
   nm1 = d - 1;
   j = 1;
   for (i = 1; i <= nm1; ++i) {
      if (i >= j) goto a25;
      t = X[j-1];
      X[j-1] = X[i-1];
      X[i-1] = t;
      a25: k = nv2;
      a26: if (k >= j) goto a30;
      j -= k;
      k /= 2;
      goto a26;
      a30: j += k;
   }
}

GFFT()
{
   int i, j;

   DITGFFT(A);
   DITGFFT(B);
   for (i = 0; i < d1; ++i)
      for (j = 0; j < d2; ++j) {
         C_Ary[i][j].real = A[i][j].real * B[i][j].real -
                        A[i][j].imag * B[i][j].imag;
         C_Ary[i][j].imag = A[i][j].real * B[i][j].imag +
                        A[i][j].imag * B[i][j].real;
      }

   DIFIGFFT(C_Ary);
}


DITGFFT(X)
struct complex X[][MAXd2];
{
   int i, j, k, ii, jj, s, n, mk, rr, dd;
   int le, le1, le2, ip, v;
   double f;
   char reversal[MAXd1];
   struct complex alpha, alphak, T;

   n = d1 / 2;
   for (s = 0; s < two_r; s++) {
      mk = s * n;
      for (i = 0; i < n; ++i)  reversal[i] = 0;
      for (i = 0; i < n; ++i)
         if (!reversal[i] && ((j=bitmap3[i]) != i)) {
            reversal[j] = 1;
            ii = mk + i;  jj = mk + j;
            for (k = 0; k < d1; k++) {
               T.real = X[k][ii].real;  T.imag = X[k][ii].imag;
               X[k][ii].real = X[k][jj].real;
               X[k][ii].imag = X[k][jj].imag;
               X[k][jj].real = T.real;
               X[k][jj].imag = T.imag;
            }
         }
   }

 rr = log2(n);
 for (dd = 0; dd < d1; ++dd) 
  for (s = 0; s < two_r; s++) {
      le = n / 2;
      le1 = 2;
      le2 = le1 / 2;
    for (v = 0; v < rr; v++) {
      f = pi / le1;
      alphak.real = cos(f);
      alphak.imag = sin(f);
      f *= 2.0;
      alpha.real = cos(f);
      alpha.imag = sin(f);

      for (j = 0; j < le2; j++) {
         i = s * n + j;
         for (k = 0; k < le; k++) {
            ip = i + le2;
            T.real = alphak.real * X[dd][ip].real
                   - alphak.imag * X[dd][ip].imag;
            T.imag = alphak.real * X[dd][ip].imag
                   + alphak.imag * X[dd][ip].real;
            X[dd][ip].real = X[dd][i].real - T.real;
            X[dd][ip].imag = X[dd][i].imag - T.imag;
            X[dd][i].real = X[dd][i].real + T.real;
            X[dd][i].imag = X[dd][i].imag + T.imag;
            i += le1;
         }
         f = alphak.real * alpha.real - 
                       alphak.imag * alpha.imag;
         alphak.imag = alphak.real * alpha.imag +
                       alphak.imag * alpha.real;
         alphak.real = f;
      }
      le /= 2;
      le1 *= 2;  le2 *= 2;
   }
}

}

DIFIGFFT(X)
struct complex X[][MAXd2];
{
   int i, j, k, ii, jj, s, n, mk, rr, dd;
   int le, le1, le2, ip, v;
   double f;
   char reversal[MAXd1];
   struct complex alpha, alphak, T;

 n = d1 / 2;
 rr = log2(n);
 for (dd = 0; dd < d1; ++dd)
  for (s = 0; s < two_r; s++) {
      le = 1;
      le1 = n;
      le2 = le1 / 2;
   for (v = 0; v < rr; v++) {
      f = - pi / le1;
      alphak.real = cos(f);
      alphak.imag = sin(f);
      f *= 2.0;
      alpha.real = cos(f);
      alpha.imag = sin(f);

      for (j = 0; j < le2; j++) {
         i = s * n + j;
         for (k = 0; k < le; k++) {
            ip = i + le2;
            T.real = X[dd][i].real - X[dd][ip].real;
            T.imag = X[dd][i].imag - X[dd][ip].imag;
            X[dd][i].real = X[dd][i].real + X[dd][ip].real;
            X[dd][i].imag = X[dd][i].imag + X[dd][ip].imag;
            X[dd][ip].real = alphak.real * T.real - alphak.imag * T.imag;
            X[dd][ip].imag = alphak.real * T.imag + alphak.imag * T.real;
            i += le1;
         }
         f = alphak.real * alpha.real - 
                       alphak.imag * alpha.imag;
         alphak.imag = alphak.real * alpha.imag +
                       alphak.imag * alpha.real;
         alphak.real = f;
      }
      le *= 2;
      le1 /= 2;  le2 /= 2;
   }
}

   f = n; f = 1.0 / f;
   for (i = 0; i < d1; ++i)
      for (j = 0; j < d2; ++j) {
         X[i][j].real = f * X[i][j].real;
         X[i][j].imag = f * X[i][j].imag;
      }

   for (s = 0; s < two_r; s++) {
      mk = s * n;
      for (i = 0; i < n; ++i)  reversal[i] = 0;
      for (i = 0; i < n; ++i)
         if (!reversal[i] && ((j=bitmap3[i]) != i)) {
            reversal[j] = 1;
            ii = mk + i;  jj = mk + j;
            for (k = 0; k < d1; k++) {
               T.real = X[k][ii].real;  T.imag = X[k][ii].imag;
               X[k][ii].real = X[k][jj].real;
               X[k][ii].imag = X[k][jj].imag;
               X[k][jj].real = T.real;
               X[k][jj].imag = T.imag;
            }
         }
   }
}

