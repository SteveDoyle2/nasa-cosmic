
#include <stdio.h>
#include "2dcc.h"
#include "math.h"

extern double getnum();
char progname[17];
char lstname[17];
FILE *lfp;
FILE *ifp;

char tracea, tracec;

main(argc, argv)
int argc;
char *argv[];
{
   int i;
   char *np, *op;
	
   tracea = 1; tracec = 0;

   if (argc != 2) {
      printf("command is \"2dcc input_file\"\n\n");
      exit(0);
   }
   strcpy(progname, argv[1]);
   for (np = lstname, op = progname;
        (*op != '.') && (*op != '\0');
        *np++ = *op++)
       ;
   strcpy(np, ".lst");  
   readinp();
   lfp = fopen(lstname, "w");
   fprintf(lfp, "\n *** After data were read in ***\n\n");
   flist();

   pi = 3.14159265358979;   
   m = log2(d2);
   r = m + 1 - log2(d1);
   two_r = power(2, r);
   bitreversal(bitmap2, two_r);
   bitreversal(bitmap3, d1/2);

   for (i = 0; i < d1; i++) 
      DIT_FFT(A[i], d2, r);
   fprintf(lfp, "\n *** After Module reduction by DIT FFT ***\n\n");
   flist();
   betaconv(A, 1);
   fprintf(lfp, "\n *** After BETAk X conversion ***\n\n");
   flist();
   DIFFPT(A);
   fprintf(lfp, "\n *** After DIF FPT ***\n\n");
   flist();

   for (i = 0; i < d1; i++) 
      DIT_FFT(B[i], d2, r);
   betaconv(B, 1);
   DIFFPT(B);

   tracec = 1;
   GFFT();
   fprintf(lfp, "\n *** After C(X)=A(X)B(X) by GFFT ***\n\n");
   flist();
   DITIFPT(C_Ary);
   fprintf(lfp, "\n *** After DIT IFPT ***\n\n");
   flist();
   betaconv(C_Ary, -1);
   fprintf(lfp, "\n *** After BETAk**-1 Z conversion\n\n");
   flist();
   /* reconstruction */
   for (i = 0; i < d1; i++)  
      DIF_FFT(C_Ary[i], d2, r);
   
   fprintf(lfp, "\n *** After reconstruction\n\n");
   flist();

   fclose(ifp);  fclose(lfp);
}

DIFFPT(X)
struct complex X[][MAXd2];
{
   int i, j, k, l, s, n, m0, ll, mk, ml, mm, shift, rsh, dd;
   struct complex ov;

   n = d1 / 2;
   for (k = 0; k < two_r; k++) {
      dd = d1 / 2; m0 = 1;
      mk = bitmap2[k] * n;
      for (s = m-r+1; s > 0; --s) {
         for (i = 0 ; i < m0; ++i) {
            l = 2 * dd *i;  ml = 0;
            for (ll = 0; ll < dd; ++ll) {
               j = l + dd;
               /* perform butterfly */
               shift = mk + n;
               for (rsh = mk; rsh < shift; ++rsh) {
                  ov.real = X[l][rsh].real + X[j][rsh].real;
                  ov.imag = X[l][rsh].imag + X[j][rsh].imag;
                  X[j][rsh].real = X[l][rsh].real - X[j][rsh].real;
                  X[j][rsh].imag = X[l][rsh].imag - X[j][rsh].imag;
                  X[l][rsh].real = ov.real;
                  X[l][rsh].imag = ov.imag;
               }
               if (ml > 0) {   /* shift right */
                  for (shift = 0; shift < ml; ++shift) {
                     mm = mk + n - 1;
                     ov.real = X[j][mm].real;
                     ov.imag = X[j][mm].imag;
                     for (rsh = mm; rsh > mk; --rsh) {
                        X[j][rsh].real = X[j][rsh-1].real;
                        X[j][rsh].imag = X[j][rsh-1].imag;
                     }
                     X[j][mk].real = -ov.real;
                     X[j][mk].imag = -ov.imag;
                  }
               }
               ++l;  ml += m0;
            }
         }
         dd /= 2;  m0 *= 2;  
      }
   }
}

DITFPT(X)
struct complex X[][MAXd2];
{
   int i, j, k, l, s, n, m0, ll, mk, ml, mm, shift, rsh, dd;
   char reversal[MAXd1];
   struct complex ov;

   for (i = 0; i < d1; ++i)  reversal[i] = 0;
   for (i = 0; i < d1; ++i)
      if (!reversal[i] && ((j=bitmap1[i]) != i)) {
         reversal[j] = 1;
         for (k = 0; k < d2; k++) {
            ov.real = X[i][k].real;  ov.imag = X[i][k].imag;
            X[i][k].real = X[j][k].real;
            X[i][k].imag = X[j][k].imag;
            X[j][k].real = ov.real;
            X[j][k].imag = ov.imag;
         }
      }
   
   n = d1 / 2;
   for (k = 0; k < two_r; k++) {
      dd = 1; m0 = d1 / 2;
      mk = bitmap2[k] * n;
      for (s = m-r+1; s > 0; --s) {
         for (i = 0 ; i < m0; ++i) {
            l = 2 * dd *i;  ml = 0;
            for (ll = 0; ll < dd; ++ll) {
               j = l + dd;
               if (ml > 0) {   /* shift right */
                  for (shift = 0; shift < ml; ++shift) {
                     mm = mk + n - 1;
                     ov.real = X[j][mm].real;
                     ov.imag = X[j][mm].imag;
                     for (rsh = mm; rsh > mk; --rsh) {
                        X[j][rsh].real = X[j][rsh-1].real;
                        X[j][rsh].imag = X[j][rsh-1].imag;
                     }
                     X[j][mk].real = -ov.real;
                     X[j][mk].imag = -ov.imag;
                  }
               }
               /* perform butterfly */
               shift = mk + n;
               for (rsh = mk; rsh < shift; ++rsh) {
                  ov.real = X[l][rsh].real + X[j][rsh].real;
                  ov.imag = X[l][rsh].imag + X[j][rsh].imag;
                  X[j][rsh].real = X[l][rsh].real - X[j][rsh].real;
                  X[j][rsh].imag = X[l][rsh].imag - X[j][rsh].imag;
                  X[l][rsh].real = ov.real;
                  X[l][rsh].imag = ov.imag;
               }
               ++l;  ml += m0;
            }
         }
         dd *= 2;  m0 /= 2;
      }
   }
}

DITIFPT(X)
struct complex X[][MAXd2];
{
   int i, j, k, l, s, n, m0, ll, mk, ml, mm, shift, rsh, dd;
   double id1;
   struct complex ov;

   n = d1 / 2;
   for (k = 0; k < two_r; k++) {
      dd = 1; m0 = d1 / 2;
      mk = bitmap2[k] * n;
      for (s = m-r+1; s > 0; --s) {
         for (i = 0 ; i < m0; ++i) {
            l = 2 * dd *i; ml = 0;
            for (ll = 0; ll < dd; ++ll) {
               j = l + dd;
               if (ml > 0) {   /* shift left */
                  for (shift = 0; shift < ml; ++shift) {
                     mm = mk + n - 1;
                     ov.real = X[j][mk].real;
                     ov.imag = X[j][mk].imag;
                     for (rsh = mk; rsh < mm; ++rsh) {
                        X[j][rsh].real = X[j][rsh+1].real;
                        X[j][rsh].imag = X[j][rsh+1].imag;
                     }
                     X[j][rsh].real = -ov.real;
                     X[j][rsh].imag = -ov.imag;
                  }
               }
               /* perform butterfly */
               shift = mk + n;
               for (rsh = mk; rsh < shift; ++rsh) {
                  ov.real = X[l][rsh].real + X[j][rsh].real;
                  ov.imag = X[l][rsh].imag + X[j][rsh].imag;
                  X[j][rsh].real = X[l][rsh].real - X[j][rsh].real;
                  X[j][rsh].imag = X[l][rsh].imag - X[j][rsh].imag;
                  X[l][rsh].real = ov.real;
                  X[l][rsh].imag = ov.imag;
               }
               ++l;  ml += m0;
            }
         }
         dd *= 2;  m0 /= 2;
      }
   }
   /* multiply by 1/d1 */
   id1 = d1;  id1 = 1.0 / id1;
   for (i = 0; i < d1; ++i)
      for (j = 0; j < d2; ++j) {
         X[i][j].real = id1 * X[i][j].real;
         X[i][j].imag = id1 * X[i][j].imag;
      }

}


DIT_FFT(X, d, rr)
struct complex X[MAXd2];
int d, rr;
{
   int v, k, i, j, ip, i0;
   int le, le1, le2;
   double f;
   struct complex alpha, alphak, T;

   for (v = 0; v < rr; v++) {
      le = power(2, v);
      le1 = d / le;
      le2 = le1 / 2;
      f = pi / le;
      alphak.real = 1.0;
      alphak.imag = 0.0;
      alpha.real = cos(f);
      alpha.imag = sin(f);

      for (k = 0; k < le; k++) {
         i0 = le1 * k;
         for (j = 0; j < le2; j++) {
            i = i0 + j;
            ip = i + le2;
            T.real = alphak.real * X[ip].real - alphak.imag * X[ip].imag;
            T.imag = alphak.real * X[ip].imag + alphak.imag * X[ip].real;
            X[ip].real = X[i].real - T.real;
            X[ip].imag = X[i].imag - T.imag;
            X[i].real = X[i].real + T.real;
            X[i].imag = X[i].imag + T.imag;
         }
         f = alphak.real * alpha.real - 
                       alphak.imag * alpha.imag;
         alphak.imag = alphak.real * alpha.imag +
                       alphak.imag * alpha.real;
         alphak.real = f;
      }
   }
}

DIF_FFT(X, d, rr)
struct complex X[MAXd2];
int d, rr;
{
   int v, k, i, j, ip, i0;
   int le, le1, le2;
   double f;
   struct complex alpha, alphak, T;

   for (v = rr; v > 0; --v) {
      le = power(2, v-1);
      le1 = d / le;
      le2 = le1 / 2;
      f = - pi / le;
      alphak.real = 1.0;
      alphak.imag = 0.0;
      alpha.real = cos(f);
      alpha.imag = sin(f);

      for (k = 0; k < le; k++) {
         i0 = le1 * k;
         for (j = 0; j < le2; j++) {
            i = i0 + j;
            ip = i + le2;
            T.real = X[i].real - X[ip].real;
            T.imag = X[i].imag - X[ip].imag;
            X[i].real = X[i].real + X[ip].real;
            X[i].imag = X[i].imag + X[ip].imag;
            X[ip].real = alphak.real * T.real - alphak.imag * T.imag;
            X[ip].imag = alphak.real * T.imag + alphak.imag * T.real;
         }
         f = alphak.real * alpha.real - alphak.imag * alpha.imag;
         alphak.imag = alphak.real * alpha.imag + alphak.imag * alpha.real;
         alphak.real = f;
      }
   }
   /* multiply by 1/2**r */
   f = power(2, rr);  f = 1.0 / f;
   for (i = 0; i < d; ++i) {
      X[i].real = f * X[i].real;
      X[i].imag = f * X[i].imag;
   } 
}

betaconv(X, sign)
struct complex X[][MAXd2];
int sign;
{
   int n, t1, k, mk, nn;
   double bexp, tmp;
   struct complex beta, betak, T;

   n = d1/2;
   for (k = 0; k < two_r; k++) {
      bexp = pi * (2.0*k+two_r) / d2 * sign;
      betak.real = cos(bexp);
      betak.imag = sin(bexp);
      beta.real = 1.0;  beta.imag = 0.0;
      mk = bitmap2[k] * n;
      for (nn = 0; nn < n; nn++) {
         for (t1 = 0; t1 < d1; ++t1) {
            T.real = X[t1][mk].real;
            T.imag = X[t1][mk].imag;
            X[t1][mk].real = T.real * beta.real - T.imag * beta.imag;
            X[t1][mk].imag = T.real * beta.imag + T.imag * beta.real;
         }
         ++mk;
         tmp = beta.real * betak.real - beta.imag * betak.imag;
         beta.imag = beta.real * betak.imag + beta.imag * betak.real;
         beta.real = tmp;
      }
   }
}

power(x, n)
int x, n;
{
   int i,p;

   p = 1;
   for (i = 1; i <= n; i++)
      p *= x;
   return(p);
}

log2(l2)
int l2;
{
   int aa, i;

   for (aa=1,i=0; aa < l2; i++)
      aa *= 2;
   return(i);
}

readinp()
{
   int i, j;

   initinp();
   d1 = getnum();
   d2 = getnum();
   for (i = 0; i < d1; i++)
      for (j = 0; j < d2; j++) {
         A[i][j].real = getnum();
         A[i][j].imag = 0.0;
      }
   for (i = 0; i < d1; i++)
      for (j = 0; j < d2; j++) {
         B[i][j].real = getnum();
         B[i][j].imag = 0.0;
      }
}

flist()
{
   int i, j;

   fprintf(lfp,"--- Array A[][] ---");
   for (i = 0; i < d1; i++) {
      fprintf(lfp,"\n");
      for (j = 0; j < d2; j++) {
         fprintf(lfp,"%5.1f,%5.1f; ", A[i][j].real,A[i][j].imag);
      }
   }
   fprintf(lfp,"\n\n--- Array C[][] ---");
   for (i = 0; i < d1; i++) {
      fprintf(lfp,"\n");
      for (j = 0; j < d2; j++) {
         fprintf(lfp,"%5.1f,%5.1f; ", C_Ary[i][j].real,C_Ary[i][j].imag);
      }
   }
   fprintf(lfp,"\n");
}

