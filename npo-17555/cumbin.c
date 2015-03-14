#include   <math.h>
#include   <stdio.h>

/* C1: This program calculates the reliability of a K-out-of-N system with
       common component reliability p.                                      */

main()
{
                    /* C2: Following are the variables used in this program */
 double   b,          /* = the binomial parameter used by the calculations  */
          a,          /* = b / (1 - b)                                      */
          C,          /* = constant multiple for the binomial terms & sum   */
          p,          /* = input binomial parameter (probability)           */
          s,          /* = sum of binomial terms                            */
          t,          /* = individual binomial term                         */
          underflow;  /* = near underflow limit to halt the calculations at */

 long  j,   /* = indexing variable for loops                                */
       k,   /* = input value of the lower bound of the binomial sum         */
       i,   /* = lower bound of the sum used in the algorithm               */
       n,   /* = upper bound of the binomial summation                      */
       m;   /* = number of times the sum has been divided by C              */

 char  yorn;  /* = 'y' to use  and reuse this program                       */

 underflow = exp(1000. * log(0.5));

 printf("This program calculates the reliability of a k-out-of-n system\n");
 printf("with component reliability p.\n");

 yorn = 'y';
 while (yorn == 'y')
   {
     printf("Enter N = ");        /* C3: Get input values for the cumulative */
     scanf("%ld", &n);             /*     binomial algorithm.                 */
     printf("Enter K = ");
     scanf("%ld", &k);
     printf("Enter p = ");
     scanf("%lf", &p);

     if (p >= .5)                 /* C4: Use the input values in the         */
       {                          /*     algorithm if p >= .5, or            */
         i = k;
         b = p;
       }
     else                         /*     transpose the input values to do    */
       {                          /*     inverse calculation if p < .5.      */
         i = n - k + 1;
         b = 1 - p;
       }

     a = (1 - b)/b;               /* C5: Perform the initial calculations    */
     C = exp(1000 * log(b));      /*     for a, C, m, s, and t.              */
     m = n / 1000;
     t = exp((n % 1000) * log(b));
     s = t;

     for (j = n - 1.0; j >= i; j--) /* C6: Calculate & sum the binomial terms*/
       {                              
         if (t < underflow * (n - j) / (a * (j + 1)) /* C7: Check if term is */
             && m < 1)                               /*     so small that    */
	   break;                                    /*     sum is 1.        */

         if (s > .5 / C - t)                     /* C8: Adjust sum and terms */
           {                                     /*     if nearing overflow. */
             m = m - 1;
             s = s * C;
             t = t * C;
	   }

         t = t * a * (j + 1) / (n - j);
         s = s + t;
       }

     for (j = -1; j >= m; j--)    /* C9: Make sure the final answer has not  */
       s = s / C;                 /*     had C divided out or multiplied in  */
     for (j = 1; j <= m; j++)     /*     too many times.                     */
       s = s * C;

     if (p < .5)                  /* C10: Invert the answer if p < 0.5.      */
       s = 1 - s;
                                  /* C11: Print Results.                     */
     printf("\n P( K >= %ld; N = %ld; p = %1.12lf) = %1.12lf\n\n", k, n, p, s);

     printf("Would you like to run another case (y or n)?");
     scanf("%s", &yorn);
  }
}
