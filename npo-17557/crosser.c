#include   <math.h>
#include   <stdio.h>

/*  This program calculates the component reliability p required to yield
    the same K-out-of-N system reliability p.                                */

          /* C1: The following variables and functions are used in newtonp   */
long   k,   /* = minimum number of components required for system operation  */
       n;   /* = total number of components in the system                    */

double t;   /* = individual binomial term                                    */

main()
{
 extern long    k, n;
 extern double  t;

 double  cumbin(),   /* = function to calculate the cumulative binomial      */
	 absol();    /* = function to calculate absolute value               */

 double  p,      /* = binomial parameter (component reliability) estimate    */
         pnew,   /* = next estimate of binomial parameter                    */
         s,      /* = system reliability based on p, s is the estimate for v */
         deriv,  /* = derivative of the reliability curve at p               */
         eps;    /* = input allowable error in the answer or the system rel. */
 
 long    count;  /* = number of iterations to calculate p                    */
 
 char  yorn;     /* = y to continue the program                              */

 printf("This program calculates the common component reliability P\n");
 printf("required to yield the same system reliability P of a k-out-of-n\n");
 printf("system within a given error Epsilon.\n");

 yorn = 'y';
 while (yorn == 'y')
   {
     printf("Enter N = ");           /* C2: Get input values to use within   */
     scanf("%ld", &n);                /*     the program.                     */
     printf("Enter K = ");
     scanf("%ld", &k);
     printf("Enter Epsilon = ");
     scanf("%lf", &eps);

     count = 1;
     pnew = (k - 1.0) / (n - 1.0);   /* C3: Start with inflection point.     */
     
     do                              /* C4: Iterate by Newton's method using */
       {                             /*     pnew and cumbin() to get s and a */
         p = pnew;                   /*     new value of pnew.  Repeat until */
         s = cumbin(p);              /*     the error is acceptable.         */
         if (p >= 0.5)
           deriv = k * t / p;
         else
           deriv = (n - k + 1.0) * t / (1.0 - p);
         pnew = p - (s - p) / deriv;               /* C5: The Newton step.   */
         count++;
       }
     while (absol(p - pnew) > eps || absol(s - p) > eps);
                                     /* C6: Print the results.               */
     printf("\nThe component and system reliability is %16.14lf\n", pnew);
     printf("%ld iterations were required for the calculation.\n\n", count);
     printf("Would you like to run another case (y or n)?");
     scanf("%s", &yorn);
   }
}


double cumbin(p)   /* CU1: cumbin calculates the reliability of a k-out-of-n */
  double p;        /*      system with common component reliability p.       */
{
 extern long   k, n;
 extern double t;

                   /* CU1: Following are the variables used in this program  */
 double   b,          /* = the binomial parameter used by the calculations   */
          a,          /* = b / (1 - b)                                       */
          C,          /* = constant multiple for the binomial terms & sum    */
          s,          /* = sum of binomial terms                             */
          underflow;  /* = near underflow limit to halt the calculations at  */

 long  j,   /* = indexing variable for loops                                 */
       i,   /* = lower bound of the sum used in the algorithm                */
       m;   /* = number of times the sum has been divided by C               */

 underflow = exp(1000. * log(0.5));

 if (p >= .5)                 /* CU2: Use the input values in the            */
   {                          /*      algorithm if p >= .5, or               */
     i = k;
     b = p;
   }
 else                         /*      transpose the input values to do       */
   {                          /*      inverse calculation if p < .5.         */
     i = n - k + 1;
     b = 1 - p;
   }
 
 a = (1 - b)/b;               /* CU3: Perform the initial calculations for   */
 C = exp(1000 * log(b));      /*      a, C, m, s, and t.                     */
 m = n / 1000;
 t = exp((n % 1000) * log(b));
 s = t;

 for (j = n - 1; j >= i; j--) /* CU4: Calculate & sum the binomial terms.  */
   {                              
     if (t < underflow * (n - j) / (a * (j + 1)) /* CU5: Check if term is so */
	 && m < 1)                               /*      small that sum is 1.*/
       break;                                  
     if (s > .5 / C - t)                     /* CU6: Adjust sum and terms if */
       {                                     /*      nearing overflow.       */
         m = m - 1;
         s = s * C;
         t = t * C;
       }
     t = t * a * (j + 1) / (n - j);
     s = s + t;
   }
 for (j = -1; j >= m; j--)    /* CU7: Make sure the final answer has not had */
   {                          /*      C divided out or multiplied in too     */
     s = s / C;               /*      many times.                            */
     t = t / C;
   }
 for (j = 1; j <= m; j++)
   {
     s = s * C;
     t = t * C;
   }

 if (p < .5)                  /* CU8: Invert the answer if p < 0.5.          */
   s = 1 - s;
 return(s);
}


double absol(value)           /* A1: Function to return the absolute value   */
  double value;               /*     of the number passed to it.             */
  {
    if (value < 0.0)
      return(-1.0 * value);
    else
      return(value);
  }
