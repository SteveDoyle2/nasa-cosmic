/* An FPT Algorithm
	Computing 2-D Cyclic Convolution
*/

#define MAXd1  32
#define MAXd2  32

double pi;		/* pi = 3.14159265 */
int d1;			/* d1 = 2**(m-r+1) */
int d2;			/* d2 = 2**m */
int m, r;
int two_r;		/* 2**r */

struct complex {
	double real;	/* real part */
	double imag;	/* imaginary part */
};
struct complex A[MAXd1][MAXd2],		/* array A,B,C */
	       B[MAXd1][MAXd2],
	       C_Ary[MAXd1][MAXd2];

int bitmap1[MAXd1],	/* bit reversal map */
    bitmap2[MAXd2];
    bitmap3[MAXd1];