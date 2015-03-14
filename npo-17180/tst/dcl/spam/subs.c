/**********************************************************************
   %M% %I% - miscellaneous routines use by spam.  These routines are
   not spam-specific; they may be called usefully by any program.
**********************************************************************/
#include <ctype.h>
#include <stdio.h>
/**********************************************************************/
/*                  string conversion functions                       */
/**********************************************************************/
upper_case(buf)		/* convert string to upper case */
char *buf;
{
	int i;
	for (i=0;i<strlen(buf);i++) 
		if (islower(buf[i])) buf[i]=toupper(buf[i]);
}

lower_case(buf)		/* convert string to lower case */
char *buf;
{
	int i;
	for (i=0;i<strlen(buf);i++) 
		if (isupper(buf[i])) buf[i]=tolower(buf[i]);
}

/***********************************************************************/
/*                       misc math functions                           */
/***********************************************************************/
min(i,j)
int i,j;
{
	if (i<j) return(i); return(j);
}
max(i,j)
int i,j;
{
	if (i>j) return(i); return(j);
}
resample(array1,n1,a1,b1,array2,n2,a2,b2)	/* resample array1 into */
unsigned char *array1,*array2;			/* array2.  a and b are */
int n1,n2;					/* real endpoints of    */
float a1,b1,a2,b2;				/* intervals.           */
{
    float e,m,q,const;
    register long int result;
    register long int f;
    register int i,j;
    register unsigned char *in,*out;
    m=(a2-a1)*(n1-1)/(b1-a1);
    q=(b2-a2)*(n1-1)/(b1-a1);
    const = 1.0/(n2-1)*q;
    out = array2;
    for (i=0;i<n2;i++) {
	e = i*const+m;
	f = (e - (int)e)*65536;
        j = (int)e;
	if (j<-1 || (j==-1 && f!=1.0) || j>=n1 || (j+1>=n1 && f!=0.0))
	    *out++ = 0;
	else {
	    in = array1+j;
	    result=0;
	    if (j>=0) result += (65536-f) * *in;
	    if (j+1<n1) result += f * *++in;
	    result+=32768;
	    *out++ = result>>16;
	}
    }
}

/******************************************************************************/
/*                         pattern matching functions                         */
/******************************************************************************/
substring(buf,command)
char *buf,*command;	/* return true if first is substring of second, */
{			/* ignoring leading spaces of first.            */
	int i=0,j=0;
	while (isspace(buf[i])) i++;
	while (isspace(buf[i])==0 && i<strlen(buf) && j<=strlen(command)) {
	    if ((isupper(buf[i])?tolower(buf[i]):buf[i])!=
	        (isupper(command[j])?tolower(command[j]):command[j]))
		return(0);
            i++;
	    j++;
        }
	return(j);
}

/******************************************************************************/
/*                          Wildcard Pattern Matcher                          */
/*                                                                            */
/*                                by D. Sirag                                 */
/*                                                                            */
/*     This is a wildcard pattern matching subroutine which takes two         */
/* strings 'a' and 'b' and determines if the two strings are equivalent. The  */
/* two strings do not have to be identical because string 'a' may contain     */
/* 'wildcard' characters which match with one or more characters in 'b'. The  */
/* wildcard character is indicated by 'x'.                                    */
/******************************************************************************/

wildmatch(x,a,b)
char x;           /*  Wildcard Character                                   */
char a[];         /*  String containing wildcards                          */
char b[];         /*  Target String                                        */
{ int i=0;        /*  Current Character Index                              */
  int la, lb;     /*  Length of 'a' and 'b'                                */
  char *sb;       /*  First character in 'b' matched by the wildcard       */
  char *j;        /*  First character in 'b' not matched by the wildcard   */
  
  /*  Strings can not match if a>b (Wildcards match at least 1 character)  */
  if ((la = strlen(a)) > (lb = strlen(b))) return(0);

  /*  Remove all initial matching characters                               */
  while (a[i] == b[i]) if ((i++) >= la) return(1);

  /*  If first non-matching character is not a wildcard then strings 
        can't match.                                                       */
  if (a[i++] != x) return(0);

  /*  Set 'sb' to mark start of region that can be matched by the wildcard */
  sb = &b[i];
  /*  Set 'j' to mark end of region that can be matched by the wildcard    */
  j = (&b[lb - (la - i)]);

  /*  Try all the possible matches of the wildcard till one of them works  */
  while(j >= sb)
    if (wildmatch(x,&a[i],j--)) return(1);
  /*  If none of the possibilities worked, then no match                   */
  return(0); }


/******************************************************************************/
/*               spline interpolation functions                               */
/* The spline_fit function was coded from an algorithm in	              */
/*                                                               	      */
/*      Numerical Analysis                                           	      */
/*      by Richard L. Burden, J. Douglas Faires, Albert C. Reynolds  	      */
/*      copyright 1978, 1981 by Prindle, Weber and Schmidt           	      */
/*      Boston, Massachusetts (2d ed.)                               	      */
/******************************************************************************/
spline_fit(x,n,a,b,c,d)				/* calculate coefficients     */
float *x,*a,*b,*c,*d;
int n;
{
    int i,j;
    float h[256],alpha[256],l[256],u[256],z[256];
    for (i=0;i<=n-1;i++) h[i]=x[i+1]-x[i];                      /* step 1  */
    for (i=1;i<=n-1;i++)                                        /* step 2  */
        alpha[i]=(3*(a[i+1]*h[i-1]-a[i]*(x[i+1]-x[i-1])+a[i-1]*h[i]))/
            (h[i-1]*h[i]);
    l[0]=1.0;                                                   /* step 3  */
    u[0]=0.0;
    z[0]=0.0;
    for (i=1;i<=n-1;i++) {                                      /* step 4  */
        l[i]=2*(x[i+1]-x[i-1])-h[i-1]*u[i-1];
        u[i]=h[i]/l[i];
        z[i]=(alpha[i]-h[i-1]*z[i-1])/l[i];
    }
    l[n]=1.0;                                                   /* step 5  */
    z[n]=0.0;
    c[n]=z[n];
    for (j=n-1;j>=0;j--) {                                      /* step 6  */
        c[j]=z[j]-u[j]*c[j+1];
        b[j]=(a[j+1]-a[j])/h[j]-h[j]*(c[j+1]+2*c[j])/3.0;
        d[j]=(c[j+1]-c[j])/(3*h[j]);
    }
}

float value_at(xval,x,n,a,b,c,d)
float xval,*x,*a,*b,*c,*d;
int n;
{
    int i;
    float dx,ddx;
    dx=xval-x[0];
    if (dx<=0) return(a[0]+dx*(b[0]+dx*(c[0]+dx*d[0])));
    else for (i=0;i<n;i++) {
        ddx=xval-x[i+1];
        if (ddx<0) return(a[i]+dx*(b[i]+dx*(c[i]+dx*d[i])));
        dx=ddx;
    }
    return(a[i]+dx*(b[i]+dx*(c[i]+dx*d[i])));
}
