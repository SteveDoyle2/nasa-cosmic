/*   CLIPS Version 4.30   4/25/89 */
 
   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                     MATH MODULE                     */
   /*******************************************************/

#include <stdio.h>

#include "setup.h"
#include "clips.h"

#if EX_MATH

#include <math.h>
#ifndef PI
#define PI   3.14159265358979323846
#endif
#ifndef PID2
#define PID2 1.57079632679489661923	/* PI divided by 2 */
#endif

#define dtrunc(x) (((x) < 0) ? ceil(x) : floor(x))
#endif

/*****************************/
/* DEFINITIONS               */
/*****************************/

mathfctns()
  {
#if   EX_MATH
   float my_cos(),   my_sin(),  my_tan(),   my_sec(),   my_csc(),   my_cot();
   float my_acos(),  my_asin(), my_atan(),  my_asec(),  my_acsc(),  my_acot();
   float my_cosh(),  my_sinh(), my_tanh(),  my_sech(),  my_csch(),  my_coth();
   float my_acosh(), my_asinh(), my_atanh(), my_asech(), my_acsch(), my_acoth();

   float my_min(), my_max(), mod(); 
   float my_exp(), my_log(), my_log10(), my_sqrt();
   float my_trunc(), my_pi(), my_deg_rad(), my_rad_deg();
   float my_deg_grad(), my_grad_deg(), my_abs(), my_pow();
   float oddp(), evenp(), integerp();

   define_function("cos",      'f', (int (*)()) my_cos,      "my_cos");
   define_function("sin",      'f', (int (*)()) my_sin,      "my_sin");
   define_function("tan",      'f', (int (*)()) my_tan,      "my_tan");
   define_function("sec",      'f', (int (*)()) my_sec,      "my_sec");
   define_function("csc",      'f', (int (*)()) my_csc,      "my_csc");
   define_function("cot",      'f', (int (*)()) my_cot,      "my_cot");
   define_function("acos",     'f', (int (*)()) my_acos,     "my_acos");
   define_function("asin",     'f', (int (*)()) my_asin,     "my_asin");
   define_function("atan",     'f', (int (*)()) my_atan,     "my_atan");
   define_function("asec",     'f', (int (*)()) my_asec,     "my_asec");
   define_function("acsc",     'f', (int (*)()) my_acsc,     "my_acsc");
   define_function("acot",     'f', (int (*)()) my_acot,     "my_acot");
   define_function("cosh",     'f', (int (*)()) my_cosh,     "my_cosh");
   define_function("sinh",     'f', (int (*)()) my_sinh,     "my_sinh");
   define_function("tanh",     'f', (int (*)()) my_tanh,     "my_tanh");
   define_function("sech",     'f', (int (*)()) my_sech,     "my_sech");
   define_function("csch",     'f', (int (*)()) my_csch,     "my_csch");
   define_function("coth",     'f', (int (*)()) my_coth,     "my_coth");
   define_function("acosh",    'f', (int (*)()) my_acosh,    "my_acosh");
   define_function("asinh",    'f', (int (*)()) my_asinh,    "my_asinh");
   define_function("atanh",    'f', (int (*)()) my_atanh,    "my_atanh");
   define_function("asech",    'f', (int (*)()) my_asech,    "my_asech");
   define_function("acsch",    'f', (int (*)()) my_acsch,    "my_acsch");
   define_function("acoth",    'f', (int (*)()) my_acoth,    "my_acoth");

   define_function("mod",      'f', (int (*)()) mod,         "mod");
   define_function("min",      'f', (int (*)()) my_min,      "my_min");
   define_function("max",      'f', (int (*)()) my_max,      "my_max");
   define_function("exp",      'f', (int (*)()) my_exp,      "my_exp");
   define_function("log",      'f', (int (*)()) my_log,      "my_log");
   define_function("log10",    'f', (int (*)()) my_log10,    "my_log10");
   define_function("sqrt",     'f', (int (*)()) my_sqrt,     "my_sqrt");
   define_function("trunc",    'f', (int (*)()) my_trunc,    "my_trunc");
   define_function("pi",       'f', (int (*)()) my_pi,       "my_pi");
   define_function("deg-rad",  'f', (int (*)()) my_deg_rad,  "my_deg_rad");
   define_function("rad-deg",  'f', (int (*)()) my_rad_deg,  "my_rad_deg");
   define_function("deg-grad", 'f', (int (*)()) my_deg_grad, "my_deg_grad");
   define_function("grad-deg", 'f', (int (*)()) my_grad_deg, "my_grad_deg");
   define_function("abs",      'f', (int (*)()) my_abs,      "my_abs");
   define_function("**",       'f', (int (*)()) my_pow,      "my_pow");   
   define_function("oddp",     'f', (int (*)()) oddp,        "oddp");
   define_function("evenp",    'f', (int (*)()) evenp,       "evenp");
   define_function("integerp", 'f', (int (*)()) integerp,    "integerp");
#endif   
  }

#if   EX_MATH

float clcos(),clsin(),cltan(),clacos(),clasin(),clatan(),
      clcosh(),clsinh(),cltanh(),clacosh(),clasinh(),clatanh(),
      clasech(),clacsch(),clacoth(),
      clexp(),cllog(),cllog10(),clsqrt();


/*******************************************/
/*   CLIPS TRIGONOMETRIC FUNCTIONS         */
/*******************************************/

int single_number_check(fnstr,num_ptr)
  char *fnstr;
  float *num_ptr;
  {
   VALUE valstruct;

   if (arg_num_check(fnstr,EXACTLY,1) == -1) return(FALSE);
   if (arg_type_check(fnstr,1,NUMBER,&valstruct) == FALSE) return(FALSE);
   
   *num_ptr = get_valfloat(valstruct);
   return(TRUE);
  }

float my_cos()
  {
    float num,result;

    extern float clcos();
                         
    if (single_number_check("cos",&num) == FALSE) return(0.0);
    result = clcos(num);
    return(result);
  }

float my_sin()                                 
  {
    float num,result;

    extern float clsin();

    if (single_number_check("sin",&num) == FALSE) return(0.0);
    result = clsin(num);
    return(result);
  }

int check_multiple(cmp_num) 
  float cmp_num; 
  {
   float mult_num, look_num;

   mult_num = (float) dtrunc( (double) (cmp_num / PI) );
   look_num = fabs(fabs(cmp_num / PI) - fabs(mult_num));
   if (look_num < 0.000001) 
     { 
      cl_print("werror","Singularity at asymptote in trigonometric function\n");
      set_execution_error(TRUE);
      return TRUE;
     }
   else 
     { return FALSE; }
  }  

float my_tan()
  {
    float num,result;

    extern float cltan();

    if (single_number_check("tan",&num) == FALSE) return (0.0);
    if (check_multiple(num - PID2) == TRUE) return(0.0);
    result = cltan(num);
    return(result);
  }

int test_zero_range(nval)
  float nval; 
  {
    if ((nval >= -0.000001) && (nval <= 0.000001)) return TRUE;
    else return FALSE; 
  }


float my_sec()
  {
    float num,result;

    extern float clcos();

    if (single_number_check("sec",&num) == FALSE) return(0.0);
    if (check_multiple(num - PID2) == TRUE) return(0.0);
    result = clcos(num);
    result = 1.0 / result;
    return(result); 
  }

float my_csc()
  {
    float num,result;

    extern float clsin();

    if (single_number_check("csc",&num) == FALSE) return(0.0);
    if (check_multiple(num) == TRUE) return(0.0);
    result = clsin(num);
    result = 1.0 / result;
    return(result);
  }

float my_cot()
  {
    float num,result;

    extern float cltan();

    if (single_number_check("cot",&num) == FALSE) return(0.0);
    if (check_multiple(num) == TRUE) return(0.0);
    result = clcos(num)/clsin(num);
    return(result); 
  }

float my_acos()
  {
    float num,result;

    extern float clacos();

    if (single_number_check("acos",&num) == FALSE) return(0.0);
    if ((num > 1.000000000)||(num < -1.000000000)) {
	 cl_print("werror","Argument overflow for acos function.\n");
         set_execution_error(TRUE);
	 return(0.0); }
    result = clacos(num);
    return(result);       
  }

float my_asin()
  {
    float num,result;

    extern float clasin();

    if (single_number_check("asin",&num) == FALSE) return(0.0);
    if ((num > 1.000000000)||(num < -1.000000000)) {
	 cl_print("werror","Argument overflow for asin function.\n");
         set_execution_error(TRUE);
	 return(0.0); }
    result = clasin(num);
    return(result);
  }

float my_atan()
  {
    float num,result;

    extern float clatan();

    if (single_number_check("atan",&num) == FALSE) return(0.0);
    result = clatan(num);
    return(result);
  }

float my_asec()
  {
    float num,result;

    extern float clacos();

    if (single_number_check("asec",&num) == FALSE) return(0.0);
    if ((num < 1.000000000)&&(num > -1.000000000)) {
	 cl_print("werror","Argument overflow for asec function.\n");
         set_execution_error(TRUE);
	 return(0.0); }
    num = 1.0 / num;
    result = clacos(num);
    return(result);
  }

float my_acsc()
  {
    float num,result;

    extern float clasin();

    if (single_number_check("acsc",&num) == FALSE) return(0.0);
    if ((num < 1.000000000)&&(num > -1.000000000)) {
	 cl_print("werror","Argument overflow for acsc function.\n");
         set_execution_error(TRUE);
	 return(0.0); }
    num = 1.0 / num;
    result = clasin(num);
    return(result);
  }


float my_acot()
  {
    float num,result;

    extern float clatan();

    if (single_number_check("acot",&num) == FALSE) return(0.0);
    if (test_zero_range(num) != TRUE) 
        num = 1.0 / num;
    else return(PID2);
    result = clatan(num);
    return(result);
  }


float my_cosh()
    {
    float num,result;

    extern float clcosh();

    if (single_number_check("cosh",&num) == FALSE) return(0.0);
    result = clcosh(num);
    return(result);
    }


float my_sinh()
    {
    float num,result;

    extern float clsinh();

    if (single_number_check("sinh",&num) == FALSE) return(0.0);
    result = clsinh(num);
    return(result);
    }


float my_tanh()
    {
    float num,result;

    extern float cltanh();

    if (single_number_check("tanh",&num) == FALSE) return(0.0);
    result = cltanh(num);
    return(result);
    }


float my_sech()
    {
    float num,result;

    extern float clcosh();

    if (single_number_check("sech",&num) == FALSE) return(0.0);
    result = clcosh(num);
    result = 1.0 / result;
    return(result); 
    }


float my_csch()
    {
    float num,result;

    extern float clsinh();

    if (single_number_check("csch",&num) == FALSE) return(0.0);
    if (test_zero_range(num) == TRUE) {
	 cl_print("werror","Argument overflow for csch function.\n");
         set_execution_error(TRUE);
	 return(0.0); }
    result = clsinh(num);
    result = 1.0 / result;
    return(result); 
    }


float my_coth()
    {
    float num,result;

    extern float cltanh();

    if (single_number_check("coth",&num) == FALSE) return(0.0);
    if (test_zero_range(num) == TRUE) {
	set_execution_error(TRUE);
        cl_print("werror","Argument overflow for coth function.\n");
        return(0.0); }
    result = cltanh(num);
    result = 1.0 / result;
    return(result); 
    }


float my_acosh()
    {
    float num,result;

    extern float clacosh();

    if (single_number_check("acosh",&num) == FALSE) return(0.0);
    if (num < 1.000000000) {
	    set_execution_error(TRUE);
         cl_print("werror","Argument overflow for acosh function.\n");
         return(0.0); }
    result = clacosh(num);
    return(result);
    }


float my_asinh()
    {
    float num,result;

    extern float clasinh();

    if (single_number_check("asinh",&num) == FALSE) return(0.0);
    result = clasinh(num);
    return(result);
    }


float my_atanh()
    {
    float num,result;

    extern float clatanh();

    if (single_number_check("atanh",&num) == FALSE) return(0.0);
    if ((num > 1.000000000)||(num < -1.000000000)) {
	    cl_print("werror","Argument overflow for atanh function.\n");
         set_execution_error(TRUE);
	    return(0.0); }
    result = clatanh(num);
    return(result);
    }


float my_asech()
    {
    float num,result;

    extern float clasech();

    if (single_number_check("asech",&num) == FALSE) return(0.0);
    if ((num > 1.000000000)||(num < 0.000000001)) {
	    cl_print("werror","Argument overflow for asech function.\n");
         set_execution_error(TRUE);
	    return(0.0); }
    result = clasech(num);
    return(result);
    }


float my_acsch()
    {
    float num,result;

    extern float clacsch();

    if (single_number_check("acsch",&num) == FALSE) return(0.0);
    if (test_zero_range(num) == TRUE) {
	    set_execution_error(TRUE);
         cl_print("werror","Argument overflow for acsch function.\n");
         return(0.0); }
    result = clacsch(num);
    return(result);
    }


float my_acoth()
    {
    float num,result;

    extern float clacoth();

    if (single_number_check("acoth",&num) == FALSE) return(0.0);
    if ((num < 1.000000001)&&(num > -1.000000001)) {
	    cl_print("werror","Argument overflow for acoth function.\n");
         set_execution_error(TRUE);
	    return(0.0); }
    result = clacoth(num);
    return(result);
    }


float my_exp()
    {
    float num,result;

    extern float clexp();

    if (single_number_check("exp",&num) == FALSE) return(0.0);
    result = clexp(num);
    return(result);
    }


float my_log()
    {
    float num,result;

    extern float cllog();

    if (single_number_check("log",&num) == FALSE) return(0.0);
    if (num < 0.000001) {
   	  set_execution_error(TRUE);
	  cl_print("werror","Argument overflow for log function.\n");
	  return(0.0); }
    result = cllog(num);
    return(result);
    }


float my_log10()
    {
    float num,result;

    extern float cllog10();

    if (single_number_check("log10",&num) == FALSE) return(0.0);
    if (num < 0.000001) {
   	  set_execution_error(TRUE);
	  cl_print("werror","Argument overflow for log10 function.\n");
	  return(0.0); }
    result = cllog10(num);
    return(result);
    }


float my_sqrt()
    {
    float num,result;

    extern float clsqrt();

    if (single_number_check("sqrt",&num) == FALSE) return(0.0);
    if (num < 0.00000) {
   	  set_execution_error(TRUE);
	  cl_print("werror","Argument overflow for sqrt function.\n");
	  return(0.0); }
    result = clsqrt(num);
    return(result);
    }


/****************************************/
/* min                                  */
/****************************************/
float my_min()
  {
   VALUE arg_value;
   float mvalue, avalue;
   int num_a, i;

   if ((num_a = arg_num_check("min",AT_LEAST,1)) == -1) return(0.0);

   if (arg_type_check("min",1,NUMBER,&arg_value) == FALSE) return(0.0);
   mvalue = get_valfloat(arg_value);
   
   for (i = 2 ; i <= num_a ; i++)
     {
      if (arg_type_check("min",i,NUMBER,&arg_value) == FALSE) return(0.0);
      avalue = get_valfloat(arg_value);
      if (avalue < mvalue)
       { mvalue = avalue; }
     }
   
   return(mvalue);
  } 

/****************************************/
/* max                                  */
/****************************************/
float my_max()
  {
   VALUE arg_value;
   float mvalue, avalue;
   int num_a, i;

   if ((num_a = arg_num_check("max",AT_LEAST,1)) == -1) return(0.0);
   
   if (arg_type_check("max",1,NUMBER,&arg_value) == FALSE) return(0.0);
   mvalue = get_valfloat(arg_value);
   
   for (i = 2 ; i <= num_a ; i++)
     {
      if (arg_type_check("min",i,NUMBER,&arg_value) == FALSE) return(0.0);
      avalue = get_valfloat(arg_value);
      if (avalue > mvalue)
       { mvalue = avalue; }
     }
   
   return(mvalue);
  }


/****************************************/
/* exponentiation                       */
/****************************************/
float my_pow()
  {
   VALUE value1, value2;
   
   if (arg_num_check("**",EXACTLY,2) == -1) return(0.0);

   if (arg_type_check("**",1,NUMBER,&value1) == FALSE) return(0.0);
   if (arg_type_check("**",2,NUMBER,&value2) == FALSE) return(0.0);

    if (((get_valfloat(value1) == 0.0) && 
        (get_valfloat(value2) <= 0.0)) ||
       ((get_valfloat(value1) < 0.0) &&
        (dtrunc((double) get_valfloat(value2)) != get_valfloat(value2))))
     {
      cl_print("werror","Domain error for ** function\n");
      set_execution_error(TRUE);
      return(0.0);
     }
     
   return ((float) pow((double) get_valfloat(value1), 
                       (double) get_valfloat(value2)));
  }

/*************************************/
/* mod                               */
/*************************************/
float mod()
  {
   VALUE item1, item2;
   double fnum1, fnum2;

   if (arg_num_check("mod",EXACTLY,2) == -1) return(0.0);
   
   if (arg_type_check("mod",1,NUMBER,&item1) == FALSE) return(0.0);  
   if (arg_type_check("mod",2,NUMBER,&item2) == FALSE) return(0.0);
   
   fnum1 = (double) item1.val.fvalue;
   fnum2 = (double) item2.val.fvalue;
   
   return ( (float) (fnum1 - (dtrunc(fnum1 / fnum2) * fnum2)));
  }

float my_trunc()
  {  
   float fnum;
   
   if (single_number_check("trunc",&fnum) == FALSE) return(0.0);
   return((float) dtrunc((double) fnum));
  } 

float my_pi() 
  {
   if (arg_num_check("pi",EXACTLY,0) == -1) return(PI);
   return(PI);
  }
   
                   
float my_deg_rad() {
        
	float num,result;

	if (single_number_check("deg-rad",&num) == FALSE) return(0.0);
	result = num * PI / 180.0;
	return(result);

    }

float my_rad_deg() {
        
	float num,result;

	if (single_number_check("rad-deg",&num) == FALSE) return(0.0);
	result = num * 180.0 / PI;
	return(result);

    }

float my_deg_grad() {
        
	float num,result;
      
	if (single_number_check("deg-grad",&num) == FALSE) return(0.0);
	result = num / 3.6;
	return(result);

    }

float my_grad_deg() {
        
	float num,result;

	if (single_number_check("grad-deg",&num) == FALSE) return(0.0);
	result = num * 3.6;       
	return(result);

    }

float my_abs()                                 
  {
    float num,result;

    extern float clsin();

    if (single_number_check("abs",&num) == FALSE) return(0.0);

    if (num < 0.0)
      { result = -num; }
    else
      { result = num; }
    
    return(result);
  }

/***************************************************************************/
/*                                                                         */
/*                 SYSTEM DEPENDENT FUNCTIONS                              */
/*                                                                         */
/***************************************************************************/
float clcos(num)
   float num; {

     double arg,result;
     float rval;

     arg = (double) num;
     result = cos(arg);
     rval = (float) result;
     return(rval);
   }

float clsin(num)
    float num; {

    double arg,result;
    float rval;

    arg = (double) num;
    result = sin(arg);
    rval = (float) result;
    return(rval);
    }


float cltan(num)
    float num; {

    double arg,result;
    float rval;

    arg = (double) num;
    result = tan(arg);
    rval = (float) result;
    return(rval);
    }


float clacos(num)
    float num; {

    double arg,result;
    float rval;

    arg = (double) num;

    result = acos(arg);
    rval = (float) result;
    return(rval);
    }


float clasin(num)
    float num; {

    double arg,result;
    float rval;

    arg = (double) num;
    result = asin(arg);
    rval = (float) result;
    return(rval);
    }


float clatan(num)
    float num; {

    double arg,result;
    float rval;

    arg = (double) num;
    result = atan(arg);
    rval = (float) result;
    return(rval);
    }


float clcosh(num)
    float num; {

    double arg,result;
    float rval;

    arg = (double) num;
    result = cosh(arg);
    rval = (float) result;
    return(rval);
    }


float clsinh(num)
    float num; {

    double arg,result;
    float rval;

    arg = (double) num;
    result = sinh(arg);
    rval = (float) result;
    return(rval);
    }


float cltanh(num)
    float num; {

    double arg,result;
    float rval;

    arg = (double) num;
    result = tanh(arg);
    rval = (float) result;
    return(rval);
    }



float clacosh(num)
    float num; {

    double arg,result;
    float rval;

    arg = (double) num;
    result = log(arg + sqrt(arg*arg-1.0));
    rval = (float) result;
    return(rval);
    }


float clasinh(num)
    float num; {

    double arg,result;
    float rval;

    arg = (double) num;
    result = log(arg + sqrt(arg*arg+1.0));
    rval = (float) result;
    return(rval);
    }


float clatanh(num)
    float num; {

    double arg,result;
    float rval;

    arg = (double) num;
    result = (0.5)*log((1.0 + arg)/(1.0 - arg));
    rval = (float) result;
    return(rval);
    }


float clasech(num)
    float num; {

    double arg,result;
    float rval;

    arg = (double) num;
    result = log(1.0/arg + sqrt(1.0/(arg*arg) - 1.0));
    rval = (float) result;
    return(rval);
    }


float clacsch(num)
    float num; {

    double arg,result;
    float rval;

    arg = (double) num;
    result = log(1.0/arg + sqrt(1.0/(arg*arg) + 1.0));
    rval = (float) result;
    return(rval);
    }


float clacoth(num)
    float num; {

    double arg,result;
    float rval;

    arg = (double) num;
    result = (0.5)*log((arg + 1.0)/(arg - 1.0));
    rval = (float) result;
    return(rval);
    }


float clexp(num)
    float num; {

    double arg,result;
    float rval;

    arg = (double) num;
    result = exp(arg);
    rval = (float) result;
    return(rval);
    }


float cllog(num)
    float num; {

    double arg,result;
    float rval;

    arg = (double) num;
    result = log(arg);
    rval = (float) result;
    return(rval);
    }


float cllog10(num)
    float num; {

    double arg,result;
    float rval;

    arg = (double) num;
    result = log10(arg);
    rval = (float) result;
    return(rval);
    }


float clsqrt(num)
    float num; {

    double arg,result;
    float rval;

    arg = (double) num;
    result = sqrt(arg);
    rval = (float) result;
    return(rval);
    }
    
/****************************************/
/* ODDP:                                */
/****************************************/
float oddp()
  {
   VALUE valstruct;
   float num, halfnum, truncnum;

   if (arg_num_check("oddp",EXACTLY,1) == -1) return(FALSE);

   runknown(1,&valstruct);

   if (get_valtype(valstruct) != NUMBER)
     {
      exp_type_error("oddp",1,"integer");
      set_execution_error(TRUE);
      return(CLIPS_FALSE);
     }
   
   num = get_valfloat(valstruct);

   truncnum = dtrunc((double) num);
   if (truncnum != num) 
     {
      exp_type_error("oddp",1,"integer");
      set_execution_error(TRUE);
      return (0.0);
     }
     
   halfnum = num / 2.0;
   truncnum = dtrunc((double) halfnum);
   if (truncnum == halfnum) return(0.0);
   
   return(1.0);
  }
  
/****************************************/
/* EVENP:                               */
/****************************************/
float evenp()
  {
   VALUE valstruct;
   float num, halfnum, truncnum;

   if (arg_num_check("evenp",EXACTLY,1) == -1) return(FALSE);

   runknown(1,&valstruct);

   if (get_valtype(valstruct) != NUMBER)
     {
      exp_type_error("evenp",1,"integer");
      set_execution_error(TRUE);
      return(CLIPS_FALSE);
     }
   
   num = get_valfloat(valstruct);

   truncnum = dtrunc((double) num);
   if (truncnum != num) 
     {
      exp_type_error("evenp",1,"integer");
      set_execution_error(TRUE);
      return (0.0);
     }
     
   halfnum = num / 2.0;
   truncnum = dtrunc((double) halfnum);
   if (truncnum != halfnum) return(0.0); 
   
   return(1.0);
  }
  
/****************************************/
/* INTEGERP:                               */
/****************************************/
float integerp()
  {
   VALUE valstruct;
   float num, truncnum;

   if (arg_num_check("integerp",EXACTLY,1) == -1) return(FALSE);

   runknown(1,&valstruct);

   if (get_valtype(valstruct) != NUMBER) return(CLIPS_FALSE);

   num = get_valfloat(valstruct);
   
   truncnum = dtrunc((double) num);
   
   if (truncnum != num) 
     { return(CLIPS_FALSE); }
     
   return(CLIPS_TRUE);
  } 

#endif
