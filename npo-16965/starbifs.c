




	/*****************************************************************
	 * starbifs.c			version 1.0 vax/vms, 5-8-86
	 *****************************************************************
	 * Contains function definitions for most of the built-in
	 *   commands of the STAR language.  These functions are linked
	 *   into the initialized semantic network through UNITs of type
	 *   CONNECTION.
	 *
	 * The text which appears under the attribute "comment" in the
	 *   named RECORD defining each built-in function is contained in
	 *   the global variable "bif_init_g", at the end of the file.
	 *
	 * Functions primarily responsible for the operation of STAR
	 *   built-in commands have names which end in "_f". "Supporting"
	 *   functions which assist these functions have names which end
	 *   in "_s".
	 *
	 * The routines and data structures contained in this file may be
	 *   referenced by external routines for a given application, but
	 *   in general should not be altered.  In particular, the
	 *   functions ending in "_f" may be freely called from within
	 *   external functions linked with STAR, provided they are
	 *   defined in a language which passes arguments by value.  For
	 *   languages which pass arguments by reference, the parallel
	 *   functions ending in simply "f" as defined in the file
	 *   "starplus.c" may be called.
	 *****************************************************************/

	/*****************************************************************
	 * The following suffix conventions have been used throughout in
	 *   the STAR code for the naming of various quantities:
	 *
	 *	.._t	a type name,
	 *	.._g	a global variable,
	 *	.._f	a built-in function in STAR,
	 *	.._s	a supporting function.
	 *
	 * This has been done in order to minimize the possibility of
	 *   conflicts between names in STAR and names in the applcation
	 *   routines, as well as avoiding conflicts with reserved words
	 *   in C in some cases.
	 *****************************************************************/

#include <stdio.h>
#include <signal.h>
#include <setjmp.h>

#include "stardefs.h"
#include "starcomm.h"


/* I. BUILT-IN FUNCTIONS. <><><><><><><><><><><><><><><><><><><><><><><><><><>*/

/* EXTERNAL REFERENCES. ------------------------------------------------------*/

	/*****************************************************************
	 * These references are necessary for correct operation of the
	 *   macros "terr_check", "cerr_check", "verr_check" and
	 *   "perr_check", defined in the file "starcomm.h".
	 *****************************************************************/

extern struct unit_t *terr_s();
extern struct unit_t *cerr_s();
extern struct unit_t *verr_s();
extern struct unit_t *perr_s();

/* NUMERICAL FUNCTIONS. ------------------------------------------------------*/

struct unit_t *negate_f(num1)
struct unit_t *num1;
  {
  char *fname="negate";
  extern struct unit_t *new_unit_s();
  struct unit_t *result;
  terr_check(num1,NUM,fname,1);
  result = new_unit_s();
  u_type(result) = NUM;
  n_value(result) = -n_value(num1);
  addto_garbage_s(result);
  return(result);
  }

struct unit_t *add_f(num1,num2)
struct unit_t *num1,*num2;
  {
  char *fname="add";
  extern struct unit_t *new_unit_s();
  struct unit_t *result;
  terr_check(num1,NUM,fname,1);
  terr_check(num2,NUM,fname,2);
  result = new_unit_s();
  u_type(result) = NUM;
  n_value(result) = n_value(num1) + n_value(num2);
  addto_garbage_s(result);
  return(result);
  }

struct unit_t *subtract_f(num1,num2)
struct unit_t *num1,*num2;
  {
  char *fname="subtract";
  extern struct unit_t *new_unit_s();
  struct unit_t *result;
  terr_check(num1,NUM,fname,1);
  terr_check(num2,NUM,fname,2);
  result = new_unit_s();
  u_type(result) = NUM;
  n_value(result) = n_value(num1) - n_value(num2);
  addto_garbage_s(result);
  return(result);
  }

struct unit_t *multiply_f(num1,num2)
struct unit_t *num1,*num2;
  {
  char *fname="multiply";
  extern struct unit_t *new_unit_s();
  struct unit_t *result;
  terr_check(num1,NUM,fname,1);
  terr_check(num2,NUM,fname,2);
  result = new_unit_s();
  u_type(result) = NUM;
  n_value(result) = n_value(num1) * n_value(num2);
  addto_garbage_s(result);
  return(result);
  }

struct unit_t *divide_f(num1,num2)
struct unit_t *num1,*num2;
  {
  char *fname="divide";
  extern struct unit_t *new_unit_s();
  struct unit_t *result;
  terr_check(num1,NUM,fname,1);
  terr_check(num2,NUM,fname,2);
  verr_check(num2,n_value(num2)!=0,fname,2);
  result = new_unit_s();
  u_type(result) = NUM;
  n_value(result) = n_value(num1) / n_value(num2);
  addto_garbage_s(result);
  return(result);
  }

struct unit_t *minimum_f(num1,num2)
struct unit_t *num1,*num2;
  {
  char *fname="minimum";
  extern struct unit_t *new_unit_s();
  struct unit_t *result;
  terr_check(num1,NUM,fname,1);
  terr_check(num2,NUM,fname,2);
  result = new_unit_s();
  u_type(result) = NUM;
  n_value(result) = (n_value(num1) < n_value(num2))
		  ? n_value(num1) : n_value(num2);
  addto_garbage_s(result);
  return(result);
  }

struct unit_t *maximum_f(num1,num2)
struct unit_t *num1,*num2;
  {
  char *fname="maximum";
  extern struct unit_t *new_unit_s();
  struct unit_t *result;
  terr_check(num1,NUM,fname,1);
  terr_check(num2,NUM,fname,2);
  result = new_unit_s();
  u_type(result) = NUM;
  n_value(result) = (n_value(num1) > n_value(num2))
		  ? n_value(num1) : n_value(num2);
  addto_garbage_s(result);
  return(result);
  }

/* TOKEN FUNCTIONS. ----------------------------------------------------------*/

struct unit_t *locate_s(tok1)
struct unit_t *tok1;
  {
  extern struct unit_t *new_unit_s();
  extern struct l_element_t *new_le_s();
  extern struct r_element_t *new_re_s();
  extern struct unit_t directory_g,nil_g,name_g;
  extern struct unit_t *get_s();
  struct unit_t *lis2;
  struct l_element_t *le1,*le2,*old_le2,*le3;
  int i;
  le1 = l_start(&directory_g);
  for(i=(t_start(tok1))[0]-'A'; i>0; --i) le1 = le_link(le1);
  lis2 = le_value(le1);
  le2 = l_start(lis2);
  old_le2 = 0;
  while(le2)
    {
    i = strcmp(t_start(get_s(le_value(le2),&name_g)),t_start(tok1));
    if(i == 0) return(le_value(le2));
    if(i > 0) break;
    old_le2 = le2;
    le2 = le_link(le2);
    }
  ++l_size(lis2);
  le3 = new_le_s();
  le_link(le3) = le2;
  if(old_le2 == 0) l_start(lis2) = le3;
  else le_link(old_le2) = le3;
  le_value(le3) = new_unit_s();
  ++u_count(le_value(le3));
  u_type(le_value(le3)) = REC;
  u_named(le_value(le3)) = 1;
  r_size(le_value(le3)) = 1;
  r_start(le_value(le3)) = new_re_s();
  re_attribute(r_start(le_value(le3))) = &name_g;
  ++u_count(&name_g);
  re_value(r_start(le_value(le3))) = tok1;
  ++u_count(tok1);
  return(le_value(le3));
  }

struct unit_t *locate_f(tok1)
struct unit_t *tok1;
  {
  char *fname="locate";
  extern struct unit_t *locate_s();
  terr_check(tok1,TOK,fname,1);
  return(locate_s(tok1));
  }

struct unit_t *test_f(tok1)
struct unit_t *tok1;
  {
  char *fname="test";
  extern struct unit_t directory_g,name_g,nil_g;
  extern struct unit_t *get_s();
  struct l_element_t *le;
  int i;
  terr_check(tok1,TOK,fname,1);
  le = l_start(&directory_g);
  for(i=(t_start(tok1))[0]-'A'; i>0; --i) le = le_link(le);
  le = l_start(le_value(le));
  while(le)
    {
    if(strcmp(t_start(get_s(le_value(le),&name_g)),t_start(tok1)) == 0)
      return(le_value(le));
    le = le_link(le);
    }
  return(&nil_g);
  }

/* STRING FUNCTIONS. ---------------------------------------------------------*/

struct unit_t *character_f(str1,num1)
struct unit_t *str1,*num1;
  {
  char *fname="character";
  extern struct unit_t *new_unit_s();
  struct unit_t *result;
  int i;
  terr_check(str1,STR,fname,1);
  terr_check(num1,NUM,fname,2);
  verr_check(str1,s_size(str1)>0,fname,1);
  i = (int) n_value(num1);
  i = (i>0) ? i : s_size(str1)+1+i;
  verr_check(num1,i>=1 && i<=s_size(str1),fname,2);
  result = new_unit_s();
  u_type(result) = STR;
  s_size(result) = 1;
  s_start(result) = (char *) malloc(2);
  s_start(result)[0] = s_start(str1)[i-1];
  s_start(result)[1] = '\0';
  addto_garbage_s(result);
  return(result);
  }

struct unit_t *fetch_f(str1,num1)
struct unit_t *str1,*num1;
  {
  char *fname="fetch";
  extern struct unit_t *new_unit_s();
  struct unit_t *result;
  int i,n;
  terr_check(str1,STR,fname,1);
  terr_check(num1,NUM,fname,2);
  n = (int) n_value(num1);
  n = (n>0) ? n : -n;
  verr_check(num1,n<=s_size(str1),fname,2);
  result = new_unit_s();
  u_type(result) = STR;
  s_size(result) = n;
  s_start(result) = (char *) malloc(n+1);
  if(n_value(num1) > 0)
    for(i=0; i<n; ++i) s_start(result)[i] = s_start(str1)[i];
  else
    for(i=0; i<n; ++i) s_start(result)[i] = s_start(str1)[s_size(str1)-n+i];
  s_start(result)[n] = '\0';
  addto_garbage_s(result);
  return(result);
  }

struct unit_t *release_f(str1,num1)
struct unit_t *str1,*num1;
  {
  char *fname="release";
  extern struct unit_t *new_unit_s();
  struct unit_t *result;
  int i,n;
  terr_check(str1,STR,fname,1);
  terr_check(num1,NUM,fname,2);
  n = (int) n_value(num1);
  n = (n>0) ? n : -n;
  verr_check(num1,n<=s_size(str1),fname,2);
  result = new_unit_s();
  u_type(result) = STR;
  s_size(result) = s_size(str1)-n;
  s_start(result) = (char *) malloc(s_size(str1)-n+1);
  if(n_value(num1) > 0)
    for(i=0; i<s_size(result); ++i) s_start(result)[i] = s_start(str1)[n+i];
  else
    for(i=0; i<s_size(result); ++i) s_start(result)[i] = s_start(str1)[i];
  s_start(result)[s_size(result)] = '\0';
  addto_garbage_s(result);
  return(result);
  }

struct unit_t *join_f(str1,str2)
struct unit_t *str1,*str2;
  {
  char *fname="join";
  extern struct unit_t *new_unit_s();
  struct unit_t *result;
  int i;
  terr_check(str1,STR,fname,1);
  terr_check(str2,STR,fname,2);
  result = new_unit_s();
  u_type(result) = STR;
  s_size(result) = s_size(str1) + s_size(str2);
  s_start(result) = (char *) malloc(s_size(str1) + s_size(str2) + 1);
  for(i=0; i<s_size(str1); ++i) s_start(result)[i] = s_start(str1)[i];
  for(i=0; i<s_size(str2); ++i)
    s_start(result)[i+s_size(str1)] = s_start(str2)[i];
  s_start(result)[s_size(result)] = '\0';
  addto_garbage_s(result);
  return(result);
  }

struct unit_t *find_f(str1,str2)
struct unit_t *str1,*str2;
  {
  char *fname="find";
  extern struct unit_t *new_unit_s();
  struct unit_t *result;
  int i,j,flag;
  terr_check(str1,STR,fname,1);
  terr_check(str2,STR,fname,2);
  flag = 0;
  for(i=0; i<=s_size(str2)-s_size(str1); ++i)
    {
    for(j=0; j<s_size(str1); ++j)
      if(s_start(str1)[j] != s_start(str2)[i+j]) break;
    if(j==s_size(str1))
      {flag = 1; break;}
    }
  result = new_unit_s();
  u_type(result) = NUM;
  n_value(result) = flag ? i : s_size(str2);
  addto_garbage_s(result);
  return(result);
  }

struct unit_t *length_f(str1)
struct unit_t *str1;
  {
  char *fname="length";
  extern struct unit_t *new_unit_s();
  struct unit_t *result;
  int n;
  terr_check(str1,STR,fname,1);
  n = s_size(str1);
  result = new_unit_s();
  u_type(result) = NUM;
  n_value(result) = n;
  addto_garbage_s(result);
  return(result);
  }

/* LIST FUNCTIONS. -----------------------------------------------------------*/

struct unit_t *select_f(lis1,num1)
struct unit_t *lis1,*num1;
  {
  char *fname="select";
  struct l_element_t *le;
  int i,j;
  terr_check(lis1,LIS,fname,1);
  terr_check(num1,NUM,fname,2);
  verr_check(lis1,l_size(lis1)>0,fname,1);
  i = (int) n_value(num1);
  i = (i>0) ? i : l_size(lis1)+1+i;
  verr_check(num1,i>=1 && i<=l_size(lis1),fname,2);
  le = l_start(lis1);
  for(j=1; j<i; ++j) le = le_link(le);
  return(le_value(le));
  }

struct unit_t *replace_f(lis1,num1,uni1)
struct unit_t *lis1,*num1,*uni1;
  {
  char *fname="replace";
  extern struct unit_t *new_unit_s();
  extern struct l_element_t *new_le_s();
  struct unit_t *result;
  struct l_element_t *le1,*le2;
  int i,j;
  terr_check(lis1,LIS,fname,1);
  terr_check(num1,NUM,fname,2);
  verr_check(lis1,l_size(lis1)>0,fname,1);
  i = (int) n_value(num1);
  i = (i>0) ? i : l_size(lis1)+1+i;
  verr_check(num1,i>=1 && i<=l_size(lis1),fname,2);
  result = new_unit_s();
  u_type(result) = LIS;
  le1 = l_start(lis1);
  l_start(result) = new_le_s();
  le2 = l_start(result);
  le_value(le2) = (i == 1) ? uni1 : le_value(le1);
  ++u_count(le_value(le2));
  for(j=2; j<=l_size(lis1); ++j)
    {
    le1 = le_link(le1);
    le_link(le2) = new_le_s();
    le2 = le_link(le2);
    le_value(le2) = (j == i) ? uni1 : le_value(le1);
    ++u_count(le_value(le2));
    }
  l_size(result) = l_size(lis1);
  addto_garbage_s(result);
  return(result);
  }

struct unit_t *delete_f(lis1,num1)
struct unit_t *lis1,*num1;
  {
  char *fname="delete";
  extern struct unit_t *new_unit_s();
  extern struct l_element_t *new_le_s();
  struct unit_t *result;
  struct l_element_t *le1,*le2;
  int i,j;
  terr_check(lis1,LIS,fname,1);
  terr_check(num1,NUM,fname,2);
  verr_check(lis1,l_size(lis1)>0,fname,1);
  i = (int) n_value(num1);
  i = (i>0) ? i : l_size(lis1)+1+i;
  verr_check(num1,i>=1 && i<=l_size(lis1),fname,2);
  result = new_unit_s();
  u_type(result) = LIS;
  l_start(result) = 0;
  for(j=1; j<=l_size(lis1)-1; ++j)
    {
    if(j == 1)
      {
      le1 = l_start(lis1);
      l_start(result) = new_le_s();
      le2 = l_start(result);
      }
    else
      {
      le1 = le_link(le1);
      le_link(le2) = new_le_s();
      le2 = le_link(le2);
      }
    if(j == i) le1 = le_link(le1);
    le_value(le2) = le_value(le1);
    ++u_count(le_value(le2));
    }
  l_size(result) = l_size(lis1) - 1;
  addto_garbage_s(result);
  return(result);
  }

struct unit_t *insert_f(lis1,num1,uni1)
struct unit_t *lis1,*num1,*uni1;
  {
  char *fname="insert";
  extern struct unit_t *new_unit_s();
  extern struct l_element_t *new_le_s();
  struct unit_t *result;
  struct l_element_t *le1,*le2;
  int i,j;
  terr_check(lis1,LIS,fname,1);
  terr_check(num1,NUM,fname,2);
  i = (int) n_value(num1);
  i = (i>0) ? i : l_size(lis1)+2+i;
  verr_check(num1,i>=1 && i<=l_size(lis1)+1,fname,2);
  result = new_unit_s();
  u_type(result) = LIS;
  le1 = l_start(lis1);
  l_start(result) = new_le_s();
  le2 = l_start(result);
  for(j=1; j<=l_size(lis1)+1; ++j)
    {
    if(j > 1)
      {
      le_link(le2) = new_le_s();
      le2 = le_link(le2);
      }
    if(j == i) le_value(le2) = uni1;
    else
      {
      le_value(le2) = le_value(le1);
      le1 = le_link(le1);
      }
    ++u_count(le_value(le2));
    }
  l_size(result) = l_size(lis1) + 1;
  addto_garbage_s(result);
  return(result);
  }

struct unit_t *take_f(lis1,num1)
struct unit_t *lis1,*num1;
  {
  char *fname="take";
  extern struct unit_t *new_unit_s();
  extern struct l_element_t *new_le_s();
  struct unit_t *result;
  struct l_element_t *le1,*le2;
  int i,j;
  terr_check(lis1,LIS,fname,1);
  terr_check(num1,NUM,fname,2);
  i = (int) n_value(num1);
  i = (i>0) ? i : -i;
  verr_check(num1,i<=l_size(lis1),fname,2);
  result = new_unit_s();
  u_type(result) = LIS;
  l_start(result) = 0;
  le1 = l_start(lis1);
  if(n_value(num1) < 0)
    for(j=1; j<=l_size(lis1)-i; ++j)
      le1 = le_link(le1);
  for(j=1; j<=i; ++j)
    {
    if(j == 1)
      {
      l_start(result) = new_le_s();
      le2 = l_start(result);
      }
    else
      {
      le1 = le_link(le1);
      le_link(le2) = new_le_s();
      le2 = le_link(le2);
      }
    le_value(le2) = le_value(le1);
    ++u_count(le_value(le2));
    }
  l_size(result) = i;
  addto_garbage_s(result);
  return(result);
  }

struct unit_t *drop_f(lis1,num1)
struct unit_t *lis1,*num1;
  {
  char *fname="drop";
  extern struct unit_t *new_unit_s();
  extern struct l_element_t *new_le_s();
  struct unit_t *result;
  struct l_element_t *le1,*le2;
  int i,j;
  terr_check(lis1,LIS,fname,1);
  terr_check(num1,NUM,fname,2);
  i = (int) n_value(num1);
  i = (i>0) ? i : -i;
  verr_check(num1,i<=l_size(lis1),fname,2);
  result = new_unit_s();
  u_type(result) = LIS;
  l_start(result) = 0;
  le1 = l_start(lis1);
  if(n_value(num1) > 0)
    for(j=1; j<=i; ++j)
      le1 = le_link(le1);
  for(j=1; j<=l_size(lis1)-i; ++j)
    {
    if(j == 1)
      {
      l_start(result) = new_le_s();
      le2 = l_start(result);
      }
    else
      {
      le1 = le_link(le1);
      le_link(le2) = new_le_s();
      le2 = le_link(le2);
      }
    le_value(le2) = le_value(le1);
    ++u_count(le_value(le2));
    }
  l_size(result) = l_size(lis1)-i;
  addto_garbage_s(result);
  return(result);
  }

struct unit_t *append_f(lis1,lis2)
struct unit_t *lis1,*lis2;
  {
  char *fname="append";
  extern struct unit_t *new_unit_s();
  extern struct l_element_t *new_le_s();
  struct unit_t *result;
  struct l_element_t *le1,*le2;
  terr_check(lis1,LIS,fname,1);
  terr_check(lis2,LIS,fname,2);
  result = new_unit_s();
  u_type(result) = LIS;
  l_start(result) = 0;
  le2 = 0;
  le1 = l_start(lis1);
  while(le1)
    {
    if(le2 == 0)
      {
      l_start(result) = new_le_s();
      le2 = l_start(result);
      }
    else
      {
      le_link(le2) = new_le_s();
      le2 = le_link(le2);
      }
    le_value(le2) = le_value(le1);
    ++u_count(le_value(le2));
    le1 = le_link(le1);
    }
  le1 = l_start(lis2);
  while(le1)
    {
    if(le2 == 0)
      {
      l_start(result) = new_le_s();
      le2 = l_start(result);
      }
    else
      {
      le_link(le2) = new_le_s();
      le2 = le_link(le2);
      }
    le_value(le2) = le_value(le1);
    ++u_count(le_value(le2));
    le1 = le_link(le1);
    }
  l_size(result) = l_size(lis1) + l_size(lis2);
  addto_garbage_s(result);
  return(result);
  }

struct unit_t *size_f(lis1)
struct unit_t *lis1;
  {
  char *fname="size";
  extern struct unit_t *new_unit_s();
  struct unit_t *result;
  int n;
  terr_check(lis1,LIS,fname,1);
  n = l_size(lis1);
  result = new_unit_s();
  u_type(result) = NUM;
  n_value(result) = n;
  addto_garbage_s(result);
  return(result);
  }

struct unit_t *union_f(lis1,lis2)
struct unit_t *lis1,*lis2;
  {
  char *fname="union";
  extern struct unit_t *new_unit_s();
  extern struct l_element_t *new_le_s();
  extern struct unit_t true_g;
  struct unit_t *result;
  struct l_element_t *le1,*le2,*le3;
  terr_check(lis1,LIS,fname,1);
  terr_check(lis2,LIS,fname,2);
  result = new_unit_s();
  u_type(result) = LIS;
  l_size(result) = 0;
  l_start(result) = 0;
  le1 = l_start(lis1);
  while(le1)
    {
    le2 = l_start(result);
    while(le2)
      {
      if(compare_s(le_value(le2),le_value(le1))) break;
      le2 = le_link(le2);
      }
    if(le2 == 0)
      {
      if(l_start(result) == 0)
	{
	l_start(result) = new_le_s();
	le3 = l_start(result);
	}
      else
	{
	le_link(le3) = new_le_s();
	le3 = le_link(le3);
	}
      ++l_size(result);
      le_value(le3) = le_value(le1);
      ++u_count(le_value(le3));
      }
    le1 = le_link(le1);
    }
  le1 = l_start(lis2);
  while(le1)
    {
    le2 = l_start(result);
    while(le2)
      {
      if(compare_s(le_value(le2),le_value(le1))) break;
      le2 = le_link(le2);
      }
    if(le2 == 0)
      {
      if(l_start(result) == 0)
	{
	l_start(result) = new_le_s();
	le3 = l_start(result);
	}
      else
	{
	le_link(le3) = new_le_s();
	le3 = le_link(le3);
	}
      ++l_size(result);
      le_value(le3) = le_value(le1);
      ++u_count(le_value(le3));
      }
    le1 = le_link(le1);
    }
  addto_garbage_s(result);
  return(result);
  }

struct unit_t *intersection_f(lis1,lis2)
struct unit_t *lis1,*lis2;
  {
  char *fname="intersection";
  extern struct unit_t *new_unit_s();
  extern struct l_element_t *new_le_s();
  extern struct unit_t true_g;
  struct unit_t *result;
  struct l_element_t *le1,*le2,*le3,*le4;
  terr_check(lis1,LIS,fname,1);
  terr_check(lis2,LIS,fname,2);
  result = new_unit_s();
  u_type(result) = LIS;
  l_size(result) = 0;
  l_start(result) = 0;
  le1 = l_start(lis1);
  while(le1)
    {
    le2 = l_start(lis2);
    while(le2)
      {
      if(compare_s(le_value(le1),le_value(le2)))
	{
	le3 = l_start(result);
	while(le3)
	  {
	  if(compare_s(le_value(le1),le_value(le3))) break;
	  le3 = le_link(le3);
	  }
	if(le3 == 0)
	  {
	  ++l_size(result);
	  if(l_start(result) == 0)
	    {
	    l_start(result) = new_le_s();
	    le4 = l_start(result);
	    }
	  else
	    {
	    le_link(le4) = new_le_s();
	    le4 = le_link(le4);
	    }
	  le_value(le4) = le_value(le1);
	  ++u_count(le_value(le4));
	  }
	break;
	}
      le2 = le_link(le2);
      }
    le1 = le_link(le1);
    }
  addto_garbage_s(result);
  return(result);
  }

struct unit_t *difference_f(lis1,lis2)
struct unit_t *lis1,*lis2;
  {
  char *fname="difference";
  extern struct unit_t *new_unit_s();
  extern struct l_element_t *new_le_s();
  extern struct unit_t true_g;
  struct unit_t *result;
  struct l_element_t *le1,*le2,*le3,*le4;
  terr_check(lis1,LIS,fname,1);
  terr_check(lis2,LIS,fname,2);
  result = new_unit_s();
  u_type(result) = LIS;
  l_size(result) = 0;
  l_start(result) = 0;
  le1 = l_start(lis1);
  while(le1)
    {
    le2 = l_start(lis2);
    while(le2)
      {
      if(compare_s(le_value(le1),le_value(le2))) break;
      le2 = le_link(le2);
      }
    if(le2 == 0)
      {
      le3 = l_start(result);
      while(le3)
	{
	if(compare_s(le_value(le1),le_value(le3))) break;
	le3 = le_link(le3);
	}
      if(le3 == 0)
	{
	++l_size(result);
	if(l_start(result) == 0)
	  {
	  l_start(result) = new_le_s();
	  le4 = l_start(result);
	  }
	else
	  {
	  le_link(le4) = new_le_s();
	  le4 = le_link(le4);
	  }
	le_value(le4) = le_value(le1);
	++u_count(le_value(le4));
	}
      }
    le1 = le_link(le1);
    }
  addto_garbage_s(result);
  return(result);
  }

/* RECORD FUNCTIONS. ---------------------------------------------------------*/

struct unit_t *get_s(rec1,att1)
struct unit_t *rec1,*att1;
  {
  extern struct unit_t nil_g;
  struct r_element_t *re;
  re = r_start(rec1);
  while(re)
    {
    if(re_attribute(re) == att1) return(re_value(re));
    re = re_link(re);
    }
  return(&nil_g);
  }

struct unit_t *get_f(rec1,att1)
struct unit_t *rec1,*att1;
  {
  char *fname="get";
  extern struct unit_t *get_s();
  extern struct unit_t attribute_g;
  terr_check(rec1,REC,fname,1);
  terr_check(att1,REC,fname,2);
  return(get_s(rec1,att1));
  }

struct unit_t *put_f(rec1,att1,uni1)
struct unit_t *rec1,*att1,*uni1;
  {
  char *fname="put";
  extern struct unit_t *new_unit_s();
  extern struct r_element_t *new_re_s();
  extern struct unit_t attribute_g;
  struct unit_t *result;
  struct r_element_t *re1,*re2;
  int flag;
  terr_check(rec1,REC,fname,1);
  terr_check(att1,REC,fname,2);
  flag = 0;
  result = new_unit_s();
  u_type(result) = REC;
  r_size(result) = r_size(rec1);
  r_start(result) = 0;
  re1 = r_start(rec1);
  while(re1)
    {
    if(r_start(result) == 0)
      {
      r_start(result) = new_re_s();
      re2 = r_start(result);
      }
    else
      {
      re_link(re2) = new_re_s();
      re2 = re_link(re2);
      }
    re_attribute(re2) = re_attribute(re1);
    ++u_count(re_attribute(re2));
    if(re_attribute(re2) == att1)
      {
      flag = 1;
      re_value(re2) = uni1;
      }
    else re_value(re2) = re_value(re1);
    ++u_count(re_value(re2));
    re1 = re_link(re1);
    }
  if(flag == 0)
    {
    if(r_start(result) == 0)
      {
      r_start(result) = new_re_s();
      re2 = r_start(result);
      }
    else
      {
      re_link(re2) = new_re_s();
      re2 = re_link(re2);
      }
    ++r_size(result);
    re_attribute(re2) = att1;
    ++u_count(re_attribute(re2));
    re_value(re2) = uni1;
    ++u_count(re_value(re2));
    }
  addto_garbage_s(result);
  return(result);
  }

struct unit_t *omit_f(rec1,att1)
struct unit_t *rec1,*att1;
  {
  char *fname="omit";
  extern struct unit_t *new_unit_s();
  extern struct r_element_t *new_re_s();
  extern struct unit_t attribute_g;
  struct unit_t *result;
  struct r_element_t *re1,*re2;
  terr_check(rec1,REC,fname,1);
  terr_check(att1,REC,fname,2);
  result = new_unit_s();
  u_type(result) = REC;
  r_size(result) = r_size(rec1);
  r_start(result) = 0;
  re1 = r_start(rec1);
  while(re1)
    {
    if(re_attribute(re1) != att1)
      {
      if(r_start(result) == 0)
	{
	r_start(result) = new_re_s();
	re2 = r_start(result);
	}
      else
	{
	re_link(re2) = new_re_s();
	re2 = re_link(re2);
	}
      re_attribute(re2) = re_attribute(re1);
      ++u_count(re_attribute(re2));
      re_value(re2) = re_value(re1);
      ++u_count(re_value(re2));
      }
    else --r_size(result);
    re1 = re_link(re1);
    }
  addto_garbage_s(result);
  return(result);
  }

struct unit_t *detach_f(rec1,lis1)
struct unit_t *rec1,*lis1;
  {
  char *fname="detach";
  extern struct unit_t *new_unit_s();
  extern struct r_element_t *new_re_s();
  struct unit_t *result;
  struct r_element_t *re1,*re2;
  struct l_element_t *le1;
  terr_check(rec1,REC,fname,1);
  terr_check(lis1,LIS,fname,2);
  result = new_unit_s();
  u_type(result) = REC;
  r_size(result) = 0;
  r_start(result) = 0;
  le1 = l_start(lis1);
  while(le1)
    {
    re1 = r_start(rec1);
    while(re1)
      {
      if(re_attribute(re1) == le_value(le1))
	{
	if(r_start(result) == 0)
	  {
	  r_start(result) = new_re_s();
	  re2 = r_start(result);
	  }
	else
	  {
	  re_link(re2) = new_re_s();
	  re2 = re_link(re2);
	  }
	++r_size(result);
	re_attribute(re2) = re_attribute(re1);
	++u_count(re_attribute(re2));
	re_value(re2) = re_value(re1);
	++u_count(re_value(re2));
	}
      re1 = re_link(re1);
      }
    le1 = le_link(le1);
    }
  addto_garbage_s(result);
  return(result);
  }

struct unit_t *attach_f(rec1,rec2)
struct unit_t *rec1,*rec2;
  {
  char *fname="attach";
  extern struct unit_t *new_unit_s();
  extern struct r_element_t *new_re_s();
  struct unit_t *result;
  struct r_element_t *re1,*re2,*re3;
  terr_check(rec1,REC,fname,1);
  terr_check(rec2,REC,fname,2);
  result = new_unit_s();
  u_type(result) = REC;
  r_size(result) = 0;
  r_start(result) = 0;
  re1 = r_start(rec1);
  while(re1)
    {
    re2 = r_start(result);
    while(re2)
      {
      if(re_attribute(re2) == re_attribute(re1)) break;
      re2 = re_link(re2);
      }
    if(re2 == 0)
      {
      if(r_start(result) == 0)
	{
	r_start(result) = new_re_s();
	re3 = r_start(result);
	}
      else
	{
	re_link(re3) = new_re_s();
	re3 = re_link(re3);
	}
      ++r_size(result);
      re_attribute(re3) = re_attribute(re1);
      ++u_count(re_attribute(re3));
      re_value(re3) = re_value(re1);
      ++u_count(re_value(re3));
      }
    re1 = re_link(re1);
    }
  re1 = r_start(rec2);
  while(re1)
    {
    re2 = r_start(result);
    while(re2)
      {
      if(re_attribute(re2) == re_attribute(re1)) break;
      re2 = re_link(re2);
      }
    if(re2 == 0)
      {
      if(r_start(result) == 0)
	{
	r_start(result) = new_re_s();
	re3 = r_start(result);
	}
      else
	{
	re_link(re3) = new_re_s();
	re3 = re_link(re3);
	}
      ++r_size(result);
      re_attribute(re3) = re_attribute(re1);
      ++u_count(re_attribute(re3));
      re_value(re3) = re_value(re1);
      ++u_count(re_value(re3));
      }
    re1 = re_link(re1);
    }
  if(r_size(result) > 0) re_link(re3) = 0;
  addto_garbage_s(result);
  return(result);
  }

struct unit_t *key_f(rec1)
struct unit_t *rec1;
  {
  char *fname="key";
  extern struct unit_t *new_unit_s();
  extern struct l_element_t *new_le_s();
  struct unit_t *result;
  struct r_element_t *re1;
  struct l_element_t *le1;
  terr_check(rec1,REC,fname,1);
  result = new_unit_s();
  u_type(result) = LIS;
  l_start(result) = 0;
  le1 = 0;
  re1 = r_start(rec1);
  while(re1)
    {
    if(le1 == 0)
      {
      l_start(result) = new_le_s();
      le1 = l_start(result);
      }
    else
      {
      le_link(le1) = new_le_s();
      le1 = le_link(le1);
      }
    le_value(le1) = re_attribute(re1);
    ++u_count(le_value(le1));
    re1 = re_link(re1);
    }
  l_size(result) = r_size(rec1);
  addto_garbage_s(result);
  return(result);
  }

struct unit_t *image_f(rec1)
struct unit_t *rec1;
  {
  char *fname="image";
  extern struct unit_t *new_unit_s();
  extern struct l_element_t *new_le_s();
  struct unit_t *result;
  struct r_element_t *re1;
  struct l_element_t *le1;
  terr_check(rec1,REC,fname,1);
  result = new_unit_s();
  u_type(result) = LIS;
  l_start(result) = 0;
  le1 = 0;
  re1 = r_start(rec1);
  while(re1)
    {
    if(le1 == 0)
      {
      l_start(result) = new_le_s();
      le1 = l_start(result);
      }
    else
      {
      le_link(le1) = new_le_s();
      le1 = le_link(le1);
      }
    le_value(le1) = re_value(re1);
    ++u_count(le_value(le1));
    re1 = re_link(re1);
    }
  l_size(result) = r_size(rec1);
  addto_garbage_s(result);
  return(result);
  }

struct unit_t *build_f(lis1,lis2)
struct unit_t *lis1,*lis2;
  {
  char *fname="build";
  extern struct unit_t *new_unit_s();
  extern struct r_element_t *new_re_s();
  extern struct unit_t attribute_g;
  struct unit_t *result;
  struct r_element_t *re1;
  struct l_element_t *le1,*le2;
  terr_check(lis1,LIS,fname,1);
  terr_check(lis2,LIS,fname,2);
  verr_check(lis2,l_size(lis2)==l_size(lis1),fname,2);
  le1 = l_start(lis1);
  while(le1)
    {
    verr_check(lis1,isa_s(le_value(le1),&attribute_g),fname,1);
    le1 = le_link(le1);
    }
  result = new_unit_s();
  u_type(result) = REC;
  r_start(result) = 0;
  le1 = l_start(lis1);
  le2 = l_start(lis2);
  re1 = 0;
  while(le1)
    {
    if(re1 == 0)
      {
      r_start(result) = new_re_s();
      re1 = r_start(result);
      }
    else
      {
      re_link(re1) = new_re_s();
      re1 = re_link(re1);
      }
    re_attribute(re1) = le_value(le1);
    ++u_count(re_attribute(re1));
    re_value(re1) = le_value(le2);
    ++u_count(re_value(re1));
    le1 = le_link(le1);
    le2 = le_link(le2);
    }
  r_size(result) = l_size(lis1);
  addto_garbage_s(result);
  return(result);
  }

struct unit_t *non_value_obtain_s(rec1,att1,att2)
struct unit_t *rec1,*att1,*att2;
  {
  extern struct unit_t pattern_g,nil_g,true_g,aspects_g;
  extern struct unit_t concept_g,member_of_g,subclass_of_g;
  struct unit_t *rec2,*temp1,*temp2;
  int i;
  i = 0;
  rec2 = rec1;
  while(1)
    {
    if(u_type(rec2) != REC) return(&nil_g);
    if(i == 0) temp1 = get_s(rec2,att1);
    else
      {
      temp1 = get_s(rec2,&pattern_g);
      if(temp1 == &nil_g) goto bottom;
      if(u_type(temp1) != REC) return(&nil_g);
      temp1 = get_s(temp1,att1);
      }
    if(u_type(temp1)!=REC) goto bottom;
    temp2 = get_s(temp1,&aspects_g);
    if(temp2 == &nil_g) goto bottom;
    temp1 = get_s(temp2,att2);
    if(temp1 != &nil_g) return(temp1);
  bottom:
    if(rec2==&concept_g && i>0) return(&nil_g);
    if(i == 0) rec2 = get_s(rec2,&member_of_g);
    else rec2 = get_s(rec2,&subclass_of_g);
    if(rec2 == &nil_g) return(&nil_g);
    ++i;
    }
  }

struct unit_t *side_effects_call_s(flis,rec1)
struct unit_t *flis,*rec1;
  {
  extern struct unit_t *new_unit_s(),*evaluate_s();
  extern struct e_element_t *new_ee_s();
  struct unit_t *temp1,*temp2;
  struct e_element_t *ee;
  struct l_element_t *le;
  if(u_type(flis) != LIS) return(rec1);
  le = l_start(flis);
  while(le)
    {
    temp1 = new_unit_s();
    u_type(temp1) = EXP;
    e_size(temp1) = 2;
    ee = new_ee_s();
    e_start(temp1) = ee;
    ee_value(ee) = le_value(le);
    ++u_count(ee_value(ee));
    ee_link(ee) = new_ee_s();
    ee = ee_link(ee);
    ee_value(ee) = rec1;
    ++u_count(ee_value(ee));
    addto_garbage_s(temp1);
    temp2 = evaluate_s(temp1);
    if(temp2 == 0) return(0);
    le = le_link(le);
    }
  return(temp2);
  }

struct unit_t *to_directory_s(rec1)
struct unit_t *rec1;
  {
  extern struct l_element_t *new_le_s();
  extern struct r_element_t *new_re_s();
  extern struct unit_t *get_s();
  extern struct unit_t name_g,nil_g,directory_g,true_g;
  struct unit_t *tok1,*lis2,*uni1,*uni2;
  struct l_element_t *le1,*le2,*le3,*old_le2;
  struct r_element_t *re1,*re2,*old_re2;
  int i;
  if(u_type(rec1) != REC) return(verr_s(0,0,rec1));
  tok1 = get_s(rec1,&name_g);
  if(u_type(tok1) != TOK) return(verr_s(0,0,rec1));
  le1 = l_start(&directory_g);
  for(i=(t_start(tok1))[0]-'A'; i>0; --i) le1 = le_link(le1);
  lis2 = le_value(le1);
  le2 = l_start(lis2);
  old_le2 = 0;
  while(le2)
    {
    i = strcmp(t_start(get_s(le_value(le2),&name_g)),t_start(tok1));
    if(i > 0) break;
    if(i == 0)
      {
      re1 = r_start(rec1);
      while(re1)
	{
	uni1 = re_value(re1);
	uni2 = get_s(le_value(le2),re_attribute(re1));
        if(uni2!=&nil_g && !compare_s(uni1,uni2)) return(verr_s(0,0,rec1));
	re1 = re_link(re1);
	}
      re1 = r_start(rec1);
      while(re1)
	{
	re2 = r_start(le_value(le2));
	old_re2 = 0;
	while(re2)
	  {
	  if(re_attribute(re2) == re_attribute(re1)) break;
	  old_re2 = re2;
	  re2 = re_link(re2);
	  }
	if(re2 == 0)
	  {
	  ++r_size(le_value(le2));
	  re_link(old_re2) = new_re_s();
	  re2 = re_link(old_re2);
	  re_attribute(re2) = re_attribute(re1);
	  ++u_count(re_attribute(re2));
	  re_value(re2) = re_value(re1);
	  ++u_count(re_value(re2));
	  }
	else if(re_value(re2) == &nil_g)
	  {
	  --u_count(re_value(re2));
	  addto_garbage_s(re_value(re2));
	  re_value(re2) = re_value(re1);
	  ++u_count(re_value(re2));
	  }
	re1 = re_link(re1);
	}
      return(le_value(le2));
      }
    old_le2 = le2;
    le2 = le_link(le2);
    }
  ++l_size(lis2);
  le3 = new_le_s();
  le_link(le3) = le2;
  if(old_le2 == 0) l_start(lis2) = le3;
  else le_link(old_le2) = le3;
  le_value(le3) = rec1;
  ++u_count(le_value(le3));
  u_named(le_value(le3)) = 1;
  return(rec1);
  }

struct unit_t *to_members_s(rec1)
struct unit_t *rec1;
  {
  extern struct unit_t *new_unit_s();
  extern struct l_element_t *new_le_s();
  extern struct r_element_t *new_re_s();
  extern struct unit_t *locate_s(),*get_s(),*copy_list_s();
  extern struct unit_t member_of_g,class_g,members_g,name_g,nil_g;
  struct unit_t *rec2,*lis1;
  struct l_element_t *le;
  struct r_element_t *re,*old_re,*lis_re;
  if(u_type(rec1) != REC) return(verr_s(0,0,rec1));
  rec1 = locate_s(get_s(rec1,&name_g));
  rec2 = get_s(rec1,&member_of_g);
  if(!isa_s(rec2,&class_g)) return(verr_s(0,0,rec2));
  lis_re = r_start(rec2);
  while(lis_re)
    {
    if(re_attribute(lis_re) == &members_g) break;
    lis_re = re_link(lis_re);
    }
  lis1 = (lis_re) ? re_value(lis_re) : &nil_g;
  if(lis1 == &nil_g)
    {
    re = r_start(rec2);
    old_re = 0;
    while(re)
      {
      if(re_attribute(re) == &members_g)
	{
	--u_count(re_value(re));
	addto_garbage_s(re_value(re));
	lis1 = new_unit_s();
	re_value(re) = lis1;
	++u_count(re_value(re));
	u_type(lis1) = LIS;
	l_size(lis1) = 0;
	l_start(lis1) = 0;
	break;
	}
      old_re = re;
      re = re_link(re);
      }
    if(re == 0)
      {
      ++r_size(rec2);
      re = new_re_s();
      re_link(old_re) = re;
      re_attribute(re) = &members_g;
      ++u_count(re_attribute(re));
      lis1 = new_unit_s();
      re_value(re) = lis1;
      ++u_count(re_value(re));
      u_type(lis1) = LIS;
      l_size(lis1) = 0;
      l_start(lis1) = 0;
      re_link(re) = 0;
      }
    }
  if(u_type(lis1) != LIS) return(verr_s(0,0,rec2));
  if(u_count(lis1) > 1)
    {
    --u_count(re_value(lis_re));
    addto_garbage_s(re_value(lis_re));
    re_value(lis_re) = copy_list_s(lis1);
    ++u_count(re_value(lis_re));
    lis1 = re_value(lis_re);
    }
  if(l_start(lis1) == 0)
    {
    l_start(lis1) = new_le_s();
    le = l_start(lis1);
    }
  else
    {
    le = l_start(lis1);
    while(le_link(le) != 0)
      {
      if(le_value(le) == rec1) return(rec1);
      le = le_link(le);
      }
    if(le_value(le) == rec1) return(rec1);
    le_link(le) = new_le_s();
    le = le_link(le);
    }
  ++l_size(lis1);
  le_value(le) = rec1;
  ++u_count(le_value(le));
  return(rec1);
  }

struct unit_t *to_subclasses_s(rec1)
struct unit_t *rec1;
  {
  extern struct unit_t *new_unit_s();
  extern struct l_element_t *new_le_s();
  extern struct r_element_t *new_re_s();
  extern struct unit_t *locate_s(),*get_s(),*copy_list_s();
  extern struct unit_t subclass_of_g,class_g,subclasses_g,name_g,nil_g;
  struct unit_t *rec2,*lis1;
  struct l_element_t *le;
  struct r_element_t *re,*old_re,*lis_re;
  if(u_type(rec1) != REC) return(verr_s(0,0,rec1));
  if(!isa_s(rec1,&class_g)) return(rec1);
  rec1 = locate_s(get_s(rec1,&name_g));
  rec2 = get_s(rec1,&subclass_of_g);
  if(!isa_s(rec2,&class_g)) return(verr_s(0,0,rec2));
  lis_re = r_start(rec2);
  while(lis_re)
    {
    if(re_attribute(lis_re) == &subclasses_g) break;
    lis_re = re_link(lis_re);
    }
  lis1 = (lis_re) ? re_value(lis_re) : &nil_g;
  if(lis1 == &nil_g)
    {
    re = r_start(rec2);
    old_re = 0;
    while(re)
      {
      if(re_attribute(re) == &subclasses_g)
	{
	--u_count(re_value(re));
	addto_garbage_s(re_value(re));
	lis1 = new_unit_s();
	re_value(re) = lis1;
	++u_count(re_value(re));
	u_type(lis1) = LIS;
	l_size(lis1) = 0;
	l_start(lis1) = 0;
	break;
	}
      old_re = re;
      re = re_link(re);
      }
    if(re == 0)
      {
      ++r_size(rec2);
      re = new_re_s();
      re_link(old_re) = re;
      re_attribute(re) = &subclasses_g;
      ++u_count(re_attribute(re));
      lis1 = new_unit_s();
      re_value(re) = lis1;
      ++u_count(re_value(re));
      u_type(lis1) = LIS;
      l_size(lis1) = 0;
      l_start(lis1) = 0;
      re_link(re) = 0;
      }
    }
  if(u_type(lis1) != LIS) return(verr_s(0,0,rec2));
  if(u_count(lis1) > 1)
    {
    --u_count(re_value(lis_re));
    addto_garbage_s(re_value(lis_re));
    re_value(lis_re) = copy_list_s(lis1);
    ++u_count(re_value(lis_re));
    lis1 = re_value(lis_re);
    }
  if(l_start(lis1) == 0)
    {
    l_start(lis1) = new_le_s();
    le = l_start(lis1);
    }
  else
    {
    le = l_start(lis1);
    while(le_link(le) != 0)
      {
      if(le_value(le) == rec1) return(rec1);
      le = le_link(le);
      }
    if(le_value(le) == rec1) return(rec1);
    le_link(le) = new_le_s();
    le = le_link(le);
    }
  ++l_size(lis1);
  le_value(le) = rec1;
  ++u_count(le_value(le));
  return(rec1);
  }

struct unit_t *from_directory_s(rec1)
struct unit_t *rec1;
  {
  extern struct unit_t *get_s();
  extern struct unit_t directory_g,name_g;
  struct unit_t *lis2;
  struct l_element_t *le1,*le2,*old_le2;
  struct r_element_t *re;
  int i;
  if(u_type(rec1) != REC) return(verr_s(0,0,rec1));
  if(!u_named(rec1)) return(rec1);
  re = r_start(rec1);
  while(re)
    {
    if(re_attribute(re) == &name_g)
      if(re_bidef(re)) return(perr_s(0,0,rec1));
    re = re_link(re);
    }
  le1 = l_start(&directory_g);
  if(u_type(get_s(rec1,&name_g)) != TOK) return(verr_s(0,0,rec1));
  for(i=(t_start(get_s(rec1,&name_g)))[0]-'A'; i>0; --i) le1 = le_link(le1);
  lis2 = le_value(le1);
  le2 = l_start(lis2);
  old_le2 = 0;
  while(le2)
    {
    if(le_value(le2) == rec1)
      {
      --l_size(lis2);
      if(old_le2 == 0) l_start(lis2) = le_link(le2);
      else le_link(old_le2) = le_link(le2);
      u_named(le_value(le2)) = 0;
      --u_count(le_value(le2));
      addto_garbage_s(le_value(le2));
      if(!le_bidef(le2)) free(le2);
      }
    old_le2 = le2;
    le2 = le_link(le2);
    }
  return(rec1);
  }

struct unit_t *from_members_s(rec1)
struct unit_t *rec1;
  {
  extern struct unit_t *get_s(),*copy_list_s();
  extern struct unit_t member_of_g,members_g,class_g,nil_g;
  struct unit_t *rec2,*lis1;
  struct l_element_t *le,*old_le;
  struct r_element_t *re,*lis_re;
  if(u_type(rec1) != REC) return(verr_s(0,0,rec1));
  re = r_start(rec1);
  while(re)
    {
    if(re_attribute(re) == &member_of_g)
      if(re_bidef(re)) return(perr_s(0,0,rec1));
    re = re_link(re);
    }
  rec2 = get_s(rec1,&member_of_g);
  if(rec2 == &nil_g) return(rec1);
  if(u_type(rec2) != REC) return(verr_s(0,0,rec2));
  if(!isa_s(rec2,&class_g)) return(verr_s(0,0,rec2));
  lis_re = r_start(rec2);
  while(lis_re)
    {
    if(re_attribute(lis_re) == &members_g) break;
    lis_re = re_link(lis_re);
    }
  lis1 = (lis_re) ? re_value(lis_re) : &nil_g;
  if(u_type(lis1) != LIS) return(verr_s(0,0,rec2));
  if(u_count(lis1) > 1)
    {
    --u_count(re_value(lis_re));
    addto_garbage_s(re_value(lis_re));
    re_value(lis_re) = copy_list_s(lis1);
    ++u_count(re_value(lis_re));
    lis1 = re_value(lis_re);
    }
  le = l_start(lis1);
  old_le = 0;
  while(le)
    {
    if(le_value(le) == rec1)
      {
      --l_size(lis1);
      if(old_le == 0) l_start(lis1) = le_link(le);
      else le_link(old_le) = le_link(le);
      --u_count(le_value(le));
      addto_garbage_s(le_value(le));
      if(!le_bidef(le)) free(le);
      }
    old_le = le;
    le = le_link(le);
    }
  return(rec1);
  }

struct unit_t *from_subclasses_s(rec1)
struct unit_t *rec1;
  {
  extern struct unit_t *get_s(),*copy_list_s();
  extern struct unit_t subclass_of_g,subclasses_g,class_g,nil_g;
  struct unit_t *rec2,*lis1;
  struct l_element_t *le,*old_le;
  struct r_element_t *re,*lis_re;
  if(u_type(rec1) != REC) return(verr_s(0,0,rec1));
  re = r_start(rec1);
  while(re)
    {
    if(re_attribute(re) == &subclass_of_g)
      if(re_bidef(re)) return(perr_s(0,0,rec1));
    re = re_link(re);
    }
  if(!isa_s(rec1,&class_g)) return(rec1);
  rec2 = get_s(rec1,&subclass_of_g);
  if(rec2 == &nil_g) return(rec1);
  if(u_type(rec2) != REC) return(verr_s(0,0,rec2));
  if(!isa_s(rec2,&class_g)) return(verr_s(0,0,rec2));
  lis_re = r_start(rec2);
  while(lis_re)
    {
    if(re_attribute(lis_re) == &subclasses_g) break;
    lis_re = re_link(lis_re);
    }
  lis1 = (lis_re) ? re_value(lis_re) : &nil_g;
  if(u_type(lis1) != LIS) return(verr_s(0,0,rec2));
  if(u_count(lis1) > 1)
    {
    --u_count(re_value(lis_re));
    addto_garbage_s(re_value(lis_re));
    re_value(lis_re) = copy_list_s(lis1);
    ++u_count(re_value(lis_re));
    lis1 = re_value(lis_re);
    }
  le = l_start(lis1);
  old_le = 0;
  while(le)
    {
    if(le_value(le) == rec1)
      {
      --l_size(lis1);
      if(old_le == 0) l_start(lis1) = le_link(le);
      else le_link(old_le) = le_link(le);
      --u_count(le_value(le));
      addto_garbage_s(le_value(le));
      if(!le_bidef(le)) free(le);
      }
    old_le = le;
    le = le_link(le);
    }
  return(rec1);
  }

int assert_ok_s(att1,uni1)
struct unit_t *att1,*uni1;
  {
  extern struct unit_t name_g,member_of_g,subclass_of_g,class_g,nil_g;
  extern struct unit_t *get_s();
  extern int isa_s();
  if(uni1 == 0) return(1);
  if(att1 == &name_g)
    {
    if(u_type(uni1)!=TOK) return(0);
    else return(1);
    }
  if(att1 == &member_of_g || att1 == &subclass_of_g)
    {
    if(u_type(uni1)!=REC) return(0);
    if(get_s(uni1,&member_of_g)==&nil_g) return(0);
    if(!isa_s(uni1,&class_g)) return(0);
    else return(1);
    }
  return(1);
  }

struct unit_t *define_f(rec1)
struct unit_t *rec1;
  {
  char *fname="define";
  extern struct unit_t *value_obtain_s(),*to_directory_s();
  extern struct unit_t *to_members_s(),*to_subclasses_s();
  extern struct unit_t *side_effects_call(),*contents_s();
  extern struct unit_t nil_g,name_g,member_of_g,subclass_of_g;
  extern struct unit_t side_effects_g,true_g,if_asserted_g;
  extern int assert_ok_s();
  struct unit_t *temp1;
  struct r_element_t *re;
  terr_check(rec1,REC,fname,1);
  verr_check(rec1,contents_s(rec1,&name_g)!=0,fname,1);
  verr_check(rec1,assert_ok_s(&name_g,contents_s(rec1,&name_g)),fname,1);
  verr_check(rec1,
    assert_ok_s(&member_of_g,contents_s(rec1,&member_of_g)),fname,1);
  verr_check(rec1,
    assert_ok_s(&subclass_of_g,contents_s(rec1,&subclass_of_g)),fname,1);
  rec1 = to_directory_s(rec1);  /* Catch conflicts b4 side-eff's taken. */
  if(rec1==0) {printf(" define(1*"); return(0);}
  re = r_start(rec1);
  while(re)
    {
    if(re_attribute(re) == &member_of_g)
      {
      rec1 = to_members_s(rec1);
      if(rec1==0) {printf(" define(1*"); return(0);}
      }
    if(re_attribute(re) == &subclass_of_g)
      {
      rec1=to_subclasses_s(rec1);
      if(rec1==0) {printf(" define(1*"); return(0);}
      }
    if(get_s(re_attribute(re),&side_effects_g) == &true_g)
      {
      temp1 = non_value_obtain_s(rec1,re_attribute(re),&if_asserted_g);
      if(temp1 != &nil_g)
        if(side_effects_call_s(temp1,rec1) == 0)
	  {printf(" define(1*"); return(0);}
      }
    re = re_link(re);
    }
  return(rec1);
  }

struct unit_t *create_f(rec1,rec2)
struct unit_t *rec1,*rec2;
  {
  char *fname="create";
  extern struct r_element_t *new_re_s();
  extern struct unit_t *non_value_obtain_s(),*side_effects_call_s(),*get_s();
  extern struct unit_t name_g,side_effects_g,true_g,nil_g;
  extern struct unit_t class_g,member_of_g,if_asserted_g;
  struct unit_t *temp1;
  struct r_element_t *re;
  terr_check(rec1,REC,fname,1);
  terr_check(rec2,REC,fname,2);
  cerr_check(rec2,&class_g,fname,2);
  verr_check(rec1,u_named(rec1) && r_size(rec1)==1,fname,1);
  ++r_size(rec1);
  re = new_re_s();
  re_link(r_start(rec1)) = re;
  re_attribute(re) = &member_of_g;
  ++u_count(re_attribute(re));
  re_value(re) = rec2;
  ++u_count(re_value(re));
  if(get_s(&name_g,&side_effects_g) == &true_g)
    {
    temp1 = non_value_obtain_s(rec1,&name_g,&if_asserted_g);
    if(temp1 != &nil_g)
      if(side_effects_call_s(temp1,rec1) == 0)
        {printf(" create(1*"); return(0);}
    }
  rec1 = to_members_s(rec1);
  if(rec1 == 0) {printf(" create(1*"); return(0);}
  if(get_s(&member_of_g,&side_effects_g) == &true_g)
    {
    temp1 = non_value_obtain_s(rec1,&member_of_g,&if_asserted_g);
    if(temp1 != &nil_g)
      if(side_effects_call_s(temp1,rec1) == 0)
        {printf(" create(1*"); return(0);}
    }
  return(rec1);
  }

struct unit_t *assert_f(rec1,att1,uni1)
struct unit_t *rec1,*att1,*uni1;
  {
  char *fname="assert";
  extern struct r_element_t *new_re_s();
  extern struct unit_t *value_obtain_s();
  extern struct unit_t attribute_g,name_g,nil_g,true_g;
  extern struct unit_t side_effects_g,if_asserted_g;
  extern struct unit_t member_of_g,subclass_of_g;
  extern int assert_ok_s();
  struct unit_t *rec2,*temp1;
  struct r_element_t *re,*old_re;
  terr_check(rec1,REC,fname,1);
  terr_check(att1,REC,fname,2);
  verr_check(rec1,u_named(rec1) || att1==&name_g,fname,1);
  verr_check(uni1,assert_ok_s(att1,uni1),fname,3);
  re = r_start(rec1);
  old_re = 0;
  while(re)
    {
    if(re_attribute(re) == att1)
      {
      verr_check(att1,re_value(re) == &nil_g,fname,2);
      --u_count(re_value(re));
      addto_garbage_s(re_value(re));
      re_value(re) = uni1;
      ++u_count(re_value(re));
      break;
      }
    old_re = re;
    re = re_link(re);
    }
  if(re == 0)
    {
    ++r_size(rec1);
    re = new_re_s();
    re_link(old_re) = re;
    re_attribute(re) = att1;
    ++u_count(re_attribute(re));
    re_value(re) = uni1;
    ++u_count(re_value(re));
    }
  if(att1 == &name_g)
    {
    rec1 = to_directory_s(rec1);
    if(rec1 == 0) {printf(" assert(1*"); return(0);}
    }
  if(att1 == &member_of_g)
    {
    rec1 = to_members_s(rec1);
    if(rec1 == 0) {printf(" assert(1*"); return(0);}
    }
  if(att1 == &subclass_of_g)
    {
    rec1 = to_subclasses_s(rec1);
    if(rec1 == 0) {printf(" assert(1*"); return(0);}
    }
  if(get_s(att1,&side_effects_g) == &true_g)
    {
    temp1 = non_value_obtain_s(rec1,att1,&if_asserted_g);
    if(temp1 != &nil_g)
      if(side_effects_call_s(temp1,rec1) == 0)
        {printf(" assert(1*"); return(0);}
    }
  return(rec1);
  }

struct unit_t *retract_f(rec1,att1)
struct unit_t *rec1,*att1;
  {
  char *fname="retract";
  extern struct unit_t *value_obtain_s();
  extern struct unit_t name_g,attribute_g,side_effects_g,true_g;
  extern struct unit_t pattern_g,nil_g,aspects_g,if_retracted_g;
  extern struct unit_t concept_g,member_of_g,subclass_of_g;
  struct unit_t *rec2,*temp1,*temp2,*f;
  struct r_element_t *re,*old_re;
  struct e_element_t *ee;
  int i;
  terr_check(rec1,REC,fname,1);
  terr_check(att1,REC,fname,2);
  verr_check(rec1,u_named(rec1),fname,1);
  re = r_start(rec1);
  while(re)
    {
    if(re_attribute(re) == att1)
      perr_check(rec1,!re_bidef(re),fname,1);
    re = re_link(re);
    }
  if(att1 == &name_g)
    {
    rec1 = from_directory_s(rec1);
    if(rec1 == 0) {printf(" retract(1*"); return(0);}
    }
  if(att1 == &member_of_g)
    {
    rec1 = from_members_s(rec1);
    if(rec1 == 0) {printf(" retract(1*"); return(0);}
    }
  if(att1 == &subclass_of_g)
    {
    rec1 = from_subclasses_s(rec1);
    if(rec1 == 0) {printf(" retract(1*"); return(0);}
    }
  if(get_s(att1,&side_effects_g) == &true_g)
    {
    re = r_start(rec1);
    while(re)
      {
      if(re_attribute(re) == att1)
	if(re_value(re) != &nil_g) break;
      re = re_link(re);
      }
    if(re)
      {
      temp1 = non_value_obtain_s(rec1,att1,&if_retracted_g);
      if(temp1 != &nil_g)
	if(side_effects_call_s(temp1,rec1) == 0)
	  {printf(" retract(1*"); return(0);}
      }
    }
  re = r_start(rec1);
  old_re = 0;
  while(re)
    {
    if(re_attribute(re) == att1)
      {
      --u_count(re_attribute(re));
      addto_garbage_s(re_attribute(re));
      --u_count(re_value(re));
      addto_garbage_s(re_value(re));
      if(old_re == 0)
	r_start(rec1) = re_link(re);
      else re_link(old_re) = re_link(re);
      --r_size(rec1);
      if(!re_bidef(re)) free(re);
      }
    old_re = re;
    re = re_link(re);
    }
  return(rec1);
  }

struct unit_t *modify_f(rec1,att1,uni1)
struct unit_t *rec1,*att1,*uni1;
  {
  char *fname="modify";
  extern struct r_element_t *new_re_s();
  extern struct unit_t *value_obtain_s();
  extern struct unit_t member_of_g,subclass_of_g,attribute_g;
  extern struct unit_t name_g,nil_g,side_effects_g,true_g;
  extern struct unit_t if_retracted_g,if_asserted_g;
  extern int assert_ok_s();
  struct unit_t *temp1;
  struct r_element_t *re,*old_re;
  int flag;
  terr_check(rec1,REC,fname,1);
  terr_check(att1,REC,fname,2);
  verr_check(rec1,u_named(rec1) || att1==&name_g,fname,1);
  verr_check(uni1,assert_ok_s(att1,uni1),fname,3);
  re = r_start(rec1);
  while(re)
    {
    if(re_attribute(re) == att1)
      perr_check(rec1,!re_bidef(re),fname,1);
    re = re_link(re);
    }
  if(att1 == &name_g)
    {
    rec1 = from_directory_s(rec1);
    if(rec1 == 0) {printf(" modify(1*"); return(0);}
    }
  if(att1 == &member_of_g)
    {
    rec1 = from_members_s(rec1);
    if(rec1 == 0) {printf(" modify(1*"); return(0);}
    }
  if(att1 == &subclass_of_g)
    {
    rec1 = from_subclasses_s(rec1);
    if(rec1 == 0) {printf(" modify(1*"); return(0);}
    }
  if(get_s(att1,&side_effects_g) == &true_g)
    {
    re = r_start(rec1);
    while(re)
      {
      if(re_attribute(re) == att1)
	if(re_value(re) != &nil_g) break;
      re = re_link(re);
      }
    if(re)
      {
      temp1 = non_value_obtain_s(rec1,att1,&if_retracted_g);
      if(temp1 != &nil_g)
	if(side_effects_call_s(temp1,rec1) == 0)
	  {printf(" modify(1*"); return(0);}
      }
    }
  flag = 0;
  re = r_start(rec1);
  old_re = 0;
  while(re)
    {
    if(re_attribute(re) == att1)
      {
      flag = 1;
      --u_count(re_value(re));
      addto_garbage_s(re_value(re));
      re_value(re) = uni1;
      ++u_count(re_value(re));
      break;
      }
    old_re = re;
    re = re_link(re);
    }
  if(flag == 0)
    {
    if(old_re == 0)
      {
      r_start(rec1) = new_re_s();
      re = r_start(rec1);
      }
    else
      {
      re_link(old_re) = new_re_s();
      re = re_link(old_re);
      }
    ++r_size(rec1);
    re_attribute(re) = att1;
    ++u_count(re_attribute(re));
    re_value(re) = uni1;
    ++u_count(re_value(re));
    }
  if(att1 == &name_g)
    {
    rec1 = to_directory_s(rec1);
    if(rec1 == 0) {printf(" modify(1*"); return(0);}
    }
  if(att1 == &member_of_g)
    {
    rec1 = to_members_s(rec1);
    if(rec1 == 0) {printf(" modify(1*"); return(0);}
    }
  if(att1 == &subclass_of_g)
    {
    rec1 = to_subclasses_s(rec1);
    if(rec1 == 0) {printf(" modify(1*"); return(0);}
    }
  if(get_s(att1,&side_effects_g) == &true_g)
    {
    temp1 = non_value_obtain_s(rec1,att1,&if_asserted_g);
    if(temp1 != &nil_g)
      if(side_effects_call_s(temp1,rec1) == 0)
        {printf(" modify(1*"); return(0);}
    }
  return(rec1);
  }

struct unit_t *revise_f(rec1,lis1)
struct unit_t *rec1,*lis1;
  {
  char *fname="revise";
  extern struct unit_t *value_obtain_s();
  extern struct unit_t name_g,member_of_g,subclass_of_g;
  extern struct unit_t side_effects_g,true_g;
  extern struct unit_t if_retracted_g,nil_g;
  struct unit_t *uni1;
  struct r_element_t *re1,*old_re1,*re2,*old_re2;
  struct l_element_t *le;
  int n;
  terr_check(rec1,REC,fname,1);
  terr_check(lis1,LIS,fname,2);
  re2 = r_start(rec1);
  while(re2)
    {
    if(re_bidef(re2))
      {
      le = l_start(lis1);
      while(le)
	{
	if(le_value(le) == re_attribute(re2)) break;
	le = le_link(le);
	}
      perr_check(lis1,le,fname,2);
      }
    re2 = re_link(re2);
    }
  re1 = r_start(rec1);
  old_re1 = 0;
  n = 0;
  le = l_start(lis1);
  while(le)
    {
    re2 = re1;
    old_re2 = old_re1;
    while(re2)
      {
      if(le_value(le) == re_attribute(re2))
	{
	if(old_re2 == 0) r_start(rec1) = re_link(re2);
	else(re_link(old_re2)) = re_link(re2);
	if(re1 == re2) re1 = re_link(re1);
	re_link(re2) = re1;
	if(old_re1 == 0) r_start(rec1) = re2;
	else re_link(old_re1) = re2;
	old_re1 = re2;
	++n;
	break;
	}
      old_re2 = re2;
      re2 = re_link(re2);
      }
    le = le_link(le);
    }
  re2 = re1;
  while(re2)
    {
    if(re_attribute(re2) == &name_g)
      {
      rec1 = from_directory_s(rec1);
      if(rec1 == 0) {printf(" revise(1*"); return(0);}
      }
    if(re_attribute(re2) == &member_of_g)
      {
      rec1 = from_members_s(rec1);
      if(rec1 == 0) {printf(" revise(1*"); return(0);}
      }
    if(re_attribute(re2) == &subclass_of_g)
      {
      rec1 = from_subclasses_s(rec1);
      if(rec1 == 0) {printf(" revise(1*"); return(0);}
      }
    if(get_s(re_attribute(re2),&side_effects_g) == &true_g)
      {
      uni1 = non_value_obtain_s(rec1,re_attribute(re2),&if_retracted_g);
      if(uni1 != &nil_g)
        if(side_effects_call_s(uni1,rec1) == 0)
	  {printf(" revise(1*"); return(0);}
      }
    --u_count(re_attribute(re2));
    addto_garbage_s(re_attribute(re2));
    --u_count(re_value(re2));
    addto_garbage_s(re_value(re2));
    old_re2 = re2;
    re2 = re_link(re2);
    if(old_re1 == 0) r_start(rec1) = re2;
    else re_link(old_re1) = re2;
    --r_size(rec1);
    if(!re_bidef(old_re2)) free(old_re2);
    }
  return(rec1);
  }

struct unit_t *merge_f(rec1,rec2)
struct unit_t *rec1,*rec2;
  {
  char *fname="merge";
  extern struct r_element_t *new_re_s();
  extern struct unit_t *value_obtain_s(),*get_s(),*contents_s();
  extern struct unit_t attribute_g,name_g,nil_g,true_g;
  extern struct unit_t side_effects_g,if_asserted_g;
  extern struct unit_t member_of_g,subclass_of_g;
  extern int assert_ok_s();
  struct unit_t *temp1;
  struct r_element_t *re1,*old_re1,*re2;
  terr_check(rec1,REC,fname,1);
  terr_check(rec2,REC,fname,2);
  verr_check(rec1,u_named(rec1),fname,1);
  if(get_s(rec1,&member_of_g)==&nil_g && contents_s(rec1,&member_of_g));
    verr_check(rec2,
      assert_ok_s(&member_of_g,contents_s(rec2,&member_of_g)),fname,2);
  if(get_s(rec1,&subclass_of_g)==&nil_g && contents_s(rec1,&subclass_of_g));
    verr_check(rec2,
      assert_ok_s(&subclass_of_g,contents_s(rec2,&subclass_of_g)),fname,2);
  re2 = r_start(rec2);
  while(re2)
    {
    re1 = r_start(rec1);
    old_re1 = 0;
    while(re1)
      {
      if(re_attribute(re1) == re_attribute(re2))
	{
	if(re_value(re1) != &nil_g) goto bottom;
	--u_count(re_value(re1));
	addto_garbage_s(re_value(re1));
	re_value(re1) = re_value(re2);
	++u_count(re_value(re1));
	break;
	}
      old_re1 = re1;
      re1 = re_link(re1);
      }
    if(re1 == 0)
      {
      ++r_size(rec1);
      re1 = new_re_s();
      re_link(old_re1) = re1;
      re_attribute(re1) = re_attribute(re2);
      ++u_count(re_attribute(re1));
      re_value(re1) = re_value(re2);
      ++u_count(re_value(re1));
      }
    if(re_attribute(re1) == &member_of_g)
      {
      rec1 = to_members_s(rec1);
      if(rec1 == 0) {printf(" merge(1*"); return(0);}
      }
    if(re_attribute(re1) == &subclass_of_g)
      {
      rec1 = to_subclasses_s(rec1);
      if(rec1 == 0) {printf(" merge(1*"); return(0);}
      }
    if(get_s(re_attribute(re1),&side_effects_g) == &true_g)
      {
      temp1 = non_value_obtain_s(rec1,re_attribute(re1),&if_asserted_g);
      if(temp1 != &nil_g)
	if(side_effects_call_s(temp1,rec1) == 0)
	  {printf(" merge(1*"); return(0);}
      }
  bottom: re2 = re_link(re2);
    }
  return(rec1);
  }

struct unit_t *value_obtain_s(rec1,att1)
struct unit_t *rec1,*att1;
  {
  extern struct unit_t attribute_g,nil_g,pattern_g,true_g,value_g,aspects_g;
  extern struct unit_t concept_g,member_of_g,subclass_of_g;
  struct unit_t *rec2,*temp1,*temp2;
  int i;
  i = 0;
  rec2 = rec1;
  while(1)
    {
    if(u_type(rec2) != REC) return(&nil_g);
    if(i == 0) temp1 = get_s(rec2,att1);
    else
      {
      temp1 = get_s(rec2,&pattern_g);
      if(temp1 == &nil_g) goto bottom;
      if(u_type(temp1) != REC) return(&nil_g);
      temp1 = get_s(temp1,att1);
      }
    if(temp1 == &nil_g) goto bottom;
    if(u_type(temp1)!=REC) return(temp1);
    temp2 = get_s(temp1,&aspects_g);
    if(temp2 == &nil_g) return(temp1);
    temp1 = get_s(temp2,&value_g);
    if(temp1 != &nil_g) return(temp1);
  bottom:
    if(rec2==&concept_g && i>0) return(&nil_g);
    if(i == 0) rec2 = get_s(rec2,&member_of_g);
    else rec2 = get_s(rec2,&subclass_of_g);
    if(rec2 == &nil_g) return(&nil_g);
    ++i;
    }
  }

struct unit_t *dot_f(rec1)
struct unit_t *rec1;
  {
  char *fname="dot";
  extern struct unit_t variable_g,bindings_g;
  struct unit_t *lis1;
  terr_check(rec1,REC,fname,1);
  cerr_check(rec1,&variable_g,fname,1);
  lis1 = get_s(rec1,&bindings_g);
  verr_check(rec1,u_type(lis1)==LIS,fname,1);
  verr_check(rec1,l_size(lis1)>0,fname,1);
  return(le_value(l_start(lis1)));
  }

struct unit_t *ensure_bindings_s(rec1)
struct unit_t *rec1;
  {
  extern struct unit_t *new_unit_s(),*copy_list_s();
  extern struct r_element_t *new_re_s();
  extern struct unit_t nil_g,bindings_g;
  struct r_element_t *re,*old_re;
  re = r_start(rec1);
  old_re = 0;
  while(re)
    {
    if(re_attribute(re) == &bindings_g)
      {
      if(u_type(re_value(re)) != LIS) return(re_value(re));
      if(u_count(re_value(re)) > 1)
        {
        --u_count(re_value(re));
        addto_garbage_s(re_value(re));
        re_value(re) = copy_list_s(re_value(re));
        ++u_count(re_value(re));
        return(re_value(re));
        }
      return(re_value(re));
      }
    old_re = re;
    re = re_link(re);
    }
  ++r_size(rec1);
  re_link(old_re) = new_re_s();
  re = re_link(old_re);
  re_attribute(re) = &bindings_g;
  ++u_count(re_attribute(re));
  re_value(re) = new_unit_s();
  ++u_count(re_value(re));
  u_type(re_value(re)) = LIS;
  l_size(re_value(re)) = 0;
  l_start(re_value(re)) = 0;
  return(re_value(re));
  }

struct unit_t *new_f(rec1,uni1)
struct unit_t *rec1,*uni1;
  {
  char *fname="new";
  extern struct l_element_t *new_le_s();
  extern struct unit_t variable_g,bindings_g;
  extern struct unit_t control_g,alternatives_g;
  struct unit_t *lis1;
  struct l_element_t *le1;
  terr_check(rec1,REC,fname,1);
  cerr_check(rec1,&variable_g,fname,1);
  perr_check(rec1,rec1!=&control_g,fname,1);
  perr_check(rec1,rec1!=&alternatives_g,fname,1);
  lis1 = ensure_bindings_s(rec1);
  verr_check(rec1,u_type(lis1)==LIS,fname,1);
  le1 = new_le_s();
  le_value(le1) = uni1;
  ++u_count(le_value(le1));
  le_link(le1) = l_start(lis1);
  l_start(lis1) = le1;
  ++l_size(lis1);
  return(rec1);
  }

struct unit_t *set_f(rec1,uni1)
struct unit_t *rec1,*uni1;
  {
  char *fname="set";
  extern struct unit_t variable_g,bindings_g;
  extern struct unit_t control_g,alternatives_g;
  extern struct unit_t *copy_list_s();
  struct unit_t *lis1;
  struct r_element_t *re;
  terr_check(rec1,REC,fname,1);
  cerr_check(rec1,&variable_g,fname,1);
  perr_check(rec1,rec1!=&control_g,fname,1);
  perr_check(rec1,rec1!=&alternatives_g,fname,1);
  re = r_start(rec1);
  while(re)
    {
    if(re_attribute(re) == &bindings_g) break;
    re = re_link(re);
    }
  verr_check(rec1,re,fname,1);
  lis1 = re_value(re);
  verr_check(rec1,u_type(lis1)==LIS,fname,1);
  verr_check(rec1,l_size(lis1)>0,fname,1);
  if(u_count(lis1) > 1)
    {
    --u_count(lis1);
    addto_garbage_s(lis1);
    re_value(re) = copy_list_s(lis1);
    ++u_count(re_value(re));
    lis1 = re_value(re);
    }
  --u_count(le_value(l_start(lis1)));
  addto_garbage_s(le_value(l_start(lis1)));
  le_value(l_start(lis1)) = uni1;
  ++u_count(le_value(l_start(lis1)));
  return(rec1);
  }

struct unit_t *old_f(rec1)
struct unit_t *rec1;
  {
  char *fname="old";
  extern struct unit_t variable_g,bindings_g;
  extern struct unit_t control_g,alternatives_g;
  extern struct unit_t *copy_list_s();
  struct unit_t *lis1;
  struct l_element_t *le1;
  struct r_element_t *re;
  terr_check(rec1,REC,fname,1);
  cerr_check(rec1,&variable_g,fname,1);
  perr_check(rec1,rec1!=&control_g,fname,1);
  perr_check(rec1,rec1!=&alternatives_g,fname,1);
  re = r_start(rec1);
  while(re)
    {
    if(re_attribute(re) == &bindings_g) break;
    re = re_link(re);
    }
  verr_check(rec1,re,fname,1);
  lis1 = re_value(re);
  verr_check(rec1,u_type(lis1)==LIS,fname,1);
  verr_check(rec1,l_size(lis1)>0,fname,1);
  if(u_count(lis1) > 1)
    {
    --u_count(lis1);
    addto_garbage_s(lis1);
    re_value(re) = copy_list_s(lis1);
    ++u_count(re_value(re));
    lis1 = re_value(re);
    }
  le1 = l_start(lis1);
  l_start(lis1) = le_link(l_start(lis1));
  --l_size(lis1);
  --u_count(le_value(le1));
  addto_garbage_s(le_value(le1));
  if(!le_bidef(le1)) free(le1);
  return(rec1);
  }

struct unit_t *determine_f(rec1,att1)
struct unit_t *rec1,*att1;
  {
  char *fname="determine";
  terr_check(rec1,REC,fname,1);
  terr_check(att1,REC,fname,2);
  return(value_obtain_s(rec1,att1));
  }

struct unit_t *estimate_f(rec1,att1)
struct unit_t *rec1,*att1;
  {
  char *fname="estimate";
  extern struct unit_t *non_value_obtain_s();
  extern struct unit_t default_g;
  terr_check(rec1,REC,fname,1);
  terr_check(att1,REC,fname,2);
  return(non_value_obtain_s(rec1,att1,&default_g));
  }

struct unit_t *calculate_f(rec1,att1)
struct unit_t *rec1,*att1;
  {
  char *fname="calculate";
  extern struct unit_t *non_value_obtain_s(),*evaluate_s();
  extern struct unit_t if_needed_g;
  terr_check(rec1,REC,fname,1);
  terr_check(att1,REC,fname,2);
  return(evaluate_s(non_value_obtain_s(rec1,att1,&if_needed_g)));
  }

struct unit_t *obtain_f(rec1,att1,att2)
struct unit_t *rec1,*att1,*att2;
  {
  char *fname="obtain";
  extern struct unit_t attribute_g,nil_g,pattern_g,true_g,value_g,aspects_g;
  extern struct unit_t concept_g,member_of_g,subclass_of_g;
  struct unit_t *rec2,*temp1,*temp2;
  int i;
  terr_check(rec1,REC,fname,1);
  terr_check(att1,REC,fname,2);
  terr_check(att2,REC,fname,3);
  i = 0;
  rec2 = rec1;
  while(1)
    {
    if(u_type(rec2) != REC) return(&nil_g);
    if(i == 0) temp1 = get_s(rec2,att1);
    else
      {
      temp1 = get_s(rec2,&pattern_g);
      if(temp1 == &nil_g) goto bottom;
      if(u_type(temp1) != REC) return(&nil_g);
      temp1 = get_s(temp1,att1);
      }
    if(temp1 == &nil_g) goto bottom;
    if(u_type(temp1)!=REC)
      {
      if(att2 == &value_g) return(temp1);
      else goto bottom;
      }
    temp2 = get_s(temp1,&aspects_g);
    if(temp2 == &nil_g)
      {
      if(att2 == &value_g) return(temp1);
      else goto bottom;
      }
    temp1 = get_s(temp2,att2);
    if(temp1 != &nil_g) return(temp1);
  bottom:
    if(rec2==&concept_g && i>0) return(&nil_g);
    if(i == 0) rec2 = get_s(rec2,&member_of_g);
    else rec2 = get_s(rec2,&subclass_of_g);
    if(rec2 == &nil_g) return(&nil_g);
    ++i;
    }
  }

struct unit_t *path_f(rec1)
struct unit_t *rec1;
  {
  char *fname="path";
  extern struct unit_t *new_unit_s();
  extern struct l_element_t *new_le_s();
  extern struct unit_t concept_g,name_g,nil_g;
  extern struct unit_t member_of_g,class_g,subclass_of_g;
  struct unit_t *result,*rec2;
  struct l_element_t *le;
  terr_check(rec1,REC,fname,1);
  result = new_unit_s();
  u_type(result) = LIS;
  l_start(result) = new_le_s();
  le_value(l_start(result)) = rec1;
  ++u_count(le_value(l_start(result)));
  l_size(result) = 1;
  rec2 = rec1;
  while(rec2 != &concept_g)
    {
    if(u_type(rec2) != REC)
      {
      addto_garbage_s(result);
      return(result);
      }
    if(get_s(rec2,&member_of_g) == &class_g)
      rec2 = get_s(rec2,&subclass_of_g);
    else if(l_size(result) == 1) rec2 = get_s(rec2,&member_of_g);
    else
      {
      addto_garbage_s(result);
      return(result);
      }
    le = new_le_s();
    le_value(le) = rec2;
    ++u_count(le_value(le));
    le_link(le) = l_start(result);
    l_start(result) = le;
    ++l_size(result);
    }
  addto_garbage_s(result);
  return(result);
  }

struct l_element_t *enum_call_s(rec1,lis1,le1)
struct unit_t *rec1,*lis1;
struct l_element_t *le1;
  {
  extern struct l_element_t *new_le_s();
  extern struct unit_t class_g,subclasses_g,members_g;
  struct unit_t *lis2;
  struct l_element_t *le2;
  if(u_type(rec1) != REC) return(0);
  lis2 = get_s(rec1,&members_g);
  if(u_type(lis2) != LIS) return(le1);
  le2 = l_start(lis2);
  while(le2)
    {
    ++l_size(lis1);
    if(le1 == 0)
      {
      l_start(lis1) = new_le_s();
      le1 = l_start(lis1);
      }
    else
      {
      le_link(le1) = new_le_s();
      le1 = le_link(le1);
      }
    le_value(le1) = le_value(le2);
    ++u_count(le_value(le1));
    le2 = le_link(le2);
    }
  lis2 = get_s(rec1,&subclasses_g);
  if(u_type(lis2) == LIS)
    {
    le2 = l_start(lis2);
    while(le2)
      {
      le1 = enum_call_s(le_value(le2),lis1,le1);
      le2 = le_link(le2);
      }
    }
  return(le1);
  }

struct unit_t *enumerate_f(rec1)
struct unit_t *rec1;
  {
  char *fname="enumerate";
  extern struct unit_t *new_unit_s();
  extern struct unit_t class_g;
  struct unit_t *result;
  terr_check(rec1,REC,fname,1);
  cerr_check(rec1,&class_g,fname,1);
  result = new_unit_s();
  u_type(result) = LIS;
  l_size(result) = 0;
  l_start(result) = 0;
  enum_call_s(rec1,result,0);
  return(result);
  }

/* EXPRESSION FUNCTIONS. -----------------------------------------------------*/

struct unit_t *operation_f(exp1)
struct unit_t *exp1;
  {
  char *fname="operation";
  terr_check(exp1,EXP,fname,1);
  return(ee_value(e_start(exp1)));
  }

struct unit_t *application_f(exp1)
struct unit_t *exp1;
  {
  char *fname="application";
  extern struct unit_t *new_unit_s();
  extern struct l_element_t *new_le_s();
  struct unit_t *result;
  struct e_element_t *ee1;
  struct l_element_t *le1;
  terr_check(exp1,EXP,fname,1);
  result = new_unit_s();
  u_type(result) = LIS;
  l_start(result) = 0;
  le1 = 0;
  ee1 = ee_link(e_start(exp1));
  while(ee1)
    {
    if(le1 == 0)
      {
      l_start(result) = new_le_s();
      le1 = l_start(result);
      }
    else
      {
      le_link(le1) = new_le_s();
      le1 = le_link(le1);
      }
    le_value(le1) = ee_value(ee1);
    ++u_count(le_value(le1));
    ee1 = ee_link(ee1);
    }
  l_size(result) = e_size(exp1) - 1;
  addto_garbage_s(result);
  return(result);
  }

struct unit_t *formulate_f(rec1,lis1)
struct unit_t *rec1,*lis1;
  {
  char *fname="formulate";
  extern struct unit_t *new_unit_s();
  extern struct e_element_t *new_ee_s();
  extern struct unit_t function_g;
  struct unit_t *result;
  struct e_element_t *ee1;
  struct l_element_t *le1;
  terr_check(rec1,REC,fname,1);
  terr_check(lis1,LIS,fname,2);
  cerr_check(rec1,&function_g,fname,1);
  result = new_unit_s();
  u_type(result) = EXP;
  ee1 = new_ee_s();
  e_start(result) = ee1;
  ee_value(ee1) = rec1;
  ++u_count(ee_value(ee1));
  le1 = l_start(lis1);
  while(le1)
    {
    ee_link(ee1) = new_ee_s();
    ee1 = ee_link(ee1);
    ee_value(ee1) = le_value(le1);
    ++u_count(ee_value(ee1));
    le1 = le_link(le1);
    }
  e_size(result) = l_size(lis1) + 1;
  addto_garbage_s(result);
  return(result);
  }

/* LOGICAL FUNCTIONS. --------------------------------------------------------*/

struct unit_t *number_f(uni1)
struct unit_t *uni1;
  {
  char *fname="number";
  extern struct unit_t true_g,false_g;
  if(u_type(uni1)==NUM) return(&true_g);
  else return(&false_g);
  }

struct unit_t *token_f(uni1)
struct unit_t *uni1;
  {
  char *fname="token";
  extern struct unit_t true_g,false_g;
  if(u_type(uni1)==TOK) return(&true_g);
  else return(&false_g);
  }

struct unit_t *string_f(uni1)
struct unit_t *uni1;
  {
  char *fname="string";
  extern struct unit_t true_g,false_g;
  if(u_type(uni1)==STR) return(&true_g);
  else return(&false_g);
  }

struct unit_t *list_f(uni1)
struct unit_t *uni1;
  {
  char *fname="list";
  extern struct unit_t true_g,false_g;
  if(u_type(uni1)==LIS) return(&true_g);
  else return(&false_g);
  }

struct unit_t *record_f(uni1)
struct unit_t *uni1;
  {
  char *fname="record";
  extern struct unit_t true_g,false_g;
  if(u_type(uni1)==REC) return(&true_g);
  else return(&false_g);
  }

struct unit_t *expression_f(uni1)
struct unit_t *uni1;
  {
  char *fname="expression";
  extern struct unit_t true_g,false_g;
  if(u_type(uni1)==EXP) return(&true_g);
  else return(&false_g);
  }

struct unit_t *connection_f(uni1)
struct unit_t *uni1;
  {
  char *fname="connection";
  extern struct unit_t true_g,false_g;
  if(u_type(uni1)==CON) return(&true_g);
  else return(&false_g);
  }

struct unit_t *null_f(uni1)
struct unit_t *uni1;
  {
  char *fname="connection";
  extern struct unit_t true_g,false_g,nil_g;
  if(uni1 == &nil_g) return(&true_g);
  else return(&false_g);
  }

int compare_s(uni1,uni2)
struct unit_t *uni1,*uni2;
  {
  extern struct unit_t *get_s();
  extern struct unit_t true_g,false_g,nil_g;
  struct unit_t *name1,*name2;
  struct l_element_t *le1,*le2;
  struct r_element_t *re1,*re2;
  struct e_element_t *ee1,*ee2;
  int i;
  if(uni1 == uni2) return(1);
  if(u_type(uni1) != u_type(uni2)) return(0);
  switch(u_type(uni1))
    {
    case NUM: if(n_value(uni1)==n_value(uni2)) return(1);
	      else return(0);
    case TOK: if(t_size(uni1)!=t_size(uni2)) return(0);
	      for(i=0; i<t_size(uni1); ++i)
		if(t_start(uni1)[i]!=t_start(uni2)[i]) return(0);
	      return(1);
    case STR: if(s_size(uni1)!=s_size(uni2)) return(0);
	      for(i=0; i<s_size(uni1); ++i)
		if(s_start(uni1)[i]!=s_start(uni2)[i]) return(0);
	      return(1);
    case LIS: if(l_size(uni1)!=l_size(uni2)) return(0);
	      le1 = l_start(uni1);
	      le2 = l_start(uni2);
	      while(le1)
		{
		if(!compare_s(le_value(le1),le_value(le2))) return(0);
		le1 = le_link(le1);
		le2 = le_link(le2);
		}
	      return(1);
    case REC: if(u_named(uni1)) return(0);
	      if(u_named(uni2)) return(0);
	      if(r_size(uni1)!=r_size(uni2)) return(0);
	      re1 = r_start(uni1);
	      while(re1)
		{
		re2 = r_start(uni2);
		while(re2)
		  {
		  if(compare_s(re_attribute(re1),re_attribute(re2)))
		    {
		    if(compare_s(re_value(re1),re_value(re2))) break;
		    else return(0);
		    }
		  re2 = re_link(re2);
		  }
		if(re2 == 0) return(0);
		re1 = re_link(re1);
		}
	      return(1);
    case EXP: if(e_size(uni1)!=e_size(uni2)) return(0);
	      ee1 = e_start(uni1);
	      ee2 = e_start(uni2);
	      while(ee1)
		{
		if(!compare_s(ee_value(ee1),ee_value(ee2))) return(0);
		ee1 = ee_link(ee1);
		ee2 = ee_link(ee2);
		}
	      return(1);
    case CON: return(0);
    }
  }

struct unit_t *equal_f(uni1,uni2)
struct unit_t *uni1,*uni2;
  {
  char *fname="equal";
  extern struct unit_t true_g,false_g;
  return(compare_s(uni1,uni2) ? &true_g : &false_g);
  }

struct unit_t *less_f(num1,num2)
struct unit_t *num1,*num2;
  {
  char *fname="less";
  extern struct unit_t true_g,false_g;
  terr_check(num1,NUM,fname,1);
  terr_check(num2,NUM,fname,2);
  if(n_value(num1) < n_value(num2)) return(&true_g);
  else return(&false_g);
  }

struct unit_t *greater_f(num1,num2)
struct unit_t *num1,*num2;
  {
  char *fname="greater";
  extern struct unit_t true_g,false_g;
  terr_check(num1,NUM,fname,1);
  terr_check(num2,NUM,fname,2);
  if(n_value(num1) > n_value(num2)) return(&true_g);
  else return(&false_g);
  }

struct unit_t *in_f(uni1,lis1)
struct unit_t *uni1,*lis1;
  {
  char *fname="in";
  extern struct unit_t true_g,false_g;
  struct l_element_t *le1;
  terr_check(lis1,LIS,fname,2);
  le1 = l_start(lis1);
  while(le1)
    {
    if(compare_s(uni1,le_value(le1))) return(&true_g);
    le1 = le_link(le1);
    }
  return(&false_g);
  }

struct unit_t *subset_f(lis1,lis2)
struct unit_t *lis1,*lis2;
  {
  char *fname="subset";
  extern struct unit_t true_g,false_g;
  struct l_element_t *le1,*le2;
  terr_check(lis1,LIS,fname,1);
  terr_check(lis2,LIS,fname,2);
  le1 = l_start(lis1);
  while(le1)
    {
    le2 = l_start(lis2);
    while(le2)
      {
      if(compare_s(le_value(le1),le_value(le2))) break;
      le2 = le_link(le2);
      }
    if(le2 == 0) return(&false_g);
    le1 = le_link(le1);
    }
  return(&true_g);
  }

int isa_s(rec1,rec2)
struct unit_t *rec1,*rec2;
  {
  extern struct unit_t concept_g,nil_g,member_of_g,class_g,subclass_of_g;
  struct unit_t *rec3;
  rec3 = get_s(rec1,&member_of_g);
  while(1)
    {
    if(rec3 == rec2) return(1);
    if(rec3 == &concept_g) return(0);
    if(rec3 == &nil_g) return(0);
    if(u_type(rec3)!=REC) return(0);
    rec3 = get_s(rec3,&subclass_of_g);
    }
  }

struct unit_t *isa_f(rec1,rec2)
struct unit_t *rec1,*rec2;
  {
  char *fname="isa";
  extern struct unit_t name_g,nil_g,class_g,true_g,false_g;
  terr_check(rec1,REC,fname,1);
  terr_check(rec2,REC,fname,2);
  verr_check(rec1,u_named(rec1),fname,1);
  verr_check(rec2,u_named(rec2),fname,2);
  cerr_check(rec2,&class_g,fname,2);
  return(isa_s(rec1,rec2) ? &true_g : &false_g);
  }

struct unit_t *within_f(rec1,rec2)
struct unit_t *rec1,*rec2;
  {
  char *fname="within";
  extern struct unit_t concept_g,name_g,nil_g;
  extern struct unit_t member_of_g,class_g,subclass_of_g;
  extern struct unit_t true_g,false_g;
  struct unit_t *rec3;
  terr_check(rec1,REC,fname,1);
  terr_check(rec2,REC,fname,2);
  verr_check(rec1,u_named(rec1),fname,1);
  verr_check(rec2,u_named(rec1),fname,2);
  cerr_check(rec1,&class_g,fname,1);
  cerr_check(rec2,&class_g,fname,2);
  rec3 = get_s(rec1,&subclass_of_g);
  while(1)
    {
    if(rec3 == rec2) return(&true_g);
    if(rec3 == &concept_g) return(&false_g);
    if(u_type(rec3)!=REC) return(&false_g);
    if(get_s(rec3,&member_of_g)!=&class_g) return(&false_g);
    rec3 = get_s(rec3,&subclass_of_g);
    }
  }

struct unit_t *not_f(rec1)
struct unit_t *rec1;
  {
  char *fname="not";
  extern struct unit_t true_g,false_g,boolean_g;
  if(rec1==&true_g) return(&false_g);
  if(rec1==&false_g) return(&true_g);
  terr_check(rec1,REC,fname,1);
  cerr_check(rec1,&boolean_g,fname,1);
  verr_check(rec1,0,fname,1);
  }

struct unit_t *and_f(rec1,rec2)
struct unit_t *rec1,*rec2;
  {
  char *fname="and";
  extern struct unit_t true_g,false_g,boolean_g;
  if(rec1 == &false_g)
    {
    if(rec2==&true_g || rec2==&false_g) return(&false_g);
    terr_check(rec2,REC,fname,2);
    cerr_check(rec2,&boolean_g,fname,2);
    verr_check(rec2,0,fname,2);
    }
  if(rec1 == &true_g)
    {
    if(rec2 == &true_g) return(&true_g);
    if(rec2 == &false_g) return(&false_g);
    terr_check(rec2,REC,fname,2);
    cerr_check(rec2,&boolean_g,fname,2);
    verr_check(rec2,0,fname,2);
    }
  terr_check(rec1,REC,fname,1);
  cerr_check(rec1,&boolean_g,fname,1);
  verr_check(rec1,0,fname,1);
  }

struct unit_t *or_f(rec1,rec2)
struct unit_t *rec1,*rec2;
  {
  char *fname="or";
  extern struct unit_t true_g,false_g,boolean_g;
  if(rec1 == &true_g)
    {
    if(rec2==&true_g || rec2==&false_g) return(&true_g);
    terr_check(rec2,REC,fname,2);
    cerr_check(rec2,&boolean_g,fname,2);
    verr_check(rec2,0,fname,2);
    }
  if(rec1 == &false_g)
    {
    if(rec2 == &true_g) return(&true_g);
    if(rec2 == &false_g) return(&false_g);
    terr_check(rec2,REC,fname,2);
    cerr_check(rec2,&boolean_g,fname,2);
    verr_check(rec2,0,fname,2);
    }
  terr_check(rec1,REC,fname,1);
  cerr_check(rec1,&boolean_g,fname,1);
  verr_check(rec1,0,fname,1);
  }

struct unit_t *quan_call_s(lis1,lis2,exp1,rec1,rec2)
struct unit_t *lis1,*lis2,*exp1,*rec1,*rec2;
  {
  extern struct unit_t *evaluate_s();
  extern int status_g;
  struct unit_t *temp;
  struct l_element_t *le1;
  status_g = OK;
  if(u_type(lis1) != LIS) return(rec2);
  le1 = l_start(lis1);
  while(le1)
    {
    if(l_start(lis2) == 0) return(rec2);
    --u_count(le_value(l_start(lis2)));
    addto_garbage_s(le_value(l_start(lis2)));
    le_value(l_start(lis2)) = le_value(le1);
    ++u_count(le_value(l_start(lis2)));
    temp = evaluate_s(exp1);
    if(temp == 0)
      {status_g = BAD; return(0);}
    if(temp == rec1) return(rec1);
    le1 = le_link(le1);
    }
  return(rec2);
  }

struct unit_t *exists_f(lis1,rec1,exp1)
struct unit_t *lis1,*rec1,*exp1;
  {
  char *fname="exists";
  extern struct l_element_t *new_le_s();
  extern struct unit_t variable_g,nil_g,true_g,false_g;
  extern int status_g;
  struct unit_t *result,*lis2;
  struct l_element_t *le2;
  terr_check(lis1,LIS,fname,1);
  terr_check(rec1,REC,fname,2);
  terr_check(exp1,EXP,fname,3);
  cerr_check(rec1,&variable_g,fname,2);
  lis2 = ensure_bindings_s(rec1);
  ++l_size(lis2);
  le2 = new_le_s();
  le_value(le2) = &nil_g;
  ++u_count(le_value(le2));
  le_link(le2) = l_start(lis2);
  l_start(lis2) = le2;
  result = quan_call_s(lis1,lis2,exp1,&true_g,&false_g);
  if(l_start(lis2) != 0)
    {
    --l_size(lis2);
    le2 = l_start(lis2);
    l_start(lis2) = le_link(le2);
    --u_count(le_value(le2));
    addto_garbage_s(le_value(le2));
    if(!le_bidef(le2)) free(le2);
    }
  if(status_g == BAD)
    {printf(" exists(3*"); return(0);}
  return(result);
  }

struct unit_t *every_f(lis1,rec1,exp1)
struct unit_t *lis1,*rec1,*exp1;
  {
  char *fname="every";
  extern struct l_element_t *new_le_s();
  extern struct unit_t variable_g,nil_g,true_g,false_g;
  extern int status_g;
  struct unit_t *result,*lis2;
  struct l_element_t *le2;
  terr_check(lis1,LIS,fname,1);
  terr_check(rec1,REC,fname,2);
  terr_check(exp1,EXP,fname,3);
  cerr_check(rec1,&variable_g,fname,2);
  lis2 = ensure_bindings_s(rec1);
  ++l_size(lis2);
  le2 = new_le_s();
  le_value(le2) = &nil_g;
  ++u_count(le_value(le2));
  le_link(le2) = l_start(lis2);
  l_start(lis2) = le2;
  result = quan_call_s(lis1,lis2,exp1,&false_g,&true_g);
  if(l_start(lis2) != 0)
    {
    --l_size(lis2);
    le2 = l_start(lis2);
    l_start(lis2) = le_link(le2);
    --u_count(le_value(le2));
    addto_garbage_s(le_value(le2));
    if(!le_bidef(le2)) free(le2);
    }
  if(status_g == BAD)
    {printf(" every(3*"); return(0);}
  return(result);
  }

struct l_element_t *whic_call_s(lis1,lis2,exp1,lis3,le3)
struct unit_t *lis1,*lis2,*exp1,*lis3;
struct l_element_t *le3;
  {
  extern struct unit_t *evaluate_s();
  extern struct l_element_t *new_le_s();
  extern struct unit_t true_g;
  extern int status_g;
  struct unit_t *temp;
  struct l_element_t *le1;
  status_g = OK;
  if(u_type(lis1) != LIS) return(le3);
  le1 = l_start(lis1);
  while(le1)
    {
    if(l_start(lis2) == 0) return(le3);
    --u_count(le_value(l_start(lis2)));
    addto_garbage_s(le_value(l_start(lis2)));
    le_value(l_start(lis2)) = le_value(le1);
    ++u_count(le_value(l_start(lis2)));
    temp = evaluate_s(exp1);
    if(temp == 0)
      {status_g = BAD; return(0);}
    if(temp == &true_g)
      {
      ++l_size(lis3);
      if(le3 == 0)
	{
	l_start(lis3) = new_le_s();
	le3 = l_start(lis3);
	}
      else
	{
	le_link(le3) = new_le_s();
	le3 = le_link(le3);
	}
      le_value(le3) = le_value(le1);
      ++u_count(le_value(le3));
      }
    le1 = le_link(le1);
    }
  return(le3);
  }

struct unit_t *which_f(lis1,rec1,exp1)
struct unit_t *lis1,*rec1,*exp1;
  {
  char *fname="which";
  extern struct unit_t *new_unit_s();
  extern struct l_element_t *new_le_s();
  extern struct unit_t variable_g,nil_g;
  extern int status_g;
  struct unit_t *result,*lis2;
  struct l_element_t *le2,*le3;
  terr_check(lis1,LIS,fname,1);
  terr_check(rec1,REC,fname,2);
  terr_check(exp1,EXP,fname,3);
  cerr_check(rec1,&variable_g,fname,2);
  result = new_unit_s();
  u_type(result) = LIS;
  l_size(result) = 0;
  l_start(result) = 0;
  lis2 = ensure_bindings_s(rec1);
  ++l_size(lis2);
  le2 = new_le_s();
  le_value(le2) = &nil_g;
  ++u_count(le_value(le2));
  le_link(le2) = l_start(lis2);
  l_start(lis2) = le2;
  whic_call_s(lis1,lis2,exp1,result,0);
  if(l_start(lis2) != 0)
    {
    --l_size(lis2);
    le2 = l_start(lis2);
    l_start(lis2) = le_link(le2);
    --u_count(le_value(le2));
    addto_garbage_s(le_value(le2));
    if(!le_bidef(le2)) free(le2);
    }
  if(status_g == BAD)
    {
    le3 = l_start(result);
    while(le3)
      {
      --u_count(le_value(le3));
      addto_garbage_s(le_value(le3));
      le2 = le_link(le3);
      if(!le_bidef(le3)) free(le3);
      le3 = le2;
      }
    free(result);
    printf(" which(3*");
    return(0);
    }
  return(result);
  }

/* I/O FUNCTIONS. ------------------------------------------------------------*/

struct unit_t *parse_f()
  {
  char *fname="parse";
  extern struct unit_t *parse_s();
  extern int i_level_g,status_g;
  struct unit_t *result;
  int i_level_save,i;
  i_level_save = i_level_g;
  i_level_g = 0;
  do result=parse_s(TER); while (status_g!=OK);
  while(from_s(TER) != '\n');
  addto_garbage_s(result);
  i_level_g = i_level_save;
  return(result);
  }

struct unit_t *display_f(uni1)
struct unit_t *uni1;
  {
  char *fname="display";
  extern struct unit_t *display_s();
  extern int i_level_g,column_g;
  int i_level_save;
  i_level_save = i_level_g;
  i_level_g = 0;
  column_g = 0;
  display_s(TER,uni1,1);
  putc('\n',stdout);
  i_level_g = i_level_save;
  return(uni1);
  }

struct unit_t *input_f()
  {
  char *fname="input";
  extern struct unit_t *new_unit_s();
  struct unit_t *result;
  int lim,i;
  char ch;
  result = new_unit_s();
  u_type(result) = STR;
  s_start(result) = (char *) malloc(32);
  lim = 32;
  i = 0;
  do
    {
    ch = from_s(TER);
    if(i > lim-1)
      s_start(result) = (char *) realloc(s_start(result),lim*=2);
    s_start(result)[i++] = ch;
    } while(ch != '\n');
  s_start(result) = (char *) realloc(s_start(result),i+1);
  s_start(result)[i] = '\0';
  s_size(result) = i;
  addto_garbage_s(result);
  return(result);
  }

struct unit_t *output_f(str1)
struct unit_t *str1;
  {
  char *fname="output";
  terr_check(str1,STR,fname,1);
  printf("%s",s_start(str1));
  return(str1);
  }

struct unit_t *format_f(str1,lis1)
struct unit_t *str1,*lis1;
  {
  char *fname="format";
  extern struct unit_t *new_unit_s();
  extern struct unit_t *display_s(),*evaluate_s();
  extern char *outstring_g,*outchar_g;
  extern int outstringlim_g,i_level_g,column_g;
  struct unit_t *result,*temp;
  struct l_element_t *le;
  char *to,*from;
  int tostringlim,n,i,j,i_level_save;
  terr_check(str1,STR,fname,1);
  terr_check(lis1,LIS,fname,2);
  to = (char *) malloc(32);
  to[0] = '\0';
  tostringlim = 32;
  from = s_start(str1);
  i = 0;
  j = 0;
  while(from[j] != '\0')
    {
    n = 0;
    if(from[j] != '^') to[i++] = from[j++];
    else if(from[j+1] == '^') {j++; to[i++] = from[j++];}
    else if(from[j+1]<'0' || from[j+1]>'9') j++;
    else
      {
      j++;
      while(from[j]>='0' && from[j]<='9') n = n*10 + (from[j++]-'0');
      for(le=l_start(lis1); le && n>1; --n) le=le_link(le);
      if(le)
        {
        temp = evaluate_s(le_value(le));
        if(temp == 0)
          {printf(" format(2*"); return(0);}
        if(u_type(temp) == STR)
          {
          while(i + strlen(s_start(temp)) > tostringlim-1)
            to = (char *) realloc(to,tostringlim*=2);
          strcat(to,s_start(temp));
          i += strlen(s_start(temp));
          }
        else
          {
          outstring_g = (char *) malloc(32);
          outchar_g = outstring_g;
          outstringlim_g = 32;
          i_level_save = i_level_g;
          i_level_g = 0;
          column_g = 0;
          display_s(MEM,temp,1);
          i_level_g = i_level_save;
          while(i + strlen(outstring_g) > tostringlim-1)
            to = (char *) realloc(to,tostringlim*=2);
          strcat(to,outstring_g);
          i += strlen(outstring_g);
          }
        }
      }
    if(i >= tostringlim-1)
      to = (char *) realloc(to,tostringlim*=2);
    to[i] = '\0';
    }
  to = (char *) realloc(to,i+1);
  to[i] = '\0';
  result = new_unit_s();
  u_type(result) = STR;
  s_size(result) = i;
  s_start(result) = to;
  return(result);
  }

save_call_s(rec1)
struct unit_t *rec1;
  {
  extern struct unit_t *new_unit_s();
  extern struct r_element_t *new_re_s();
  extern struct unit_t *display_s();
  extern struct unit_t subclasses_g,members_g;
  extern int i_level_g,column_g;
  struct unit_t *lis1,*uni2;
  struct l_element_t *le1;
  struct r_element_t *re1,*re2;
  int i_level_save;
  if(u_type(rec1) != REC) return;
  lis1 = get_s(rec1,&members_g);
  if(u_type(lis1) != LIS) return;
  le1 = l_start(lis1);
  while(le1)
    {
    if(u_type(le_value(le1)) != REC) goto bottom;
    if(u_bidef(le_value(le1)))
      {
      re1 = r_start(le_value(le1));
      while(re1)
	{
	if(!re_bidef(re1)) break;
	re1 = re_link(re1);
	}
      if(re1 == 0) goto bottom;
      to_s(FIL,'m'); to_s(FIL,'e'); to_s(FIL,'r');
      to_s(FIL,'g'); to_s(FIL,'e');
      to_s(FIL,'('); to_s(FIL,'\n'); to_s(FIL,' ');
      i_level_save = i_level_g;
      i_level_g = 1;
      column_g = 1;
      display_s(FIL,le_value(le1),2);
      i_level_g = i_level_save;
      to_s(FIL,'\n'); to_s(FIL,' ');
      uni2 = new_unit_s();
      u_type(uni2) = REC;
      r_size(uni2) = 1;
      r_start(uni2) = new_re_s();
      re2 = r_start(uni2);
      re_attribute(re2) = re_attribute(re1);
      ++u_count(re_attribute(re2));
      re_value(re2) = re_value(re1);
      ++u_count(re_value(re2));
      re1 = re_link(re1);
      while(re1)
	{
	if(!re_bidef(re1))
	  {
	  ++r_size(uni2);
	  re_link(re2) = new_re_s();
	  re2 = re_link(re2);
	  re_attribute(re2) = re_attribute(re1);
	  ++u_count(re_attribute(re2));
	  re_value(re2) = re_value(re1);
	  ++u_count(re_value(re2));
	  }
	re1 = re_link(re1);
	}
      addto_garbage_s(uni2);
      i_level_save = i_level_g;
      i_level_g = 1;
      column_g = 1;
      display_s(FIL,uni2,1);
      i_level_g = i_level_save;
      to_s(FIL,'\n'); to_s(FIL,' '); to_s(FIL,')');
      to_s(FIL,'\n'); to_s(FIL,'\n');
      }
    else
      {
      to_s(FIL,'d'); to_s(FIL,'e'); to_s(FIL,'f');
      to_s(FIL,'i'); to_s(FIL,'n'); to_s(FIL,'e');
      to_s(FIL,'('); to_s(FIL,'\n'); to_s(FIL,' ');
      i_level_save = i_level_g;
      i_level_g = 1;
      column_g = 1;
      display_s(FIL,le_value(le1),1);
      i_level_g = i_level_save;
      to_s(FIL,'\n'); to_s(FIL,' '); to_s(FIL,')');
      to_s(FIL,'\n'); to_s(FIL,'\n');
      }
    bottom: le1 = le_link(le1);
    }
  lis1 = get_s(rec1,&subclasses_g);
  if(u_type(lis1) == LIS)
    {
    le1 = l_start(lis1);
    while(le1)
      {
      save_call_s(le_value(le1));
      le1 = le_link(le1);
      }
    }
  }

struct unit_t *save_f(str1)
struct unit_t *str1;
  {
  char *fname="save";
  extern struct unit_t concept_g;
  extern FILE *fopen(),*fclose();
  extern FILE *outfile_g;
  FILE *fp;
  terr_check(str1,STR,fname,1);
  fp = fopen(s_start(str1),"w");
  verr_check(str1,fp != NULL,fname,1);
  outfile_g = fp;
  save_call_s(&concept_g);
  fclose(fp);
  return(str1);
  }

struct unit_t *stash_f(str1,lis1)
struct unit_t *str1,*lis1;
  {
  char *fname="stash";
  extern struct unit_t *display_s();
  extern FILE *fopen(),*fclose();
  extern FILE *outfile_g;
  extern int i_level_g,column_g;
  struct l_element_t *le;
  FILE *fp;
  int i_level_save;
  terr_check(str1,STR,fname,1);
  terr_check(lis1,LIS,fname,2);
  fp = fopen(s_start(str1),"w");
  verr_check(str1,fp != NULL,fname,1);
  outfile_g = fp;
  le = l_start(lis1);
  while(le)
    {
    to_s(FIL,'d'); to_s(FIL,'e'); to_s(FIL,'f');
    to_s(FIL,'i'); to_s(FIL,'n'); to_s(FIL,'e');
    to_s(FIL,'('); to_s(FIL,'\n'); to_s(FIL,' ');
    i_level_save = i_level_g;
    i_level_g = 1;
    column_g = 1;
    display_s(FIL,le_value(le),1);
    i_level_g = i_level_save;
    to_s(FIL,'\n'); to_s(FIL,' '); to_s(FIL,')');
    to_s(FIL,'\n'); to_s(FIL,'\n');
    le = le_link(le);
    }
  fclose(fp);
  return(str1);
  }

struct unit_t *load_f(str1)
struct unit_t *str1;
  {
  char *fname="load";
  extern struct unit_t *parse_s(),*display_s(),*evaluate_s();
  extern FILE *fopen(),*fclose();
  extern FILE *infile_g;
  extern int i_level_g,status_g,line_g;
  struct unit_t *uni1;
  char ch;
  FILE *fp,*old_infile;
  int i_level_save;
  terr_check(str1,STR,fname,1);
  fp = fopen(s_start(str1),"r");
  verr_check(str1,fp != NULL,fname,1);
  old_infile = infile_g;
  infile_g = fp;
  i_level_save = i_level_g;
  i_level_g = 0;
  line_g = 1;
  while((ch=eat_s(FIL)) != EOF)
    {
    unfrom_s(FIL,ch);
    uni1 = parse_s(FIL);
    if(status_g != OK)
      {
      printf("line %d in file ",line_g);
      display_s(TER,str1,1);
      printf("\n----------\n");
      printf(" load(1*");
      fclose(fp);
      infile_g = old_infile;
      i_level_g = i_level_save;
      return(0);
      }
    addto_garbage_s(uni1);
    if(evaluate_s(uni1) == 0)
      {
      printf(" load(1* (line %d in file ",line_g);
      display_s(TER,str1,1);
      printf(")");
      fclose(fp);
      infile_g = old_infile;
      i_level_g = i_level_save;
      return(0);
      }
    }
  fclose(fp);
  infile_g = old_infile;
  i_level_g = i_level_save;
  return(str1);
  }

struct unit_t *read_f(str1)
struct unit_t *str1;
  {
  char *fname="read";
  extern struct unit_t *new_unit_s();
  extern FILE *fopen(),*fclose();
  struct unit_t *result;
  FILE *fp;
  char ch;
  int lim,i;
  terr_check(str1,STR,fname,1);
  result = new_unit_s();
  u_type(result) = STR;
  s_start(result) = (char *) malloc(32);
  lim = 32;
  i = 0;
  fp = fopen(s_start(str1),"r");
  verr_check(str1,fp != NULL,fname,1);
  while((ch=getc(fp)) != EOF)
    {
    if(i > lim-1)
      s_start(result) = (char *) realloc(s_start(result),lim*=2);
    s_start(result)[i++] = ch;
    }
  s_start(result) = (char *) realloc(s_start(result),i+1);
  s_start(result)[i] = '\0';
  s_size(result) = i;
  fclose(fp);
  return(result);
  }

struct unit_t *write_f(str1,str2)
struct unit_t *str1,*str2;
  {
  char *fname="write";
  extern FILE *fopen(),*fclose();
  FILE *fp;
  terr_check(str1,STR,fname,1);
  terr_check(str2,STR,fname,2);
  fp = fopen(s_start(str1),"w");
  verr_check(str1,fp != NULL,fname,1);
  fprintf(fp,"%s",s_start(str2));
  fclose(fp);
  return(str1);
  }

struct unit_t *extend_f(str1,str2)
struct unit_t *str1,*str2;
  {
  char *fname="extend";
  extern FILE *fopen(),*fclose();
  FILE *fp;
  terr_check(str1,STR,fname,1);
  terr_check(str2,STR,fname,2);
  fp = fopen(s_start(str1),"a");
  verr_check(str1,fp != NULL,fname,1);
  fprintf(fp,"%s",s_start(str2));
  fclose(fp);
  return(str1);
  }

struct unit_t *spell_f(uni1)
struct unit_t *uni1;
  {
  char *fname="spell";
  extern struct unit_t *new_unit_s();
  extern struct unit_t *display_s();
  extern char *outstring_g,*outchar_g;
  extern int outstringlim_g,i_level_g,column_g;
  struct unit_t *result;
  int i_level_save;
  outstring_g = (char *) malloc(32);
  outchar_g = outstring_g;
  outstringlim_g = 32;
  i_level_save = i_level_g;
  i_level_g = 0;
  column_g = 0;
  display_s(MEM,uni1,1);
  result = new_unit_s();
  u_type(result) = STR;
  s_size(result) = outchar_g - outstring_g;
  s_start(result) = (char *) realloc(outstring_g,outchar_g-outstring_g+1);
  *outchar_g = '\0';
  i_level_g = i_level_save;
  return(result);
  }

struct unit_t *unspell_f(str1)
struct unit_t *str1;
  {
  char *fname="unspell";
  extern struct unit_t *parse_s(),*display_s();
  extern char *instring_g,*inchar_g;
  extern int status_g;
  struct unit_t *result;
  char *save_instring,*save_inchar;
  terr_check(str1,STR,fname,1);
  save_instring = instring_g;
  save_inchar = inchar_g;
  instring_g = s_start(str1);
  inchar_g = instring_g;
  result = parse_s(MEM);
  if(status_g == BAD)
    {
    printf("character position %d in...\n",inchar_g-instring_g);
    display_s(TER,str1,1);
    printf("\n----------\n");
    printf(" unspell(*");
    instring_g = save_instring;
    inchar_g = save_inchar;
    return(0);
    }
  instring_g = save_instring;
  inchar_g = save_inchar;
  return(result);
  }

struct unit_t *scan_f(str1)
struct unit_t *str1;
  {
  char *fname="scan";
  extern struct unit_t *parse_s();
  extern struct unit_t nil_g;
  extern char *instring_g,*inchar_g;
  extern int status_g;
  struct unit_t *result;
  char *save_instring,*save_inchar;
  terr_check(str1,STR,fname,1);
  save_instring = instring_g;
  save_inchar = inchar_g;
  instring_g = s_start(str1);
  inchar_g = instring_g;
  result = parse_s(SIL);
  if(status_g == BAD)
    {
    instring_g = save_instring;
    inchar_g = save_inchar;
    return(&nil_g);
    }
  instring_g = save_instring;
  inchar_g = save_inchar;
  return(result);
  }

/* EVALUATION FUNCTIONS. -----------------------------------------------------*/

struct unit_t *quote_f(uni1)
struct unit_t *uni1;
  {
  char *fname="quote";
  return(0);
  }

struct unit_t *evaluate_f(uni1)
struct unit_t *uni1;
  {
  char *fname="evaluate";
  extern struct unit_t *evaluate_s();
  struct unit_t *result;
  result = evaluate_s(uni1);
  if(result == 0) printf(" evaluate(1*");
  return(result);
  }

struct unit_t *prepare_f(uni1)
struct unit_t *uni1;
  {
  char *fname="prepare";
  extern struct unit_t *new_unit_s();
  extern struct l_element_t *new_le_s();
  extern struct r_element_t *new_re_s();
  extern struct unit_t *evaluate_s();
  struct unit_t *result;
  struct l_element_t *le1,*le2;
  struct r_element_t *re1,*re2;
  if(u_type(uni1) == LIS)
    {
    result = new_unit_s();
    u_type(result) = LIS;
    l_size(result) = 0;
    l_start(result) = 0;
    le1 = l_start(uni1);
    while(le1)
      {
      ++l_size(result);
      if(l_start(result) == 0)
	{
	l_start(result) = new_le_s();
	le2 = l_start(result);
	}
      else
	{
	le_link(le2) = new_le_s();
	le2 = le_link(le2);
	}
      le_value(le2) = evaluate_s(le_value(le1));
      if(le_value(le2) == 0)
	{le_link(le2)=0; printf(" prepare(1*");
	 addto_garbage_s(result); return(0);}
      ++u_count(le_value(le2));
      le1 = le_link(le1);
      }
    addto_garbage_s(result);
    return(result);
    }
  else if(u_type(uni1) == REC)
    {
    result = new_unit_s();
    u_type(result) = REC;
    r_size(result) = 0;
    r_start(result) = 0;
    re1 = r_start(uni1);
    while(re1)
      {
      ++r_size(result);
      if(r_start(result) == 0)
	{
	r_start(result) = new_re_s();
	re2 = r_start(result);
	}
      else
	{
	re_link(re2) = new_re_s();
	re2 = re_link(re2);
	}
      re_attribute(re2) = evaluate_s(re_attribute(re1));
      if(re_attribute(re2) == 0)
	{re_value(re2)=0; re_link(re2)=0;
	 printf(" prepare(1*"); addto_garbage_s(result); return(0);}
      ++u_count(re_attribute(re2));
      re_value(re2) = evaluate_s(re_value(re1));
      if(re_value(re2) == 0)
	{re_link(re2)=0; printf(" prepare(1*"); return(0);}
      ++u_count(re_value(re2));
      re1 = re_link(re1);
      }
    addto_garbage_s(result);
    return(result);
    }
  else return(evaluate_s(uni1));
  }

struct unit_t *apply_f(rec1,lis1)
struct unit_t *rec1,*lis1;
  {
  char *fname="apply";
  extern struct unit_t *new_unit_s();
  extern struct e_element_t *new_ee_s();
  extern struct unit_t *evaluate_s();
  extern struct unit_t function_g;
  struct unit_t *uni1;
  struct l_element_t *le;
  struct e_element_t *ee;
  terr_check(rec1,REC,fname,1);
  terr_check(lis1,LIS,fname,2);
  cerr_check(rec1,&function_g,fname,1);
  uni1 = new_unit_s();
  u_type(uni1) = EXP;
  e_size(uni1) = l_size(lis1) + 1;
  ee = new_ee_s();
  e_start(uni1) = ee;
  ee_value(ee) = rec1;
  ++u_count(ee_value(ee));
  le = l_start(lis1);
  while(le)
    {
    ee_link(ee) = new_ee_s();
    ee = ee_link(ee);
    ee_value(ee) = le_value(le);
    ++u_count(ee_value(ee));
    le = le_link(le);
    }
  addto_garbage_s(uni1);
  if((uni1 = evaluate_s(uni1)) == 0)
    {printf(" apply(1*"); return(0);}
  return(uni1);
  }

struct unit_t *if_f(rec1,uni1)
struct unit_t *rec1,*uni1;
  {
  char *fname="if";
  extern struct unit_t *evaluate_s();
  extern struct unit_t true_g,false_g,nil_g,boolean_g;
  struct unit_t *uni2;
  if(rec1 == &true_g)
    {
    if((uni2 = evaluate_s(uni1)) == 0)
      {printf(" if(2*"); return(0);}
    return(uni2);
    }
  if(rec1 == &false_g) return(&nil_g);
  terr_check(rec1,REC,fname,1);
  cerr_check(rec1,&boolean_g,fname,1);
  verr_check(rec1,0,fname,1);
  }

struct unit_t *ifelse_f(rec1,uni1,uni2)
struct unit_t *rec1,*uni1,*uni2;
  {
  char *fname="ifelse";
  extern struct unit_t *evaluate_s();
  extern struct unit_t true_g,false_g,nil_g,boolean_g;
  struct unit_t *uni3;
  if(rec1 == &true_g)
    {
    if((uni3 = evaluate_s(uni1)) == 0)
      {printf(" ifelse(2*"); return(0);}
    return(uni3);
    }
  if(rec1 == &false_g)
    {
    if((uni3 = evaluate_s(uni2)) == 0)
      {printf(" ifelse(3*"); return(0);}
    return(uni3);
    }
  terr_check(rec1,REC,fname,1);
  cerr_check(rec1,&boolean_g,fname,1);
  verr_check(rec1,0,fname,1);
  }

struct unit_t *branch_f(uni1,lis1)
struct unit_t *uni1,*lis1;
  {
  char *fname="branch";
  extern struct unit_t *evaluate_s(),*contents_s();
  extern struct unit_t nil_g,case_g,action_g;
  struct unit_t *uni2;
  struct l_element_t *le;
  int i;
  terr_check(lis1,LIS,fname,2);
  le = l_start(lis1);
  while(le)
    {
    verr_check(lis1,u_type(le_value(le)) == REC,fname,2);
    verr_check(lis1,contents_s(le_value(le),&case_g) != 0,fname,2);
    verr_check(lis1,contents_s(le_value(le),&action_g) != 0,fname,2);
    le = le_link(le);
    }
  le = l_start(lis1);
  i = 1;
  while(le)
    {
    uni2 = evaluate_s(get_s(le_value(le),&case_g));
    if(uni2 == 0)
      {printf(" {case [%d branch(2*",i); return(0);}
    if(compare_s(uni1,uni2))
      {
      uni2 = evaluate_s(get_s(le_value(le),&action_g));
      if(uni2 == 0)
	{printf(" {action [%d branch(2*",i); return(0);}
      return(uni2);
      }
    le = le_link(le);
    ++i;
    }
  return(&nil_g);
  }

struct unit_t *do_f(lis1)
struct unit_t *lis1;
  {
  char *fname="do";
  extern struct unit_t *evaluate_s(),*contents_s();
  extern struct unit_t result_value_g,nil_g;
  struct unit_t *uni1,*uni2;
  struct l_element_t *le;
  int i;
  terr_check(lis1,LIS,fname,1);
  le = l_start(lis1);
  i = 1;
  while(le)
    {
    uni1 = evaluate_s(le_value(le));
    if(uni1 == 0)
      {printf(" [%d do(1*",i); return(0);}
    if(u_type(uni1) == REC)
      if((uni2 = contents_s(uni1,&result_value_g))) return(uni2);
    le = le_link(le);
    ++i;
    }
  return(&nil_g);
  }

struct unit_t *repeat_f(lis1)
struct unit_t *lis1;
  {
  char *fname="repeat";
  extern struct unit_t *evaluate_s(),*contents_s();
  extern struct unit_t break_value_g,skip_value_g,nil_g;
  struct unit_t *uni1,*uni2;
  struct l_element_t *le;
  int i;
  terr_check(lis1,LIS,fname,1);
  while(1)
    {
    le = l_start(lis1);
    i = 1;
    while(le)
      {
      uni1 = evaluate_s(le_value(le));
      if(uni1 == 0)
	{printf(" [%d repeat(1*",i); return(0);}
      if(u_type(uni1) == REC)
	{
	if((uni2 = contents_s(uni1,&break_value_g))) return(uni2);
	if(contents_s(uni1,&skip_value_g)) break;
	}
      le = le_link(le);
      ++i;
      }
    }
  }

struct unit_t *while_f(uni1,lis1)
struct unit_t *uni1,*lis1;
  {
  char *fname="while";
  extern struct unit_t *evaluate_s(),*contents_s();
  extern struct unit_t break_value_g,skip_value_g,nil_g;
  extern struct unit_t true_g,false_g,boolean_g;
  struct unit_t *uni2,*uni3;
  struct l_element_t *le;
  int i;
  terr_check(lis1,LIS,fname,2);
  while(1)
    {
    uni2 = evaluate_s(uni1);
    if(uni2 == 0)
      {printf(" while(1*"); return(0);}
    if(uni2 == &false_g) return(&nil_g);
    if(uni2 != &true_g)
      {
      if(u_type(uni2) != REC)
        {terr_s(0,0,uni2); printf(" while(1*"); return(0);}
      if(!isa_s(uni2,&boolean_g))
        {cerr_s(0,0,uni2); printf(" while(1*"); return(0);}
      verr_s(0,0,uni2); printf(" while(1*"); return(0);
      }
    le = l_start(lis1);
    i = 1;
    while(le)
      {
      uni2 = evaluate_s(le_value(le));
      if(uni2 == 0)
	{printf(" [%d while(2*",i); return(0);}
      if(u_type(uni2) == REC)
	{
	if((uni3 = contents_s(uni2,&break_value_g))) return(uni3);
	if(contents_s(uni2,&skip_value_g)) break;
	}
      le = le_link(le);
      ++i;
      }
    }
  }

struct unit_t *for_f(uni1,uni2,uni3,lis1)
struct unit_t *uni1,*uni2,*uni3,*lis1;
  {
  char *fname="for";
  extern struct unit_t *evaluate_s(),*contents_s();
  extern struct unit_t break_value_g,skip_value_g,true_g,false_g,nil_g;
  struct unit_t *uni4,*uni5;
  struct l_element_t *le1;
  int i;
  terr_check(lis1,LIS,fname,4);
  if(evaluate_s(uni1) == 0)
    {printf(" for(1*"); return(0);}
  while(1)
    {
    uni4 = evaluate_s(uni2);
    if(uni4 == 0)
      {printf(" for(2*"); return(0);}
    if(uni4 == &false_g) return(&nil_g);
    if(uni4 != &true_g) verr_check(uni2,0,fname,2);
    le1 = l_start(lis1);
    i = 1;
    while(le1)
      {
      uni4 = evaluate_s(le_value(le1));
      if(uni4 == 0)
	{printf(" [%d for(4*",i); return(0);}
      if(u_type(uni4) == REC)
	{
	if((uni5 = contents_s(uni4,&break_value_g))) return(uni5);
	if(contents_s(uni4,&skip_value_g)) break;
	}
      le1 = le_link(le1);
      ++i;
      }
    if(evaluate_s(uni3) == 0)
      {printf(" for(3*"); return(0);}
    }
  }

struct unit_t *through_f(lis1,rec1,lis2)
struct unit_t *lis1,*rec1,*lis2;
  {
  char *fname="through";
  extern struct l_element_t *new_le_s();
  extern struct unit_t variable_g,break_value_g,skip_value_g,nil_g;
  extern struct unit_t *copy_list_s(),*evaluate_s(),*contents_s();
  struct unit_t *uni1,*uni2,*bind;
  struct l_element_t *le1,*le2,*le3;
  int i;
  terr_check(rec1,REC,fname,1);
  terr_check(lis1,LIS,fname,2);
  terr_check(lis2,LIS,fname,3);
  cerr_check(rec1,&variable_g,fname,2);
  lis1 = copy_list_s(lis1);
  le1 = l_start(lis1);
  if(le1 == 0) return(&nil_g);
  bind = ensure_bindings_s(rec1);
  le3 = new_le_s();
  le_value(le3) = 0;
  le_link(le3) = l_start(bind);
  l_start(bind) = le3;
  ++l_size(bind);
  while(le1)
    {
    le3 = l_start(bind);
    if(le_value(le3) != 0)
      {
      --u_count(le_value(le3));
      addto_garbage_s(le_value(le3));
      }
    le_value(le3) = le_value(le1);
    ++u_count(le_value(le3));
    le2 = l_start(lis2);
    i = 1;
    while(le2)
      {
      uni1 = evaluate_s(le_value(le2));
      if(uni1 == 0)
	{
        printf(" [%d through(3*",i);
        le3 = l_start(bind);
        --u_count(le_value(le3));
        addto_garbage_s(le_value(le3));
        l_start(bind) = le_link(le3);
        --l_size(bind);
        if(!le_bidef(le3)) free(le3);
        return(0);
        }
      if(u_type(uni1) == REC)
	{
	if((uni2 = contents_s(uni1,&break_value_g)))
	  {
	  le3 = l_start(bind);
	  --u_count(le_value(le3));
          addto_garbage_s(le_value(le3));
	  l_start(bind) = le_link(le3);
	  --l_size(bind);
	  if(!le_bidef(le3)) free(le3);
	  return(uni2);
	  }
	if(contents_s(uni1,&skip_value_g)) break;
	}
      le2 = le_link(le2);
      ++i;
      }
    le1 = le_link(le1);
    }
  le3 = l_start(bind);
  --u_count(le_value(le3));
  addto_garbage_s(le_value(le3));
  l_start(bind) = le_link(le3);
  --l_size(bind);
  if(!le_bidef(le3)) free(le3);
  return(&nil_g);
  }

/* RULE-BASED OPERATION. -----------------------------------------------------*/

struct unit_t *invoke_f(rec1,lis1)
struct unit_t *rec1,*lis1;
  {
  char *fname="invoke";
  extern struct unit_t *new_unit_s();
  extern struct l_element_t *new_le_s();
  extern struct l_element_t *enum_call_s();
  extern struct unit_t *evaluate_s(),*contents_s(),*aerr_s();
  extern struct unit_t control_g,alternatives_g,class_g,subclasses_g,nil_g;
  extern struct unit_t members_g,condition_g,true_g,action_g,stop_value_g;
  extern struct unit_t arguments_g,temporary_g,variable_g,bindings_g;
  extern struct unit_t single_test_g,single_application_g;
  extern struct unit_t multiple_application_g,mode_g;
  struct unit_t *control_bindings,*alternatives_bindings,*lis2,*lis3,*lis4,*r;
  struct unit_t *temp1,*temp2;
  struct l_element_t *le1,*le2,*le3,*le4,*alts_le,*old_alts_le;
  int activity_flag,i;
  terr_check(rec1,REC,fname,1);
  terr_check(lis1,LIS,fname,2);
  cerr_check(rec1,&class_g,fname,1);
  control_bindings = ensure_bindings_s(&control_g);
  ++l_size(control_bindings);
  le1 = new_le_s();
  le_link(le1) = l_start(control_bindings);
  l_start(control_bindings) = le1;
  le_value(le1) = rec1;
  ++u_count(le_value(le1));
  alternatives_bindings = ensure_bindings_s(&alternatives_g);
  ++l_size(alternatives_bindings);
  le1 = new_le_s();
  le_link(le1) = l_start(alternatives_bindings);
  l_start(alternatives_bindings) = le1;
  le_value(le1) = new_unit_s();
  u_type(le_value(le1)) = LIS;
  ++u_count(le_value(le1));
  l_size(le_value(le1)) = 0;
  l_start(le_value(le1)) = 0;
  enum_call_s(rec1,le_value(le1),0);
  lis2 = get_s(rec1,&arguments_g);
  if(u_type(lis2) != LIS) return(aerr_s(rec1));
  if(l_size(lis2) != l_size(lis1)) return(aerr_s(rec1));
  r = &nil_g;
  le1 = l_start(lis1);
  le2 = l_start(lis2);
  while(le1)
    {
    if(!isa_s(le_value(le2),&variable_g))
      {r=0; derr_s(rec1); le1=le_link(le1); le2=le_link(le2); continue;}
    lis4 = ensure_bindings_s(le_value(le2));
    if(u_type(lis4) != LIS)
      {r=0; derr_s(rec1); le1=le_link(le1); le2=le_link(le2); continue;}
    le4 = new_le_s();
    le_value(le4) = evaluate_s(le_value(le1));
    if(le_value(le4) == 0)
      {r=0; le1=le_link(le1); le2=le_link(le2); continue;}
    ++u_count(le_value(le4));
    le_link(le4) = l_start(lis4);
    l_start(lis4) = le4;
    ++l_size(lis4);
    le1 = le_link(le1);
    le2 = le_link(le2);
    }
  if(r == 0) goto out;
  lis3 = get_s(rec1,&temporary_g);
  if(u_type(lis3) == LIS)
    {
    le3 = l_start(lis3);
    while(le3)
      {
      if(!isa_s(le_value(le3),&variable_g))
        {r=0; derr_s(rec1); le3=le_link(le3); continue;}
      lis4 = ensure_bindings_s(le_value(le3));
      if(u_type(lis4) != LIS)
        {r=0; derr_s(rec1); le3=le_link(le3); continue;}
      le4 = new_le_s();
      le_value(le4) = &nil_g;
      ++u_count(le_value(le4));
      le_link(le4) = l_start(lis4);
      l_start(lis4) = le4;
      ++l_size(lis4);
      le3 = le_link(le3);
      }
    if(r == 0) goto out;
    }
  while(1)
    {
    activity_flag = 0;
    alts_le = l_start(le_value(l_start(alternatives_bindings)));
    old_alts_le = 0;
    while(alts_le)
      {
      temp1 = evaluate_s(get_s(le_value(alts_le),&condition_g));
      if(temp1 == 0)
	{printf(" {condition {members invoke(1*"); return(0);}
      if(temp1 == &true_g)
	{
	activity_flag = 1;
	temp1 = evaluate_s(get_s(le_value(alts_le),&action_g));
	if(temp1 == 0)
	  {printf(" {action {members invoke(1*"); return(0);}
	if(u_type(temp1) == REC)
	  if(temp2 = contents_s(temp1,&stop_value_g))
	    {r = temp2; goto out; }
        if(get_s(le_value(alts_le),&mode_g) == &single_application_g)
	  {
	  --l_size(le_value(l_start(alternatives_bindings)));
	  if(old_alts_le == 0)
	   l_start(le_value(l_start(alternatives_bindings))) = le_link(alts_le);
	  else le_link(old_alts_le) = le_link(alts_le);
          addto_garbage_s(le_value(alts_le));
          if(!le_bidef(alts_le)) free(alts_le);
          alts_le = old_alts_le;
	  }
	}
      if(get_s(le_value(alts_le),&mode_g) == &single_test_g)
	{
	--l_size(le_value(l_start(alternatives_bindings)));
	if(old_alts_le == 0)
	  l_start(le_value(l_start(alternatives_bindings))) = le_link(alts_le);
	else le_link(old_alts_le) = le_link(alts_le);
        addto_garbage_s(le_value(alts_le));
        if(!le_bidef(alts_le)) free(alts_le);
        alts_le = old_alts_le;
	}
      if(activity_flag == 1) break;
      old_alts_le = alts_le;
      if(old_alts_le == 0)
        alts_le = l_start(le_value(l_start(alternatives_bindings)));
      else alts_le = le_link(old_alts_le);
      }
    if(activity_flag == 0) goto out;
    }
out: le1 = l_start(control_bindings);
  --l_size(control_bindings);
  l_start(control_bindings) = le_link(le1);
  --u_count(le_value(le1));
  addto_garbage_s(le_value(le1));
  if(!le_bidef(le1)) free(le1);
  le1 = l_start(alternatives_bindings);
  --l_size(alternatives_bindings);
  l_start(alternatives_bindings) = le_link(le1);
  --u_count(le_value(le1));
  addto_garbage_s(le_value(le1));
  if(!le_bidef(le1)) free(le1);
  if(u_type(lis2) == LIS)
    {
    le2 = l_start(lis2);
    i = 1;
    while(le2)
      {
      lis4 = get_s(le_value(le2),&bindings_g);
      if(u_type(lis4) != LIS)
        {
        r=0;
        printf(" {bindings [%d {arguments invoke(1*",i);
        goto con1;
        }
      le4 = l_start(lis4);
      if(le4 == 0)
        {
        r=0;
        printf(" {bindings [%d {arguments invoke(1*",i);
        goto con1;
        }
      --l_size(lis4);
      l_start(lis4) = le_link(le4);
      --u_count(le_value(le4));
      addto_garbage_s(le_value(le4));
      if(!le_bidef(le4)) free(le4);
con1: le2 = le_link(le2);
      ++i;
      }
    }
  if(u_type(lis3) == LIS)
    {
    le3 = l_start(lis3);
    i = 1;
    while(le3)
      {
      lis4 = get_s(le_value(le3),&bindings_g);
      if(u_type(lis4) != LIS)
        {
        r=0;
        printf(" {bindings [%d {arguments invoke(1*",i);
        goto con2;
        }
      le4 = l_start(lis4);
      if(le4 == 0)
        {
        r=0;
        printf(" {bindings [%d {arguments invoke(1*",i);
        goto con2;
        }
      --l_size(lis4);
      l_start(lis4) = le_link(le4);
      --u_count(le_value(le4));
      addto_garbage_s(le_value(le4));
      if(!le_bidef(le4)) free(le4);
con2: le3 = le_link(le3);
      ++i;
      }
    }
  return(r);
  }

/* CONTROL FLOW FUNCTIONS. ---------------------------------------------------*/

struct unit_t *flow_control_s(att1,uni1)
struct unit_t *att1,*uni1;
  {
  extern struct unit_t *new_unit_s();
  extern struct r_element_t *new_re_s();
  struct unit_t *uni2;
  struct r_element_t *re;
  uni2 = new_unit_s();
  u_type(uni2) = REC;
  r_size(uni2) = 1;
  re = new_re_s();
  r_start(uni2) = re;
  re_attribute(re) = att1;
  ++u_count(re_attribute(re));
  re_value(re) = uni1;
  ++u_count(re_value(re));
  addto_garbage_s(uni2);
  return(uni2);
  }

struct unit_t *result_f(uni1)
struct unit_t *uni1;
  {
  char *fname="result";
  extern struct unit_t result_value_g;
  return(flow_control_s(&result_value_g,uni1));
  }

struct unit_t *break_f(uni1)
struct unit_t *uni1;
  {
  char *fname="break";
  extern struct unit_t break_value_g;
  return(flow_control_s(&break_value_g,uni1));
  }

struct unit_t *skip_f()
  {
  char *fname="skip";
  extern struct unit_t skip_value_g;
  extern struct unit_t nil_g;
  return(flow_control_s(&skip_value_g,&nil_g));
  }

struct unit_t *return_f(uni1)
struct unit_t *uni1;
  {
  char *fname="return";
  extern struct unit_t return_value_g;
  return(flow_control_s(&return_value_g,uni1));
  }

struct unit_t *stop_f(uni1)
struct unit_t *uni1;
  {
  char *fname="stop";
  extern struct unit_t stop_value_g;
  return(flow_control_s(&stop_value_g,uni1));
  }

/* MISCELLANEOUS FUNCTIONS. --------------------------------------------------*/

struct unit_t *pause_f(str1)
struct unit_t *str1;
  {
  char *fname="pause";
  extern struct unit_t *parse_s(),*display_s(),*evaluate_s();
  extern struct unit_t nil_g;
  extern int status_g,i_level_g,column_g;
  struct unit_t *uni1;
  int i_level_save,i;
  terr_check(str1,STR,fname,1);
  printf("%s\n",s_start(str1));
  i_level_save = i_level_g;
  while(1)
    {
    i_level_g = 0;
    do uni1=parse_s(TER); while (status_g!=OK);
    while(from_s(TER) != '\n');
    addto_garbage_s(uni1);
    if(uni1 == &nil_g)
      {
      i_level_g = i_level_save;
      return(str1);
      }
    if((uni1=evaluate_s(uni1)) != 0)
      {
      i_level_g = 0;
      column_g = 0;
      display_s(TER,uni1,1);
      }
    printf("\n");
    }
  }

struct unit_t *copy_number_s(num1)
struct unit_t *num1;
  {
  extern struct unit_t *new_unit_s();
  struct unit_t *result;
  result = new_unit_s();
  u_type(result) = NUM;
  n_value(result) = n_value(num1);
  addto_garbage_s(result);
  return(result);
  }

struct unit_t *copy_token_s(tok1)
struct unit_t *tok1;
  {
  extern struct unit_t *new_unit_s();
  struct unit_t *result;
  int i;
  result = new_unit_s();
  u_type(result) = TOK;
  t_start(result) = (char *) malloc(strlen(t_start(tok1)) + 1);
  for(i=0; i<strlen(t_start(tok1)); ++i)
    t_start(result)[i] = t_start(tok1)[i];
  t_start(result)[i] = '\0';
  t_size(result) = i;
  addto_garbage_s(result);
  return(result);
  }

struct unit_t *copy_string_s(str1)
struct unit_t *str1;
  {
  extern struct unit_t *new_unit_s();
  struct unit_t *result;
  int i;
  result = new_unit_s();
  u_type(result) = STR;
  s_start(result) = (char *) malloc(strlen(s_start(str1)) + 1);
  for(i=0; i<strlen(s_start(str1)); ++i)
    s_start(result)[i] = s_start(str1)[i];
  s_start(result)[i] = '\0';
  s_size(result) = i;
  addto_garbage_s(result);
  return(result);
  }

struct unit_t *copy_list_s(lis1)
struct unit_t *lis1;
  {
  extern struct unit_t *new_unit_s();
  extern struct l_element_t *new_le_s();
  struct unit_t *result;
  struct l_element_t *le1,*le2;
  result = new_unit_s();
  u_type(result) = LIS;
  l_start(result) = 0;
  le1 = l_start(lis1);
  le2 = 0;
  while(le1)
    {
    if(le2 == 0)
      {
      l_start(result) = new_le_s();
      le2 = l_start(result);
      }
    else
      {
      le_link(le2) = new_le_s();
      le2 = le_link(le2);
      }
    le_value(le2) = le_value(le1);
    ++u_count(le_value(le2));
    le1 = le_link(le1);
    }
  l_size(result) = l_size(lis1);
  addto_garbage_s(result);
  return(result);
  }

struct unit_t *copy_record_s(rec1)
struct unit_t *rec1;
  {
  extern struct unit_t *new_unit_s();
  extern struct r_element_t *new_re_s();
  struct unit_t *result;
  struct r_element_t *re1,*re2;
  result = new_unit_s();
  u_type(result) = REC;
  r_start(result) = 0;
  re1 = r_start(rec1);
  re2 = 0;
  while(re1)
    {
    if(re2 == 0)
      {
      r_start(result) = new_re_s();
      re2 = r_start(result);
      }
    else
      {
      re_link(re2) = new_re_s();
      re2 = re_link(re2);
      }
    re_attribute(re2) = re_attribute(re1);
    ++u_count(re_attribute(re2));
    re_value(re2) = re_value(re1);
    ++u_count(re_value(re2));
    re1 = re_link(re1);
    }
  r_size(result) = r_size(rec1);
  addto_garbage_s(result);
  return(result);
  }

struct unit_t *copy_expression_s(exp1)
struct unit_t *exp1;
  {
  extern struct unit_t *new_unit_s();
  extern struct e_element_t *new_ee_s();
  struct unit_t *result;
  struct e_element_t *ee1,*ee2;
  result = new_unit_s();
  u_type(result) = EXP;
  e_start(result) = 0;
  ee1 = e_start(exp1);
  ee2 = 0;
  while(ee1)
    {
    if(ee2 == 0)
      {
      e_start(result) = new_ee_s();
      ee2 = e_start(result);
      }
    else
      {
      ee_link(ee2) = new_ee_s();
      ee2 = ee_link(ee2);
      }
    ee_value(ee2) = ee_value(ee1);
    ++u_count(ee_value(ee2));
    ee1 = ee_link(ee1);
    }
  e_size(result) = e_size(exp1);
  addto_garbage_s(result);
  return(result);
  }

struct unit_t *copy_connection_s(con1)
struct unit_t *con1;
  {
  extern struct unit_t *new_unit_s();
  struct unit_t *result;
  int i;
  result = new_unit_s();
  u_type(result) = CON;
  c_contents(result) = c_contents(con1);
  c_label(result) = (char *) malloc(strlen(c_label(con1)) + 1);
  for(i=0; i<strlen(c_label(con1)); ++i)
    c_label(result)[i] = c_label(con1)[i];
  c_label(result)[i] = '\0';
  addto_garbage_s(result);
  return(result);
  }

struct unit_t *copy_s(uni1)
struct unit_t *uni1;
  {
  extern struct unit_t *copy_number_s(),*copy_token_s();
  extern struct unit_t *copy_string_s(),*copy_list_s();
  extern struct unit_t *copy_record_s(),*copy_expression_s();
  extern struct unit_t *copy_connection_s();
  switch(u_type(uni1))
    {
    case NUM: return(copy_number_s(uni1));
    case TOK: return(copy_token_s(uni1));
    case STR: return(copy_string_s(uni1));
    case LIS: return(copy_list_s(uni1));
    case REC: return(copy_record_s(uni1));
    case EXP: return(copy_expression_s(uni1));
    case CON: return(copy_connection_s(uni1));
    default : return(uni1);
    }
  }

/* BUILT-IN FUNCTION TABLE. --------------------------------------------------*/

struct built_in_function_entry_t bif_init_g[] = 
	/*******************************************************
	 * Contains the necessary information for run-time
	 *   initialization of the built-in commands in STAR.
	 *   Each function is specified by name (lowercase, as
	 *   this describes the function's reference name),
	 *   optional abbreviation character, number of argu-
	 *   ments, implementing C function, text for the label
	 *   to the CONNECTION for the function, method of
	 *   passing arguments (always "BY_VALUE") and text for
	 *   the "comment" field to the function.  This
	 *   information is used by the function "initialize_s"
	 *   to perform the actual initialization.
	 *******************************************************/
  {
    {"negate","-",1,negate_f,"C_NEGATE_FUNCTION",BY_VALUE,
 "     (NUMBER1) => NUMBER\
\n\
\n     Arithmetic negation."},
    {"add","+",2,add_f,"C_ADD_FUNCTION",BY_VALUE,
 "     (NUMBER1 NUMBER2) => NUMBER\
\n\
\n     Addition."},
    {"subtract","-",2,subtract_f,"C_SUBTRACT_FUNCTION",BY_VALUE,
 "     (NUMBER1 NUMBER2) => NUMBER\
\n\
\n     Subtraction."},
    {"multiply","*",2,multiply_f,"C_MULTIPLY_FUNCTION",BY_VALUE,
 "     (NUMBER1 NUMBER2) => NUMBER\
\n\
\n     Multiplication."},
    {"divide","/",2,divide_f,"C_DIVIDE_FUNCTION",BY_VALUE,
 "     (NUMBER1 NUMBER2) => NUMBER\
\n\
\n     Division."},
    {"minimum","",2,minimum_f,"C_MINIMUM_FUNCTION",BY_VALUE,
 "     (NUMBER1 NUMBER2) => NUMBER\
\n\
\n     Minimum of two NUMBERs."},
    {"maximum","",2,maximum_f,"C_MAXIMUM_FUNCTION",BY_VALUE,
 "     (NUMBER1 NUMBER2) => NUMBER\
\n\
\n     Maximum of two NUMBERs."},
    {"locate","",1,locate_f,"C_LOCATE_FUNCTION",BY_VALUE,
 "     (TOKEN1) => <concept>\
\n\
\n     Return the RECORD of name TOKEN1, creating one if\
\nnecessary."},
    {"test","",1,test_f,"C_TEST_FUNCTION",BY_VALUE,
 "     (TOKEN1) => <concept>\
\n\
\n     Return the RECORD of name TOKEN1 if it exists,\
\notherwise, return 'nil'."},
    {"character","",2,character_f,"C_CHARACTER_FUNCTION",BY_VALUE,
 "     (STRING1 NUMBER1) => STRING\
\n\
\n     Return a single-character STRING containing the\
\nNUMBER1'th character of STRING1.  If NUMBER1 < 0, count\
\nfrom the end of STRING1."},
    {"fetch","",2,fetch_f,"C_FETCH_FUNCTION",BY_VALUE,
 "     (STRING1 NUMBER1) => STRING\
\n\
\n     Return a STRING containing the first NUMBER1\
\ncharacters of STRING1.  If NUMBER1 < 0, use the last\
\n|NUMBER1| characters."},
    {"release","",2,release_f,"C_RELEASE_FUNCTION",BY_VALUE,
 "     (STRING1 NUMBER1) => STRING\
\n\
\n     Return a STRING containing all but the first NUMBER1\
\ncharacters of STRING1.  If NUMBER1 < 0, use all but the\
\nlast |NUMBER1| characters."},
    {"join","",2,join_f,"C_JOIN_FUNCTION",BY_VALUE,
 "     (STRING1 STRING2) => STRING\
\n\
\n     Return a STRING containing the concatenation of\
\nSTRING1 and STRING2."},
    {"find","",2,find_f,"C_FIND_FUNCTION",BY_VALUE,
 "     (STRING1 STRING2) => NUMBER\
\n\
\n     Search for the first occurrence of STRING1 in\
\nSTRING2, returning the number of characters before the\
\noccurrence if found, or the length of STRING2 if not\
\nfound."},
    {"length","",1,length_f,"C_LENGTH_FUNCTION",BY_VALUE,
 "     (STRING1) => NUMBER\
\n\
\n     Return the number of characters in STRING1."},
    {"select","",2,select_f,"C_SELECT_FUNCTION",BY_VALUE,
 "     (LIST1 NUMBER1) => UNIT\
\n\
\n     Return the NUMBER1'th element of LIST1.  If NUMBER1 <\
\n0, select the |NUMBER1|'th element from the end of LIST1."},
    {"replace","",3,replace_f,"C_REPLACE_FUNCTION",BY_VALUE,
 "     (LIST1 NUMBER1 UNIT1) => LIST\
\n\
\n     Return a copy of LIST1 with the NUMBER1'th element\
\nreplaced by UNIT1.  If NUMBER1 < 0, replace the\
\n|NUMBER1|'th element from the end of LIST1."},
    {"delete","",2,delete_f,"C_DELETE_FUNCTION",BY_VALUE,
 "     (LIST1 NUMBER1) => LIST\
\n\
\n     Return a copy of LIST1 with the NUMBER1'th element\
\ndeleted.  If NUMBER1 < 0, delete the |NUMBER1|'th element\
\nfrom the end of LIST1."},
    {"insert","",3,insert_f,"C_INSERT_FUNCTION",BY_VALUE,
 "     (LIST1 NUMBER1 UNIT1) => LIST\
\n\
\n     Return a copy of LIST1 with UNIT1 inserted so that it\
\noccupies the NUMBER1'th position.  If NUMBER1 < 0, insert\
\nat the |NUMBER1|'th position from the end of LIST1."},
    {"take","",2,take_f,"C_TAKE_FUNCTION",BY_VALUE,
 "     (LIST1 NUMBER1) => LIST\
\n\
\n     Return a LIST containing the first NUMBER1 elements\
\nof LIST1.  If NUMBER1 < 0, containing the last |NUMBER1|\
\nelements of LIST1."},
    {"drop","",2,drop_f,"C_DROP_FUNCTION",BY_VALUE,
 "     (LIST1 NUMBER1) => LIST\
\n\
\n     Return a LIST containing all but the first NUMBER1\
\nelements of LIST1.  If NUMBER1 < 0, containing all but the\
\nlast |NUMBER1| elements."},
    {"append","",2,append_f,"C_APPEND_FUNCTION",BY_VALUE,
 "     (LIST1 LIST2) => LIST\
\n\
\n     Return a LIST containing the elements of LIST1\
\nfollowed by the elements of LIST2."},
    {"size","",1,size_f,"C_SIZE_FUNCTION",BY_VALUE,
 "     (LIST1) => NUMBER\
\n\
\n     Return the number of elements in LIST1."},
    {"union","",2,union_f,"C_UNION_FUNCTION",BY_VALUE,
 "     (LIST1 LIST2) => LIST\
\n\
\n     Treating LIST1 and LIST2 as sets, return a LIST\
\ncontaining the union of the the elements in LIST1 and\
\nLIST2."},
    {"intersection","",2,intersection_f,"C_INTERSECTION_FUNCTION",BY_VALUE,
 "     (LIST1 LIST2) => LIST\
\n\
\n     Treating LIST1 and LIST2 as sets, return a LIST\
\ncontaining the intersection of the elements in LIST1 and\
\nLIST2."},
    {"difference","",2,difference_f,"C_DIFFERENCE_FUNCTION",BY_VALUE,
 "     (LIST1 LIST2) => LIST\
\n\
\n     Return a LIST corresponding to the set difference of\
\nthe elements in LIST1 and the elements in LIST2."},
    {"get","",2,get_f,"C_GET_FUNCTION",BY_VALUE,
 "     (RECORD1 <attribute1>) => UNIT\
\n\
\n     Return the value of <attribute1> in RECORD1."},
    {"put","",3,put_f,"C_PUT_FUNCTION",BY_VALUE,
 "     (RECORD1 <attribute1> UNIT1) => RECORD\
\n\
\n     Return a copy of RECORD1 with the value of\
\n<attribute1> set to be UNIT1, possibly replacing a\
\nprevious value."},
    {"omit","",2,omit_f,"C_OMIT_FUNCTION",BY_VALUE,
 "     (RECORD1 <attribute1>) => RECORD\
\n\
\n     Return a copy of RECORD1 with the entry for\
\n<attribute1> omitted."},
    {"detach","",2,detach_f,"C_DETACH_FUNCTION",BY_VALUE,
 "     (RECORD1 LIST1) => RECORD\
\n\
\n     Return a RECORD containing in order the attributes\
\nfound in LIST1 along with their corresponding values taken\
\nfrom RECORD1.  Attributes contained in LIST1 but without\
\nvalues in RECORD1 are ignored."},
    {"attach","",2,attach_f,"C_ATTACH_FUNCTION",BY_VALUE,
 "     (RECORD1 RECORD2) => RECORD\
\n\
\n     Return a RECORD containing the attributes and values\
\nof RECORD1 followed by the attributes found in RECORD2 but\
\nnot RECORD1, along with their corresponding values as\
\nspecified in RECORD2."},
    {"key","",1,key_f,"C_KEY_FUNCTION",BY_VALUE,
 "     (RECORD1) => LIST\
\n\
\n     Return a LIST of the attributes in RECORD1."},
    {"image","",1,image_f,"C_IMAGE_FUNCTION",BY_VALUE,
 "     (RECORD1) => LIST\
\n\
\n     Return a LIST of the values specified for the\
\nattributes in RECORD1."},
    {"build","",2,build_f,"C_BUILD_FUNCTION",BY_VALUE,
 "     (LIST1 LIST2) => RECORD\
\n\
\n     Return a RECORD formed by matching one-for-one the\
\nattributes in LIST1 with the values in LIST2."},
    {"define","",1,define_f,"C_DEFINE_FUNCTION",BY_VALUE,
 "     (RECORD1) => <concept>\
\n\
\n     Enter RECORD1 in the internal directory of named\
\nRECORDs and perform all side-effects resulting from\
\nasserting the specified values for the attributes in\
\nRECORD1.  Return the resulting RECORD1."},
    {"create","",2,create_f,"C_CREATE_FUNCTION",BY_VALUE,
 "     (<concept1> <class1>) => <concept>\
\n\
\n     Assert that <concept1> is a member of <class1>,\
\nreturning the new <concept1>."},
    {"assert","",3,assert_f,"C_ASSERT_FUNCTION",BY_VALUE,
 "     (<concept1> <attribute1> UNIT1) => <concept>\
\n\
\n     Modify <concept1> to include the pair <attribute1>\
\nand UNIT1, performing all generated side-effects and\
\nreturning the new <concept1> as the result.  <attribute1>\
\nmay not have a value in <concept1> at the start of this\
\noperation."},
    {"retract","",2,retract_f,"C_RETRACT_FUNCTION",BY_VALUE,
 "     (<concept1> <attribute1>) => <concept>\
\n\
\n     Modify <concept1> by removing <attribute1> and its\
\nvalue, if present, performing all generated side-effects\
\nand returning the new <concept1> as the result."},
    {"modify","",3,modify_f,"C_MODIFY_FUNCTION",BY_VALUE,
 "     (<concept1> <attribute1> UNIT1) => <concept>\
\n\
\n     Modify <concept1> so that the value of <attribute1>\
\nis UNIT1, possibly replacing a previous value.  Perform\
\nall side-effects generated by the retracting or asserting\
\nof values, returning the new <concept1> as the result."},
    {"revise","",2,revise_f,"C_REVISE_FUNCTION",BY_VALUE,
 "     (<concept1> LIST1) => <concept>\
\n\
\n     Modify <concept1> so that it contains in order the\
\nattributes in LIST1 along with their values as specified\
\nin <concept1>, performing all generated side-effects and\
\nreturning the new <concept1> as the result.  Attributes in\
\nLIST1 but without values in the original <concept1> are\
\nignored."},
    {"merge","",2,merge_f,"C_MERGE_FUNCTION",BY_VALUE,
 "     (<concept1> RECORD1) => <concept>\
\n\
\n     Modify <concept1> by including within it all\
\nattributes found in RECORD1 but not in <concept1>, along\
\nwith their values as specified in RECORD1.  Perform all\
\nside-effects generated by this process and return the new\
\n<concept1> as the result."},
    {"dot",".",1,dot_f,"C_DOT_FUNCTION",BY_VALUE,
 "     (<variable1>) => UNIT\
\n\
\n     Return the current value of <variable1>, being the\
\nfirst element in the LIST given for the 'bindings'\
\nattribute of <variable1>."},
    {"new","",2,new_f,"C_NEW_FUNCTION",BY_VALUE,
 "     (<variable1> UNIT1) => <variable>\
\n\
\n     Create a new binding for <variable1> by inserting\
\nUNIT1 as a new first element in the LIST specified for the\
\n'bindings' attribute of <variable1>, returning <variable1>\
\nas the result."},
    {"set","",2,set_f,"C_SET_FUNCTION",BY_VALUE,
 "     (<variable1> UNIT1) => <variable>\
\n\
\n     Modify the current value of <variable1> by replacing\
\nthe first element of the LIST given for the 'bindings'\
\nattribute of <variable1> with UNIT1.  Return <variable1>\
\nas the result."},
    {"old","",1,old_f,"C_OLD_FUNCTION",BY_VALUE,
 "     (<variable1>) => <variable>\
\n\
\n     Remove the current top binding of <variable1>,\
\nspecified as the first element in the LIST given for the\
\n'bindings' attribute of <variable1>, returning <variable1>\
\nas the result."},
    {"determine","",2,determine_f,"C_DETERMINE_FUNCTION",BY_VALUE,
 "     (<concept1> <attribute1>) => UNIT\
\n\
\n     Return the value of <attribute1> in <concept1>.  If\
\nnone exists, use an inherited value for <attribute1> as\
\nfound in a class containing <concept1>, starting with its\
\nimmediate parent class."},
    {"estimate","",2,estimate_f,"C_ESTIMATE_FUNCTION",BY_VALUE,
 "     (<concept1> <attribute1>) => UNIT\
\n\
\n     Return an estimated value of <attribute1> for\
\n<concept1>, as found in the 'default' aspect of\
\n<attribute1> in <concept1>.  If none exists, use an\
\ninherited default aspect as found in a class containing\
\n<concept1>, starting with its immediate parent class."},
    {"calculate","",2,calculate_f,"C_CALCULATE_FUNCTION",BY_VALUE,
 "     (<concept1> <attribute1>) => UNIT\
\n\
\n     Return a calculated value of <attribute1> for\
\n<concept1>, as found by evaluating the 'if_needed' aspect\
\nof <attribute1> in <concept1>.  If none exists, use an\
\ninherited 'if_needed' aspect as found in a class\
\ncontaining <concept1>, starting with its immediate parent\
\nclass."},
    {"obtain","",3,obtain_f,"C_OBTAIN_FUNCTION",BY_VALUE,
 "     (<concept1> <attribute1> <attribute2>) => UNIT\
\n\
\n     Return the <attribute2> aspect of <attribute1> as\
\nfound in <concept1>.  If none exists, use an inherited\
\naspect, as found in a class containing <concept1>,\
\nstarting with its immediate parent class."},
    {"path","",1,path_f,"C_PATH_FUNCTION",BY_VALUE,
 "     (<concept1>) => LIST\
\n\
\n     Return a LIST with first element 'concept', last\
\nelement <concept1>, and intermediate elements specifying\
\nthe chain of subclasses leading from 'concept' to\
\n<concept1>."},
    {"enumerate",":",1,enumerate_f,"C_ENUMERATE_FUNCTION",BY_VALUE,
 "     (<class1>) => LIST\
\n\
\n     Return a LIST containing all members of <class1> or\
\nany of its subclasses."},
    {"operation","",1,operation_f,"C_OPERATION_FUNCTION",BY_VALUE,
 "     ('EXPRESSION1) => <function>\
\n\
\n     Return the operating function within EXPRESSION1."},
    {"application","",1,application_f,"C_APPLICATION_FUNCTION",BY_VALUE,
 "     ('EXPRESSION1) => LIST\
\n\
\n     Return a LIST of the unevaluated arguments contained\
\nwithin EXPRESSION1."},
    {"formulate","",2,formulate_f,"C_FORMULATE_FUNCTION",BY_VALUE,
 "     (<function1> LIST1) => EXPRESSION\
\n\
\n     Construct and return an EXPRESSION in which\
\n<function1> operates on the arguments specified in LIST1."},
    {"number","",1,number_f,"C_NUMBER_FUNCTION",BY_VALUE,
 "     (UNIT1) => <boolean>\
\n\
\n     Return 'true' if UNIT1 is a NUMBER, otherwise return\
\n'false'."},
    {"token","",1,token_f,"C_TOKEN_FUNCTION",BY_VALUE,
 "     (UNIT1) => <boolean>\
\n\
\n     Return 'true' if UNIT1 is a TOKEN, otherwise return\
\n'false'."},
    {"string","",1,string_f,"C_STRING_FUNCTION",BY_VALUE,
 "     (UNIT1) => <boolean>\
\n\
\n     Return 'true' if UNIT1 is a STRING, otherwise return\
\n'false'."},
    {"list","",1,list_f,"C_LIST_FUNCTION",BY_VALUE,
 "     (UNIT1) => <boolean>\
\n\
\n     Return 'true' if UNIT1 is a LIST, otherwise return\
\n'false'."},
    {"record","",1,record_f,"C_RECORD_FUNCTION",BY_VALUE,
 "     (UNIT1) => <boolean>\
\n\
\n     Return 'true' if UNIT1 is a RECORD, otherwise return\
\n'false'."},
    {"expression","",1,expression_f,"C_EXPRESSION_FUNCTION",BY_VALUE,
 "     (UNIT1) => <boolean>\
\n\
\n     Return 'true' if UNIT1 is an EXPRESSION, otherwise\
\nreturn 'false'."},
    {"connection","",1,connection_f,"C_CONNECTION_FUNCTION",BY_VALUE,
 "     (UNIT1) => <boolean>\
\n\
\n     Return 'true' if UNIT1 is a CONNECTION, otherwise\
\nreturn 'false'."},
    {"null","",1,null_f,"C_NULL_FUNCTION",BY_VALUE,
 "     (UNIT1) => <boolean>\
\n\
\n     Return 'true' if UNIT1 is the named RECORD 'nil',\
\notherwise return 'false'."},
    {"equal","=",2,equal_f,"C_EQUAL_FUNCTION",BY_VALUE,
 "     (UNIT1 UNIT2) => <boolean>\
\n\
\n     Return 'true' or 'false' based on equivalence of\
\nmemory address for named RECORDs or CONNECTIONs and\
\ndisplayed form for other types of UNITs."},
    {"less","<",2,less_f,"C_LESS_FUNCTION",BY_VALUE,
 "     (NUMBER1 NUMBER2) => <boolean>\
\n\
\n     Return 'true' if NUMBER1 is less than NUMBER2,\
\notherwise return 'false'."},
    {"greater",">",2,greater_f,"C_GREATER_FUNCTION",BY_VALUE,
 "     (NUMBER1 NUMBER2) => <boolean>\
\n\
\n     Return 'true' if NUMBER1 is greater than NUMBER2,\
\notherwise return 'false'."},
    {"in","",2,in_f,"C_IN_FUNCTION",BY_VALUE,
 "     (UNIT1 LIST1) => <boolean>\
\n\
\n     Return 'true' if UNIT1 is contained in LIST1,\
\notherwise return 'false'."},
    {"subset","",2,subset_f,"C_SUBSET_FUNCTION",BY_VALUE,
 "     (LIST1 LIST2) => <boolean>\
\n\
\n     Return 'true' if the elements of LIST1 form a subset\
\nof the elements in LIST2, otherwise return 'false'."},
    {"isa","",2,isa_f,"C_ISA_FUNCTION",BY_VALUE,
 "     (<concept1> <class1>) => <boolean>\
\n\
\n     Return 'true' if <concept1> is a member of <class1>\
\nor a subclass of <class1>, otherwise return 'false'."},
    {"within","",2,within_f,"C_WITHIN_FUNCTION",BY_VALUE,
 "     (<class1> <class2>) => <boolean>\
\n\
\n     Return 'true' if <class1> is a subclass of <class2>\
\nor any subclasses of <class2>, otherwise return 'false'."},
    {"not","~",1,not_f,"C_NOT_FUNCTION",BY_VALUE,
 "     (<boolean1>) => <boolean>\
\n\
\n     Logical negation."},
    {"and","&",2,and_f,"C_AND_FUNCTION",BY_VALUE,
 "     (<boolean1> <boolean2>) => <boolean>\
\n\
\n     Logical conjunction of two boolean values."},
    {"or","|",2,or_f,"C_OR_FUNCTION",BY_VALUE,
 "     (<boolean1> <boolean2>) => <boolean>\
\n\
\n     Logical disjunction of two boolean values."},
    {"exists","",3,exists_f,"C_EXISTS_FUNCTION",BY_VALUE,
 "     (LIST1 <variable1> 'UNIT1) => <boolean>\
\n\
\n     Return 'true' if there is an element of LIST1 for\
\nwhich UNIT1 evaluates to 'true' when <variable1> is bound\
\nto that element.  Otherwise return 'false'."},
    {"every","",3,every_f,"C_EVERY_FUNCTION",BY_VALUE,
 "     (LIST1 <variable1> 'UNIT1) => <boolean>\
\n\
\n     Return 'true' if for each element of LIST1,\
\nevaluation of UNIT1 returns 'true' when <variable1> is\
\nbound to that element.  Otherwise return 'false'."},
    {"which","",3,which_f,"C_WHICH_FUNCTION",BY_VALUE,
 "     (LIST1 <variable1> 'UNIT1) => LIST\
\n\
\n     Return a LIST containing each element of LIST1 for\
\nwhich UNIT1 evaluates to 'true' when <variable1> is bound\
\nto that element."},
    {"parse","",0,parse_f,"C_PARSE_FUNCTION",BY_VALUE,
 "     () => UNIT\
\n\
\n     Parse input from the terminal and create a UNIT,\
\nreturning that UNIT as the result."},
    {"display","",1,display_f,"C_DISPLAY_FUNCTION",BY_VALUE,
 "     (UNIT1) => UNIT\
\n\
\n     Display UNIT1 at the terminal, returning UNIT1 as the\
\nresult."},
    {"input","",0,input_f,"C_INPUT_FUNCTION",BY_VALUE,
 "     () => STRING\
\n\
\n     Read in a line of characters from the terminal,\
\nforming a STRING containing those characters (including\
\nthe final carriage return) and return that STRING as the\
\nresult."},
    {"output","",1,output_f,"C_OUTPUT_FUNCTION",BY_VALUE,
 "     (STRING1) => STRING\
\n\
\n     Write the contents of STRING1 to the terminal and\
\nreturn STRING1 as the result."},
    {"format","",2,format_f,"C_FORMAT_FUNCTION",BY_VALUE,
 "     (STRING1 LIST1) => STRING\
\n\
\n     Copies STRING1, replacing all occurrences of the\
\ncircumflex ('^') character followed by a positive integer\
\nspecification with the spelled version of the indicated\
\nelement in LIST1, following evaluation of that element.\
\nDouble quotes are omitted in STRINGs converted from the\
\nLIST.  Two adjacent circumflex characters are translated\
\nto a single circumflex character in the resultant STRING,\
\nwith no conversion taking place."},
    {"save","",1,save_f,"C_SAVE_FUNCTION",BY_VALUE,
 "     (STRING1) => STRING\
\n\
\n     Save in symbolic form all changes made to the\
\ninitialized knowledge base, using the contents of STRING1\
\nas a path/filename.  Return STRING1 as the result."},
    {"stash","",2,stash_f,"C_STASH_FUNCTION",BY_VALUE,
 "     (STRING1 LIST1) => STRING\
\n\
\n     Save in symbolic form the definitions of the\
\nnamed RECORDs contained in LIST1, using the contents of\
\nSTRING1 as a path/filename.  Return STRING1 as the\
\nresult."},
    {"load","",1,load_f,"C_LOAD_FUNCTION",BY_VALUE,
 "     (STRING1) => STRING\
\n\
\n     Parse and evaluate the UNITs contained in symbolic\
\nform in the file specified by STRING1."},
    {"read","",1,read_f,"C_READ_FUNCTION",BY_VALUE,
 "     (STRING1) => STRING\
\n\
\n     Read the contents of the text file specified by\
\nSTRING1 and form a STRING containing those characters.\
\nReturn this STRING as the result."},
    {"write","",2,write_f,"C_WRITE_FUNCTION",BY_VALUE,
 "     (STRING1 STRING2) => STRING\
\n\
\n     Create or rewrite the file specified by STRING1,\
\nusing the characters of STRING2 as text.  Return STRING1\
\nas the result."},
    {"extend","",2,extend_f,"C_EXTEND_FUNCTION",BY_VALUE,
 "     (STRING1 STRING2) => STRING\
\n\
\n     Create or append to the file specified by STRING1,\
\nusing the characters of STRING2 as text.  Return STRING1\
\nas the result."},
    {"spell","",1,spell_f,"C_SPELL_FUNCTION",BY_VALUE,
 "     (UNIT1) => STRING\
\n\
\n     Construct and return a STRING corresponding to the\
\ncharacters in the displayed form of UNIT1."},
    {"unspell","",1,unspell_f,"C_UNSPELL_FUNCTION",BY_VALUE,
 "     (STRING1) => UNIT\
\n\
\n     Parse STRING1 for the specification of a UNIT,\
\nreturning that UNIT as the result."},
    {"scan","",1,scan_f,"C_SCAN_FUNCTION",BY_VALUE,
 "     (STRING1) => UNIT\
\n\
\n     Parse STRING1 for the specification of a UNIT,\
\nreturning that UNIT as the result.  Differs from\
\n'unspell' in that errors are not announced; rather,\
\nthe value 'nil' is simply returned in such cases."},
    {"quote","'",1,quote_f,"C_QUOTE_FUNCTION",BY_VALUE,
 "     (UNIT1) => UNIT\
\n\
\n     Spare UNIT1 from evaluation, returning it in its\
\nunevaluated form as the result."},
    {"evaluate","",1,evaluate_f,"C_EVALUATE_FUNCTION",BY_VALUE,
 "     ('UNIT1) => UNIT\
\n\
\n     Evaluation of a UNIT."},
    {"prepare","",1,prepare_f,"C_PREPARE_FUNCTION",BY_VALUE,
 "     (UNIT1) => UNIT\
\n\
\n     Evaluation of a UNIT with the difference that if\
\nUNIT1 is a RECORD or LIST, all UNITs contained as elements\
\nof that RECORD or LIST are also evaluated."},
    {"apply","",2,apply_f,"C_APPLY_FUNCTION",BY_VALUE,
 "     (<function1> LIST1) => UNIT\
\n\
\n     Application of <function1> to the elements of LIST1\
\nas arguments."},
    {"if","",2,if_f,"C_IF_FUNCTION",BY_VALUE,
 "     (<boolean1> 'UNIT1) => UNIT\
\n\
\n     If <boolean1> is 'true', evaluate UNIT1 and return\
\nthe result of that evaluation.  Otherwise return 'nil'."},
    {"ifelse","",3,ifelse_f,"C_IFELSE_FUNCTION",BY_VALUE,
 "     (<boolean1> 'UNIT1 'UNIT2) => UNIT\
\n\
\n     If <boolean1> is 'true', evaluate UNIT1 and return\
\nthe result of that evaluation.  Otherwise evaluate UNIT2\
\nand return the result of that evaluation."},
    {"branch","",2,branch_f,"C_BRANCH_FUNCTION",BY_VALUE,
 "     (UNIT1 LIST1) => UNIT\
\n\
\n     Search the RECORDs which are the elements of LIST1\
\nfor one with a value for the attribute 'case' which\
\nevaluates to a UNIT equal to UNIT1.  If such a RECORD is\
\nfound, evaluate the value of the 'action' attribute for\
\nthat RECORD and return the result of evaluation."},
    {"do","",1,do_f,"C_DO_FUNCTION",BY_VALUE,
 "     (LIST1) => UNIT\
\n\
\n     Sequentially evaluate the elements of LIST1,\
\ninterrupting the process if the result of evaluating an\
\nelement is a RECORD with a value for the attribute\
\n'result_value'.  In this case, return the value of\
\n'result_value' for that RECORD as the result of 'do'.\
\nOtherwise, return 'nil' following evaluation of the last\
\nelement in LIST1."},
    {"repeat","",1,repeat_f,"C_REPEAT_FUNCTION",BY_VALUE,
 "     (LIST1) => UNIT\
\n\
\n     Repeatedly evaluate the elements of LIST1 in a\
\nsequential manner, skipping to the next iteration or\
\nexiting the looping process if the evaluation of an\
\nelement results in a RECORD with 'skip_value' or\
\n'break_value' attribute, respectively.  In the latter\
\ncase, return the value of 'break_value' in the RECORD as\
\nthe result for 'repeat'."},
    {"while","",2,while_f,"C_WHILE_FUNCTION",BY_VALUE,
 "     ('UNIT1 LIST1) => UNIT\
\n\
\n     Repeatedly evaluate the elements of LIST1 in a\
\nsequential manner as long as UNIT1 evaluates to 'true'\
\nprior to each iteration.  Skip to the next iteration or\
\nexit the loop in the same manner as for the function\
\n'repeat'."},
    {"for","",4,for_f,"C_FOR_FUNCTION",BY_VALUE,
 "     ('UNIT1 'UNIT2 'UNIT3 LIST1) => UNIT\
\n\
\n     Evaluate UNIT1 and continue by repeatedly carrying\
\nout a cyclic process of evaluating UNIT2, sequentially\
\nevaluating the elements of LIST1, then evaluating UNIT3.\
\nThe evaluation of UNIT2 is expected to produce a boolean\
\nvalue.  If this value is not 'true' for a particular\
\niteration, exit the looping process directly, returning\
\nthe value 'nil'.  Also, skip to the next iteration or\
\nexit the loop in the same manner as for the function\
\n'repeat'."},
    {"through","",3,through_f,"C_THROUGH_FUNCTION",BY_VALUE,
 "     (LIST1 <variable1> LIST2) => UNIT\
\n\
\n     Repeatedly evaluate the elements of LIST2 in a\
\nsequential manner, one iteration for each element of\
\nLIST1.  Before each iteration, bind <variable1> to the\
\ncurrent element of LIST1.  Skip to the next iteration or\
\nexit the loop in the same manner as for the function\
\n'repeat'."},
    {"invoke","",2,invoke_f,"C_INVOKE_FUNCTION",BY_VALUE,
 "     (<class1> LIST1) => UNIT\
\n\
\n     Invoke rule-based operation, using the members of\
\n<class1> as candidate rules.  LIST1 specifies the values\
\nof the arguments to the ruleset, similar to the arguments\
\nto a function.  Rule-based operation continues until no\
\nrules fire on a given cycle, the list of candidate rules\
\nbecomes empty or the result of applying a particular rule\
\nis a RECORD with attribute 'stop_value' (resulting from\
\nuse of the 'stop' function).  In this case, return the\
\nvalue associated with 'stop_value' as the result.  Uses\
\nthe built-in variables 'control' and 'alternatives'."},
    {"result","",1,result_f,"C_RESULT_FUNCTION",BY_VALUE,
 "     (UNIT1) => RECORD\
\n\
\n     Return a RECORD containing a single attribute,\
\n'result_value', with UNIT1 as its value.  Used in\
\nconjunction with the 'do' function."},
    {"break","",1,break_f,"C_BREAK_FUNCTION",BY_VALUE,
 "     (UNIT1) => RECORD\
\n\
\n     Return a RECORD containing a single attribute,\
\n'break_value', with UNIT1 as its value.  Used in\
\nconjunction with the functions 'repeat', 'while', 'for'\
\nand 'through'."},
    {"skip","",0,skip_f,"C_SKIP_FUNCTION",BY_VALUE,
 "     () => RECORD\
\n\
\n     Return a RECORD containing a single attribute,\
\n'skip_value', with 'nil' as its value.  Used in\
\nconjunction with the functions 'repeat', 'while', 'for'\
\nand 'through'."},
    {"return","",1,return_f,"C_RETURN_FUNCTION",BY_VALUE,
 "     (UNIT1) => RECORD\
\n\
\n     Return a RECORD containing a single attribute,\
\n'return_value', with UNIT1 as its value.  Used to return a\
\nresulting value from a user-defined function."},
    {"stop","",1,stop_f,"C_STOP_FUNCTION",BY_VALUE,
 "     (UNIT1) => RECORD\
\n\
\n     Return a RECORD containing a single attribute,\
\n'stop_value', with UNIT1 as its value.  Used in\
\nconjunction with the function 'invoke'."},
    {"pause","",1,pause_f,"C_PAUSE_FUNCTION",BY_VALUE,
 "     (STRING1) => STRING\
\n\
\n     Often called from within a user-defined function for\
\ndiagnostic purposes.  STRING1 is printed at the user\
\nterminal, minus the surrounding quotes, then a series of\
\nUNITs may be entered at the terminal to be evaluated and\
\nthe results displayed.  The process terminates when the\
\nvalue 'nil' is entered.  STRING1 is returned as the\
\nresult."},

    {0,0,0,0,0,0,0}
  };
