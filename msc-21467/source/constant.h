/*   CLIPS Version 4.30   4/25/89 */

/****************************************************************/
/* --------------- CLIPS Constants and Macros ----------------- */
/****************************************************************/

#define FALSE 0	
#define TRUE 1
#define CLIPS_FALSE 0
#define CLIPS_TRUE 1
#define OFF 0
#define ON 1
#define LHS 0
#define RHS 1
#define EOS '\0'

#define EXACTLY       0
#define AT_LEAST      1
#define NO_MORE_THAN  2

#define INSIDE  0
#define OUTSIDE 1

/* TOKEN AND TYPE VALUES */

#define NUMBER      0
#define STRING      1
#define WORD        2
#define LNOT        3
#define LAND        4
#define LOR         5
#define COAMP       6
#define SEPARATOR   7
#define BINDER      8
#define MULTIPLE    9
#define LPAREN     10
#define RPAREN     11
#define SINGLE     12
#define STOP       13
#define OPERATOR   14
#define BWORD      15
#define BWORDS     16
#define KUNKNOWN   17

#define INDEX      18
#define POINTER    19


#define FCALL      20  
#define PATTERN    21
#define PAT_AND    22


#define NOTBWORD   23
#define NOTBWORDS  24
#define PAT_OR     23
#define PAT_NOT    24
#define PAT_TEST   25

#define BLOCKED    26
#define RVOID      27

#define MAY_BE_POINTER 28


typedef long int FACT_ID;
typedef int TYPE;
