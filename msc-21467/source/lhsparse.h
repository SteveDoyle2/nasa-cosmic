/*   CLIPS Version 4.30   4/25/89 */

#ifndef _EXPRESSN_
#include "expressn.h"
#endif

/****************************************************************/
/* NODE STRUCTURE:  Stores information about the intermediate   */  
/*   parsed representation of the lhs of a rule.                */
/*                                                              */
/*   Type:        One of PAT_AND, PAT_OR, PATTERN, PAT_TEST,    */
/*                STRING, WORD, NUMBER, COAMP, FCALL, SINGLE,   */
/*                MULTIPLE, BWORD, or BWORDS.                   */
/*   State:       Set to 'n' if not'ed PATTERN or if a literal  */
/*                value is preceded by a '~'. Otherwise set to  */
/*                'o'.                                          */
/*   Fvalue:      Holds numeric value is type is NUMBER.        */
/*   Svalue:      Holds symbolic value if type is STRING or     */
/*                WORD. Holds fact address is type is PATTERN.  */
/*   Expression:  Holds expression if type is PAT_TEST, COAMP,  */ 
/*                or FCALL.                                     */
/*   Right:       Points to node structure to the 'right'.      */
/*   Bottom:      Points to node structure to the 'bottom'.     */
/****************************************************************/
struct node
  {
   int type;                     
   char state;                   
   float fvalue;                
   struct draw *svalue;          
   struct test *expression;      
   struct node *right;
   struct node *bottom;
  };
  
  
#if STUDENT

#define MAX_SALIENCE  2
#define MIN_SALIENCE -2

#else

#define MAX_SALIENCE  10000
#define MIN_SALIENCE -10000

#endif
 
