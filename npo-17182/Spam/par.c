/********************************************************************
   par.c 2.15 - parameter processor
   written by alan mazer, version 2: may 1984

   revision 1:  modified for one-token lookahead using function
       next_token_type().

   This is a general routine for processing a command line of
   valued and non-valued keywords, and values without keywords.
   The arguments are the input buffer, presumably just filled by
   the user, a table of allowable keywords, and their number.
   The table is an array of type 'parameter', as defined below.
   Par() goes through the input, assigning values to variables as
   necessary.  Details follow.
********************************************************************/

/********************************************************************/
/* structure definition - each allowable keyword has an entry in    */
/* the kwd table which specifies the keyword name, the type value   */
/* it takes, whether or not it's required, the number of values it  */
/* takes (possibly variable), the prompt to use for "prompt" mode,  */
/* a ptr to the target variables, the maximum value length for      */
/* strings, and an optional integer ptr to the number of values     */
/* the user input for the keyword.                                  */
/********************************************************************/

#include <stdio.h>
#include "spam.h"	/* necessary for spam session logging only           */

par(buf,kwd_table,nkeys)
char *buf;
struct parameter kwd_table[];
int nkeys;
{
    int i,j,k,l,m,n;
    int prompt_flag[20],def[20];
    int prompt_user_flag,type,found,index;
    unsigned char token[80],tmp[80],prompt[80];
    int parm_count,parm_type,last_type;
    if (nkeys>20) {
	printf("Par error: too many parameters.  Consult programmer.\n");
        return;
    }
/*
/*  For each non-numeric valued keyword, initialize value to null string.
/*  For each keyword, if value count is requested, initialize count to 0.
*/
    for (i=0;i<nkeys;i++) {
        if (kwd_table[i].value_type>=2) *kwd_table[i].value = '\0';
        if (kwd_table[i].input_count!=0) *kwd_table[i].input_count = 0;
    }
/*
/*  The def[] array indicates for each keyword whether it has been used.  This
/*  is necessary when keywords are omitted and values must be matched with
/*  unused keywords of corresponding type.
*/
    for (i=0;i<nkeys;i++) def[i]=0;
/*
/*  The following line clears the prompt mode flag.  If the user specifies the
/*  keyword "prompt" anywhere in the input, the subroutine will prompt for
/*  the values of any unused keywords.  This is done near the bottom of
/*  this subroutine.  When prompting is requested, prompt_user_flag is set.
*/
    prompt_user_flag=0;
/*
/*  Set prompt flag for those required keywords.  Par will prompt for values
/*  for those keywords which are required, as marked by the prompt_flag array.
*/
    for (i=0;i<nkeys;i++) {
	if (kwd_table[i].required_flag) prompt_flag[i]=1;
	else prompt_flag[i]=0;
    }
/*
/*  Get token using get_token().  Function returns token type, defined as
/*  follows:
/*      -1=eol, 0=int, 1=float, 2=quoted string, 3=unquoted string, 4=equal sign
/*  This is similar to type characteristic of keywords above.  Quotes are
/*  not returned along with quoted strings; they are removed first.
*/
    type = get_token(buf,token);	/* type is as above or -1 for EOL.    */
/*
/*  Main loop
*/
    while (type != -1) {                /* while buffer is not empty . . .    */
        index=(-1);			/* index is keyword index.  None yet. */
        while (index==-1 && type!=-1) { /* get index for kwd or value . . .   */
            found=0;                    /* no kwds matching token yet.        */
/*
/* if token is unquoted string (which it must be to be keyword), check against
/* the key names in kwd_table.  Accept abbreviations, but check for ambiguity.
/* If token matches non-valued keyword, or token is followed by equals sign,
/* or token matches valued-keyword and is followed by value of appropriate
/* type, must be legal.  Check for ambiguous keywords and print errors as
/* necessary.  Otherwise, save keyword number in index and ignore trailing
/* equal sign, if any.
*/
            if (type==3) for (i=0;i<nkeys;i++) {
                if (substring(token,kwd_table[i].name) && def[i]==0 &&
			(kwd_table[i].value_type==3 ||
		        next_token_type(buf)==4 || 
			(next_token_type(buf) == kwd_table[i].value_type) ||
			(next_token_type(buf)==3 && kwd_table[i].value_type==2))
			) {
                    found++;
		    if (found==1) index=i;
                    else if (found==2) {
                        printf("\"%s\" is ambiguous.  Please choose from:\n",
			    token);
                        printf("%s\n",kwd_table[index].name);
                        printf("%s\n",kwd_table[i].name);
                    }
                    else if (found>2) printf("%s\n",kwd_table[i].name);
		}
	    }
/*
/* if no keyword was found above, check to
/* see if it equals "prompt".  (The prompt keyword is described above.)
/* If the user is requesting prompting, set prompt_user_flag, and get the
/* next token.  Note that we can't just fill prompt_flag array because
/* some keywords may still be used later in the input.
*/
            if (found==0 && substring(token,"prompt")) {        /* or prompt */
                prompt_user_flag=1;
                type = get_token(buf,token);
            }
/*
/* if no keyword was found above, and . . .
/* if token is "stop", exit the parameter processor.  If not, we know
/* that token is not legal keyword, so if it's followed by an equals sign,
/* print an error, eat the equals sign, and get the next token.
*/
            else if (found==0 && strcmp(token,"stop")==0) return(1);
	    else if (found==0 && next_token_type(buf)==4) {
		printf("\"%s\" is not a legal keyword for this command.",token);
		printf("  Ignored.\n");
		type=get_token(buf,token);	/* eat equals sign */
		type=get_token(buf,token);	/* get next token  */
	    }
/*
/* if no keyword was found above . . .
/* token didn't match keyword so must be value.  If type is unquoted 
/* string (3), type is essentially quoted string (2); i.e., quotes are often
/* used to designate that strings are not keywords, even though get_token()
/* never passes the quotes themselves.  So may as well set type to 2 and
/* simplify type comparisons.  So set index to point to first unused
/* keyword of the appropriate token type.  If none, print error and get
/* next token.
*/
            else if (found==0) {            /* not keyword, so must be value */
                if (type==3) type=2;        /* (so type 3 must really be 2)  */
                for (i=0;i<nkeys && index==-1;i++) 
                    if (kwd_table[i].value_type==type && def[i]==0) index=i;
                if (index==-1) {
		    printf("Can't find an unused ");
                    if (type==0) printf("integer-");
                    else if (type==1) printf("floating-point-");
                    else if (type==2) printf("character-");
                    else printf("?-");
		    printf("valued keyword to match \"%s\".  Ignored.\n",token);
                    type=get_token(buf,token);
                }
            }
/*
/* else if token uniquely matched keyword, get next token.  If next is equal
/* sign and equal sign is legal in context, read another.  For each additional
/* equal sign, print warning and read another token.
*/
	    else if (found==1) {
                type=get_token(buf,token);
                if (type==4 && kwd_table[index].value_type<=2)
		    type=get_token(buf,token); /* 1 = is ok */
                while (type==4) {
		    printf("Extra equals sign (=) ignored.\n");
		    type=get_token(buf,token);
                }
	    }
/*
/* if keyword was ambiguous originally, get new one here and reloop.
*/
            else if (found>1) { /* not value, command, or recognized keyword */
                query("Keyword? ",token);
                index=(-1);
            }
        }
/*
/* If we've gotten this far, we must have located keyword.  Moreover, if
/* keyword was specified above, then we presently have the value in token.
/* If keyword was determined based on the value, then we still have that
/* value in token.  Set prompt_flag for keyword to -1 to indicate that we
/* have value for keyword (if prompt_user_flag==1, all prompt_flag entries
/* >=zero cause queries   ) and get the values to go with the keyword.
/* The first value is in token on entry.  (Getvals() returns true if
/* user entered "stop" at some point.)
*/
        if (index!=-1 && def[index]==0) {
	    prompt_flag[index]=(-1);
            if (get_vals(&type,token,buf,index,kwd_table,nkeys,def)) return(1);
	    def[index]=1;			/* mark keyword as used. */
        }
/*
/* If buffer is not empty, continue.
*/
    }
/*
/* Done reading input line.  If prompting for prompt keyword, print 
/* explanatory info.  
*/
    if (prompt_user_flag) {
        printf("Enter value(s) or \"yes\" to include the displayed ");
        printf("keyword, or press RETURN:\n");
    }
/*
/* For each keyword: if prompt keyword is in effect and keyword hasn't been
/* used, print the keyword followed by a question mark.  If the user
/* types 'y' or 'Y' for a non-valued keyword, or enters a value for a
/* valued keyword, call getvals().  Similarly, if keyword value is
/* required, print prompt, get token, and call get_vals(). 
*/
    for (index=0;index<nkeys;index++) {                  /* opt,prompting req,*/
        if (prompt_user_flag && prompt_flag[index]!=-1) {   /* and no val yet */
            sprintf(prompt,"%s? ",kwd_table[index].prompt);
            query(prompt,tmp);
            if ((kwd_table[index].value_count!=0 && tmp[0]!='\0')
		    || tmp[0]=='y' || tmp[0]=='Y') {
	        if (tmp[0]=='y' || tmp[0]=='Y') type=(-1);
		else type=get_token(tmp,token);
                if (get_vals(&type,token,tmp,index,kwd_table,nkeys,def))
		    return(1);
	        if (type!=-1) printf("Warning:  Extra input ignored.\n");
            }
        }
        else if (prompt_flag[index]==1) {                        /* required */
            sprintf(prompt,"%s? ",kwd_table[index].prompt);
            query(prompt,tmp);
	    type=get_token(tmp,token);
            if (get_vals(&type,token,tmp,index,kwd_table,nkeys,def)) return(1);
	    if (token[0]!='\0') printf("Warning:  Extra input ignored.\n");
        }
    }
    return(0);
}



get_vals(type_addr,par_token,par_buf,index,kwd_table,nkeys,def)
/********************************************************************/
/*     revision 1:  added token_ok variable.  Variable=1 initially, */
/*         but is reset if current token is bad.  Get_token is only */
/*         called later if token_ok is true (1).  This ensures that */
/*         if values are omitted, subsequent tokens in the input    */
/*         buffer won't be prematurely swallowed.                   */
/********************************************************************/
int *type_addr,index,nkeys;
char *par_token,*par_buf;
struct parameter *kwd_table;
int *def;
{
    int k,parm_count,last_type,parm_type,i,token_ok,type,check_buf;
    int var_valued=0;
    unsigned char local_buf[80],token[80],prompt[80];
/*
/* make copies of value count and value type, as well as input buffer and
/* token.  If value count is negative (indicating variable number of values),
/* get max number of values by negating, and set var_valued.
*/
    parm_count = kwd_table[index].value_count;
    if (parm_count<0) {
	parm_count=(-parm_count);
	var_valued=1;
    }
    parm_type = kwd_table[index].value_type;
    if (parm_count==0) strcpy(kwd_table[index].value,"found");
    strcpy(local_buf,par_buf);
    strcpy(token,par_token);
    type=(*type_addr);
/*
/* token_ok is cleared if a token is badly-typed.  Later, the token won't be
/* removed from the input buffer, but will be left in just in case it was a
/* keyword or something.
*/
    token_ok=1;
/*
/* check_buf is set when get_vals() must read in a new input buffer because
/* the current buffer has badly-typed data.  If check_buf is set, get_vals()
/* will check to make sure that all buf values are used for the keyword, and
/* will print errors as necessary.
*/
    check_buf=0;
/*
/* for each value up to value count:
/* if current token is empty, i.e., eol has been reached, print prompt
/* and get new input buffer.
*/
    for (i=0;i!=parm_count;i++) {
        while (type==-1) {
            printf("A value for \"%s\" is required.  Please specify.\n",
                kwd_table[index].name);
	    i=0;
            sprintf(prompt,"%s? ",kwd_table[index].prompt);
            query(prompt,local_buf);
	    check_buf=1;	/* make sure all values input are used */
            type=get_token(local_buf,token);
	    token_ok=0;
        }
/*
/* if token is "stop" return true.  Par will then return true, and original
/* calling function will (should) return to the command level (READY).
*/
        if (strcmp(token,"stop")==0) return(1);
/*
/* if (while) type is wrong for keyword, print message and get new input 
/* buffer.  Note that if any value of a multi-valued keyword is badly formed,
/* all will be tossed out (i=0 below).  Allow the user to exit loop using
/* "stop" if desired.
*/
        while (((type==0 || type==1) && parm_type!=type) ||
            ((parm_type==0 || parm_type==1) && parm_type!=type)) {
            pr_respec(parm_type,kwd_table[index].name);
	    i=0;
            sprintf(prompt,"%s? ",kwd_table[index].prompt);
            query(prompt,local_buf);
	    check_buf=1;	       /* make sure all values input are used */
            type=get_token(local_buf,token);
            if (strcmp(token,"stop")==0) return(1);
            token_ok=0;
        }
/*
/* We now have value of correct type, so store.  Variable i contains the
/* number of the correct value, i>=0.  If the value is a string, the string
/* is stored at the corresponding address by strncpy; if max_value_length
/* is 80, and the target array is 80 bytes long, strncpy will copy up to 
/* 80 characters, null-padding if necessary.  Just in case the string is
/* exactly max_value_length characters long, the last element is zeroed.
*/
        if (type==0)
            sscanf(token,"%d",kwd_table[index].value+i*4);
        else if (type==1)
            sscanf(token,"%f",kwd_table[index].value+i*4);
        else {
            strncpy(kwd_table[index].value+
		kwd_table[index].max_value_length*i,token,
                kwd_table[index].max_value_length);
            *(kwd_table[index].value+(i+1)*
            kwd_table[index].max_value_length-1) = '\0';
        }
/*
/* If calling program requested value count, store it at given address.
*/
        if (kwd_table[index].input_count!=0) 
	    *kwd_table[index].input_count = i+1;
/*
/* Save type of current value in last_type.  If value came out of original
/* input buffer, get next value from buffer (par_buf).  If get_vals() had
/* to request the input and it's in local_buf, get the next value from there.
*/
        last_type=min(2,type);
        if (token_ok) {
	    type=get_token(par_buf,token);
	    *type_addr=type;
	    strcpy(par_token,token);
        }
	else type=get_token(local_buf,token);
/*
/* If keyword takes a variable number of parameters . . .
/* if new value is unquoted string, see if legitimate keyword, and if so,
/* set current-value number to parm_count-1; this will, after increment at end 
/* of loop be equal to the max number of values stored in parm-count, causing
/* an exit from the loop.
/*     If new value is numeric or quoted string, but type is not the same   
/* as last value, this must be value for different keyword, so signal as
/* above by setting current-value number to last value number (parm_count-1);
*/
        if (var_valued) {
            if (type==3) {	      /* keyword-string ambiguity */
	        for (k=0;k<nkeys;k++)
		    if (substring(token,kwd_table[k].name) && def[k]==0)
			i=parm_count-1;
                if (i!=parm_count-1) type=2;   /* not keyword, must be string */
            }
            if (type!=last_type) i=parm_count-1;
        }
/*
/* continue getting values until done . . .
*/
    }
/*
/* If get_vals() queried user for buffer rather than using passed buffer,
/* check to make sure that we used all the values that the user gave, and
/* print warning, if not.
*/
    if (check_buf && get_token(local_buf,token)!=-1) 
	printf("Warning:  Ignoring extra input.\n");
/*
/* return to par().
*/
    return(0);
}



/****************************************************************************/
/* pr_respec - routine informs user of error and prepares for reinput.      */
/****************************************************************************/
pr_respec(parm_type,token)
int parm_type;
char *token;
{
    printf("Value for \"%s\" should be ",token);
    if (parm_type==0) printf("an integer.  ");
    else if (parm_type==1) printf("a floating-point (real) number.\n");
    else if (parm_type==2) printf("a character string.\n");
    else printf("a ?  ");
    printf("Please reenter or type \"stop\".\n");
}
