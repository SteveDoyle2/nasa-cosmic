/********************************************************************
   token.c 2.15 (token-handling subroutines)
   author:  alan mazer, version 2: may 1984

   get_token:
       1) skip leading spaces and non-recognized chars, if any.
          print warnings, if necessary.
       2) if at end of line, return empty token.
       3) if equal sign, save in token; type is 4.
       4) if -,digit or decimal point, must be number.  assume type
           is integer (0), until decimal point indicates real (1).
           save in token.
       5) if string (surrounded by double quotes), type=2.  read
           everything up to end of line, and if no ending quote,
           assume at end.  save in token.  (saved without quotes.)
       6) if keyword (unquoted string), save until ",","=",sp,eol.
       7) remove part saved in token from buf.

   revision 1 (july 1984):
       added "more ? " feature which allows line continuation for
       commands.  Any line ending in a back-slash (\) may be
       continued onto the next line at the "more ? " prompt.  The
       only restriction is that the \ character may not come in the
       middle of a numeric constant or an unquoted string; if this
       happens, the subroutine will split the constant or string in
       half, provoking a flurry of error messages or other
       undesired effects.
********************************************************************/

#include <ctype.h>
get_token(buf,token)
char *buf;
unsigned char *token;
{
    int i=0,o=0,type=0;
    unsigned char c;
    token[0] = '\0';
    c=buf[0];
    while (isdigit(c)==0 && isalpha(c)==0 && c!='-' && c!='=' && c!='?' &&
	c!='.' && c!='\0' && c!='"' && c!='/' && c!='[' && c!=']') {
	if (c=='\\' && i==strlen(buf)-1) {
	    query("More ? ",buf);
	    i=(-1);
	}
	else if (isspace(c)==0 && c!=',')
	    printf("Ignoring illegal character: %c\n",c);
        c=buf[++i];
    }
    if (c=='\0') return(-1);
    if (c=='=') {
	token[o++] = buf[i++];
	token[o] = '\0';
        type=4;
    }
    else if (c=='-' || (c=='.' && isdigit(buf[i+1])) || isdigit(c)) {
        type=0;
        if (c=='-') token[o++]=buf[i++];
        while (isdigit(buf[i]) || buf[i]=='.') {
            if (buf[i]=='.') {
                if (type==0) {
		    type=1;
                    token[o++] = buf[i++];
		}
                else {
		    printf("Extra decimal point ignored.\n");
		    i++;
		}
            }
            else token[o++] = buf[i++];
        }
        if (buf[i]=='-') i++;	/* allow - to be used for numerical ranges */
        token[o] = '\0';
        if (strcmp(token,"-")==0 || strcmp(token,"-.")==0) {
	    printf("Replacing illegal number \"%s\" with \"0.0\".\n",token);
	    strcpy(token,"0.0");
        }
    }
    else if (c=='"') {
        type=2;
        i++;
        while (buf[i] != '"' && i<strlen(buf)) {
            if (buf[i]!='\\' || i!=strlen(buf)-1) token[o++]=buf[i++];
	    else {
		query("More ? ",buf);
		i=0;
	    }
        }
        if (buf[i] != '"') printf("Ending quote assumed.\n");
        else i++;
        token[o] = '\0';
    }
    else {
        type=3;
        while (isspace(buf[i])==0 && i<strlen(buf) && buf[i]!='=' &&
	    buf[i]!=',' && buf[i]!='\\') token[o++]=buf[i++];
        token[o] = '\0';
    }
    strcpy(buf,buf+i);
    return(type);
}


/*********************************************************************/
/*  next_token_type - function  returns type of the the next token   */
/*                    in the input buffer without removing it.       */
/*********************************************************************/
next_token_type(buf)
char *buf;
{
    int i=0;
    unsigned char c;
    c=buf[0];
    while (isdigit(c)==0 && isalpha(c)==0 && c!='-' && c!='=' && c!='?' &&
	c!='.' && c!='\0' && c!='"') {
        c=buf[++i];
    }
    if (c=='\0') return(-1);
    if (c=='=') return(4);
    else if (c=='-' || c=='.' || isdigit(c)) {
        if (c=='-') i++;
        while (isdigit(buf[i]) || buf[i]=='.') {
            if (buf[i]=='.') return(1);
            else i++;
        }
        return(0);
    }
    else if (c=='"') return(2);
    else return(3);
}
