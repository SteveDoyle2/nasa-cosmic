/*************************************************************************
   func.c 2.15 - subroutine accepts mathematical expression of new plot
   in terms of current plots and image and displays new plot or image.
*************************************************************************/
#include <stdio.h>
#include <ctype.h>
#include "spam.h"

struct {char type; int *value;} s[50];    		/* parsing stack  */
int tos;
struct {int *data; char name[20];} symtab[50];		/* symbol table   */
int st;
struct {unsigned char op; int *arg1; int *arg2; int *arg3;} program[50];
int pc;

int trace=0,image_mode;	   /* set trace to see inner workings of parser. */
int calc_mode=0;	   /* the default calculation mode is int*4.     */

func(buf)
unsigned char *buf;
{
    char func_buf[80],name[80],buf2[5];
    int *data,*getloc(),*image_buffer,baddiv();
    float f1=1.0;
    short stemp,c,c18=18,c10=10,c255=255;
    register int *fp,*fq;
    register unsigned char *p;
    register i,j,k;
    int t1,t2,index;
    unsigned char *point,*malloc();
    int bad_name=1;
    static struct parameter kwds[]={
        {"function",2,1,1,
	    "function (\"plotname|image=expression\" with quotes) ",
	    "",80,0}
        };
    if (display_mode==1) {
	printf("No func commands in display mode.  Use \"return\" to exit.\n");
	return;
    }
    kwds[0].value = func_buf;
    if (par(buf,kwds,1)) return;	/* get function, return on error.  */
    lower_case(func_buf);
/*
/*      set pointers for parsing table (tos), symbol table (st), and 
/*      program (pc).  all pointers point to the last element inserted into
/*      the table, -1 if none.
*/
    tos = -1;
    st = -1;
    pc = -1;
/*
/*      store name in "name", and set image_mode iff name is "image".
*/
    i=0;
    j=0;
    if (isalpha(func_buf[0])==0 && func_buf[0]!='?') {
	printf("Output curve has bad or missing name.\n");
	return;
    }
    while (i<strlen(func_buf) && isspace(func_buf[i])==0 && func_buf[i]!='=' &&
        func_buf[i]!='\\') name[j++]=func_buf[i++];
    name[j]='\0';
    image_mode=0;
    if (substring(name,"image")) image_mode=1;
    while (isspace(func_buf[i]) && i<strlen(func_buf)) i++;
    if (func_buf[i]!='=') {
	printf("Function is unquoted or missing equals sign.\n");
	return;
    }
/*
/*      compile code for expression using eval.  the eval routine returns
/*      a set of quadruples as defined by the struct def above where the first
/*      element is an integer opcode, <=10 for integer operations.  the 
/*      remaining members of the entry contain pointers to the first and
/*      second operands, and a pointer showing where to place the result.
/*      pointers are zero if the source of data is the image, in which case
/*      space will be allocated and filled below (for each spectrum in the 
/*      image, one at a time).  trace the symbol table and program, if desired.
*/
    if (eval(func_buf+i+1)) return;
    if (trace) {
        printf("Symbol table\n");
        for (i=0;i<=st;i++) printf("%d %s\n",symtab[i].data,symtab[i].name);
        printf("Program (op,arg):\n");
        for (i=0;i<=pc;i++) printf("%d %d %d %d\n",program[i].op,
            program[i].arg1,program[i].arg2,program[i].arg3);
    }
/*
/*      execute the program on the image and possibly other plots. space
/*      for a single spectrum ("image_buffer") is allocated and its location
/*      substituted in the program for the zero pointers mentioned above.
/*      s[tos-1].value referenced below points to the final result after
/*      execution as directed by the compiling routine, eval().  Execute is
/*      called for each spectrum in the image.  Note that here and elsewhere in
/*      these routines, the same variables are used to store both integers and
/*      floats.  Floats are often necessary, so pointers are coerced frequently.
*/
    if (image_mode) {
        if (!img_in_core) {
	    printf("Image keyword not allowed without image.\n");
	    return;
        }
        image_buffer = getloc("image");
        if (image_buffer==0) return;
        for (i=0;i<=pc;i++) {
            if (program[i].arg1==0) program[i].arg1=image_buffer;
            if (program[i].arg2==0) program[i].arg2=image_buffer;
            if (program[i].arg3==0) program[i].arg3=image_buffer;
        }
        fq=s[tos-1].value+img_numchan-1;
        p = img_data;
        for (i=img_nl;i!=0;i--) {
            for (j=img_bw;j!=0;j--) {
                fp=image_buffer;
                for (k=img_numchan;k!=0;k--) {
                    if (calc_mode==0) *(int *)fp++ = *p;
                    else *(float *)fp++ = *p;
                    p+=img_bw;
                }
                execute();
                for (k=img_numchan;k!=0;k--) {
                    p-=img_bw;
                    if (calc_mode==0) *p=(unsigned char)*(int *)fq--;
                    else *p=(unsigned char)(*(float *)fq-- + 0.5);
                }
                fq+=img_numchan;
                p++;
            }
            p+=img_ns-img_bw;
        }
        imgcode();
        redraw();
        stemp=40+(img_bw-32);	/* rewrite band # (may be preceded by ratio) */
        vdtxtsiz(&c10,&f1);
        sprintf(buf2,"%d",disp_band);
        c=strlen(buf2);
        vdtext(&unit,&c4,&c255,&c255,&c255,&stemp,&c18,&c1,&c,buf2);
        draw_plots();		/* redraw plot boxes */
        img_modified = 1;
    }
    else {
/*
/*          execute the program on plots and create another plot.  then see if
/*          the specified plot name is in use.  if it is, k = index and the plot
/*          is replaced; otherwise k = -1 and a new plot is added.  allocate
/*          space for the new plot data, create the plot using save_plot_data,
/*          and return.
*/
        execute();
	while (bad_name) {
            get_plot_num(name,0,&index);
            if (index==-2) printf("\"%s\" is ambiguous.  ",name);
	    else if (index==-1 && check_old_name(name)) ; /* bad name if true */
	    else {
		get_libplot_num(name,&t1,&t2,1,3);
		if (t1!=-1) printf(
		    "There is already a library plot named \"%s\".\n",name);
		else bad_name=0;
            }
	    if (bad_name) {
		query("Name (RETURN for READY) ? ",name);
		if (name[0]=='\0') return;
	    }
	}
        point = malloc(img_numchan);
        if (point==NULL) {
            printf("Insufficient memory.\n");
            return;
        }
        data = s[tos-1].value;
        for (i=0;i<img_numchan;i++) {
            if (calc_mode==0) point[i] = (unsigned char)*((int *)data+i);
            else point[i] = (unsigned char)(*((float *)data+i)+0.5);
        }
        save_plot_data(name,point,3,0,0,0,0,index,0);
	draw_plots();			              /* draw new plot. */
        free(point);
    }
/*
/*      free allocated areas
*/
    for (i=0;i<=st;i++) free(symtab[i].data);
}


/***************************************************************************/
/* eval - eval takes the right-hand side of the expression, and compiles a */
/* set of operations which are stored in the program array to do the       */
/* calculation on the image or plots.                                      */
/*                                                                         */
/*     The algorithm essentially implements shift-reduce parsing for the   */
/* following grammar (E=expression, T=term, I=identifier or constant,      */
/* F=factor, M=function):                                                  */
/*                                                                         */
/*		E -> E+T | E-T | +T | -T | T                               */
/*		T -> T*F | T/F | F                                         */
/*		F -> I | (E) | M(E)                                        */
/*                                                                         */
/* Labels ending in zero denote that the program expects to see            */
/* the start of a  nonterminal symbol   at that point.  For example, at    */
/* label e0, the algorithm expects an expression start, i.e., a + or -     */
/* symbol; if neither of these is found, then control falls through to     */
/* t0/f0  where we look for the start of a term or factor (identical at    */
/* that point).                                                            */
/*     Labels ending in one indicate that the the algorithm is looking for */
/* the context of the expression, term, factor, function (M), and that a   */
/* reduction, as well as a possible operator call, are forthcoming.        */
/*                                                                         */
/* Details follow.                                                         */
/***************************************************************************/
eval(buf)
char *buf;
{
        int *getloc();
        int i;
/*
/* see if we need float mode first, and then make sure there aren't spaces.
*/
        for (i=0;i<strlen(buf);i++) if (buf[i]=='.') calc_mode=1;
        for (i=0;i<strlen(buf);i++) if (isspace(buf[i])) {
	    strcpy(buf+i,buf+i+1);
	    i--;
        }
/*
/* get first lexeme; return on error.
*/
        if (push_next_lexeme(buf)) return(1);
/*
/* assume current lexeme denotes start of expression, so look for unary op.  ops
/* + and - when used in a binary context are handled elsewhere.
/* (push_next_lexeme returns true only on errors.)
*/
e0:	if (s[tos].type=='+' || s[tos].type=='-') {
		if (push_next_lexeme(buf)) return(1);
		goto t0;
	}
/* 
/* not start of expression, so look for identifier, function name, parenthesis.
/* if identifier (could be plot or constant), reduce to factor and look for
/* context.  If parenthesis, goto e0 looking for start of expression.  If 
/* mathematical function (e.g., abs), get next lexeme and find context at m1:.
/* if none of the above, print an error.
*/
t0:
f0:
	if (s[tos].type=='I') {
		s[tos].type='F';
		if (push_next_lexeme(buf)) return(1);
		goto f1;
	}
	if (s[tos].type=='(') {
		if (push_next_lexeme(buf)) return(1);
		goto e0;
	}
	if (s[tos].type=='M') {
		if (push_next_lexeme(buf)) return(1);
		goto m1;
	}
	printf("Found %c instead of (, constant, plotname, or function.\n",
		s[tos].type);
	return(1);
/*
/* find context of mathematical function (e.g., abs).  This is really not 
/* necessary, since all it does it verify that a parenthesis follows the
/* function name and this is already known (push_next_lexeme looks for the
/* parenthesis to distinguish between identifiers and function names).
/* It's included here to keep things reasonably consistent and independent
/* of lexical analysis.  get next lexeme and look for inside expression.
*/
m1:	if (s[tos-1].type=='M' && s[tos].type=='(') {
		if (push_next_lexeme(buf)) return(1);
		goto e0;
	}
	printf("Mathematical function in illegal context.\n");
	return(1);
/*
/* find context of term.  if term followed by * or /, get next lexeme, and
/* find look for factor at f0.  if preceded by E+, E-, +, or -, find
/* context of expression at label e1:.  if term alone, change to E and find
/* context at label e1:.  if none of these apply, there must be an error.
*/
t1:	if (s[tos-1].type=='T' && (s[tos].type=='*' || s[tos].type=='/')) {
		if (push_next_lexeme(buf)) return(1);
		goto f0;
	}
	if (tos>2 && s[tos-3].type=='E' && 
	    (s[tos-2].type=='+' || s[tos-2].type=='-') && s[tos-1].type=='T') {
                if (++pc>49) {
                    printf("Program overflow.  Consult programmer.\n");
                    return(1);
                }
		if (s[tos-2].type=='+') program[pc].op = 1+10*calc_mode;
		else /* s[tos-2].type=='-' */ program[pc].op = 2+10*calc_mode;
                program[pc].arg1 = s[tos-3].value;
                program[pc].arg2 = s[tos-1].value;
                program[pc].arg3 = getloc("");
                if (program[pc].arg3==0) return(1);
		s[tos-3].type='E';
                s[tos-3].value = program[pc].arg3;
		s[tos-2]=s[tos];
		tos-=2;
		goto e1;
	}
	if (tos>1 && (s[tos-2].type=='+' || s[tos-2].type=='-') && 
            s[tos-1].type=='T') {
		if (s[tos-2].type=='-') {
                    if (++pc>49) {
                        printf("Program overflow.  Consult programmer.\n");
                        return(1);
                    }
                    program[pc].op = 5+10*calc_mode;
                    program[pc].arg1 = s[tos-1].value;
                    program[pc].arg2 = 0;
                    program[pc].arg3 = getloc("");
                    if (program[pc].arg3==0) return(1);
                }
		s[tos-2].type='E';
		s[tos-2].value=program[pc].arg3;
		s[tos-1]=s[tos];
		tos--;
		goto e1;
	}
	if (s[tos-1].type=='T') {
		s[tos-1].type='E';
		goto e1;
	}
	printf("Unknown problem evaluating function.  Sorry.\n");
	return(1);
/*
/* look for context of factor.  If         preceded by * or /, perform 
/* operation, reduce to T, and find context at t1:.  Otherwise, simply
/* reduce to T, and find context, again at t1:.  
*/
f1:	if (tos>2 && s[tos-3].type=='T' && 
	    (s[tos-2].type=='*' || s[tos-2].type=='/') && s[tos-1].type=='F') {
                if (++pc>49) {
                    printf("Program overflow.  Consult programmer.\n");
                    return(1);
                }
		if (s[tos-2].type=='*') program[pc].op = 3+10*calc_mode;
		else /* s[tos-2].type=='/' */ program[pc].op = 4+10*calc_mode;
                program[pc].arg1 = s[tos-3].value;
                program[pc].arg2 = s[tos-1].value;
                program[pc].arg3 = getloc("");
                if (program[pc].arg3==0) return(1);
		s[tos-3].type='T';
                s[tos-3].value = program[pc].arg3;
		s[tos-2]=s[tos];
		tos-=2;
		goto t1;
	}
	if (s[tos-1].type=='F') {
		s[tos-1].type='T';
		goto t1;
	}
	printf("Unknown problem evaluating function.  Sorry.\n");
	return(1);
/*
/* Find context of expression.  Reduce and call operators (and functions like
/* abs) as possible.
*/
e1:	if (tos>2 && s[tos-3].type=='M' && s[tos-2].type=='(' && 
	    s[tos-1].type=='E' && s[tos].type==')') {
		if (strcmp(s[tos-3].value,"abs")==0) {
                    if (++pc>49) {
                        printf("Program overflow.  Consult programmer.\n");
                        return(1);
                    }
                    program[pc].op = 6+10*calc_mode;
                    program[pc].arg1 = s[tos-1].value;
                    program[pc].arg2 = 0;
                    program[pc].arg3 = getloc("");
                    if (program[pc].arg3==0) return(1);
		    s[tos-3].value = program[pc].arg3;
                }
		else if (strcmp(s[tos-3].value,"sum")==0) {
                    if (++pc>49) {
                        printf("Program overflow.  Consult programmer.\n");
                        return(1);
                    }
                    program[pc].op = 7+10*calc_mode;
                    program[pc].arg1 = s[tos-1].value;
                    program[pc].arg2 = 0;
                    program[pc].arg3 = getloc("");
                    if (program[pc].arg3==0) return(1);
		    s[tos-3].value = program[pc].arg3;
                }
		else {
                    printf("Illegal math function.  Ignored.\n");
                    s[tos-3].value = s[tos-1].value;
                }
		s[tos-3].type='F';
		tos-=3;
		if (push_next_lexeme(buf)) return(1);
		goto f1;
	}
	if (tos>1 && s[tos-2].type=='(' && s[tos-1].type=='E' && 
	    s[tos].type==')') {
		s[tos-2].type='F';
		s[tos-2].value=s[tos-1].value;
		tos-=2;
		if (push_next_lexeme(buf)) return(1);
		goto f1;
	}
	if (s[tos-1].type=='E' && (s[tos].type=='+' || s[tos].type=='-')) {
		if (push_next_lexeme(buf)) return(1);
		goto t0;
	}
	if (tos==1 && s[tos-1].type=='E' && s[tos].type=='~') {
		if (trace) printf("Done!\n");
		if (trace) print_stack();
		return(0);
	}
	printf("Unbalanced parentheses or missing operator.\n");
	return(1);
}


/****************************************************************************/
/* print_stack - this routine is used by debugging only.  If global "trace" */
/* which is defined at the top of this file is set to 1, print_stack will   */
/* periodically print out what's on the stack to show reductions, etc.      */
/****************************************************************************/
print_stack()
{
	int i;
	printf("Stack:\n");
	for (i=0;i<=tos;i++) printf("%10c",s[i].type);
	printf("\n");
	for (i=0;i<=tos;i++) printf("%10d",s[i].value);
	printf("\n");
}


/************************************************************************/
/* push_next_lexeme - get next lexical unit from the input stream and   */
/* place it on the stack, s[].  if plotname, allocate space for it and  */
/* read the data in.  if constant, allocate space and fill with const.  */
/************************************************************************/
push_next_lexeme(buf)
char *buf;
{
	char token[80];
	int *p1,*getloc(),const;
	int i,j,o,real;
        unsigned char *malloc();
/*
/* if single-character token, store character in type member of stack
/* entry, 0 in value member, and push onto the stack.  Remove from
/* input.
*/
	if (buf[0]=='(' || buf[0]==')' || buf[0]=='-' || buf[0]=='+' ||
		buf[0]=='*' || buf[0]=='/' || buf[0]=='=') {
		if (++tos>49) {
                    printf("Parse stack overflow.  Consult programmer.\n");
                    return(1);
                }
		s[tos].type=buf[0];
		s[tos].value = 0;
		strcpy(buf,buf+1);
		if (trace) printf("Next lexeme = %c\n",s[tos].type);
	}
/*
/* else if at end of input string, lexeme is "~" with 0 value.  Push.
*/
	else if (buf[0]=='\0') {
		if (trace) printf("End of lexeme list\n");
		if (++tos>49) {
                    printf("Parse stack overflow.  Consult programmer.\n");
                    return(1);
                }
		s[tos].type='~';
		s[tos].value = 0;
	}
/*
/* else if alphabetic character or ?, must be start of func (e.g., abs) or 
/* plot name.  store in token and remove from buffer.  if followed by 
/* left parenthesis, store pointer to name as value and use type M.
/* (M is an arbitrary letter.)  otherwise, lexeme must be a plot or
/* "image"; if former, allocate storage for the data, and push onto stack with
/* type I and value pointing to the data.  otherwise, push type I and null
/* onto stack.
*/
	else if (isalpha(buf[0]) || buf[0]=='?') {
		i=0;
		o=0;
		while (isalpha(buf[i]) || isdigit(buf[i]) || buf[i]=='_' || 
		    buf[i]=='?')
		        token[o++]=buf[i++];
		token[o]='\0';
		strcpy(buf,buf+i);
                if (buf[0]=='(') {
		    p1=(int *) malloc(strlen(token+1));
		    if (p1==NULL) {
			printf("Insufficient memory.\n");
			return(1);	/* error - get READY prompt again */
		    }
		    strcpy(p1,token);
		    if (++tos>49) {
                        printf("Parse stack overflow.  Consult programmer.\n");
                        return(1);
                    }
		    s[tos].type='M';
		    s[tos].value = p1;
		}
		else {
		    get_plot_num(token,0,&j);
                    if (substring(token,"image") && j==-1 && image_mode) p1=0;
		    else {
			if (j==-1)
                            printf("Plot \"%s\" doesn't exist.  ",token);
			else if (j==-2)
			    printf("\"%s\" is ambiguous.  ",token);
                        while (j<0) {
			    query("Name of plot (RETURN for READY) ? ",token);
			    if (token[0]=='\0') return(1);
			    if (!check_old_name(token)) {
				get_plot_num(token,0,&j);
			        if (j==-1)
                                    printf("Plot \"%s\" doesn't exist.  ",
					token);
			        else if (j==-2)
				    printf("\"%s\" is ambiguous.  ",token);
			    }
                        }
		        p1 = getloc(token);
                        if (p1==0) return(1);
		        for (i=0;i<img_numchan;i++) {
                             if (calc_mode==0) 
                                 *((int *)p1+i)= *(plots[j].data+i);
                             else *((float *)p1+i)= *(plots[j].data+i);
                        }
                    }
		    if (++tos>49) {
                        printf("Parse stack overflow.  Consult programmer.\n");
                        return(1);
                    }
		    s[tos].type='I';
		    s[tos].value = p1;
		}
		if (trace) printf("Next lexeme = %s\n",token);
	}
/*
/* else if digit, allocate storage and fill img_numchan array locations
/* with the constant.  integer or real values are both okay.  type is I,
/* and the value member of the stack entry points to the data, as usual.
*/
        else if (isdigit(buf[0]) || buf[0]=='.') {
		i=0;
		o=0;
		real=0;
		while (isdigit(buf[i]) || buf[i]=='.') {
			if (buf[i]=='.') {
			    if (real) printf("Second decimal point ignored.\n");
			    real=1;
			}
			token[o++]=buf[i++];
		}
		token[o]='\0';
		strcpy(buf,buf+i);
		p1 = getloc(token);
                if (p1==0) return(1);
		if (real) sscanf(token,"%f",(float *)&const);
                else /* int number */ if (calc_mode==1) {
                    sscanf(token,"%d",(int *)&const);
                    *((float *)&const) = *((int *)&const);
                }
		else sscanf(token,"%d",(int *)&const);
		if (calc_mode==0) for (i=0;i<img_numchan;i++)
                    *((int *)p1+i)= *((int *)&const);
		else for (i=0;i<img_numchan;i++)
                    *((float *)p1+i)= *((float *)&const);
		if (++tos>49) {
                    printf("Parse stack overflow.  Consult programmer.\n");
                    return(1);
                }
		s[tos].type='I';
		s[tos].value = p1;
		if (trace) printf("Next lexeme = %s\n",token);
	}
/*
/* else lexeme is illegal, so print message and attempt to continue.
*/
	else {
	    printf("Illegal operator in function.\n");
	    return(1);
	}
	if (trace) print_stack();
/*
/* return
*/
	return(0);
}


/*************************************************************************
    execute - takes the program created by eval and executes it
*************************************************************************/
execute()
{
    int i;
    int radd(),rsub(),rmul(),rdiv(),rneg(),rabs(),rsum();
    int iadd(),isub(),imul(),idiv(),ineg(),iabs(),isum();
    static struct {int (*subr)();} oprtn[]=
       {iadd,isub,imul,idiv,ineg,iabs,isum,iadd,iadd,iadd,
       radd,rsub,rmul,rdiv,rneg,rabs,rsum,radd,radd,radd};
    int *data;
    for (i=0;i<=pc;i++)
        (*oprtn[program[i].op-1].subr)
            (program[i].arg1,program[i].arg2,program[i].arg3);
    data = s[tos-1].value;
    if (calc_mode==0) for (i=0;i<img_numchan;i++) {
	if (*((int *)data+i)>255) *((int *)data+i)=255;
	if (*((int *)data+i)<0) *((int *)data+i)=0;
    }
    else for (i=0;i<img_numchan;i++) {
	if (*((float *)data+i)>255) *((float *)data+i)=255;
        if (*((float *)data+i)<0) *((float *)data+i)=0;
    }
}


/************************************************************************/
/* The following routines implement the basic mathematical operators    */
/* allowed for plot math.  Each takes takes three pointers, performs    */
/* the operation on the data specified by the first pointer(s) and      */
/* the result using pointer three.                                      */
/************************************************************************/
radd(p1,p2,p3)
float *p1,*p2,*p3;
{
    int i; register float *a,*b,*c;
    a=p1; b=p2; c=p3;
    for (i=0;i<img_numchan;i++) *c++ = *a++ + *b++;
}

rsub(p1,p2,p3)
float *p1,*p2,*p3;
{
    int i; register float *a,*b,*c;
    a=p1; b=p2; c=p3;
    for (i=0;i<img_numchan;i++) *c++ = *a++ - *b++;
}

rmul(p1,p2,p3)
float *p1,*p2,*p3;
{
    int i; register float *a,*b,*c;
    a=p1; b=p2; c=p3;
    for (i=0;i<img_numchan;i++) *c++ = *a++ * *b++;
}

rdiv(p1,p2,p3)
float *p1,*p2,*p3;
{
    int i; register float *a,*b,*c;
    a=p1; b=p2; c=p3;
    for (i=0;i<img_numchan;i++) {
        if (*b!=0.0) *c++ = *a++ / *b++;
        else {
            *c++ = 255.0;
            a++;
            b++;
        }
    }
}

rneg(p1,p2,p3)
float *p1,*p2,*p3;
{
    int i; register float *a,*c;
    a=p1; c=p3;
    for (i=0;i<img_numchan;i++) *c++ = - *a++;
}

rabs(p1,p2,p3)
float *p1,*p2,*p3;
{
    int i; register float *a,*c;
    a=p1; c=p3;
    for (i=0;i<img_numchan;i++) if (*p1<0) *c++ = - *a++; else *c++ = *a++;
}

rsum(p1,p2,p3)
float *p1,*p2,*p3;
{
    int i; register float *a,*c; float sum=0.0;
    a=p1; c=p3;
    for (i=0;i<img_numchan;i++) sum+= *a++;
    for (i=0;i<img_numchan;i++) *c++ =sum;
}

iadd(p1,p2,p3)
int *p1,*p2,*p3;
{
    int i; register *a,*b,*c;
    a=p1; b=p2; c=p3;
    for (i=0;i<img_numchan;i++) *c++ = *a++ + *b++;
}

isub(p1,p2,p3)
int *p1,*p2,*p3;
{
    int i; register *a,*b,*c;
    a=p1; b=p2; c=p3;
    for (i=0;i<img_numchan;i++) *c++ = *a++ - *b++;
}

imul(p1,p2,p3)
int *p1,*p2,*p3;
{
    int i; register *a,*b,*c;
    a=p1; b=p2; c=p3;
    for (i=0;i<img_numchan;i++) *c++ = *a++ * *b++;
}

idiv(p1,p2,p3)
int *p1,*p2,*p3;
{
    int i; register *a,*b,*c;
    a=p1; b=p2; c=p3;
    for (i=0;i<img_numchan;i++) {
	if (*b!=0) *c++ = *a++ / *b++;
	else {
	    *c++ = 255;
	    a++;
	    b++;
	}
    }
}

ineg(p1,p2,p3)
int *p1,*p2,*p3;
{
    int i; register *a,*c;
    a=p1; c=p3;
    for (i=0;i<img_numchan;i++) *c++ = - *a++;
}

iabs(p1,p2,p3)
int *p1,*p2,*p3;
{
    int i; register *a,*c;
    a=p1; c=p3;
    for (i=0;i<img_numchan;i++) if (*p1<0) *c++ = - *a++; else *c++ = *a++;
}

isum(p1,p2,p3)
int *p1,*p2,*p3;
{
    int i; register *a,*c; float sum=0.0;
    a=p1; c=p3;
    for (i=0;i<img_numchan;i++) sum+= *a++;
    for (i=0;i<img_numchan;i++) *c++ =sum;
}


/**************************************************************************
    getloc allocates space for a new spectrum, updates the symbol table,
    and returns the pointer.
**************************************************************************/
int *getloc(buf)
char *buf;
{
    int *calloc(),*p1;
    p1 = calloc(img_numchan,4);
    if (p1==NULL) {
        printf("Insufficient memory.\n");
	return(0);
    }
    else {
        if (++st>49) {
            printf("Symbol table overflow.  Consult programmer.\n");
            return(0);
        }
        symtab[st].data = p1;
        if (strlen(buf)!=0) strcpy(symtab[st].name,buf);
        else sprintf(symtab[st].name,"t%d",st);
        return(p1);
    }
}


/******************************************************************************
    baddiv - print out error
******************************************************************************/
static int mesgout=0;
baddiv()
{
    if (!mesgout) {
        printf("Illegal operation, probably div-by-zero.  ");
        printf("Future errors will be ignored.\n");
        mesgout=1;
    }
}
