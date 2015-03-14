/************************************************************************/
/*									*/
/* mixture - applies the mixture analysis on a portion of an image or   */
/* on a plot; using either amplitude encoding, slope encoding, binary   */
/* encoding, euclidean distance, and least square fit method.		*/
/* 									*/
/* NOTE: some of the codes are written using static arrays.  This may   */
/*       cause some difficulties when img_numchan will be expanded      */
/*       beyond 512.							*/
/*									*/
/* Date: Sep. 11, 1985							*/
/*									*/
/************************************************************************/
#include	<stdio.h>
#include	<math.h>
#include	<ctype.h>		/* mixture.c 2.15 */
#include	"spam.h"

#define N8 8
#define N256 256
#define N512 512
#define N1024 1024

struct sample {
   unsigned char data[N512];
};
struct sample samples[N8];
int	num_minerals;
float   spec_lib_subset[N8][N512];	/* hold library plot data	*/
float   orthon_basis[N8][N512]; 	/* orth. basis for library plot */
float   trunc_spec_lib_subset[N8][N8]; 
float   trunc_meas_spec[N8];
float   trunc_mix_spec[N8];
float	mix_prop[N8];
float	mix_coeff[N8];
float   factors[N8];
float	*inverse;
float	*meas_spec;
char	lib_name[N8][32];
int     *positions;
int     cue[N8];
float   squared_distance[1],geom_illum[1];


/************************************************************************/
/*									*/
/* mixture - main driver for the mixture programs.  Depending on the    */
/* user specified parameters, the driver will call an appropriate       */
/* subroutine (either mixture-plot or mixture-area).			*/
/*									*/
/************************************************************************/
mixture(buf)
unsigned char *buf;
{
   int i,err,opt;
   char plot[N8*32],mode[10],option[10];
   static struct parameter kwds[]={
      {"mode",2,1,1,"plot or area of image","",10,0},
      {"option",2,1,1,"method used for mixture analysis","",10,0},
      {"references",2,1,-8,
         "names of reference spectra (separate with spaces)","",32,0}
   };

   if (img_in_core==0) {
      printf("No image has been selected.\n");
      return;
   }
   kwds[0].value = mode;
   kwds[1].value = option;
   kwds[2].value = plot;
   kwds[2].input_count = &num_minerals;
   if (par(buf,kwds,3)) return;
   lower_case(mode);
   lower_case(option);
   lower_case(plot);
/*
/* implementation dependent checks
*/
   if (num_minerals > 8) {
      printf("Current implementation allows up to eight input minerals.\n");
      return;
   }
   if (img_numchan > 512) {
      printf("Current implementation allows up to 512 bands.\n");
      return;
   }
   for (i=0; i<num_minerals; i++)
      strcpy(lib_name[i],plot+i*32);
/*
/* in plot mode
*/
   if (strcmp(mode,"plot")==0) {
      if (plots_num==16) {
	 printf("There aren't any colors left.  Please erase a plot.\n");
	 return;
      }
      if (plots_num==0) {
	 printf("There aren't any plots up.\n");
	 return;
      }
      if (strcmp(option,"least")!=0) 
	 printf("Only least square fit is used in plot mode.\n");
      err = mixplot();
      if (err!=0) {
         printf("Error occured in mixture-plot routine.\n");
	 printf("Consult a programmer.\n");
	 return;
      }
   }
/*
/* in area mode
*/
   else if (strcmp(mode,"area")==0) {
      display_mode=1;
      if (substring(option,"leastsquares")) opt= 1;
      else if (substring(option,"amplitude")) opt= 2;
      else if (substring(option,"slope")) opt= 3;
      else if (substring(option,"binary")) opt= 4;
      else if (substring(option,"euclidean")) opt= 5;
      else {
         printf("Illegal option : valid options are least, amplitude,\n");
	 printf("slope, binary, or euclidean.\n");
         return;
      }
      err = mixarea(opt);
      if (err!=0) {
	 printf("Error occured in mixture-area routine.\n");
	 printf("Consult a programmer.\n");
	 return;
      }
   }
   else {
      printf("Illegal mode : there are only area mode and plot mode.\n");
      return;
   }
   return;
}


/************************************************************************/
/*									*/
/* mixarea - one of two sub-modes of the mixture program which applies  */
/* the mixture analysis over a certain portion of an image (v.s. plot). */
/* Depending upon user specified method (using amplitude encodeing,	*/
/* slope encoding, binary encoding, euclidean distance, or least square */
/* fit method), this will call an appropriate subroutine.		*/
/* This routine will generate a set of maps, one map per referenced     */
/* library plot, to be drawn on a display monitor later on.		*/
/*									*/
/************************************************************************/
int mixarea(option)
int option;
{
   int i,err;
   short c512=512;
   int st_line=1,num_lines,end_line;
   unsigned char buf[80];

   query("Area (starting line, number of lines) ? ",buf);
   for (i=0;i<strlen(buf);i++) if (ispunct(buf[i])) buf[i]=' ';
   sscanf(buf,"%d%d",&st_line,&num_lines);
   end_line=st_line+num_lines-1;
   while ((st_line < 0) || (st_line > img_nl)) {
      query("Illegal range.  Starting_line ? ",buf);
      sscanf(buf,"%d",&st_line);
   }
   while ((end_line > img_nl) || (end_line < 1) || (end_line<st_line)) {	
       query("Illegal range.  Number of lines ? ",buf);
       sscanf(buf,"%d",&num_lines);
       end_line=st_line+num_lines-1;
   }
   printf("Generating mixture maps . . .\n");
   vdareafi(&unit,&c2,&c0,&c0,&c0,&c1,&c1,&c512,&c512);
   vddispln(&unit,&c2);
   if (option == 1) {
      err = mixleast(st_line,num_lines);
      if (err != 0) return(-1);
      else return(0);
   }
   else if (option < 5 & option >= 2) {
      err = mixbin(st_line, num_lines, option);
      if (err != 0) return(-1);
      else return (0);
   }
   else if (option == 5) {
      err = mixucl(st_line, num_lines);
      if (err != 0) return(-1);
      else return(0);
   }
   return(0);
}


/************************************************************************/
/*									*/
/* mixplot - one of two mixture sub-modes which applies the mixture on	*/
/* a plot.  Given a number of reference library plots, a plot to be     */
/* analyzed, this routine will determine mixture proportions of each    */
/* reference mineral and generate a plot which is the linear combination*/
/* of all the reference minerals.					*/ 
/*									*/
/************************************************************************/
mixplot()
{
   int err,i,index,not_done;
   char	name[32];
   unsigned char *plot_buf;

   err = get_mineral_data(0);
   if (err!=0) return(0);
   positions = (int *)malloc(N512*2*sizeof(int));
   meas_spec=(float *)malloc(img_numchan*sizeof(float));
   plot_buf=(unsigned char *)malloc(img_numchan);
   inverse = (float *)malloc(1024*N8*sizeof(int));
   if(inverse == NULL) {
      printf("Insufficient memory space\n");
      free(meas_spec);
      free(positions);
      return(-1);
   } 
   process1(spec_lib_subset,img_numchan,num_minerals,orthon_basis,
	    trunc_spec_lib_subset);
   process2(trunc_spec_lib_subset,num_minerals,inverse,positions,factors);
   not_done = 1;
   while(not_done) {
      query("Plot name (RETURN to stop) ? ",name);
      if (name[0] == '\0') not_done = 0;
      else if (check_old_name(name)) ; /* bad name if true */
      else {
	 get_plot_num(name,0,&index);
	 if (index<0) printf("Plot name is ambiguous or invalid.  ");
         while (index<0) {
            query("Name (RETURN to quit) ? "
               ,name);
	    if (name[0]=='\0') break;
	    if (!check_old_name(name)) {
	       get_plot_num(name,0,&index);
               if (index<0) printf("Plot name is ambiguous or invalid.  ");
	    }
         }
         if (index<0) break;
         for (i=0; i<img_numchan; i++)
            meas_spec[i]=(float)plots[index].data[i];
	 process3(meas_spec,img_numchan,num_minerals,orthon_basis,
		  trunc_meas_spec,squared_distance);
	 process4(trunc_meas_spec,num_minerals,inverse,positions,
		  cue,mix_coeff,mix_prop,geom_illum);
/*
/* draw the least square fit curve and print out proportion
*/
         getfit(plot_buf);
         sprintf(name,"%s-MIXTURE",name);
         save_plot_data(name,plot_buf,99,0,0,0,0,-1,0);
         draw_plots();
      }
   }
   free(plot_buf);
   free(meas_spec);
   free(positions);
   free(inverse);
   return(0);
}



/************************************************************************/
/*									*/
/* mixbin - This is a routine in case that a method for the mixture     */
/* analysis is chosen to be one of amplitude encoding, slope encoding,  */
/* or binary encoding (slope & amplitude).				*/
/*									*/
/************************************************************************/
mixbin(sl,nl,option)
int sl,nl,option;
{
   int err,i,j,k,k1,joff,ltemp,scale,lmax,lmin;
   int th1,th2,flag=0;
   unsigned char *data,*image_mix_coef,*image_res_val;

   err = get_mineral_data(1);
   if (err!=0) return(0);
   data = (unsigned char *)malloc(img_numchan);
   image_mix_coef = (unsigned char *)malloc(img_bw*nl*num_minerals);
   if (image_mix_coef == NULL) {
      printf("Insufficient memory space\n");
      free(data);
      return(-1);
   }
   joff = 0;
   if (option < 4) lmin = img_numchan;
   else lmin = img_numchan<<1;
   lmax = 0; 
   k1 = (sl-1) * (img_numchan>>3);
   for (i=0; i<nl; i++) 
      for (j=0; j<img_bw; j++) {
	 get_code(k1,data);
	 k1++;
	 for (k=0; k<num_minerals; k++){
	    plot_dist(data,samples[k].data,&th1,&th2,0);
	    if (option == 2) ltemp = img_numchan-th1;
	    if (option == 3) ltemp = img_numchan-th2;
	    if (option == 4) ltemp = (img_numchan<<1) - (th1+th2); 
	    if (ltemp < lmin) lmin = ltemp;
	    if (ltemp > lmax) lmax = ltemp;
	    image_mix_coef[joff++] = ltemp;
         }
      }
   scale = 255/(lmax-lmin);
   joff = 0;
   for (i=0; i<nl; i++)
      for (j=0; j<img_bw; j++) 
 	for (k=0; k<num_minerals; k++) {
	   ltemp = (image_mix_coef[joff]-lmin) * scale;
	   image_mix_coef[joff++] = ltemp+17;
	}
   err = draw_mixture_maps(image_mix_coef,image_res_val,sl,nl,flag);
   free(image_mix_coef);
   free(data);
   if (err!=0) return(-1);
   return(0);
}



/************************************************************************/
/*									*/
/* mixleast - This is a routine in case that the method used for the    */
/* mixture analysis is chosen as the least square fit method.		*/
/*									*/
/************************************************************************/
mixleast(st_line,num_lines)
int st_line, num_lines;
{
   int err,i,j,k,k1,joff,ioff,flag=1,cnt=0;
   float *rtemp,norm_factor;
   float sum_prod_mix_prop[N8][N8];
   float max_geom_illum[1],sum_geom_illum[1],sum_mix_prop[N8];
   float sum_squared_distance[1];
   float residual[1],max_residual[1],sum_residual[1],sum_squared_residual[1];	
   unsigned char *image_mix_coef,*image_res_val,temp;
 
   err = get_mineral_data(0);
   if (err!=0) return(0);
   sum_squared_distance[0]=0.0;
   max_residual[0]=sum_residual[0]=sum_squared_residual[0]=0.0;
   max_geom_illum[0]=sum_geom_illum[0]=0.0;
   for (i=0; i<N8; i++)
     for (j=0; j<N8; j++)
	sum_prod_mix_prop[i][j]=0.0;
   for (i=0; i<N8; i++)
     sum_mix_prop[i]=0.0;
   positions = (int *)malloc(N512*2*sizeof(int));
   meas_spec=(float *)malloc(img_numchan*sizeof(float));
   inverse = (float *)malloc(1024*N8*4);
   if(inverse == NULL) {
      printf("Insufficient memory space\n");
      free(meas_spec);
      free(positions);
      return(-1);
   }
   image_mix_coef = (unsigned char *)malloc(img_bw*num_lines*num_minerals);
   if(image_mix_coef == NULL) {
      printf("Insufficient memory space\n");
      free(meas_spec);
      free(positions);
      free(inverse);
      return(-1);
   }
   rtemp = (float *)malloc(img_bw*num_lines*sizeof(float));
   if(rtemp == NULL) {
      printf("Insufficient memory space\n");
      free(image_mix_coef);
      free(meas_spec);
      free(positions);
      free(inverse);
      return(-1);
   }
   image_res_val = (unsigned char *)malloc(img_bw*num_lines);
   if(image_res_val == NULL) {
      printf("Insufficient memory space\n");
      free(rtemp);
      free(image_mix_coef);
      free(meas_spec);
      free(positions);
      free(inverse);
      return(-1);
   }
   process1(spec_lib_subset,img_numchan,num_minerals,orthon_basis,
	    trunc_spec_lib_subset);
   process2(trunc_spec_lib_subset, num_minerals, inverse, positions,factors);
   ioff = (st_line-1)*img_bw*img_numchan;
   joff = 0;
   for (i=0; i<num_lines; i++) {
      for (j=0; j<img_bw; j++) {
	 k1 = j;
	 for(k=0; k<img_numchan; k++) {
	    temp = *(img_data + ioff +k1);
	    meas_spec[k]=(float)temp;
	    k1+=img_bw;
         }
         process3(meas_spec,img_numchan,num_minerals,orthon_basis,
		  trunc_meas_spec,squared_distance);
         process4(trunc_meas_spec,num_minerals,inverse,positions,
		  cue,mix_coeff,mix_prop,geom_illum);
	 process5(num_minerals,img_numchan,mix_coeff,mix_prop,
		  sum_mix_prop,sum_prod_mix_prop,
	          trunc_spec_lib_subset,trunc_meas_spec,trunc_mix_spec,
		  squared_distance,sum_squared_distance,
		  geom_illum,max_geom_illum,sum_geom_illum,
		  residual,max_residual,sum_residual,sum_squared_residual);
	 rtemp[cnt++]=residual[0];
         for (k=0; k<num_minerals; k++)
            image_mix_coef[joff++] = (unsigned char)(mix_prop[k]*238.0+17.0);
      }
      ioff+=img_bw*img_numchan;
   }
   norm_factor=sqrt(sum_squared_residual[0]/(float)(num_lines*img_bw));
   for (i=0; i<num_lines*img_bw; i++)
      image_res_val[i] = (unsigned char)(rtemp[i]/norm_factor*47.0+17.0);
   err = draw_mixture_maps(image_mix_coef,image_res_val,st_line,num_lines,flag);
   free(meas_spec);
   free(positions);
   free(inverse);
   free(image_mix_coef);
   free(image_res_val);
   free(rtemp);
   if(err!=0) return(-1);
   return(0);
}


/************************************************************************/
/*									*/
/* mixucl - This is a routine in case that the method used for the      */
/* mixture analysis is chosen to be the euclidean distance method.      */
/*									*/
/************************************************************************/
mixucl(st_line,num_lines)
int st_line,num_lines;
{
   int err,i,j,k,k1,joff,ioff,ltemp,flag=0;
   unsigned char *image_mix_coef,*image_res_val,temp;
   float avg,fval,ftemp,ftemp1,*square_lib;

   err = get_mineral_data(0);
   if (err!=0) return(0);
   meas_spec=(float *)malloc(img_numchan*sizeof(float));
   square_lib=(float *)malloc(num_minerals*sizeof(float));
/*
/* prepare the sum of square and zero mean spec_lib_subset
*/
   for (i=0; i<num_minerals; i++) {
      avg = 0.0;
      for (j=0; j<img_numchan; j++) 
	 avg += spec_lib_subset[i][j];
      avg /= (float)img_numchan;
      ftemp = 0.0;
      for (j=0; j<img_numchan; j++) {
	 fval = spec_lib_subset[i][j] - avg;
	 ftemp += fval * fval;
	 spec_lib_subset[i][j] = fval;
      }
      square_lib[i] = sqrt(ftemp);
   }
   image_mix_coef = (unsigned char *)malloc(img_bw*num_lines*num_minerals);
   if (image_mix_coef == NULL) {
      printf("Insufficient memory\n");
      return(-1);
   }
   ioff = (st_line-1)*img_bw*img_numchan;
   joff = 0;
   for (i=0; i<num_lines; i++) {
      for (j=0; j<img_bw; j++) {
/*
/* prepare sum of squares and zero mean meas_spec
*/
	 k1 = j;
	 avg = 0.0;
	 for (k=0; k<img_numchan; k++) {
	    temp = *(img_data + ioff +k1);
	    meas_spec[k]=(float)temp;
	    avg += (float)temp;
	    k1+=img_bw;
	 }
	 avg /= (float)img_numchan;
	 ftemp1 = 0.0;
	 for (k=0; k<img_numchan; k++) {
	    fval = meas_spec[k] - avg;
	    ftemp1 += fval * fval;
	    meas_spec[k] = fval;
	 }
	 ftemp1 = sqrt(ftemp1);
	 for (k=0; k<num_minerals; k++){
	    ftemp = 0.0;
	    for (k1=0; k1<img_numchan; k1++) 
	       ftemp  += meas_spec[k1] * spec_lib_subset[k][k1];
	    if (ftemp1==0.0 || square_lib[k]==0.0) ltemp=0;
	    else {
	        ftemp = ftemp/(ftemp1*square_lib[k]);
	        ltemp = (ftemp+1.0) * 119 + 17;
	    }
	    image_mix_coef[joff++] = ltemp;
	 } 
      }
      ioff += img_bw*img_numchan;
   }
   err = draw_mixture_maps(image_mix_coef,image_res_val,st_line,num_lines,flag);
   free(image_mix_coef);
   free(meas_spec);
   free(square_lib);
   if (err!= 0) return(-1);
   return(0);
}


/***************************************************************************
	 process1.c  Procedure to Calculate an Orthonormal
		   Basis for the Subspace Generated by a Specified 
		   Subset of Library Spectral Vectors and to
		   calculate a Truncated Subset of Library
		   Spectral Vectors. All vectors are stored in matrices
		   row-wise to achieve maximal computational speed
		   and to impose a uniform convention.       
	Inputs: spec_lib_subset = up to 8 library spectral vectors
				  having up to 512 channels.
		num_channels    = number of spectral channels <= 512
		num_minerals    = number of minerals <= 8
	Outputs: orthon_basis   = orthonormal basis for the subspace 
				  spanned by the subset of library         
				  vectors.
		 trunc_spec_lib_subset = truncated subset of library spectral
				    vectors formed by calculating their
				    components with respect to the
			            orthonormal basis.
****************************************************************************/
process1(spec_lib_subset,num_channels,num_minerals,orthon_basis,
		trunc_spec_lib_subset)
int num_channels,num_minerals;
float spec_lib_subset[N8][N512],orthon_basis[N8][N512];
float trunc_spec_lib_subset[N8][N8];
{
	register int i,j,k;
	float temp;
	
	temp = 0.;
	for (i=0;i<num_channels;i++)
		temp += spec_lib_subset[0][i]*spec_lib_subset[0][i];
	temp = sqrt(temp);
	for (i=0;i<num_channels;i++)
		orthon_basis[0][i] = spec_lib_subset[0][i]/temp;
	for (k=1;k<num_minerals;k++) {
		for (i=0;i<num_channels;i++)
			orthon_basis[k][i] = spec_lib_subset[k][i];
		for (j=0;j<k;j++) {
			temp = 0.;
			for (i=0;i<num_channels;i++)
			   temp += orthon_basis[j][i]*spec_lib_subset[k][i];
			for (i=0;i<num_channels;i++)
				orthon_basis[k][i] -= temp*orthon_basis[j][i];
		}
		temp = 0.;
		for (i=0;i<num_channels;i++)
			temp += orthon_basis[k][i]*orthon_basis[k][i];
		temp = sqrt(temp);
		for (i=0;i<num_channels;i++)
			orthon_basis[k][i] /= temp;
	}
	for (k=0;k<num_minerals;k++)
	for (j=0;j<num_minerals;j++) {
		temp = 0.;
		for (i=0;i<num_channels;i++)
			temp += orthon_basis[j][i]*spec_lib_subset[k][i];
		trunc_spec_lib_subset[k][j] = temp;
	}
}

/*****************************************************************************
		 process2.c Procedure to calculate and store inverses
			for all the subsets of the specified subset of
			library spectral vectors and to calculate a
			lookup-array yielding the initial position for the
			row of the inverses-matrix containing the rows
			of the inverse matrix corresponding to a 
			non-empty subset of the specified subset of
			library spectral vectors. All vectors are stored in
			matrices row-wise to achieve maximal computational
			speed and to impose a uniform convention.
		Inputs: trunc_spec_lib_subset = truncated subset of library
						spectral vectors.
			num_minerals = number of minerals 
				     = number of library spectral vectors.
		Output: inverses = contains (num_minerals)*2**(num_minerals-1)
				   vectors, each containing num_minerals
				   components.
			positions = contains 2**(num_minerals) entries, each
				    giving the position of a row in inverses.   
 			factors = contains num_minerals entries, each giving a
				  'noise' multiplier to assess the variance
				  for the estimated proportion for the 
				  corresponding mineral. 
*********************************************************************"*******/
process2(trunc_spec_lib_subset,num_minerals,inverses,positions,factors)

int num_minerals;
int positions[N256];
float trunc_spec_lib_subset[N8][N8],inverses[N1024*N8],factors[N8];
{
	register int i,j,k,i1,i2,i11;
	int i_max,index,bitpos,size;
	int num_bits[N256]; 
	float M[N8][N8],G[N8][N8];

	i_max=1;			 	/* i_max = 2**num_minerals -1 */
	for ( i=0 ; i<num_minerals ; i++ )
		i_max *= 2;
	i_max--;   
	weightgen(num_bits,i_max);		
	positiongen(positions,num_bits,i_max);
	for ( i = 1 ; i <= i_max ; i++ ) {    	 /* gen. inv. for i-th subset */
		bitpos=1;
		index=0;
		for ( j = 0 ; j < num_minerals ; j++ ) {
			if (bitpos & i) {
				for ( k = 0 ; k < num_minerals ; k++ ) 
			     	   M[k][index] = trunc_spec_lib_subset[j][k]; 
				index++;
			}
			bitpos <<= 1;	/* shifts bit left */
		}
		size = num_bits[i];
						/* calculate inverses */
		generalized_inverse(M,G,size,factors,num_minerals); 
						/* store inverses */
		for ( i1 = 0 ; i1 < size ; i1++ )
		for ( i2 = 0 ; i2 < num_minerals ; i2++ ) {
			i11 = i1 + positions[i] - 1 ;
			inverses[i11*N8+i2] = G[i1][i2];
		}
	}
}
				
bitcount(n,bc) 	/* counts number of bits = bc in n  */
unsigned char n;
int *bc;
{
	for (*bc = 0 ; n != 0 ; n >>= 1)
		if (n & 01)
			*bc += 1;
}

weightgen(num_bits,i_max)  /* generates array of bit counts */
int num_bits[N256],i_max;   /* num_bits[i] = # bits in i-base 2 */
{
	unsigned char n;
	int i,bc;
	
	for ( i = 1 ; i <= i_max ; i++) {
		n=i;
		bitcount(n,&bc);
		num_bits[i]=bc;
	}
}

positiongen(positions,num_bits,i_max) /* generates array of initial positions */
int positions[N256],num_bits[N256],i_max;
{
	int i;
	
	positions[1]=1;
	for( i = 2 ; i <= i_max ; i++)
		positions[i] = positions[i-1] + num_bits[i-1];
}

/************************************************************************/
/* generalized_inverse - calculates the generalized inverse for a matrix*/
/* .  M[] is the original matrix and G[] is the resultant matrix.  size */
/* is the number of non-zero vectors in matrices.			*/
/************************************************************************/
generalized_inverse(M,G,size,factors,num_minerals)
float	M[N8][N8],G[N8][N8],factors[N8];
int	size,num_minerals;
{
	int i,j,k;
	float	A[N8][N8],sum;

	/* calculate M(transpose) */
	for ( i = 0; i < size; i++)
		for ( j = 0; j < num_minerals; j++)
			G[i][j] = M[j][i];

	/* calculate M(transpose)*M */
	for ( i = 0; i < size; i++) {
		for ( j = 0; j < size; j++) {
			A[i][j] = 0.0;
			for ( k = 0; k < num_minerals; k++)
				A[i][j] = A[i][j] + G[i][k] * M[k][j];
		}
	}

	/* LU Decomposition */
	for ( i = 0; i < size-1; i++)
		for ( j = i + 1; j < size; j++) {
			A[j][i] = A[j][i] / A[i][i];
			for ( k = i + 1; k < size; k++)
				A[j][k] = A[j][k] - A[i][k] * A[j][i];
		}

	/* solve inverse(LU)*M(transpose) */
	for ( i = 0; i < num_minerals; i++)
		for ( j = 0; j < size; j++) {
			sum = 0.0;
			for ( k = 0; k < j; k++)
				sum = sum + A[j][k] * M[k][i];
			M[j][i] = G[j][i] - sum;
		}
	for ( i = (num_minerals - 1); i >= 0; i--)
		for ( j = (size-1); j >= 0; j--) {
			sum = 0.0;
			for ( k = (size-1); k > j; k--)
				sum = sum + A[j][k] * M[k][i];
			if (A[j][j] == 0.0) M[j][i] = 0.0;
			else
				M[j][i] = (M[j][i] - sum) / A[j][j];
		}

	for (i = 0; i < num_minerals; i++)
		for (j = 0; j < num_minerals; j++)
			G[i][j] = 0.0;
	for (i = 0; i < size; i++)
		for (j = 0; j < num_minerals; j++) {
			G[i][j] = M[i][j];
		}
 	if (size==num_minerals) {
		for (i=0;i<num_minerals;i++) {
			factors[i]=0.;
		 	for (j=0;j<num_minerals;j++) {
				factors[i] += (G[i][j]*G[i][j]);
			}
			factors[i]=sqrt(factors[i]);
		}
	}
}

/***************************************************************************
	 process3.c Procedure to calculate a truncated measured spectrum
		from a measured spectrum by projecting it onto a subspace
		spanned by a specified subset of library spectral vectors 
		and then calculating the components of the projected vector
		with respect to an orthogonal basis for the subspace. Also,
		the squared distance from the subspace is calculated.
	Inputs: 	meas_spec 	= measured spectral vector 
			num_channels 	= number of spectral channels  	
					= number of components of meas_spec
			num_minerals	= number of minerals
					= number of comp. of trunc_meas_spec
	Outputs:	trunc_meas_spec	= truncated measured spectral vector
			squared_distance = squared distance from meas_spec
					   tg subspace
					 = squared distance from meas_spec
					   to trunc_meas_spec
					(this must be passed as an address)
****************************************************************************/
process3(meas_spec,num_channels,num_minerals,orthon_basis,
	 trunc_meas_spec,squared_distance)

int num_channels,num_minerals;
float meas_spec[N512],orthon_basis[N8][N512],trunc_meas_spec[N8];
float squared_distance[1];
{
	register int i,j;

	squared_distance[0] = 0. ;
	for ( i = 0 ; i < num_channels ; i++ )
	   squared_distance[0] += meas_spec[i]*meas_spec[i];

	for ( i = 0 ; i < num_minerals ; i++ ) {
	   trunc_meas_spec[i] = 0. ;
	   for ( j = 0 ; j < num_channels ; j++ )
	      trunc_meas_spec[i] += orthon_basis[i][j]*meas_spec[j];
	      squared_distance[0] -= trunc_meas_spec[i]*trunc_meas_spec[i];
	}
}



/***************************************************************************
	 process4.c Procedure to calc. mixture coefficents, proportions and
		geometric illumination factor from a truncated measured 
 		spectrum, matrix of inverses, and other input data.
		All vectors are stored in matrices row-wise to increase
		speed and to impose a uniform convention.

	Inputs  : trunc_meas_spec = truncated measured spectrum
		  num_minerals    = number of minerals
		  inverses        = matrix of vectors, dot producting with
				    which yields mixture coefficients
		  positions	  = array of row indices of inverses
				    allowing the first row corresponding
				    to each subset of minerals to be found
		  cue             = ordered list of minerals to speed up
				    algorithm (not implemented in this version)
	Outputs : mix_coeff       = mixture coefficients
		  mix_prop        = mixture proportions
		  geom_illum        = geometric illumination factor
		  cue             = same as input cue but updated 
****************************************************************************/
process4(trunc_meas_spec,num_minerals,inverses,positions,cue,mix_coeff,
	 mix_prop,geom_illum)

int num_minerals;
int positions[N256],cue[N8];
float trunc_meas_spec[N8],inverses[N1024*N8];
float mix_coeff[N8],mix_prop[N8],geom_illum[1];
{
   register int i,j,k;
   int rindex,bitpos,incr;
   float temp;

      i = 1 ;					/* i = 2**num_minerals -1 */
      for ( j = 0 ; j < num_minerals ; j++ )
	 i *= 2 ;
      i-- ;
			/* i parameterizes candidate subsets of minerals */
      j = 0 ;		/* calculate mix_coeff[j] */
      bitpos = 1 ;	/* bit index for current mineral */
      incr = 0 ;	/* index of mineral in particular subset */

      while ( j < num_minerals ) {	/* calculate mix_coeff[j] */
	if ( bitpos & i ) {
	   rindex = positions[i] - 1 + incr ;
	   temp = 0. ;		/* calculate dot product */
	   for ( k = 0 ; k < num_minerals ; k++ )
	      temp+=inverses[rindex*N8+k]*trunc_meas_spec[k];
	      if ( temp >= 0. ) {
	         mix_coeff[j] = temp ;
		 j++ ;
		 bitpos <<= 1 ;
		 incr++ ;
	      }
	      else {
	         mix_coeff[j] = 0. ;
		 j = 0 ;
		 i ^= bitpos ;
		 bitpos = 1 ;
		 incr = 0 ;
	      }
	}
	else {
	   bitpos <<= 1 ;
	   j++ ;
	}
      }			/* end while loop */
      temp = 0. ;       
      for ( i = 0 ; i < num_minerals ; i++ )
 	 temp += mix_coeff[i] ;
      geom_illum[1] = temp ;
      if ( temp != 0. ) {
	 for ( i = 0 ; i < num_minerals ; i++ )
	    mix_prop[i] = mix_coeff[i]/temp ;
      }
}




/****************************************************************************
	 process5.c Procedure to calculate a powerful assortment of 
		statistics

	Inputs: num_minerals 	=  number of minerals (<=8)
		num_channels	=  number of channels (<=8)
		trunc_meas_spec	=  truncated measured spectrum
		trunc_spec_lib_subset = truncated spectral library subset
		mix_coeff	=  mixture coefficients
		squared_distance = squared distance from truncated measured
				   spectrum to measured spectrum
		max_residual	=  maximum residual value
		geom_illum	=  geometric illumination
		max_geom_illum	=  maximum geometric illumination
		mix_prop	=  mixture proportions
		sum_mix_prop	=  sum of mixture proportions vector
		sum_prod_mix_prop = sum of product of mixture proportions
		sum_squared_residual = sum of squared residual
		sum_residual	=  sum of residual values
		sum_geom_illum	=  sum of geometric illumination values				sum_squared_distance = sum of squared distances

	Outputs:trunc_mix_spec	=  truncated mixture spectrum = product of
				   row matrix mix_coeff and the matrix
				   trunc_spec_lib_subset, the entries are
				   the components of the least squared estimate
				   for trunc_meas_spec as a non negative linear
				   combination of the rows of 
				   trunc_spec_lib_subset,
				   expressed in terms of the rows of 
				   orthon_basis
		residual	=  residual = sqrt( ( squared_distance +
				   square of distance from trunc_meas_spec
				   to trunc_mix_spec ) / ( num_channels ) )
		max_residual	=  maximum residual value
		max_geom_illum	=  maximum value of geometric illumination
		sum_mix_prop	=  sum of mixture proportions vector
		sum_prod_mix_prop = sum of outer product of mixture
				    proportions vector (stored as a lower
				    triangular matrix)
		sum_residual 	=  sum of residual values
		sum_squared_residual = sum of squared residual
		sum_geom_illlum	=  sum of geometric illumination values
		sum_geom_illum	=  sum of geometric illumination values				sum_squared_distance = sum of squared distances
****************************************************************************/
process5(num_minerals,num_channels,mix_coeff,mix_prop,
	 sum_mix_prop,sum_prod_mix_prop,
	 trunc_spec_lib_subset,trunc_meas_spec,trunc_mix_spec,
	 squared_distance,sum_squared_distance,
	 geom_illum,max_geom_illum,sum_geom_illum,
	 residual,max_residual,sum_residual,sum_squared_residual)

int num_minerals,num_channels;
float mix_coeff[N8],mix_prop[N8];
float sum_mix_prop[N8],sum_prod_mix_prop[N8][N8];
float trunc_spec_lib_subset[N8][N8],trunc_meas_spec[N8],trunc_mix_spec[N8];
float squared_distance[1],sum_squared_distance[1];
float geom_illum[1],max_geom_illum[1],sum_geom_illum[1];
float residual[1],max_residual[1],sum_residual[1],sum_squared_residual[1];
{
register int i,j;
float temp,temp1;

   for (i=0; i<num_minerals; i++) {
      trunc_mix_spec[i]=0.;
      if (mix_coeff[i]!=0.)
         for (j=0; j<=i; j++)
            trunc_mix_spec[j]+=mix_coeff[i]*trunc_spec_lib_subset[i][j];
   }
   temp1=squared_distance[0];
   for (i=0; i<num_minerals; i++) {
      temp=trunc_mix_spec[i]-trunc_meas_spec[i];
      temp1+=temp*temp;
   }
   residual[0]=sqrt(temp1/num_channels);
   sum_squared_residual[0]+=residual[0]*residual[0];

/*
   more statistics, if necessary

   if (residual[0]>max_residual[0])
      max_residual[0]=residual[0];
   sum_residual[0]+=residual[0];
   sum_squared_distance[0]+=squared_distance[0];
   if (geom_illum[0]>max_geom_illum[0])
      max_geom_illum[0]=geom_illum[0];
   sum_geom_illum[0]+=geom_illum[0];
   for (i=0; i<num_minerals; i++) {
      sum_mix_prop[i]+=mix_prop[i];
      for (j=0; j<=i; j++)
         sum_prod_mix_prop[i][j]+=mix_prop[i]*mix_prop[j];
   }
*/
}



/************************************************************************/
/*									*/
/* get_mineral_data - Checks if user specified plots are in the plot    */
/* structure.  If it is, data gets retieved into a static array called  */
/* spec_lib_subset[][].  If an encoding scheme is used for the analysis,*/
/* then the encoded data for the plot will be retrieved rather than the */
/* real data.								*/
/*									*/
/************************************************************************/
get_mineral_data(option)
int option;
{
   int i,index[N8],j;
   unsigned char avg;

   for (i = 0; i < num_minerals; i++) {
      get_plot_num(lib_name[i],0,&index[i]);
      if (index<0) printf("\"%s\" is ambiguous or not one of the plots.\n",
	 lib_name[i]);
      while (index[i] < 0) {
	 query("Name (RETURN for READY) ? ",lib_name[i]);
 	 if (lib_name[i][0]=='\0') return(-1);
   	 if (!check_old_name(lib_name[i])) {
            get_plot_num(lib_name[i],0,&index[i]);
            if (index<0)
	       printf("\"%s\" is ambiguous or not one of the plots.\n",
	          lib_name[i]);
         }
      }
      if (option == 0) {
 	 for (j=0; j<img_numchan; j++)
	    spec_lib_subset[i][j] = (float)plots[index[i]].data[j];
      }
      else {
         avg=(plots[index[i]].sum+(img_numchan>>1))/img_numchan;	
	 compact_code(plots[index[i]].data,samples[i].data,avg);
      }
   }
   return(0);
}



/************************************************************************/
/*									*/
/* getfit - Given already computed mixture proportions for each ref-    */
/* erenced mineral, this will generate a mixture plot which is simply   */
/* the linear combinations of reference plots.				*/
/*									*/
/************************************************************************/
getfit(plot_buf)
unsigned char *plot_buf;
{
   int i,k,itemp;
   float temp;

   for (i=0; i<num_minerals; i++)
      printf("%20s : %f\n",lib_name[i],mix_coeff[i]);
   for (k=0; k<img_numchan; k++) {
      temp = 0.0;
      for (i=0; i<num_minerals; i++) 
	 temp += mix_coeff[i]*spec_lib_subset[i][k];
	 itemp = temp;
	 if (itemp > 255) itemp = 255;
	 if (itemp < 0 ) itemp = 0;
	 plot_buf[k] = itemp;
   }
}



/************************************************************************/
/*									*/
/* draw_mixture_maps							*/
/*    This draws mixture decomposition maps for each specified mineral  */ 
/* and a map for the residual values.  Residual map is computed only    */
/* for the "area-least" mode and the parameter flag should be setted    */
/* to value one in order for the residual map to be drawn.		*/
/*                  							*/
/************************************************************************/
draw_mixture_maps(image_mix_coef,image_res_val,st_line,nl,flag)
unsigned char *image_mix_coef,*image_res_val;
int nl,st_line,flag;
{
   short int c,c255=255,c512=512,x_cord[2],y_cord[2];
   short int IMG_XOFFSET,IMG_YOFFSET,MAP_XOFFSET,interval,leftover;
   short int r=3,rr=15,left,top,right,bottom;
   float f1=1.0;
   char	buf[80];
   int i,h,lastmap,ns,r_slash;
   unsigned char *band,*nc_ptr,*buffer;

/*
/* initilization
*/
   vdareafi(&unit,&c2,&c0,&rr,&c0,&c1,&c1,&c512,&c512);
   ns=nl*img_bw;
   IMG_YOFFSET=256-(img_nl/2);
   IMG_XOFFSET=30;
   MAP_XOFFSET=IMG_XOFFSET+img_bw;
   buffer=(unsigned char *)malloc(ns);
   if (buffer==NULL) {
      printf("Insufficient memory.\n");
      return(-1);
   }
/*
/* draw an image strip with line indicators every 50th lines
*/
   band=(unsigned char *)calloc(img_nl*img_bw/4,4);
   if (band==NULL) {
      printf("Insufficient memory.\n");
      return(-1);
   }
   extract(1,1,band);
   left=1;
   vdtxtsiz(&c7,&f1);
   for (i=1; i<=img_nl;i++) {
      if ((i%50)==0) {
	 top=i+IMG_YOFFSET;
	 sprintf(buf,"%3d",i+1);
	 if (top>=25) vdtext(&unit,&c2,&c0,&c255,&c0,&left,&top,&c1,&c3,buf);
	 x_cord[0]=15;
	 x_cord[1]=IMG_XOFFSET;
	 y_cord[0]=top;
	 y_cord[1]=top;
	 vdvector(&unit,&c2,&c0,&c255,&c0,&c2,x_cord,y_cord);
      }
   }
   left=IMG_XOFFSET;
   top=IMG_YOFFSET+1;
   right=left+img_bw-1;
   bottom=top+img_nl-1;
   vdareawr(&unit,&c2,&ns,band,&left,&top,&right,&bottom);
   for (r_slash=strlen(img_name)-1;r_slash>=0 && img_name[r_slash]!='/' &&
      img_name[r_slash]!=']';r_slash--);
   r_slash++;
   strcpy(buf,img_name+r_slash);
   upper_case(buf);	
   top=IMG_YOFFSET-5;
   if (top<25) top+=25;
   c=strlen(buf);
   left=IMG_XOFFSET+(img_bw/2)-(c7*c/2);
   if (left<1) left=1;
   vdtext(&unit,&c2,&c0,&r,&c0,&left,&top,&c1,&c,buf);
   vdflush(&unit);
/*
/* draw mixture maps
*/
   if (flag==0) leftover=(c512-MAP_XOFFSET)-(num_minerals*img_bw);
   else leftover=(c512-MAP_XOFFSET)-((num_minerals+1)*img_bw);
   if (leftover>0) {
      if (flag==0) interval=leftover/(num_minerals+1);
      else interval=leftover/(num_minerals+2);
      lastmap=num_minerals;
   }
   else {
      leftover=c512;
      for (i=1; i<=num_minerals && leftover>num_minerals; i++)
         leftover=(c512-MAP_XOFFSET)-i*img_bw;
      lastmap=i-1;
      leftover=(c512-MAP_XOFFSET)-(lastmap*img_bw);
      interval=leftover/(lastmap+1);
      printf("There is not enough space on monitor to display all maps.\n");
      printf("Only first %d map(s) will be drawn.\n",lastmap);
   }
   for (h = 0; h < lastmap; h++) {
      top=IMG_YOFFSET+st_line;
      bottom=top+nl-1;
      left=(h+1)*interval+h*img_bw+MAP_XOFFSET;
      right=left+img_bw-1;
      nc_ptr=image_mix_coef+h;
      for (i=0; i<ns; i++) {
	 if (*nc_ptr>16) buffer[i] = *nc_ptr; /* avoid LUT graphics entries */
	 else buffer[i]=17;
	 nc_ptr+=num_minerals;
      }
      vdareawr(&unit,&c2,&ns,buffer,&left,&top,&right,&bottom);
      c=strlen(lib_name[h]);
      left=(h+1)*interval+h*img_bw+MAP_XOFFSET+(img_bw/2)-(c*c7/2);
      if (left<1) left=1;
      if ((h%2) == 0) top=IMG_YOFFSET+st_line-15;
      else  top=IMG_YOFFSET+st_line-5;
      if (top<25) top+=25;
      vdtext(&unit,&c2,&c0,&r,&c0,&left,&top,&c1,&c,lib_name[h]);
   }
   if (flag==1) {
      nc_ptr=image_res_val;
      for (i=0; i<ns; i++) {
	 if (*nc_ptr>16) {
	    if (*nc_ptr>255) buffer[i]=255;
	    else buffer[i] = *nc_ptr;
	 }
	 else buffer[i]=17;
	 nc_ptr++;
      }
      top=IMG_YOFFSET+st_line;
      bottom=top+nl-1;
      left=(h+1)*interval+h*img_bw+MAP_XOFFSET;
      right=left+img_bw-1;
      vdareawr(&unit,&c2,&ns,buffer,&left,&top,&right,&bottom);
      strcpy(lib_name[0],"RESIDUAL");
      c=8;
      left=(h+1)*interval+h*img_bw+MAP_XOFFSET+(img_bw/2)-(c*c7/2);
      if (left<1) left=1;
      if ((h%2) == 0) top=IMG_YOFFSET+st_line-15;
      else  top=IMG_YOFFSET+st_line-5;
      if (top<25) top+=25;
      vdtext(&unit,&c2,&c0,&r,&c0,&left,&top,&c1,&c,lib_name[0]);
   }
   vdflush(&unit);
   cfree(band);
   free(buffer);
   return(0);
}
