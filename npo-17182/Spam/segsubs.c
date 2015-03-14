/**********************************************************************
    segsubs.c 2.15 - Segmentation subroutines
**********************************************************************/
#include <stdio.h>
#include <math.h>
#include "spam.h"

get_plot(fname,code_opt)
unsigned char *fname, code_opt;
{
	int f1,l1,nread,i,j;
	unsigned char buff[128],avg;
	char  numbyte, numbyte2;

	numbyte = (img_numchan+7)>>3;
	numbyte2 = numbyte<<1;
	
	f1 = vosopen(fname,0,0,img_numchan);
	if (f1 == -1) {
		printf("Error finding/reading the file cluster.spc.\n");
		return(f1);  
	}

	l1 = 0;
	while((nread=vosread(f1,spectra[l1].data,l1+1,1,1,img_numchan)) != -1)
		l1 += 1;

	vosclose(f1);

	if (code_opt == 1) for(i=0; i<l1; i++) {
		get_avg(spectra[i].data,&avg);
		compact_code(spectra[i].data,spectra[i].encoding,avg);
	}	
	return(l1);
}

put_plot(class,fname)
int class;
unsigned char *fname;
{
	int f1,l1;

	f1=vosopen(fname,1,class,img_numchan);
	if (f1 == -1) {
		printf("Can't create file %s.\n",fname);
		return(f1);
		}

	for(l1=0; l1<class; l1++)
		voswrite(f1,spectra[l1].data,l1+1,1,1,img_numchan);

	vosclose(f1);
	return(0);
}
/******************************************************************************/
/* get first-pass clustering result file (cluster.map)                        */
/******************************************************************************/
get_list(listi,draw,fname)
unsigned char *listi,draw,*fname;
{
	int f1;
	int l;

	f1 = vosopen(fname,0,0,img_bw);
	if (f1 == -1) {
		printf("Can't read the cluster map.  Make sure that you");
                printf(" have saved a map using the\n\"cluster\" command.\n");
		return(f1);
	}

	if (vosread(f1,listi,1,1,img_nl,img_bw)==-1) {
        	printf("Error reading cluster map.  Map may be too small.\n");
                vosclose(f1);
                return(-1);
        }

	if (vosread(f1,&l,img_nl+1,1,1,1)!=-1) {
        	printf("Cluster map is too large for this image.\n");
                vosclose(f1);
                return(-1);
        }
	vosclose(f1);

	if (draw == 1) image_draw(listi,-1);
	return(0);
}
/******************************************************************************/
/* put first-pass clustering result file                                      */
/******************************************************************************/
put_list(listi,fname)
unsigned char *listi,*fname;

{
	int f1,l;
	unsigned char com[80];

	query("Do you wish to save the clustering result?(y/n) : ",com);

	if (com[0] == 'n' || com[0]=='N') return(-1);

	f1 = vosopen(fname,1,img_nl,img_bw); 
	if (f1 == -1) {
		printf("Can't create the file.\n");
		return(f1);
		}

	for (l=0; l<img_nl; l++)
		voswrite(f1,(listi+(img_bw*l)),l+1,1,1,img_bw);
	vosclose(f1);

	return(0);
}

/*******************************************************************/
/*       find the distance between the coded spectrums             */
/*******************************************************************/
plot_dist(p1,p2,th1,th2,bitmasks) 
unsigned char *p1,*p2,*bitmasks;
int *th1, *th2;
{
	int i;
	char  numbyte, numbyte2;

	numbyte = (img_numchan+7)>>3;
	*th1 = 0;
	for (i=0; i<numbyte; i++)
            if (bitmasks==0 || bitmasks[i]==255) *th1 += hamm_lut[*p1++][*p2++];
            else *th1 += hamm_lut[(*p1++)&bitmasks[i]][(*p2++)&bitmasks[i]];

	*th2 = 0;
	for (i=0; i<numbyte; i++)
	    if (bitmasks==0 || bitmasks[i]==255) *th2 += hamm_lut[*p1++][*p2++];
	    else *th2 += hamm_lut[(*p1++)&bitmasks[i]][(*p2++)&bitmasks[i]];

	return;
}

/******************************************************************/
/* encode spectrum using trinary coding method                    */
/******************************************************************/
flat_spec(p1,avg,thresh)
unsigned char *p1,avg;
int thresh;
{
	int i,hd,gap;

	hd = 0;
	for (i=0; i<img_numchan; i++) {
	if (*p1 > avg) gap = *p1 - avg;
		else   gap = avg - *p1;
	if (gap > thresh) hd += 1;
	p1 += 1;
	}
	return(hd);
}
/*******************************************************************/
/* get image data from specified x and y co-ordinates              */
/*******************************************************************/
get_data(x,y,p1)
int x,y;
unsigned char *p1;
{
	int i;
	register unsigned char *p2;

	p2 = (unsigned char *)img_data + (img_ns*y) + x;
	for (i=0; i<img_numchan; i++){
		*p1 = *p2;
		p2 += img_bw;
		p1 += 1;
		}
	return;
}
get_code(j,p1)
int j;
unsigned char *p1;
{
	int i;
	register unsigned char *p2;
	char  numbyte, numbyte2;

	numbyte = (img_numchan+7)>>3;
	numbyte2 = numbyte<<1;

	p2 = hamm_ampbits + (j*numbyte);
	for (i=0; i<numbyte; i++)
		*p1++ = *p2++;

	p2 = hamm_slopebits + (j*numbyte);
	for (i=0; i<numbyte; i++)
		*p1++ = *p2++;

	return;
}
get_zerox(j,k1,k2)
int j,*k1,*k2;
{
	register short int *p1;

	p1 = zerox_base + (j<<1);
	*k1 = *p1;
	*k2 = *(p1+1);
	return;
}
get_avg(p1,avg)
unsigned char *p1, *avg;
{
	int i,sum;

	sum = 0;
	for (i=0; i<img_numchan; i++)
		sum += *p1++;

	*avg = (float)sum/(float)img_numchan + 0.5;
	return;
}

/*************************************************************************/
/* compact double binary coding (4 byte for amplitude, 4 byte for slope) */
/* this algorithm is documented with imgcode() in get.c.                 */
/*************************************************************************/
compact_code(addr1,addr2,avg)
unsigned char *addr1, *addr2,avg;
{
	int l1;
	register unsigned char *p1, *p2;
	unsigned char hd;
	char  numbyte, numbyte2;

	numbyte = (img_numchan+7)>>3;
	numbyte2 = numbyte<<1;

	p1 = addr1;
	p2 = addr2 - 1;

/* amplitude encoding */

        for (l1=numbyte-1;l1>=0;l1--) {
	    *++p2=0; 
            if (l1) hd=0xff;
            else hd=0xff>>(numbyte*8-img_numchan);
	    while (hd!=0) {
	        while (hd!=0 && *p1<avg) {	/* 0's loop */
	            hd>>=1;
		    p1++;
                }
	        *p2=~(*p2+hd);

	        while (hd!=0 && *p1>=avg) {	/* 1's loop */
	            hd>>=1;
		    p1++;
                }
	        *p2=~(*p2+hd);
	    }
        }

/* slope encoding */

	p1 = addr1;

        for (l1=numbyte-1;l1>=0;l1--) {
            *++p2=0; 
	    if (l1) hd = 0xff;
            else hd=0xff>>(numbyte*8-img_numchan);
	    if (l1 == numbyte-1) hd >>= 1;
    
	    while (hd!=0) {
	        while (hd!=0 && *p1 >= *(p1+1)) {
		    hd>>=1;
		    p1++; 
	        }
	        *p2=~(*p2+hd);
	        while (hd!=0 && *p1 <= *(p1+1)) {
		    hd>>=1;
		    p1++;
	        }
	        *p2=~(*p2+hd);
	    }
	}
}

/*********************************************************************
          get average spectrum of a selected class
*********************************************************************/
avg_spec(listi,class,num,draw)
unsigned char *listi,draw;
int class;
int num;
{
	int i,j,k,l,k1,iscan;
	unsigned char avg;
	float temp,favg;
        char name[20];
        unsigned char *buff,*malloc();
        int *data,*calloc();
	j=0;
	iscan = 0;
        buff=malloc(img_numchan);
        data=calloc(img_numchan,4);
        if (buff==NULL || data==NULL) {
            printf("Insufficient memory.\n");
            if (buff!=NULL) free(buff);
            return(-1);
        }

/* get average spectrum plot */
	for (l=0; l<img_numchan; l++) data[l] = 0;

	for (l=0; l<img_nl && iscan<num; l++)
	for (k=0; k<img_bw && iscan<num; k++){
		if (*(listi+j) == class) {
		iscan += 1;
		get_data(k,l,buff);

		for (i=0; i<img_numchan; i++)
			data[i] = data[i]+ buff[i];
		}
		j += 1;
	}

	favg = 0.0;
	for (i=0; i<img_numchan; i++){
		temp = (float)data[i]/(float)iscan;
		favg += temp;
		spectra[class].data[i] = temp+0.5;	/* round up */
		}
	spectra[class].sum = favg;

	if (draw != 0) {
	   sprintf(name,"Average%d",class+1);
	   save_plot_data(name,spectra[class].data,4,1,1,1,1,class,0);
	   draw_plots();
	   if (draw == 2) if (var_spec(listi,class,iscan)==-1) return(-1);
	   vdflush(&unit);
	}
        free(buff);
        cfree(data);
        return(0);
}

var_spec(listi,class,num)
unsigned char *listi;
int class;
int num;
{
	int i,j,k,l,k1,iscan,avg;
	float temp;
        unsigned char *buff,*data,*malloc();
        char name[20];
	j=0;
	iscan = 0;
        buff=malloc(img_numchan);
        data=malloc(img_numchan);
        if (buff==NULL || data==NULL) {
            printf("Insufficient memory.\n");
            if (buff!=NULL) free(buff);
            return(-1);
        }

	for (l=0; l<img_numchan; l++) data[l] = 0.0;

	for (l=0; l<img_nl && iscan<num; l++)
	for (k=0; k<img_bw && iscan<num; k++){
		if (*(listi+j) == class) {
		iscan += 1;
		get_data(k,l,buff);

		for (i=0; i<img_numchan; i++) {
			temp = (buff[i]-spectra[class].data[i]);
			data[i] += temp*temp;
			}
		}
		j += 1;
	}

	avg = (float)spectra[class].sum/(float)img_numchan + 0.5;
	for (i=0; i<img_numchan; i++){
		temp = data[i]/(float)iscan;
		buff[i] = avg + (sqrt(temp) + 0.5);
		}

	sprintf(name,"Variance%d",class+1);
	save_plot_data(name,buff,4,1,1,1,1,15,0);
	draw_plots();

	for (i=0; i<img_numchan; i++) buff[i] = avg;
	sprintf(name,"Reference");
	save_plot_data(name,buff,4,1,1,1,1,14,0);
	draw_plots();
        free(buff);
        free(data);
        return(0);
}

hist_spec(listi,class,num)
unsigned char *listi;
int class;
int num;
{
	int i,j,k,l,k1,k2,k3,iscan;
	int nbins;
	int sum;
	unsigned char com[3],avg;
        unsigned char *buff,*hist_amp,*hist_slope,*hist_amp_dist;
        unsigned char *hist_slope_dist,*code,*data,*malloc();

	nbins = img_numchan;	/* may not be right? changed by asm */
	iscan = 0;
        buff=malloc(img_numchan);
        hist_amp=malloc(img_numchan);
        hist_slope=malloc(img_numchan);
        hist_amp_dist=malloc(img_numchan);
        hist_slope_dist=malloc(img_numchan);
        code=malloc(img_numchan);
        data=malloc(img_numchan);
        if (buff==NULL || hist_amp==NULL || hist_slope==NULL || 
                hist_amp_dist==NULL || hist_slope_dist==NULL || code==NULL ||
                data==NULL) {
            printf("Insufficient memory.\n");
            return(-1);
        }

	for (l=0; l<nbins; l++) {
		hist_amp[l] = 0;
		hist_slope[l] = 0;
		hist_amp_dist[l] = 0;
		hist_slope_dist[l] = 0;
		}

	sum = 0;
	for (l=0; l<img_numchan; l++)
	sum += spectra[class].data[l];
	avg = (float)sum/(float)img_numchan + 0.5;

	compact_code(spectra[class].data,code,avg);

	j = 0;
	for (l=0; l<img_nl && iscan<num; l++)
	for (k=0; k<img_bw && iscan<num; k++){
		if (*(listi+j) == class) {
		iscan +=1;
		get_code(j,buff);
		get_zerox(j,&k1,&k2);
		hist_amp[k1] += 1;
		hist_slope[k2] += 1;
	
		plot_dist(code,buff,&k1,&k2,0);
		hist_amp_dist[k1] += 1;
		hist_slope_dist[k2] += 1;	
		}
	j += 1;
	}

	query("amplitude distance histogram display?\n",com);
	if (com[0] == 'y') {
		save_hist_data(hist_amp_dist,nbins);
		draw_hist();
		}

	query("slope distance histogram display?\n",com);
 	if (com[0] == 'y') {
		save_hist_data(hist_slope_dist,nbins);
		draw_hist();
		}

	query("amplitude zero crossing histogram display?\n",com);
	if (com[0] == 'y') {
		save_hist_data(hist_amp,nbins);
		draw_hist();
		}

	query("slope zero crossing histogram display?\n",com);
	if (com[0] == 'y') {
		save_hist_data(hist_slope,nbins);
		draw_hist();
		}
	vdflush(&unit);	
        free(buff);
        free(hist_amp);
        free(hist_slope);
        free(hist_amp_dist);
        free(hist_slope_dist);
        free(code);
        free(data);
	return(0);
}


/********************************************************************
         Support routines for segmentation graphics
********************************************************************/
/********************************************************************/
/* draw image pixels for assigned color                             */
/********************************************************************/
image_draw(listi,class)
unsigned char *listi;
int class;
{
    long int k;
    register i,j;
    short int t,t1,t2;
    register unsigned char *p,*q;

    q = img_currband;
    p = listi;
    if (class!=-1) {
	for(i=0; i<img_nl; i++) for(j=0; j<img_bw; j++) {
	    if (*p == class) *q = *p + 1;
	    p++;
	    q++;
	}
    }
    else for (i=img_nl*img_bw;i!=0;i--) *q++ = *p++ + 1;
    t1=img_bw;
    t2=img_nl;
    k=img_nl*img_bw;
    vdareawr(&unit,&c4,&k,img_currband,&c1,&c1,&t1,&t2);
    vdflush(&unit);
    return;
}

plot_point(k,l,l1,box)
int k,l,l1,box;
{
	short int x[5],y[5];
	short int color;
	color=l1+1;
	x[0]=k+3-box;
	x[1]=k+2;
	x[2]=x[1];
	x[3]=x[0];
	x[4]=x[0];
	y[0]=l+2;
	y[1]=y[0];
	y[2]=l+3-box;
	y[3]=y[2];
	y[4]=y[0];
	vdvector(&unit,&c4,&color,&c0,&c0,&c5,x,y);
	vdflush(&unit);
}

/********************************************************************/
/* draw spectrum of selected strip of image pixels                  */
/********************************************************************/
spec_image_draw(listi,stretch)
unsigned char *listi;
float stretch;
{
	int k,l,l1;
	float temp;
        short int t,t1,t2;

	for(l=0; l<img_nl; l++)
	for(k=0; k<img_numchan; k++) {
	temp = *(listi++);

	if (stretch != 1.0) {
		temp = temp - 128;
		temp = temp * stretch + 128;
		if (temp > 255) temp = 255;
		else if (temp < 0) temp = 0;
		}
	l1 = temp;
        t=l1+1;
        t1=k+33;
        t2=l+1;
	vdpixwri(&unit,&c4,&t1,&t2,&t,&c0,&c0);
	}
	vdflush(&unit);
}


/****************************************************************************
    merge support routine
****************************************************************************/
change_class(listi,old,new,num_old)
unsigned char *listi;
int old,new;
int num_old;
{
	int l1,l2,j,count;

	count = 0;
	j = 0;

	for (l1=0; l1<img_nl; l1++)
	for (l2=0; l2<img_bw && count<num_old; l2++) {
	if (*(listi+j) == old) {
		*(listi+j) = new;
		count += 1;
		}	
	j += 1;
	}
}
