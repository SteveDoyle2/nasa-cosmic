/* spam.h 2.14 - header file for spam subroutines */

struct parameter {
    char *name;                       /* pointer to name of keyword          */
    int	 value_type;                  /* expected value: int(0),real,str,key */
    int required_flag;                /* 1 if required value                 */
    int value_count;                  /* number of values, -1 if variable    */
    char *prompt;                     /* short description of value          */
    char *value;                      /* pointer to target array or scalar   */
    int max_value_length;             /* for strings, max length of value    */
    int *input_count;                 /* number of values user gave          */
};

struct plot {
    unsigned char *name;              /* pointer to name of plot             */
    int mindn;                        /* minimum dn value in plot            */
    int maxdn;                        /* maximum dn value in plot            */
    int sum;                          /* sum of dn values in plot            */
    unsigned char *data;              /* data values plotted                 */
    int plottype;                     /* 0 if dataplot, 1 otherwise          */
    int data_loc[2];                  /* location of data area for dataplots */
    int avg_dims[2];                  /* dims of data area for dataplots     */
    unsigned char *variance;	      /* variance for dataplot averages      */
};

struct libentry {		      /* library structure definition        */
    char name[30];		      /* name of library/library_entry       */
    short int left;		      /* link to last entry                  */
    short int right;		      /* link to next/first entry            */
    char date[30];		      /* insertion/creation date             */
    short int lib_length;	      /* num of entries in libr (0th entry)  */
    float spectrum_swl;		      /* starting wavelength of spectrum     */
    float spectrum_ewl;		      /* ending wavelength of spectrum       */
    short int bytes_used;	      /* amount of used space in data area   */
    unsigned char *encoding;	      /* double-binary encoding of spectrum  */
    unsigned char unused[48];
    char data[512];		      /* entry data                          */
};

struct spectrum {		      /* this is used for aseg stuff.        */
    unsigned char *data;	      /* the data values                     */
    int sum;			      /* sum of the data values              */
    unsigned char *encoding;	      /* double-binary encoding              */
} spectra[16];

struct feature {		      /* feature database entry structure    */
    short int	no_peak;	      /* number of peaks in spectrum         */
    struct peak {		      /* peaks table                         */
	short int start;	      /*     start band of feature           */
	short int loc;		      /*     band of peak                    */
	short int end;		      /*     end band of feature             */
	short int depth;              /*     depth of feature in dn          */
	short int width;	      /*     width of feature in bands       */
    } *peaks;
    unsigned char *area;	      /* feature-normalized spectrum         */
};

FILE *session_log;		      /* pointer to session logfile spam.log */
int color_table[16][3];		      /* standard Spam palette		     */

                                      /* image information (get,dataplot)    */
int img_bw;                           /*     width of img band (frame)       */
int img_numchan;                      /*     number of channels, usu. 32|128 */
int img_nl;                           /*     number of lines, usu. 512       */
int img_sl;                           /*     starting line for the window    */
int img_el;                           /*     ending line for the window      */
int img_ns;                           /*     number of samps, usu. 1024|4096 */
int img_sb;                           /*     starting band for the window    */
int img_eb;                           /*     ending band for the window      */
float img_swl;                        /*     wavelength lower_limit          */
float img_ewl;                        /*     wavelength upper_limit          */
int img_in_core;                      /*     set if mem allocated for img    */
unsigned char *img_data;              /*     pointer to img data space       */
char *img_name;			      /*     name of image as used in get    */
int img_generic;                      /*     number of generic img plots     */
int img_modified;		      /*     set iff image modified in spam  */
unsigned char *img_currband;	      /*     last band displayed w/ colors   */
                                      /* plot information                    */
struct plot plots[16];                /*     declaration of plots table      */
int plots_hoff;			      /*     samp of lower-left graph corner */
int plots_voff;			      /*     line of lower-left graph corner */
int plots_width;		      /*     graph width in pixels           */
int plots_height;		      /*     graph height in pixels          */
int plots_num;                        /*     current number of plots         */
int plots_mindn;                      /*     lowest dn for norm_mode 1       */
int plots_maxdn;                      /*     highest dn for norm_mode 1      */
float plots_norm;                     /*     area to norm to for mode 2      */
int plots_norm_mode;                  /*     1=norm by amp, 2=by area        */
int plots_maxy;                       /*     highest dn value on graph       */
int plots_miny;                       /*     lowest dn value on graph        */
                                      /* cursor information                  */
int cursor_nl;                        /*     cursor number of lines          */
int cursor_ns;                        /*     cursor number of samples        */
                                      /* band display information            */
int display_mode;                     /*     display mode: 0=plots,  1=image */
int disp_band;                        /*     band number of singleband disp  */
int disp_startband;                   /*     first band for multiband disp   */
int disp_endband;                     /*     last band for multiband disp    */
                                      /* hist information                    */
int hist_on;                          /*     1 if histogram on screen (help) */
int hist_nbins;			      /*     number of bins in current hist. */
int hist_scale;                       /*     bins per bar, usually 1.        */
long int hist_nelts;		      /*     total number of values in hist. */
float hist_sum;			      /*     sum of values in hist.          */
float hist_sum2;		      /*     sum of squares for statistics.  */
int *hist_bins;			      /*     values in current hist.         */
				      /* Hamming dist tables                 */
unsigned char hamm_lut[256][256];     /*     pts to hammdist look-up table.  */
unsigned char *hamm_ampbits;          /*     encoding of spectral amplitude. */
unsigned char *hamm_slopebits;	      /*     encoding of spectral slopes.    */
short int *zerox_base;		      /*     start of zero crossing info.    */
                                      /* stretch information                 */
int stretch_start;		      /*     stretch limits                  */
int stretch_end;		      /*                                     */
                                      /* spectral library information        */
struct libentry *master_lib;	      /*     master library of spectra       */
struct libentry *user_lib;	      /*     user-created library            */
int user_lib_changed;		      /*     set when user adds to library   */
				      /* feature encoding globals            */
int feature_range;		      /*     0=none,1=ais,2=aviris,3=image   */
struct feature *feature_dbase;        /*     feature data base               */
                                      /* Virtual Device Interface constants  */
short int c0;			      /*     always equals 0                 */
short int c1;                         /*     always equals 1                 */
short int c2;                         /*     always equals 2                 */
short int c3;                         /*     always equals 3                 */
short int c4;                         /*     always equals 4                 */
short int c5;                         /*     always equals 5                 */
short int c6;                         /*     always equals 6                 */
short int c7;                         /*     always equals 7                 */
short int unit;			      /*     unit number                     */

/* installation specific constants */

/* system image-library directory.  End with '/' on unix systems. */
#define image_lib_dir "ud1:[tst.dcl.images]"

/* spectral library name */
#define spectral_lib_name "spamlib."

/* device availability defs - define with value zero if not available */
#define VT240LA50    1    	      /* VT240,LA50 available for hardcopy.   */
#define QCR          1                /* QCR is available.                    */

/* optional session logging */
#define printf prnlog		      /* comment out for no session logging   */

/* Host Operating System Information:                                      */
/* Define vms or unix, if not already defined, here.  Also define taevic   */
/* if you are running spam under the TAE executive with Vicar2.            */
/* vms and unix are usually defined automatically by system software, if   */
/* appropriate.                                                            */
/* Example:                                                                */
/* #define unix			      (using unix, but not tae)            */
