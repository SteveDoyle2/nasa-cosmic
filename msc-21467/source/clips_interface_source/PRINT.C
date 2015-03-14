/*   CLIPS Version 4.30   4/25/89 */

#include <PrintMgr.h>

#define NULL 0L

#define topMargin 20
#define leftMargin 20
#define bottomMargin 20

static THPrint hPrint = NULL;

extern CursHandle Watch;
extern WindowPtr DisplayWindow;

/**********************************************/
/* CHECKPRINTRECORD: Allocates a print record */
/*   if one does not already exist.           */
/**********************************************/
static CheckPrintRecord()
  {
   /*===========================================*/
   /* If a print record already exists, return. */
   /*===========================================*/
   
   if (hPrint != NULL) return;
    
   /*==============================*/
   /* Allocate a new print record. */
   /*==============================*/
   
   hPrint = (TPrint **) NewHandle( sizeof( TPrint ));
      
  /*=============================================*/
  /* Fill the fields of the print record hPrint  */
  /* with the default values that are stored in  */
  /* the printer resource file.                  */
  /*=============================================*/
                                      
   PrintDefault(hPrint);
  }
  
/**********************************************/
/* DOPAGESETUP: Handle Page Setup... command. */
/**********************************************/
DoPageSetup()
  {
   /*====================================*/
   /* Open the printing manager for use. */
   /*====================================*/
   
   PrOpen();
   
   /*============================================*/
   /* Create print record if one does not exist. */
   /*============================================*/
   
   CheckPrintRecord();
   
   /*===================================================*/
   /* Conduct a style dialog with the user to determine */
   /* the page dimensions and other information needed  */
   /* for page setup. The initial settings displayed in */
   /* the dialog box are taken from the most recent     */
   /* print record. Returns TRUE and saves the results  */
   /* in the print record hPrint if the user confirms   */
   /* the dialog . Otherwise returns FALSE.             */
   /*===================================================*/
   
   PrStlDialog(hPrint);
   
   /*=============================*/
   /* Close the printing manager. */
   /*=============================*/
   
   PrClose();
  }
  
/*************************************/
/* DOPRINT: Handle Print... command. */
/*************************************/
DoPrint(whichText,whichGP)
  TEHandle whichText;
  GrafPtr whichGP;
  {
   TPPrPort	printPort;
   GrafPtr savePort;
   TPrStatus prStatus;
   int copies;
   char **hText;
   long int length;

   /*========================================*/
   /* Get a handle to the text to be printed */
   /* and the length of the text.            */
   /*========================================*/
   
   if (whichGP != DisplayWindow)
     {
      hText = (**whichText).hText;
      length = (long) (**whichText).teLength;
     }
   
   /*====================================*/
   /* Open the printing manager for use. */
   /*====================================*/
   
   PrOpen();
   
   /*============================================*/
   /* Create print record if one does not exist. */
   /*============================================*/
   
   CheckPrintRecord();

   /*=================================================*/
   /* Restore normal cursor before displaying dialog. */
   /*=================================================*/
   
   InitCursor();
   
   /*=====================================================*/
   /* Conduct a job dialog with the user to determine the */
   /* print quality, range of pages to print, and so on.  */
   /* If the user cancels the print command, then close   */
   /* the Printing Manager.                               */
   /*=====================================================*/
   
   if (PrJobDialog(hPrint) == 0) 
     { 
      PrClose();
      return;
     }
     
   /*===================================*/
   /* Indicate delay with watch cursor. */
   /*===================================*/
   
   SetCursor(*Watch);
   
   /*========================*/
   /* Save the current port. */
   /*========================*/
   
   GetPort(&savePort);
   
   /*============================================*/
   /* Determine the number of copies to be made. */
   /*============================================*/
   
   if ( (**hPrint).prJob.bJDocLoop == bDraftLoop)
     { copies = (**hPrint).prJob.iCopies; }
   else
     { copies = 1; }
       
   /*==================*/
   /* Print each copy. */
   /*==================*/ 
   
   for ( ; copies > 0 ; copies--) 
	 {
      if (whichGP != DisplayWindow)
        {
         PrDoc (hText, length, hPrint,
                 (*whichGP).txFont, (*whichGP).txSize);
        }
      else
        {
         PrDisplay(hPrint,(*whichGP).txFont, (*whichGP).txSize);
        }
      
      /* Print a spooled document. */
	  PrPicFile(hPrint, 0L, 0L, 0L, &prStatus );
	 }

   /*============================*/
   /* Restore the original port. */
   /*============================*/
   
   SetPort(savePort);
	
   /*=============================*/
   /* Close the Printing Manager. */
   /*=============================*/
   
   PrClose();
  }

/***********************************************/
/* PRDOC: Prints a document to a print record. */
/***********************************************/
PrDoc(hText, count, hPrint, font, size)
  char		**hText;
  long		count;
  THPrint 	hPrint;
  int			font;
  int			size;
  {
   register int 	line = 0;
   register int 	lastLineOnPage = 0;
   int				length;
   Rect 			printRect;
   int 			linesPerPage;
   int 			lineBase;
   int 			lineHeight;
   register char 	*ptr, *p1;
   FontInfo		info;
   TPPrPort		printPort;

   /*====================================*/
   /* Initialize a printing grafPort for */
   /* use in printing a document.        */
   /*====================================*/
   
   printPort = PrOpenDoc( hPrint, 0L, 0L );
   
   /*==============================================================*/
   /* Make the current port the printing port. Note that PrOpenDoc */
   /* should have already made printPort the current port.         */
   /*==============================================================*/
   
   SetPort(printPort);
   
   /*===============================================================*/
   /* Set the appropriate font type and size for the printing port. */
   /*===============================================================*/
   
   TextFont(font);
   TextSize(size);
   
   /*===============================================*/
   /* Determine the line height and number of lines */
   /* per page for the printing port.               */
   /*===============================================*/
   
   printRect = (**hPrint).prInfo.rPage;
   GetFontInfo( &info );
   lineHeight = info.leading + info.ascent + info.descent;
   linesPerPage = 
		(printRect.bottom - printRect.top - topMargin - bottomMargin) / lineHeight;
		
   /*=======================================*/
   /* Lock the document text handle and get */
   /* a pointer to the document text.       */
   /*=======================================*/
   
   HLock(hText);
   ptr = p1 = (*hText);
   
   /*=======================================*/
   /* Print the document page by page until */
   /* the end of document is reached.       */
   /*=======================================*/
   
   while (ptr < (*hText) + count)
	 {
	  /*==================================================*/
	  /* Inform the Printing Manager to begin a new page. */
	  /*==================================================*/
	  
	  PrOpenPage( printPort, 0L );
	  
	  /*============================================*/
	  /* Compute the last line on the current page. */
	  /*============================================*/
	  
	  lastLineOnPage += linesPerPage;
	  
	  /*=================================================*/
	  /* Position the pen in the upper left of the page. */
	  /*=================================================*/
	  
	  lineBase = printRect.top + lineHeight;
	  MoveTo(printRect.left + leftMargin,lineBase);
	  
	  /*=========================================================*/
	  /* Print the current page line by line until the last line */
	  /* on the page or the end of the document is reached.      */
	  /*=========================================================*/
	  
	  while ((line < lastLineOnPage) && (ptr < (*hText) + count))
	    {
	     /*===========================================*/
		 /* Find the next line break in the document. */
		 /*===========================================*/
		 
		 while ((ptr <= (*hText)+count) && (*ptr++ != (char) '\r'));
		 
		 /*==================================================*/
		 /* Print the line. Note that tab characters have no */
		 /* special meaning to DrawText.                     */
		 /*==================================================*/
		 
		 length = (int) (ptr - p1) - 1;
		 if (length > 0) DrawText(p1,0,length);
		 
		 /*====================================*/
		 /* Position the pen at the next line. */
		 /*====================================*/
		 
		 lineBase += lineHeight;
		 MoveTo(printRect.left + leftMargin,lineBase);
		 
		 /*======================================*/
		 /* Update text pointers and line count. */
		 /*======================================*/
		 
		 p1 = ptr;
		 line++;
		}
	   
	  /*==================================*/
	  /* Inform the Printing Manager that */
	  /* the current page is finished.    */
	  /*==================================*/
	  
	  PrClosePage( printPort );
	 } 
	
   /*================================*/
   /* Lock the document text handle. */
   /*================================*/
   
   HUnlock(hText);
	
   /*==============================*/
   /* Close the printing grafPort. */
   /*==============================*/
	
   PrCloseDoc( printPort );
  }