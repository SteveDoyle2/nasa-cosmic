



                     FORTRAN Programming Tools  
                     Release II.0


                     Submitted by Art Ragosta
                     US Army Research and Technology Labs
                     NASA Ames Research Center
                     MS 207-5
                     Moffett Field, Calif   94035
                     (415) 694-5578



This package contains a set of programming tools of either general interest 
or specifically aimed at FORTRAN 77 users.  Although many of the tools are 
transportable, some of the programs and all of the command files are 
specific to the VAX 11/780 family running VMS version 4.0 or higher.

The following tools are included :

1. MERLIB - a library of FORTRAN and MACRO routines that provide useful 
capabilities for FORTRAN programmers.  This library can also be linked to 
any language that is capable of supporting the FORTRAN interface on the VAX.

2. BUGOUT - a set of programs for the debugging and optimization of FORTRAN 
source codes.  This package is specifically for the determination of coding 
problems that could result in addressing errors (eg, COMMON blocks of 
different lengths in different routines, CALL statements with more or fewer 
arguments than the SUBROUTINE statement).  Additional capabilities include
automatic compilation with /DEBUG=BOUNDS, continual traceback printout, and
CPU time used by each subprogram.

3. CAPITAL - a program to capitalize all lower case letters in a file, leaving 
all other characters unaffected.

4. CCLEAN - a program to clean up COMMON blocks and NAMELISTS.  It is 
particularly useful for reformatting after using TIDY (available from a 
variety of sources).

5. CHANGE - a quick and dirty tool for changing one text string in a file
to another without using the editor (very basic).

6. CHECK72 - a program to verify that no text has been typed beyond column
72 (or 80) of a FORTRAN source deck (or other file).

7. DOUBLE - a program to list two files side-by-side on a VT100-compatible 
terminal.

8. EIGHTY - a program used to crunch a text file whose records are longer 
than 80 columns into 80 columns.  UNEIGHTY restores the records to full length.
(This program was written because our HASP communications will not transfer
files with records longer than 80 columns.)

9. FLS, FS - .COM files used to submit FORTRAN compiles to the batch queues.

10. FORTLIST - a useful listing program that formats a FORTRAN file, produces 
a listing with page numbers and dates, and creates a table of contents and 
index by subprogram name.

11. FORTVMS - a program for reformatting a file with FORTRAN-compatible 
carriage controls into a VMS-standard listing format.

12. NAMEIO and SNAMEIO - two programs to replace NAMELIST IO with transportable,
FORTRAN 77 code.

13. STRIP72 - a program to remove all characters beyond column 72 and remove 
trailing blanks.

14. STUB - a program to create dummy subprograms with a standard format 
prologue of comment cards.

15. UNTAB - a program to replace tab characters with the correct number of 
blanks.

16. VMSFORT - a program for reformatting a file with VMS-standard carriage 
control to the FORTRAN-compatible control characters.



Notes :

A. There are several MACRO routines that were provided on earlier DECUS 
releases that are provided in object form because they are used by other 
routines.  The source code is available from DECUS.

B. All other programs are the exclusive work of the submitter except FORTLIST 
which was based on a program written by Ed Austin at the Applied Technology 
Laboratory, Ft. Eustis, Va. and later modified by the submitter and Koreen 
Clay of the Technology Development Corporation.

C. All routines in the MERLIB library are similarly the work of the 
submitter except as noted in A, above.  Some of the algorithms were derived 
from text books; these routines are referenced in the source code.

D. The .COM files are all designed to run from a system account accessed as
logical name 'MERLIN:'.  It will be necessary to change all references to
MERLIN: to an appropriate directory.

E. A program to do least-squares curve fits (CURFIT) is included to demonstrate
the power and ease-of-use of MERLIB.

F. A deliberately modified (with errors) version of CURFIT is included as 
SAMPLE.FOR for testing of the BUGOUT system.  The full output of the pre-
processor step is included as SAMPLE.OUT.

G. See the file NEWSPAPER.TXT for the major changes since the previous release
of this package.
