						    May 8, 1986

     This directory contains source code for the VAX/VMS
distribution of the STAR interpreter.  The code is divided among
several files, summarized briefly below.  Following this, a set
of guidelines is given for compiling and linking the files into
an executable module for the STAR interpreter.

  STARDEFS.H  -- definitions used throughout an application,
  STARCOMM.H  -- definitions used locally within the interpreter,

  STARCODE.C  -- main source code for the STAR interpreter,
  STARBIFS.C  -- definitions for most built-in functions in STAR,
  STARPLUS.C  -- built-in functions which are machine-dependent,
  STARHACK.C  -- machine-dependent initialization code for STAR,

  STARINI1.C  -- initialization data for the STAR knowledge base,
  STARINI2.C  --   "
  STARINI3.C  --   "
  STARINI4.C  --   "
  STARINI5.C  --   "
  STARINI6.C  --   "

  STARLINK.C  -- tables for linking external functions into STAR,
  STARUTIL.C  -- utility routines for accessing & altering UNITs,

  SAMPLEC.C   -- sample external routines defined in C,
  SAMPLEF.FOR -- sample external routines defined in FORTRAN,
  SAMPLEP.PAS -- sample external routines defined in PASCAL.

----------

     The following commands may be used to compile the source
files.  Each compilation produces an object file of the same
name as the source file, but with the trailing label replaced
with ".OBJ".  ("$" indicates the VAX/VMS prompt).

      $ CC STARCODE.C
      $ CC STARBIFS.C
      $ CC STARPLUS.C
      $ CC STARHACK.C

      $ CC STARINI1.C
      $ CC STARINI2.C
      $ CC STARINI3.C
      $ CC STARINI4.C
      $ CC STARINI5.C
      $ CC STARINI6.C

      $ CC STARLINK.C
      $ CC STARUTIL.C

      $ CC SAMPLEC.C
      $ FORTRAN SAMPLEF.FOR
      $ PASCAL SAMPLEP.PAS

     Following this, the command file "LINK.COM" may be
invoked to link the object files together into a single
executable file.

      $ @LINK

     The executable file produced is called "STAR.EXE" and
may be installed as a "foreign command" by entering

      $ STAR :== $ disk:[user.directory]STAR.EXE

where "disk", "user" and "directory" depend upon the
directory into which STAR has been loaded.  Following this,
STAR may be called directly from the system level as follows.

      $ STAR
