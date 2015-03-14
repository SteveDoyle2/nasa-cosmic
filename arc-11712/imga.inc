C----------------------------------------------------------------
C
C		I    M    G    A
C			
C     Common area used by IBASE.FOR to pass an image
C----------------------------------------------------------------
C
C Declare the necessary variables
C
	INTEGER * 2	IMAGEA(300000)
	CHARACTER * 16	CUBE_IN_A
	CHARACTER * 16	LAYER_IN_A
	INTEGER		SIZE_XA
	INTEGER		SIZE_YA
C
	COMMON /FIRST_IMAGE/	IMAGEA,
	1			CUBE_IN_A,
	2			LAYER_IN_A,
	3			SIZE_XA,
	4			SIZE_YA
