C----------------------------------------------------------------
C
C		I    M    G    B
C			
C     Common area used by IBASE.FOR to pass an image
C----------------------------------------------------------------
C
C Declare the necessary variables
C
	INTEGER * 2	IMAGEB(300000)
	CHARACTER * 16	CUBE_IN_B
	CHARACTER * 16	LAYER_IN_B
	INTEGER		SIZE_XB
	INTEGER		SIZE_YB
C
	COMMON /SECOND_IMAGE/	IMAGEB,
	1			CUBE_IN_B,
	2			LAYER_IN_B,
	3			SIZE_XB,
	4			SIZE_YB
