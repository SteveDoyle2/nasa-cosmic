C+++
C Declarations for a layer record
C
	CHARACTER * 16	CUBE_NAME	! positions 1:15, Key 0
	CHARACTER * 16	LAYER_NAME	! positions 16:31, Key 0
	CHARACTER * 80	FILE_NAME	! positions 32:111
	CHARACTER * 70	COMMENT		! positions 112:181
	CHARACTER * 16	LAYER_LABEL	! positions 182:197
	CHARACTER * 1	LAYER_TYPE	! positions 198:198
	CHARACTER * 1	DATA_REP	! positions 199:199
	CHARACTER * 1	DATA_TYPE	! positions 200:200
	CHARACTER * 1	CDA		! positions 217:220
	CHARACTER * 1	CMO		! positions 221:224
	CHARACTER * 1	CYR		! positions 225:228	
	INTEGER   	LAY_SIZE_X	! positions 201:204
	INTEGER   	LAY_SIZE_Y	! positions 205:208
	INTEGER   	PIX_SIZE_X	! positions 209:212
	INTEGER   	PIX_SIZE_Y	! positions 213:216
C
	INTEGER		D_S_MO
	INTEGER		D_S_DA
	INTEGER		D_S_YR
C---
