C
C Declare the variables in the common area
C
	INTEGER	* 2	IMAGE(8,262144)		! The Image array.
	INTEGER	* 2	ICLUST(262144)		! The assigned clusters
	REAL		CENTER(8,128)		! Centroid center
	INTEGER		PIXCNT(128)		! Number of pixels in cluster
	REAL		MIN_CENT_DIST		! Minimum distance between
						! Centroids
	INTEGER		SIZE_X,SIZE_Y		! Samples in image
	INTEGER		N_CHAN			! the number of channels
	INTEGER		MAX_CYCLES		! Maximum # of reallocation
						! cycles.
	INTEGER		TARGET_NUM_CLUST	! Targeted number of clusters
	INTEGER		NCLST			! # of allowable merging
						! operations on clusters.
	INTEGER		MIN_PIX_CLUST		! Minimum number of pixels
						! required to make a cluster.
	INTEGER		SPLIT_CLUST		! Controls the splitting of
						! clusters.
	INTEGER		INTERMAX		! Maximum # of cycles
	INTEGER * 4	GEN_SEED		! Seed for random num generator
	INTEGER		HIGHEST_CLUST		! Pointer to the last cluster
C
C Declare the common area.
C
	COMMON /CLUSTER/	IMAGE,
	1			ICLUST,
	1			CENTER,
	1			PIXCNT,
	1			MIN_CENT_DIST,
	1			SIZE_X,
	1			SIZE_Y,
	1			N_CHAN,
	1			MAX_CYCLES,
	1			TARGET_NUM_CLUST,
	1			NCLST,
	1			MIN_PIX_CLUST,
	1			SPLIT_CLUST,
	1			INTERMAX,
	1			GEN_SEED,
	1			HIGHEST_CLUST
