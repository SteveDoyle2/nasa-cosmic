# Makefile 2.15

CFLAGS = -g

SRCS=	cluster.c curvegen.c delete.c \
	digiplot.c directory.c disp.c erase.c feasubs.c feature.c filter.c \
	find.c func.c get.c hardcopy.c help.c hist.c histsubs.c identify.c \
	init.c keep.c libcode.c libplot.c \
	librsubs.c merge.c mixture.c norm.c par.c photo.c plot.c plotsubs.c \
	ratio.c return.c saveplot.c scan.c segdisp.c \
	segsubs.c snapshot.c  spamsubs.c storespec.c \
	stretch.c subs.c token.c vdi.c vos.c waveleng.c \
	plot.h

OBJS=	cluster.o curvegen.o delete.o \
	digiplot.o directory.o disp.o erase.o feasubs.o feature.o filter.o \
	find.o func.o get.o hardcopy.o help.o hist.o histsubs.o identify.o \
	init.o keep.o libcode.o libplot.o \
	librsubs.o merge.o mixture.o norm.o par.o photo.o plot.o plotsubs.o \
	ratio.o return.o saveplot.o scan.o segdisp.o \
	segsubs.o snapshot.o spamsubs.o storespec.o \
	stretch.o subs.o token.o vdi.o vos.o waveleng.o

spam:	libspam.a main.o
	cc -g main.o libspam.a -o spam -lm -lra

libspam.a: $(OBJS)
	ar ru libspam.a $(OBJS)
	ranlib libspam.a
