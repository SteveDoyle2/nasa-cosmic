.ll 7i
.TH SPAM 1 "October 1986"
.\" @(#)spam
.SH NAME
spam \- SPectral Analysis Manager
.SH SYNOPSIS
.B spam
.SH DESCRIPTION
SPAM (SPectral Analysis Manager) is an interactive spectral analysis
package.  Images must be labeled using either the makelabel utility or
the TAE program LABEL.  See the description of the "get" command below 
for more information.
.SH COMMAND LISTING
cluster [noise=thr1][cluster=thr2][manual][mask=ranges]
.br
curvegen [name=curvename]
.br
delete plot=plotname(s)
.br
digiplot name=plotname [spline]
.br
directory [userlib] [masterlib]
.br
display [bands=start [,end]]
.br
erase [clus][curv][data][fea][func][hist][lib][plot=names]
.br
feature [disp|find][spec=names][peaks=ranges|thr=%][ais|av]
.br
filter window=width [triangle]
.br
find plot=name thresh=maxerr(s) [hist|lib] [mask=ranges]
.br
func function="plotname|image=mathematical-expr"
.br
get file=imgname [sl=ln1] [nl=#lns] [sb=band1] [nb=#bands]
.br
hardcopy [plot=plotname(s)]
.br
help [command-name | general | all]
.br
hist [area=lines,samps] [Hammingdist [mask=ranges]]
.br
identify [thresh=amp_err,slope_err] [mask=ranges]
.br
keep
.br
libplot dataset=libplot-name(s)
.br
merge [classes=from-class-#, to-class-#, ...]
.br
mixture mode=area|plot option=method ref=names
.br
normalize [amplitude | area]
.br
photo [text=caption]
.br
plot [name=pnam] [ave=lns,smps [var]] [loc=ln,smp] [stats]
.br
quit
.br
ratio [bands=first,second]
.br
restore
.br
return
.br
saveplot plot=name [as name2]...
.br
scan [bands=start,end] [speed=time(ms)] [number]
.br
segdisp [map] [spectra] [classes=class_number(s)]
.br
snapshot file=filename [text=caption]
.br
storespec [area=lines,samps] [loc=line,samp]
.br
stretch [stretch=lower-val,upper-val]
.br
wavelength
.br
.sp
.SH COMMAND DESCRIPTIONS
.sp
.IP CLUSTER
.sp
cluster [noise=thr1][cluster=thr2][manual][mask=ranges]
.sp
Cluster segments the current image into a number of
distinct regions
based on similarity of spectra.  The noise keyword specifies
the maximum numbers of fluctuations about the mean for
amplitude and slope; spectra falling outside these thresholds
will be regarded as noise.  The cluster keyword indicates in
how many bands two spectra can differ in amplitude and slope
and still be considered identical.  Spam will select
values based on the data if thresholds are defaulted.
The manual keyword allows a user to interactively cluster
an image, providing a little more flexibility.
The mask
keyword may be used to specify ranges of bands which should
not be taken into account in the clustering.
.sp
Image segmentation may be saved in a disk file and then
used later with the segdisp, merge, and identify commands.
.sp
.IP CURVEGEN
.sp
curvegen [name=curvename]
.sp
Curvegen allows the user to define a curve using the
digi-pad cursor.
It may only be used when the graph area is available
and at least one plot is up.
.sp
The name option allows the user to give the plots a name;
if no name is specified, the curve will be named for you.
.sp
.IP DELETE
.sp
delete plot=plotname(s)
.sp
The delete command deletes the specified plot(s) from the
user's library.  Up to 16 plots may be deleted at a time.
.sp
.IP DIGIPLOT
.sp
digiplot name=plotname [spline]
.sp
Digiplot allows a user to enter a printed spectral plot into
their user library.  It can be used with or without an image.
.sp
The "name" keyword specifies the name of the plot; it's
required.  The "spline" keyword indicates that digiplot
should perform a cubic spline fit to a set of discrete points
rather than the default method of asking the user to trace
over the entire curve using the cursor.  Smooth plots without
narrow features are probably best encoded using the spline
option; the cubic splines often yield a smoother plot.
.sp
.IP DIRECTORY
.sp
directory [userlib] [masterlib]
.sp
The directory command lists the contents of the specified
spectral library.  The default is to list the contents of
both the user's local library and the system master
library.
.sp
.IP DISPLAY
.sp
display [bands=start [,end]]
.sp
Display allows the user to look at specific bands of the
image; bands are selected through the bands keyword.
.sp
The bands keyword takes one or two band numbers as
arguments.  If only one band is specified, the band will
be drawn in the normal band area to the left of the plots.
(The old plot marks will be retained.)  If two bands are
specified, then the normal display will disappear and
the indicated range of bands will be displayed.
Typing the command "return" will restore the screen to
its previous state.  The default, if no bands are specified,
is to display as much of the image as possible,
starting with the first band.
.sp
.IP ERASE
.sp
erase [clus][curv][data][fea][func][hist][lib][plot=names]
.sp
The erase command allows the user to selectively erase
parts of the screen.
If the command is specified without
additional keywords, all plots and the histogram or feature
extraction strips, if any,
are removed.  Curv and data remove curvegen plots and image
plots, respectively.  Func, lib, and clus
erase plots generated by the function, libplot, and cluster
commands, respectively.  Using the plot
parameter directs the program to erase only the plot(s)
specified.  Finally, the hist keyword removes the histogram
in display or graphics mode, and fea clears feature
extraction information.
.sp
Note that when image plots are removed, the marker on
the image itself remains, but changes color to black.  This
can be used to remind the user of places which have already
been looked at.
.sp
.IP FEATURE
.sp
feature [disp|find][spec=names][peaks=ranges|thr=%][ais|av]
.sp
The feature command uses a feature extraction algorithm to
allow analysis of spectra in the library and, to a certain
extent, in an image.  The feature command may
be used with or without an image; the library is resampled
to correspond to the image if one is available, and the
standard AIS or AVIRIS wavelength range otherwise.
.sp
There are two basic modes: display and find.  In display mode
Spam either displays features of library spectra
given by the spec
keyword, or if the peakloc keyword is used, searches the
libraries and displays the features
of all spectra which have peaks within each of up to ten
wavelength ranges;
in either case, features are identified using multi-colored
strips below the graph area.
In find mode, Spam takes a single spectrum
as given by the spec keyword and displays strips for those
library spectra which are similar as determined by the thr
percentage threshold.  (Two spectra are compared as follows:
First they are feature-normalized so that the total area
of the absorption peaks is 255; featureless ranges are
zeroed.  Then the difference is defined as the sum of the
absolute values of the dn value differences at each
wavelength and adjusted so that differences range between
0 and 100 percent.
.sp
The ais and aviris keywords are used to specify
a resampling range
if feature is being used without an image.  (After the first
get, feature uses the wavelength range represented by the
image.)  Once resampling has been specified, the keywords may
be omitted until another resampling is desired.
.sp
.IP FILTER
.sp
filter window=width [triangle]
.sp
The filter command performs a box filtering of the
image in the spectral direction using the specified window
width; bands currently displayed are replaced.
If the triangle keyword is used, filter will use
weights in the computations.
.sp
.IP FIND
.sp
find plot=name thresh=maxerr(s) [hist|lib] [mask=ranges]
.sp
The find command attempts to match a specified plot against
similar plots in the image or the spectral libraries.
.sp
The plot parameter is required and specifies the name
of a plot currently displayed which should be matched.
.sp
The thresh keyword specifies the maximum allowable
deviation between spectra in order for
them to be considered identical.
If only one value is given, find will compare
spectral amplitude only; if two values are
used, find will compare slope similarity also.
(The deviation is defined as the number of bands
where the binary amplitude or slope values of two spectra
differ.)
.sp
In the normal, image-search mode, find will color in those
pixels in the image which match the specified plot using the
plot's color.
The lib keyword indicates that find should search the
spectral libraries rather than the image
and list similar spectra with their amplitude and slope
deviations.
.sp
The hist keyword displays a histogram of
the deviations between the reference plot and matching
spectra in the image.  This is for image finds only.
.sp
The mask keyword may be used to specify
bands which should not be taken into account in the find,
where a range
is a pair of bands separated by a space, comma, or hyphen.
Ranges may be separated by commas or spaces.
The default is to use all bands in the image.
.sp
.IP FUNCTION
.sp
func function="plotname|image=mathematical-expr"
.sp
The func command allows the user to plot or display the
results of functions involving plots and the image.
.sp
.nf
Example:  func "funcplot=abs(kao-alun)/cal*160"
.fi
.nf
                           or
.fi
.nf
          func "image=image*128/flatspec"
.fi
.sp
The standard precedence rules apply: multiplication and
division are done first, followed by addition and
subtraction, moving left-to-right.  Negation is allowed,
but negated quantities must have parentheses if on the
right-hand side of an operator.  The wildcard character, ?,
may be used in plotnames, as usual.  Constants may be real or
integer, but if no real constants are used, the calculations
will be done using integer arithmetic; this is significantly
faster than using floating point.
If values go outside the range 0 to
255 inclusive, they will be set to the nearest cutoff
value (i.e., 0 or 255).
Finally, the functions "abs"
and "sum" return the absolute value of a spectrum and the
sum of the spectral reflectance values (duplicated to make
a vector), respectively.
.sp
.IP GET
.sp
get file=imgname [sl=ln1] [nl=#lns] [sb=band1] [nb=#bands]
.sp
The get command directs the program to fetch an image from
the disk.  The only required argument is the filename;
if a filename is not specified, Spam will prompt
for it.  The remaining keywords sl, nl, sb, and nb may
be used individually or collectively to select subparts of
the image.  The default for starting line (sl) and band (sb)
is 1; number of lines (nl) and number of bands (nb) default
to the maximum for the image as given in the label.
.sp
All files must have a label generated either through the
makelabel program or through TAE's LABEL-ADD specifying
the number of lines (NL), number of samples (NS),
starting wavelength (SW), ending wavelength (EW),
bandwidth (BW), and data format (FORMAT).
.sp
Input images
may have up to 512 bands, each of which may be up to 256
pixels wide.
.sp
.IP HARDCOPY
.sp
hardcopy [plot=plotname(s)]
.sp
The hardcopy command writes currently displayed plots and
histograms out
to the printer.  Up to four plots may be printed at a time.
If no plots are specified, hardcopy will print out the first
four plots, if possible.  Hardcopy uses the VT125 emulator
contained in VT240 and other terminals, and a
directly-connected LA50-compatible printer.
.sp
.IP HELP
.sp
help [command-name | general | all]
.sp
The help command allows the user to get help on program
operation and details of specific commands.
.sp
If a particular command is specified, help will give
details and hints about that command.  If the keyword "all"
is specified, help will give a summary description of all
Spam commands.  The default help is context-dependent, that
is, Spam will summarize the commands which are currently
legal and make sense.  Finally, "help general", the default
before the first image get, gives general notes on program
operation including image labeling.
.sp
.IP HISTOGRAM
.sp
hist [area=lines,samps] [Hammingdist [mask=ranges]]
.sp
The hist command does two types of histograms.
.sp
The first of these is a histogram of dn values.
To do a histogram of dn values within a subarea of a band,
use the area keyword and indicate the area with the
box-cursor; this can be done in display mode as well as the
normal graphics mode.  If no area is specified, the program
will histogram the current strip in graphics mode, or the
entire image in display mode.
.sp
The second variety of histogram is a graph of Hamming
distances between each pair of spectra within a user-given
area; if no area (box-cursor size) is given, the program
will default to a 5 line by 5 sample area.  Once the user
has selected the area, the program calculates the Hamming
distance between each pair of spectra represented by the
area and graphs these.  (This, too, can be done in either
mode.)
The mask keyword may be used with Hamming histograms to
specify ranges of bands which should not be taken into
account in the hist, where a range
is a pair of bands separated by a space, comma, or hyphen.
Ranges may be separated by commas or spaces.
The default is to use all bands in the image.
.sp
.IP IDENTIFY
.sp
identify [thresh=amp_err,slope_err] [mask=ranges]
.sp
The identify command reads a previously-saved cluster map and
attempts to identify the spectral classes found.  The
threshold keyword specifies amplitude and slope error
thresholds to use in comparing representative spectra with
library entries.  If defaulted, Spam will select thresholds
automatically.
The mask
keyword may be used to specify ranges of bands which should
not be taken into account in the identification.
For more information on clustering, type
"help cluster".
.sp
.IP KEEP
.sp
keep
.sp
The keep command saves the current Spam session so that
it can be continued later with the restore command.
Note that this command writes two or three files out to"
the current directory which are then deleted automatically
by restore.  These are spam.ses, spam.lst, and possibly
spam.img, depending on what you have done so far.  Keep
will not save multi-band displays, mixture maps or feature
extraction information.
.sp
.IP LIBPLOT
.sp
libplot dataset=libplot-name(s)
.sp
The libplot command displays the specified
library spectra along either the wavelength
range represented by the image or, if no gets have
been done, the feature extraction
resampling range currently being used.
.sp
The ? wildcard may be used anywhere within a dataset name
to indicate that some characters in the name have been
omitted.
.sp
Library data comes from one of two places, the system master
library, which contains a basic set of laboratory spectra,
or the user's own library, which may be created and filled
with the saveplot command.  (Type "help saveplot" for more
information on the latter feature).
.sp
.IP MERGE
.sp
merge [classes=from-class-#, to-class-#, ...]
.sp
Merge reads in a previously-saved cluster map from the disk
and merges the specified spectral classes, which may be
entered by number on the command line using
the classes keyword or
interactively (the default).  The resulting
clustering may be saved, if desired.  For more information
on clustering, type "help cluster".
.sp
.IP MIXTURE
.sp
mixture mode=area|plot option=method ref=names
.sp
The mixture command is used to break a plot or portion of
the image down into its mixture components.  There are two
modes: the area mode which
analyzes a portion of an image, then
generates a set of mixture maps, one map per reference
mineral, and the plot mode which analyzes a plot
and then generates a mixture plot and mixture proportions
for each component mineral.
.sp
The second parameter
specifies a method to be used for the mixture analysis.
Possible options in area mode are
"amp" and "slope" for
amplitude and slope encoding,
"binary" for binary encoding (amplitude and slope),
"euclidean" for euclidean distance, 
and
"least" for least-squares fit
The only allowable option in plot mode is
"least".  The
execution time and accuracy of the result increase in the
order of the options as given above.
.sp
Finally, the ref keyword specifies up to eight
currently-displayed reference spectra.  All
mixtures will be in terms of these spectra,
and the time the analysis takes is proportional
to the number of reference spectra specified.
.sp
Spam will prompt for the areas or names of plots to be
analyzed; when using
plot mode, plots to be analyzed must be displayed
ahead of time.
.sp
.IP NORMALIZE
.sp
normalize [amplitude | area]
.sp
The normalize command is used to change the normalization
of plots.  There are two normalization modes: the
amplitude-normalization mode, where all plots are 
displayed normally and the screen is scaled to allow
5 DN space above the highest DN value, and 5 DN below the
lowest, and the area-normalization mode, where data is
amplified in such a way that all of the displayed curves
have the same area.  The default is to renormalize using
the current mode.
.sp
Changing normalization mid-stream causes all current
plots to be redrawn using the new normalization.
.sp
.IP PHOTOGRAPH
.sp
photo [text=caption]
.sp
The photo command sends the current screen to QCR (Quick
Color Recorder) camera for a quick Polaroid photo.
This may be used instead of the snapshot command for
quicker photo processing.
The text keyword specifies a caption for the photo.
The caption should be quoted and no more than
30 characters long.
.sp
.IP PLOT
.sp
plot [name=pnam] [ave=lns,smps [var]] [loc=ln,smp] [stats]
.sp
The plot command plots out the DN values for each image
band at given points.
.sp
The first parameter, name, specifies a name for the
(first) area or point to be plotted; this must have no more
than 16 characters.  If the name is defaulted, Spam will name
the plot for you.
.sp
The second, ave, indicates that plots should be of
the averages of areas within each band,
rather than of single pixels.  A box cursor is provided for
easy selection of the averaging areas.  If the aver keyword
is omitted, Spam will average over the current box-cursor
area.  The var keyword, used with averaging, displays scatter
bars with the plot showing the variance of the spectra within
the averaging area.
.sp
The loc keyword allows the user to give the plot location
on the command line, rather than by using the cursor.
For area plots, the location is the line and sample of
the pixel in the lower-right corner of the averaging
area.
.sp
If the stats keyword is used, Spam will print out the mean
and standard deviation of the plot values.
.sp
.IP QUIT
.sp
quit
.sp
The quit command exits the program.
.sp
.IP RATIO
.sp
ratio [bands=first,second]
.sp
Ratio ratios two user-selected bands and displays the
result as an image band.  If no keyword is specified,
Spam will prompt the user for two cursor positions within
the graph area, and the corresponding bands will be used
for the ratio.  The bands may also be specified using the
bands keyword, numerator band first.  Ratio may only be
used in graphics mode.
.sp
.IP RESTORE
.sp
restore
.sp
The restore command restores the last Spam session
so that you can continue from where you left off.
Note that restore deletes temporary files created
by the keep command automatically, so a previous session
may only be restored once.
.sp
.IP RETURN
.sp
return
.sp
The return command returns the display to graphics mode after
a multi-band display (disp) or mixture analysis (mixture).
.sp
.IP SAVEPLOT
.sp
saveplot plot=name [as name2]...
.sp
The saveplot command saves one or more current plots in the
user's own library so that they may be recalled later with
the libplot command.  It will create a library in the user's
area, if necessary.
.sp
The "as" keyword allows the user to save the plot under a
different name.  If "as" is used, it must be spelled out;
it may not be abbreviated.
.sp
You may specify up to 16 plots, any or all of which may be
renamed.  For example, "saveplot ais1 ais2" will save those
two plots in your user library.  "saveplot ais1 as kao"
will save ais1 in your library, but rename it "kao".
"saveplot ais1 as kao, ais2, ais3 as alun" will save ais1
as "kao", ais2 as itself, and ais3 as "alun".
(The commas are optional.)
.sp
.IP SCAN
.sp
scan [bands=start,end] [speed=time(ms)] [number]
.sp
The scan command displays the bands of the current image in
rapid succession, showing quickly the areas of variability.
.sp
There are three parameters for this command, all of them
optional.  The first, bands, may be used to specify a
starting and ending band for the scan.
The default if
no range of bands is specified is to scan starting with the
first band and continuing through band 3mn, where m is 512
divided by the band length in lines and n is
512 divided by the band width in pixels.
The second parameter, speed,
specifies the time interval between bands.
The default for the speed option is a time of about 250 ms
or 4 frames per second.  The maximum scan speed is somewhat
implementation-dependent, but should be about 30 frames
per second; setting the speed parameter to 0 will
optimize the scan rate.
The third, number, causes scan to display the band number
of each band in the upper-left corner of the image area.
Normally scan does not display band numbers.
.sp
.IP SEGDISP
.sp
segdisp [map] [spectra] [classes=class_number(s)]
.sp
Segdisp reads a previously-saved cluster map from the disk
and displays portions of the map or the representative
spectra or both.  The default, if no keywords are specified,
is to display map and spectra for the clustering.
The map keyword specifies that only the map should be
displayed, while the spectra keyword displays only the
spectra.  The classes keyword is optional and
specifies which classes should be displayed; the default is
to use all classes found in the clustering.  For more
information on image clustering and segmentation, type
"help cluster".
.sp
.IP SNAPSHOT
.sp
snapshot file=filename [text=caption]
.sp
The snapshot command dumps a copy of the screen to rgb files
on the disk.  Files are simply a 512x512 dump of their
respective planes.  The file keyword specifies the name under
which to save the picture; components will have the
extensions
"r", "g", \"b\" in accordance with their
contents.
The text keyword may be used to caption the
snapshot; the caption should be quoted and no more than
30 characters long.
.sp
.IP STORESPEC
.sp
storespec [area=lines,samps] [loc=line,samp]
.sp
The storespec command stores the spectra within the specified
area(s) in a disk file, "spam.spe".
.sp
The first parameter, area, specifies the dimensions in lines
and samples of the area.  The second parameter, loc, may be
used instead of the cursor to select the region from which
spectra are to be taken;
the location is the line and sample of
the pixel in the lower-right corner of the area from which
spectra are to be taken.
.sp
.IP STRETCH
.sp
stretch [stretch=lower-val,upper-val]
.sp
The stretch command stretches the input image using display
look-up tables.  If the stretch keyword is used, the program
uses the values given.  Otherwise, it calculates the maximum
and minimum values and uses these for the stretch.
.sp
.IP WAVELENGTH
.sp
wavelength
.sp
The wavelength command prints out the wavelengths (and
band numbers) of selected
points along a plot.  Points are selected using the digi-pad
buttons.
.sp
.SH REMARKS
.IP 1) 3
Optional information is enclosed in brackets ([,]) which
should not actually be entered when using the program.
.IP 2) 3
Commands may be abbreviated to as few characters as are necessary for the
commands to be uniquely specified.
Plotnames may be shortened by using the ? wildcard; a wildcard embedded
anywhere in the plotname will match one or more characters.
.IP 3) 3
Strings can be quoted if necessary to avoid confusion with a keyword.
.IP 4) 3
Keywords can be omitted if they are unique in type; i.e., if a command has
only one string-valued keyword, the user does not need to use the keyword.
For a command such as scan which has two integer-valued keywords, it's
best to use the keywords.
The program will, however, assign ambiguous values to keywords based on
the ordering of the keywords for the command as given above.
.IP 5) 3
The keyword "prompt" is defined for all commands, and causes the program
to interactively prompt the user for all keyword values.
.IP 6) 3
Command lines may be continued using the backslash (\\) character.
The only restriction is that the backslash may not come in the middle of
a numeric constant, an unquoted character-constant, or a keyword.
