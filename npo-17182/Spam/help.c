/**********************************************************************
   help.c 2.15 - on-line Spam help
   Help provides on-line help for Spam commands.  There are three
   types of help available.  The first, the default, is invoked with
   "help" and prints a two-column list of the commands which are
   currently legal and make sense.  The second, called by "help all",
   gives two-column summary help for all commands.  The third type of
   of help is a specific command help.  "Help erase", for example,
   gives detailed information on the erase command and its options.
   (If there is no image in core, Spam gives basic help on program
   operation, labelling, etc.  After the image get, "help general"
   gives this information.)
**********************************************************************/
#include <stdio.h>
#include "spam.h"
help(buf)
unsigned char *buf;
{
    int none_of_the_above=1,all;
    char allkeys[10],keyword[80];
    static struct parameter kwds[] = {
	{"all",3,0,0,"summary of all allowable keywords","",10,0},
	{"keyword",2,0,1,"specific keyword","",80,0}
	};
    kwds[0].value = allkeys;
    kwds[1].value = keyword;
    if (par(buf,kwds,2)) return;
    start_pretty_io();
    if (substring(keyword,"cluster")) {
	space();
	lprint("> cluster [noise=thr1][cluster=thr2][manual][mask=ranges]");
	space();
	lprint("Cluster segments the current image into a number of");
	lprint("distinct regions");
	lprint("based on similarity of spectra.  The noise keyword specifies");
        lprint("the maximum numbers of fluctuations about the mean for");
        lprint("amplitude and slope; spectra falling outside these thresholds");
        lprint("will be regarded as noise.  The cluster keyword indicates in");
        lprint("how many bands two spectra can differ in amplitude and slope");
        lprint("and still be considered identical.  Spam will select");
        lprint("values based on the data if thresholds are defaulted.");
        lprint("The manual keyword allows a user to interactively cluster");
        lprint("an image, providing a little more flexibility.");
        lprint("The mask");
        lprint("keyword may be used to specify ranges of bands which should");
        lprint("not be taken into account in the clustering.");
        space();
        lprint("Image segmentation may be saved in a disk file and then");
        lprint("used later with the segdisp, merge, and identify commands.");
	space();
	none_of_the_above=0;
    }
    if (substring(keyword,"curvegen")) {
        space();
	lprint("> curvegen [name=curvename]");
	space();
	lprint("Curvegen allows the user to define a curve using the");
	lprint("digi-pad cursor.");
	lprint("It may only be used when the graph area is available");
	lprint("and at least one plot is up.");
	space();
	lprint("The name option allows the user to give the plots a name;");
        lprint("if no name is specified, the curve will be named for you.");
        space();
        none_of_the_above=0;
    }
    if (substring(keyword,"delete")) {
	space();
	lprint("> delete plot=plotname(s)");
	space();
	lprint("The delete command deletes the specified plot(s) from the");
	lprint("user's library.  Up to 16 plots may be deleted at a time.");
	space();
	none_of_the_above=0;
    }
    if (substring(keyword,"digiplot")) {
	space();
	lprint("> digiplot name=plotname [spline]");
	space();
	lprint("Digiplot allows a user to enter a printed spectral plot into");
	lprint("their user library.  It can be used with or without an image.");
	space();
	lprint("The \"name\" keyword specifies the name of the plot; it's");
	lprint("required.  The \"spline\" keyword indicates that digiplot");
	lprint("should perform a cubic spline fit to a set of discrete points");
	lprint("rather than the default method of asking the user to trace");
	lprint("over the entire curve using the cursor.  Smooth plots without");
	lprint("narrow features are probably best encoded using the spline");
	lprint("option; the cubic splines often yield a smoother plot.");
	space();
	none_of_the_above=0;
    }
    if (substring(keyword,"directory")) {
	space();
	lprint("> directory [userlib] [masterlib]");
	space();
	lprint("The directory command lists the contents of the specified");
	lprint("spectral library.  The default is to list the contents of");
	lprint("both the user's local library and the system master");
	lprint("library.");
	space();
	none_of_the_above=0;
    }
    if (substring(keyword,"display")) {
        space();
        lprint("> display [bands=start [,end]]");
        space();
        lprint("Display allows the user to look at specific bands of the");
        lprint("image; bands are selected through the bands keyword.");
	space();
        lprint("The bands keyword takes one or two band numbers as");
        lprint("arguments.  If only one band is specified, the band will");
        lprint("be drawn in the normal band area to the left of the plots.");
        lprint("(The old plot marks will be retained.)  If two bands are");
        lprint("specified, then the normal display will disappear and");
        lprint("the indicated range of bands will be displayed.");
        lprint("Typing the command \"return\" will restore the screen to");
        lprint("its previous state.  The default, if no bands are specified,");
        lprint("is to display as much of the image as possible,");
        lprint("starting with the first band.");
        space();
        none_of_the_above=0;
    }
    if (substring(keyword,"erase")) {
        space();
        lprint("> erase [clus][curv][data][fea][func][hist][lib][plot=names]");
        space();
        lprint("The erase command allows the user to selectively erase");
        lprint("parts of the screen.");
        lprint("If the command is specified without");
        lprint("additional keywords, all plots and the histogram or feature");
	lprint("extraction strips, if any,");
        lprint("are removed.  Curv and data remove curvegen plots and image");
        lprint("plots, respectively.  Func, lib, and clus");
	lprint("erase plots generated by the function, libplot, and cluster");
	lprint("commands, respectively.  Using the plot");
        lprint("parameter directs the program to erase only the plot(s)");
        lprint("specified.  Finally, the hist keyword removes the histogram");
        lprint("in display or graphics mode, and fea clears feature");
	lprint("extraction information.");
	space();
        lprint("Note that when image plots are removed, the marker on");
        lprint("the image itself remains, but changes color to black.  This");
        lprint("can be used to remind the user of places which have already");
        lprint("been looked at.");
        space();
        none_of_the_above=0;
    }
    if (substring(keyword,"feature")) {
	space();
	lprint("> feature [disp|find][spec=names][peaks=ranges|thr=%][ais|av]");
	space();
	lprint("The feature command uses a feature extraction algorithm to");
	lprint("allow analysis of spectra in the library and, to a certain");
	lprint("extent, in an image.  The feature command may");
	lprint("be used with or without an image; the library is resampled");
	lprint("to correspond to the image if one is available, and the");
	lprint("standard AIS or AVIRIS wavelength range otherwise.");
	space();
	lprint("There are two basic modes: display and find.  In display mode");
	lprint("Spam either displays features of library spectra");
	lprint("given by the spec");
	lprint("keyword, or if the peakloc keyword is used, searches the");
	lprint("libraries and displays the features");
	lprint("of all spectra which have peaks within each of up to ten");
	lprint("wavelength ranges;");
	lprint("in either case, features are identified using multi-colored");
	lprint("strips below the graph area.");
	lprint("In find mode, Spam takes a single spectrum");
	lprint("as given by the spec keyword and displays strips for those");
	lprint("library spectra which are similar as determined by the thr");
	lprint("percentage threshold.  (Two spectra are compared as follows:");
	lprint("First they are feature-normalized so that the total area");
	lprint("of the absorption peaks is 255; featureless ranges are");
	lprint("zeroed.  Then the difference is defined as the sum of the");
	lprint("absolute values of the dn value differences at each");
	lprint("wavelength and adjusted so that differences range between");
	lprint("0 and 100 percent.");
	space();
	lprint("The ais and aviris keywords are used to specify");
	lprint("a resampling range");
	lprint("if feature is being used without an image.  (After the first");
	lprint("get, feature uses the wavelength range represented by the");
	lprint("image.)  Once resampling has been specified, the keywords may");
	lprint("be omitted until another resampling is desired.");
	space();
	none_of_the_above = 0;
    }
    if (substring(keyword,"filter")) {
        space();
        lprint("> filter window=width [triangle]");
        space();
        lprint("The filter command performs a box filtering of the");
        lprint("image in the spectral direction using the specified window");
        lprint("width; bands currently displayed are replaced.");
	lprint("If the triangle keyword is used, filter will use");
	lprint("weights in the computations.");
        space();
        none_of_the_above=0;
    }
    if (substring(keyword,"find")) {
        space();
	lprint("> find plot=name thresh=maxerr(s) [hist|lib] [mask=ranges]");
        space();
	lprint("The find command attempts to match a specified plot against");
	lprint("similar plots in the image or the spectral libraries.");
	space();
	lprint("The plot parameter is required and specifies the name");
	lprint("of a plot currently displayed which should be matched.");
	space();
	lprint("The thresh keyword specifies the maximum allowable");
        lprint("deviation between spectra in order for");
	lprint("them to be considered identical.");
	lprint("If only one value is given, find will compare");
	lprint("spectral amplitude only; if two values are");
	lprint("used, find will compare slope similarity also.");
	lprint("(The deviation is defined as the number of bands");
        lprint("where the binary amplitude or slope values of two spectra");
	lprint("differ.)");
        space();
	lprint("In the normal, image-search mode, find will color in those");
	lprint("pixels in the image which match the specified plot using the");
	lprint("plot's color.");
	lprint("The lib keyword indicates that find should search the");
	lprint("spectral libraries rather than the image");
	lprint("and list similar spectra with their amplitude and slope");
	lprint("deviations.");
	space();
	lprint("The hist keyword displays a histogram of");
	lprint("the deviations between the reference plot and matching");
	lprint("spectra in the image.  This is for image finds only.");
	space();
        lprint("The mask keyword may be used to specify");
        lprint("bands which should not be taken into account in the find,");
        lprint("where a range");
        lprint("is a pair of bands separated by a space, comma, or hyphen.");
        lprint("Ranges may be separated by commas or spaces.");
        lprint("The default is to use all bands in the image.");
        space();
        none_of_the_above=0;
    }
    if (substring(keyword,"function")) {
        space();
	lprint("> func function=\"plotname|image=mathematical-expr\"");
	space();
	lprint("The func command allows the user to plot or display the");
        lprint("results of functions involving plots and the image.");
	space();
	nprint("Example:  func \"funcplot=abs(kao-alun)/cal*160\"");
        nprint("                           or");
        nprint("          func \"image=image*128/flatspec\"");
	space();
	lprint("The standard precedence rules apply: multiplication and");
	lprint("division are done first, followed by addition and");
	lprint("subtraction, moving left-to-right.  Negation is allowed,");
	lprint("but negated quantities must have parentheses if on the");
	lprint("right-hand side of an operator.  The wildcard character, ?,");
	lprint("may be used in plotnames, as usual.  Constants may be real or");
	lprint("integer, but if no real constants are used, the calculations");
        lprint("will be done using integer arithmetic; this is significantly");
        lprint("faster than using floating point.");
        lprint("If values go outside the range 0 to");
	lprint("255 inclusive, they will be set to the nearest cutoff");
	lprint("value (i.e., 0 or 255).");
        lprint("Finally, the functions \"abs\"");
        lprint("and \"sum\" return the absolute value of a spectrum and the");
        lprint("sum of the spectral reflectance values (duplicated to make");
        lprint("a vector), respectively.");
        space();
        none_of_the_above=0;
    }
    if (substring(keyword,"get")) {
        space();
        lprint("> get file=imgname [sl=ln1] [nl=#lns] [sb=band1] [nb=#bands]");
        space();
        lprint("The get command directs the program to fetch an image from");
        lprint("the disk.  The only required argument is the filename;");
        lprint("if a filename is not specified, Spam will prompt");
        lprint("for it.  The remaining keywords sl, nl, sb, and nb may");
        lprint("be used individually or collectively to select subparts of");
        lprint("the image.  The default for starting line (sl) and band (sb)");
	lprint("is 1; number of lines (nl) and number of bands (nb) default");
        lprint("to the maximum for the image as given in the label.");
	space();
        lprint("All files must have a label generated either through the");
        lprint("makelabel program or through TAE's LABEL-ADD specifying");
        lprint("the number of lines (NL), number of samples (NS),");
        lprint("starting wavelength (SW), ending wavelength (EW),");
        lprint("bandwidth (BW), and data format (FORMAT).");
        space();
        lprint("Input images");
        lprint("may have up to 512 bands, each of which may be up to 256");
        lprint("pixels wide.");
        space();
        none_of_the_above=0;
    }
    if (substring(keyword,"hardcopy")) {
        space();
	lprint("> hardcopy [plot=plotname(s)]");
        space();
	lprint("The hardcopy command writes currently displayed plots and");
        lprint("histograms out");
	lprint("to the printer.  Up to four plots may be printed at a time.");
	lprint("If no plots are specified, hardcopy will print out the first");
	lprint("four plots, if possible.  Hardcopy uses the VT125 emulator");
	lprint("contained in VT240 and other terminals, and a");
	lprint("directly-connected LA50-compatible printer.");
        space();
        none_of_the_above=0;
    }
    if (substring(keyword,"help")) {
        space();
        lprint("> help [command-name | general | all]");
        space();
        lprint("The help command allows the user to get help on program");
        lprint("operation and details of specific commands.");
	space();
        lprint("If a particular command is specified, help will give");
        lprint("details and hints about that command.  If the keyword \"all\"");
        lprint("is specified, help will give a summary description of all");
        lprint("Spam commands.  The default help is context-dependent, that");
        lprint("is, Spam will summarize the commands which are currently");
        lprint("legal and make sense.  Finally, \"help general\", the default");
        lprint("before the first image get, gives general notes on program");
        lprint("operation including image labeling.");
        space();
        none_of_the_above=0;
    }
    if (substring(keyword,"histogram")) {
        space();
	lprint("> hist [area=lines,samps] [Hammingdist [mask=ranges]]");
	space();
	lprint("The hist command does two types of histograms.");
	space();
	lprint("The first of these is a histogram of dn values.");
	lprint("To do a histogram of dn values within a subarea of a band,");
	lprint("use the area keyword and indicate the area with the");
	lprint("box-cursor; this can be done in display mode as well as the");
	lprint("normal graphics mode.  If no area is specified, the program");
	lprint("will histogram the current strip in graphics mode, or the");
	lprint("entire image in display mode.");
	space();
	lprint("The second variety of histogram is a graph of Hamming");
	lprint("distances between each pair of spectra within a user-given");
	lprint("area; if no area (box-cursor size) is given, the program");
	lprint("will default to a 5 line by 5 sample area.  Once the user");
	lprint("has selected the area, the program calculates the Hamming");
	lprint("distance between each pair of spectra represented by the");
	lprint("area and graphs these.  (This, too, can be done in either");
	lprint("mode.)");
        lprint("The mask keyword may be used with Hamming histograms to");
        lprint("specify ranges of bands which should not be taken into");
        lprint("account in the hist, where a range");
        lprint("is a pair of bands separated by a space, comma, or hyphen.");
        lprint("Ranges may be separated by commas or spaces.");
        lprint("The default is to use all bands in the image.");
        space();
        none_of_the_above=0;
    }
    if (substring(keyword,"identify")) {
        space();
        lprint("> identify [thresh=amp_err,slope_err] [mask=ranges]");
        space();
        lprint("The identify command reads a previously-saved cluster map and");
        lprint("attempts to identify the spectral classes found.  The");
        lprint("threshold keyword specifies amplitude and slope error");
        lprint("thresholds to use in comparing representative spectra with");
        lprint("library entries.  If defaulted, Spam will select thresholds");
        lprint("automatically.");
        lprint("The mask");
        lprint("keyword may be used to specify ranges of bands which should");
        lprint("not be taken into account in the identification.");
        lprint("For more information on clustering, type");
        lprint("\"help cluster\".");
        space();
        none_of_the_above=0;
    }
    if (substring(keyword,"keep")) {
        space();
        lprint("> keep");
        space();
        lprint("The keep command saves the current Spam session so that");
	lprint("it can be continued later with the restore command.");
	lprint("Note that this command writes two or three files out to");	
	lprint("the current directory which are then deleted automatically");
	lprint("by restore.  These are spam.ses, spam.lst, and possibly");
	lprint("spam.img, depending on what you have done so far.  Keep");
	lprint("will not save multi-band displays, mixture maps or feature");
	lprint("extraction information.");
        space();
        none_of_the_above=0;
    }
    if (substring(keyword,"libplot")) {
        space();
        lprint("> libplot dataset=libplot-name(s)");
        space();
        lprint("The libplot command displays the specified");
        lprint("library spectra along either the wavelength");
        lprint("range represented by the image or, if no gets have");
	lprint("been done, the feature extraction");
	lprint("resampling range currently being used.");
	space();
        lprint("The ? wildcard may be used anywhere within a dataset name");
	lprint("to indicate that some characters in the name have been");
	lprint("omitted.");
	space();
        lprint("Library data comes from one of two places, the system master");
	lprint("library, which contains a basic set of laboratory spectra,");
	lprint("or the user's own library, which may be created and filled");
	lprint("with the saveplot command.  (Type \"help saveplot\" for more");
	lprint("information on the latter feature).");
	space();
        none_of_the_above=0;
    }
    if (substring(keyword,"merge")) {
        space();
        lprint("> merge [classes=from-class-#, to-class-#, ...]");
        space();
        lprint("Merge reads in a previously-saved cluster map from the disk");
        lprint("and merges the specified spectral classes, which may be");
        lprint("entered by number on the command line using");
        lprint("the classes keyword or");
        lprint("interactively (the default).  The resulting");
        lprint("clustering may be saved, if desired.  For more information");
        lprint("on clustering, type \"help cluster\".");
        space();
        none_of_the_above=0;
    }
    if (substring(keyword,"mixture")) {
        space();
 	lprint("> mixture mode=area|plot option=method ref=names");
 	space();
 	lprint("The mixture command is used to break a plot or portion of");
 	lprint("the image down into its mixture components.  There are two");
	lprint("modes: the area mode which");
	lprint("analyzes a portion of an image, then");
	lprint("generates a set of mixture maps, one map per reference");
	lprint("mineral, and the plot mode which analyzes a plot");
	lprint("and then generates a mixture plot and mixture proportions");
	lprint("for each component mineral.");
	space();
	lprint("The second parameter");
        lprint("specifies a method to be used for the mixture analysis.");
 	lprint("Possible options in area mode are");
        lprint("\"amp\" and \"slope\" for");
        lprint("amplitude and slope encoding,");
        lprint("\"binary\" for binary encoding (amplitude and slope),");
	lprint("\"euclidean\" for euclidean distance, ");
        lprint("and");
        lprint("\"least\" for least-squares fit");
	lprint("The only allowable option in plot mode is");
	lprint("\"least\".  The");
 	lprint("execution time and accuracy of the result increase in the");
	lprint("order of the options as given above.");
	space();
	lprint("Finally, the ref keyword specifies up to eight");
	lprint("currently-displayed reference spectra.  All");
	lprint("mixtures will be in terms of these spectra,");
	lprint("and the time the analysis takes is proportional");
	lprint("to the number of reference spectra specified.");
        space();
	lprint("Spam will prompt for the areas or names of plots to be");
	lprint("analyzed; when using");
	lprint("plot mode, plots to be analyzed must be displayed");
	lprint("ahead of time.");
	space();
        none_of_the_above=0;
    }
    if (substring(keyword,"normalize")) {
        space();
        lprint("> normalize [amplitude | area]");
        space();
        lprint("The normalize command is used to change the normalization");
        lprint("of plots.  There are two normalization modes: the");
        lprint("amplitude-normalization mode, where all plots are ");
        lprint("displayed normally and the screen is scaled to allow");
        lprint("5 DN space above the highest DN value, and 5 DN below the");
        lprint("lowest, and the area-normalization mode, where data is");
        lprint("amplified in such a way that all of the displayed curves");
        lprint("have the same area.  The default is to renormalize using");
        lprint("the current mode.");
	space();
        lprint("Changing normalization mid-stream causes all current");
        lprint("plots to be redrawn using the new normalization.");
        space();
        none_of_the_above=0;
    }
    if (substring(keyword,"photograph")) {
	space();
	lprint("> photo [text=caption]");
	space();
	lprint("The photo command sends the current screen to QCR (Quick");
        lprint("Color Recorder) camera for a quick Polaroid photo.");
	lprint("This may be used instead of the snapshot command for");
	lprint("quicker photo processing.");
	lprint("The text keyword specifies a caption for the photo.");
        lprint("The caption should be quoted and no more than");
	lprint("30 characters long.");
	space();
	none_of_the_above=0;
    }
    if (substring(keyword,"plot")) {
        space();
        lprint("> plot [name=pnam] [ave=lns,smps [var]] [loc=ln,smp] [stats]");
        space();
        lprint("The plot command plots out the DN values for each image");
        lprint("band at given points.");
	space();
        lprint("The first parameter, name, specifies a name for the");
        lprint("(first) area or point to be plotted; this must have no more");
        lprint("than 16 characters.  If the name is defaulted, Spam will name");
	lprint("the plot for you.");
	space();
        lprint("The second, ave, indicates that plots should be of");
        lprint("the averages of areas within each band,");
        lprint("rather than of single pixels.  A box cursor is provided for");
        lprint("easy selection of the averaging areas.  If the aver keyword");
	lprint("is omitted, Spam will average over the current box-cursor");
	lprint("area.  The var keyword, used with averaging, displays scatter");
        lprint("bars with the plot showing the variance of the spectra within");
        lprint("the averaging area.");
	space();
	lprint("The loc keyword allows the user to give the plot location");
	lprint("on the command line, rather than by using the cursor.");
	lprint("For area plots, the location is the line and sample of");
	lprint("the pixel in the lower-right corner of the averaging");
	lprint("area.");
	space();
        lprint("If the stats keyword is used, Spam will print out the mean");
	lprint("and standard deviation of the plot values.");
        space();
        none_of_the_above=0;
    }
    if (substring(keyword,"quit")) {
        space();
        lprint("> quit");
        space();
        lprint("The quit command exits the program.");
        space();
        none_of_the_above=0;
    }
    if (substring(keyword,"ratio")) {
        space();
        lprint("> ratio [bands=first,second]");
        space();
        lprint("Ratio ratios two user-selected bands and displays the");
        lprint("result as an image band.  If no keyword is specified,");
        lprint("Spam will prompt the user for two cursor positions within");
        lprint("the graph area, and the corresponding bands will be used");
        lprint("for the ratio.  The bands may also be specified using the");
        lprint("bands keyword, numerator band first.  Ratio may only be");
        lprint("used in graphics mode.");
        space();
        none_of_the_above=0;
    }
    if (substring(keyword,"restore")) {
	space();
	lprint("> restore");
	space();
	lprint("The restore command restores the last Spam session");
	lprint("so that you can continue from where you left off.");
	lprint("Note that restore deletes temporary files created");
	lprint("by the keep command automatically, so a previous session");
	lprint("may only be restored once.");
	space();
	none_of_the_above=0;
    }
    if (substring(keyword,"return")) {
        space();
	lprint("> return");
	space();
	lprint("The return command returns the display to graphics mode after");
        lprint("a multi-band display (disp) or mixture analysis (mixture).");
        space();
        none_of_the_above=0;
    }
    if (substring(keyword,"saveplot")) {
        space();
        lprint("> saveplot plot=name [as name2]...");
        space();
        lprint("The saveplot command saves one or more current plots in the");
	lprint("user's own library so that they may be recalled later with");
	lprint("the libplot command.  It will create a library in the user's");
	lprint("area, if necessary.");
	space();
	lprint("The \"as\" keyword allows the user to save the plot under a");
	lprint("different name.  If \"as\" is used, it must be spelled out;");
	lprint("it may not be abbreviated.");
	space();
	lprint("You may specify up to 16 plots, any or all of which may be");
	lprint("renamed.  For example, \"saveplot ais1 ais2\" will save those");
	lprint("two plots in your user library.  \"saveplot ais1 as kao\"");
	lprint("will save ais1 in your library, but rename it \"kao\".");
	lprint("\"saveplot ais1 as kao, ais2, ais3 as alun\" will save ais1");
	lprint("as \"kao\", ais2 as itself, and ais3 as \"alun\".");
	lprint("(The commas are optional.)");
	space();
        none_of_the_above=0;
    }
    if (substring(keyword,"scan")) {
        space();
        lprint("> scan [bands=start,end] [speed=time(ms)] [number]");
        space();
        lprint("The scan command displays the bands of the current image in");
        lprint("rapid succession, showing quickly the areas of variability.");
	space();
        lprint("There are three parameters for this command, all of them");
        lprint("optional.  The first, bands, may be used to specify a");
        lprint("starting and ending band for the scan.");
        lprint("The default if");
        lprint("no range of bands is specified is to scan starting with the");
        lprint("first band and continuing through band 3mn, where m is 512");
        lprint("divided by the band length in lines and n is");
        lprint("512 divided by the band width in pixels.");
        lprint("The second parameter, speed,");
        lprint("specifies the time interval between bands.");
        lprint("The default for the speed option is a time of about 250 ms");
        lprint("or 4 frames per second.  The maximum scan speed is somewhat");
        lprint("implementation-dependent, but should be about 30 frames");
        lprint("per second; setting the speed parameter to 0 will");
        lprint("optimize the scan rate.");
        lprint("The third, number, causes scan to display the band number");
        lprint("of each band in the upper-left corner of the image area.");
        lprint("Normally scan does not display band numbers.");
        space();
        none_of_the_above=0;
    }
    if (substring(keyword,"segdisp")) {
        space();
        lprint("> segdisp [map] [spectra] [classes=class_number(s)]");
        space();
        lprint("Segdisp reads a previously-saved cluster map from the disk");
        lprint("and displays portions of the map or the representative");
        lprint("spectra or both.  The default, if no keywords are specified,");
        lprint("is to display map and spectra for the clustering.");
        lprint("The map keyword specifies that only the map should be");
        lprint("displayed, while the spectra keyword displays only the");
        lprint("spectra.  The classes keyword is optional and");
        lprint("specifies which classes should be displayed; the default is");
        lprint("to use all classes found in the clustering.  For more");
        lprint("information on image clustering and segmentation, type");
        lprint("\"help cluster\".");
        space();
        none_of_the_above=0;
    }
    if (substring(keyword,"snapshot")) {
        space();
        lprint("> snapshot file=filename [text=caption]");
        space();
	lprint("The snapshot command dumps a copy of the screen to rgb files");
	lprint("on the disk.  Files are simply a 512x512 dump of their");
	lprint("respective planes.  The file keyword specifies the name under");
	lprint("which to save the picture; components will have the");
	lprint("extensions");
	lprint("\"r\", \"g\", \"b\" in accordance with their");
	lprint("contents.");
	lprint("The text keyword may be used to caption the");
        lprint("snapshot; the caption should be quoted and no more than");
	lprint("30 characters long.");
	space();
        none_of_the_above=0;
    }
    if (substring(keyword,"storespec")) {
        space();
        lprint("> storespec [area=lines,samps] [loc=line,samp]");
        space();
        lprint("The storespec command stores the spectra within the specified");
	lprint("area(s) in a disk file, \"spam.spe\".");
	space();
        lprint("The first parameter, area, specifies the dimensions in lines");
	lprint("and samples of the area.  The second parameter, loc, may be");
	lprint("used instead of the cursor to select the region from which");
	lprint("spectra are to be taken;");
	lprint("the location is the line and sample of");
	lprint("the pixel in the lower-right corner of the area from which");
	lprint("spectra are to be taken.");
	space();
        none_of_the_above=0;
    }
    if (substring(keyword,"stretch")) {
        space();
	lprint("> stretch [stretch=lower-val,upper-val]");
	space();
	lprint("The stretch command stretches the input image using display");
	lprint("look-up tables.  If the stretch keyword is used, the program");
	lprint("uses the values given.  Otherwise, it calculates the maximum");
	lprint("and minimum values and uses these for the stretch.");
        space();
        none_of_the_above=0;
    }
    if (substring(keyword,"wavelength")) {
        space();
	lprint("> wavelength");
	space();
	lprint("The wavelength command prints out the wavelengths (and");
	lprint("band numbers) of selected");
	lprint("points along a plot.  Points are selected using the digi-pad");
	lprint("buttons.");
        space();
        none_of_the_above=0;
    }
/*
/*      help gives general help for "help general" or "help" with no image.
*/
    if (substring(keyword,"general") || (none_of_the_above && img_in_core==0 &&
            feature_range==0 && allkeys[0]=='\0')) {
        lprint("Welcome to SPAM!  Here are general notes about program");
        lprint("operation and some suggested initial commands.");
        lprint("Once you've gotten an image or used the");
	lprint("feature command, \"help\" will give a listing");
        lprint("of suggested commands; to see these notes again after");
        lprint("that point, use \"help general\".");
        space();
	nprint("IMPORTANT: image label");
	nprint("**********************");
#ifdef taevic
	nprint("Spam assumes that all input image data sets have the");
	nprint("following VICAR label items.");
	nprint("	   NL - number of lines");
	nprint("	   NS - number of samples");
	nprint("	   SW - starting wavelength");
	nprint("	   EW - ending wavelength");
	nprint("	   BW - band width (number of samples in a channel)");
	nprint("SW, EW, and BW need to be added to the image label after");
	nprint("the usual label generation has been done.  Use TAE/VICAR");
	nprint("program LABEL to append these three items as:");
	nprint("TAE> LABEL-ADD INP OUT \"SW=xx.x EW=xx.x BW=xx.x\"");
        space();
#else
        lprint("All files must have a label specifying");
        lprint("the number of lines (NL), number of samples (NS),");
        lprint("starting wavelength (SW), ending wavelength (EW),");
        lprint("bandwidth (BW), and data format (FORMAT).");
        lprint("This label can be generated by the makelabel");
        lprint("program on systems without TAE.");
        space();
#endif
        nprint("Typing Hints");
        nprint("************");
        lprint("1) Commands and keywords may be abbreviated.  One or more");
	lprint("characters in a plotname may be abbreviated with the");
	lprint("wildcard character, ?.  So \"lib\" may be used instead");
        lprint("of \"libplot\" and \"kao?_1\" instead of \"kaolinite_1\".");
        space();
        lprint("2) Values may be specified without keywords in most cases,");
        lprint("but character values may need to be quoted.  Keywords are");
        lprint("only needed when a command takes several parameters of the");
        lprint("same type and so type doesn't imply keyword.  Even in this");
        lprint("latter case keywords may usually be omitted if values are");
        lprint("given in the order of the keywords as shown in help.  If");
        lprint("a string value happens to correspond to a keyword for a");
        lprint("given command, put double quotes around the string.");
        space();
        lprint("3) Commands may be continued onto the next line by ending");
        lprint("the first line with a backslash.  Backslashes may go anywhere");
        lprint("except in the middle of a constant or keyword.");
        space();
        nprint("Suggested initial commands");
        nprint("**************************");
        nprint("get         Get an image from the image library.");
	nprint("feature     Analyze library spectra.  See help for more info.");
        nprint("directory   List the contents of the spectral libraries.");
        nprint("quit        Exit the program.");
        space();
        lprint("For more information on the above commands, or any other");
        lprint("Spam commands, use \"help cmd\" where cmd is the name of the");
        lprint("specific command.");
        space();
        none_of_the_above=0;
    }
    if (none_of_the_above) {
/*
/*      If no specific command is given or command is illegal, print summary
/*      line for every command which is currently legal and makes sense.
*/
        if (allkeys[0]=='\0') all=0;
        else all=1;
        space();
        if (all) lprint("Spam command summary:");
        else lprint("You may enter any of the following commands:");
        space();
  	if ((display_mode==0 && img_in_core==1) || all)
	    cprint("cluster [noise=thr1][cluster=thr2] *");
        if ((display_mode==0 && plots_num!=0 && plots_norm_mode==1)||all)
            cprint("curvegen [name=curvename]");
	cprint("delete plot=plotname(s)");
        if ((display_mode==0)||all)
	    cprint("digiplot name=plotname [spline]");
	cprint("directory [userlib][masterlib]");
        if ((img_in_core && display_mode==0)||all)
	    cprint("display [bands=start [,end]]");
	if (hist_on || (display_mode==0 && plots_num!=0) || all)
	    cprint("erase [data|hist|lib][plot=names] *");
	if (display_mode==0 || all) 
	    cprint("feature [disp|find] [ais|av] *");
        if (img_in_core||all)
	    cprint("filter window=width [triangle]");
        if ((display_mode==0 && plots_num!=0)||all)
	    cprint("find plot=name thr=maxerr(s) [lib] *");
        if (((img_in_core || plots_num!=0) && display_mode==0 && plots_norm_mode
	    ==1) || all) cprint("func func=\"name|image=math-expr\"");
	cprint("get file=imgname *");
        if ((VT240LA50 && ((display_mode==0 && plots_num!=0)||hist_on))||all)
	    cprint("hardcopy [plot=plotname(s)]");
        cprint("help [command-name | general | all]");
	if (img_in_core||all) 
	    cprint("hist [area=lines,samps] [Hamming] *");
        if ((display_mode==0 && img_in_core)||all) 
            cprint("identify [thresh=amp,slope] *");
        if (img_in_core||all) cprint("keep");
        if ((display_mode==0 && img_in_core)||all)
	    cprint("libplot dataset=specname(s)");
        if ((display_mode==0 && img_in_core)||all)
            cprint("merge [classes=from-#,to-#,...]");
 	if (img_in_core || all)
            cprint("mixture mode=area|plot ref=names *");
        if ((plots_num!=0 && display_mode==0)||all)
	    cprint("normalize [amplitude | area]");
 	if ((QCR && img_in_core)||all) cprint("photo [text=caption]");
        if ((display_mode==0 && img_in_core)||all)
            cprint("plot [name=pname][aver=lines,samps] *");
        cprint("quit");
        if ((display_mode==0 && img_in_core)||all)
            cprint("ratio [bands=first,second]");
        cprint("restore");
        if (display_mode==1||all) cprint("return");
        if ((display_mode==0 && plots_num!=0)||all)
            cprint("saveplot plot=name [as name2]...");
        if ((display_mode==0 && img_in_core)||all)
            cprint("scan [bands=start,end] [number] *");
        if ((display_mode==0 && img_in_core)||all)
            cprint("segdisp [map][spectra][classes=#s]");
        if (img_in_core||all)
	    cprint("snapshot file=filename [text=caption]");
        if ((img_in_core && display_mode==0) || all)
	    cprint("storespec [area=lines,samps] *");
        if (img_in_core||all)
	    cprint("stretch [stretch=lower-val,upper-val]");
        if ((plots_num!=0 && display_mode==0)||all)
	    cprint("wavelength");
	space();
        stop_pretty_io();
        nprint("* Command takes additional options.");
        space();
    }
    else stop_pretty_io();
}
