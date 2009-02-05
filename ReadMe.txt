Directory Structure
===================
One directory corresponds to a single stock, which in turn has the following directory structure
./Stock/cfg 	Assessment configuration options
./Stock/data 	Input data (in Lowestoft VPA format) used by the stock assessments
./Stock/res		Output data (graphs, tables) produced by the stock assessment. Not stored in repository
./Stock/run		R scripts for each specific stock to run the assessment

In addition, the "_Common" folder contains all of the shared modules, including the appropriate R packages
./_Common 		Source folder for common code module and associated scripts
./_Common/Pkgs	Source folder for appropriate R scripts

Useage concept:
==============
The file "./_Common/HAWG Common assessment module.r" provides a common and standardised set of code
for generating the outputs for a "standard" HAWG stock assessment. To take advantage of this code, 
the user should first source the module into their code eg
source(file.path("..","..","_Common","HAWG Common assessment module.r"))

This script has two roles: it checks that the user has the correct version of the
required packages installed, and it then provides the user with access to the common
functions, detailed below. The user can then proceed with their stock assessment in
the normal manner, and, when appropriate, call these functions to produce the 
standard set of graphs of tables. The user is then free to continue after this point, 
and perform extra analysises, including short term forecasts and the like. 

For an example of this in action, see "WBSS Assessment.r"

Functions
=========
do.summary.plots(stck,ica.obj)
	Generates the standard set of summary plots and diagnostic figures
	Argument	Class			Description
	stck		FLStock			Contains the information about the stock eg m, catch numbers etc
	ica.obj		FLICA			Output of an FLICA stock assessment

do.retrospective.plots(stck,idxs,ctrl,n.retro.yrs) 
	Performs a retrospective analysis and plots the results, showing retrospective results of
	SSB, Fbar and Recruits, the perception of individual cohorts, and the retrospective 
	perception of individual age groups
	Argument	Class			Description
	stck		FLStock			Contains the information about the stock eg m, catch numbers etc
	idxs		FLIndices		Contains the tuning indices
	ctrl		FLICA.control	Contains the FLICA assessment setting parameters
	n.retro.yrs	integer			Number of years for which to perform the retrospective analysis

do.SRR.plot(stck) 
	Plots a basic stock-recrutiment relationship by joing the dots for sequential years, and labelling
	each point with the corresponding year
	Argument	Class			Description
	stck		FLStock			Result of an assessment, containing stock.n etc
	
Output Figures
==============
The common assessment module does not specify the type of output format - this is left 
up to the user to decide. If no device is configured, all of the graphs will be displayed
in the graphics window. If the user wishes to save the graphs, eg to windows metafiles, png or PDF, they
should configure the output device before sourcing the common module, and close it afterwards. Note that
use of the stockassessment.org website requires the generation of png files eg

png("figures - %02d.png",units = "px", pointsize = 10, bg = "white")
<Plotting scripts>
dev.off()
