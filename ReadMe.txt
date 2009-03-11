Directory Structure
===================
One directory corresponds to a single stock, which in turn has the following directory structure
./Stock/cfg 	Assessment configuration options
./Stock/data 	Input data (in Lowestoft VPA format) used by the stock assessments
./Stock/res		Output data (graphs, tables) produced by the stock assessment. Not stored in repository

In addition, the "_Common" folder contains all of the shared modules, including the appropriate R packages
./_Common 		Source folder for common code module and associated scripts
./_Common/Pkgs	Source folder for appropriate R packages
The script "UpdatePackages.r" in the ./_Common/Pkgs folder can be used to bring all of the required packages up
to date systematically.


Useage concept:
==============
The file "./_Common/HAWG Common assessment module.r" provides a common and standardised set of code
for generating the outputs for a "standard" HAWG stock assessment. To take advantage of this code, 
the user should first source the module into their code eg
source(file.path("..","_Common","HAWG Common assessment module.r"))

This script has two roles: it checks that the user has the correct version of the
required packages installed, and it then provides the user with access to the common
functions, detailed below. The user can then proceed with their stock assessment in
the normal manner, and, when appropriate, call these functions to produce the 
standard set of graphs of tables. The user is then free to continue after this point, 
and perform extra analysises, including short term forecasts and the like. 

For an example of this in action, see "WBSS Assessment.r"

Working Directory:
=================
All code is designed to be run with the ./Stock/ directory as the working directory. Changes from this
will cause code to break - I wish there was a nice way around this, but there isn't really, unfortunately.
All paths should be setup relative to this directory.

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

catch.coh(stck)
	Plots the proportion of a cohort in the catch as a stacked line plot
	Argument	Class			Description
	stck		FLStock			Contains the information about the stock eg m, catch numbers etc

stacked.age.plot(stck,"slot")
	Plots the proportion of any slot with age-groups of a stock as a stacked line plot
	Argument	Class			Description
	stck		FLStock			Contains the information about the stock eg m, catch numbers etc
	"slot"		slot			refers to the exact slot you want to use of the stck object

stacked.age.plot(idxs,"index")
	Plots the propotion of the age-groups in the tuning index as a stacked line plot
	Argument	Class			Description	
	idxs		FLIndices		Contains the tuning indices
	"index"		slot			refers to the exact slot you want to use of the idxs object

#mat.inmat.ration(stck)
	Plots the ratio between mature and inmature biomass of a stock
	Argument	Class			Description
	stck		FLStock			Contains the information about the stock eg m, catch numbers etc

#cpue.survey(idxs,"index")
	Plots the indices per yearclass versus each other
	Argument	Class			Description	
	idxs		FLIndices		Contains the tuning indices
	"index"		slot			refers to the exact slot you want to use of the idxs object

#wt.at.age(stck,start,end)
	Plots the weight at age in the catch connecting similar ages over the years
	Argument	Class			Description
	stck		FLStock			Contains the information about the stock eg m, catch numbers etc
	start		integer			Year of start of timeseries
	end		integer			Year of end of timeseries

catch.curves(stck,start,end)
	Plots the log catch ratios in 3 different ways
	Argument	Class			Description
	stck		FLStock			Contains the information about the stock eg m, catch numbers etc
	start		integer			Year of start of timeseries
	end		integer			Year of end of timeseries
	
ref.pts(stck,model,factor)
	Plots the yield-recruit curves including reference points, and fits a SR-model
	Argument	Class			Description
	stck		FLStock			Contains the information about the stock eg m, catch numbers etc
	model		character		SR-model you want to use
	factor		integer			transformation value to reduce size of stock numbers to be able to fit SR

cor.tun(idxs)
	Plots the correlation within indices
	Argument	Class			Description	
	idxs		FLIndices		Contains the tuning indices

	
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
