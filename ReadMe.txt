Directory Structure
===================
./Output  		Output data (graphs, tables) produced by the stock assessment. Not stored in repository
./pkgs    		FLR packages used
./R       		R scripts
./Source Data 	Input data (in Lowestoft VPA format) used by the stock assessments


Useage concept:
==============
"HAWG Common assessment module.r" provides a common and standardised set of code
for doing a "standard" HAWG stock assessment. The user should first load in their
data and setup the following objects for their specific stock:

Name	Class
stck	FLStock
idxs	FLIndices
ctrl	FLICA.ctrl

"HAWG Common assessment module.r" should then be sourced - this will perform the 
assessment, and produce all the necessary graphs and output tables. The user is then
free to continue after this point, and perform extra analysises, including short
term forecasts and the like. 

For an example of this in action, see "WBSS Assessment.r"

Variables:
==========
"HAWG Common assessment module.r requires the following system variables to be defined
before it is sourced:

Variable Name		Purpose
n.retro.yrs         Number of years for which to run the retrospective
filename            Output base filename, including directory
table.fmt.str       #The table number formatting string for the ica.out file

Output Figures
==============
The common assessment module does not specify the type of output format - this is left 
up to the user to decide. If no device is configured, all of the graphs will be displayed
in the graphics window. If the user wishes to save the graphs, eg to windows metafiles or PDF, they
should configure the output device before sourcing the common module, and close it afterwards eg

win.metafile(figures - %02d.wmf",height=180/25.4,width=130/25.4,pointsize=10,restoreConsole=FALSE)
source(file.path(".","Common","HAWG Common assessment module.r"))
dev.off()
