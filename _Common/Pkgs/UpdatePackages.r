######################################################################################################
# UpdatePackages
#
# Version 1.00 5/02/2009 17:12:18
#
# Mark Payne (DTU-AQUA)
#
# Installs all the packages in the directory
#
# Developed with:
#   - R version 2.8.0
#   - FLCore 1.99-107
#   - FLAssess 1.99-102
#
# Changes:
# V 1.00  - First full version - just installs everything! Crude, but effective

# To be done:
#
# Notes:
#
####################################################################################################

required.version <- "2.8.0"
if(compareVersion(paste(version$major,version$minor,sep="."),required.version)==-1) {
 stop(paste("ERROR: Current R version is",paste(version$major,version$minor,sep="."),"This code requires at least R",required.version))
}
install.packages(pkgs=dir(pattern="zip$",full.names=TRUE),repos=NULL)
