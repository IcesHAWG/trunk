=========================
IBTS0 QA and Processing
=========================

Mark Payne
DTU-Aqua, Charlottenlund, DK
mpay@aqua.dtu.dk

$Rev: 827 $
$Date: 2014-03-09 17:34:07 +0100 (Sun, 09 Mar 2014) $

Description
===========
This codebase performs QA checks on the IBTS0 larval index (formerly known as the MIK index) and calculates the index value.

Workflow 
========
1. Note that these scripts are intended to be run with this directory (ie the directory where this file is found) as the working directory in R. If you have not already done so, please set your directory accordingly.
2. Copy database to analyse into ./data/ File should be formatted as a comma-separable file (csv) and there should only be one csv file in this directory.
3. Source the script ./src/Data_import.r to import the database into a standardised format that can be read by other scripts. The script writes a file called MIK_data_raw.RData into ./objects/
4. Run the commands given in the header of ./src/MIK_Quality_assurance.r to generate a quality assurance report on the database. It may be necessary to change the configuration of this script to reflect the years contained in the database. The report is written to the ./outputs/ directory as an HTML file that can be opened in any browser. The database that has been throug QA is written to the file ./objects/MIK_data_QA.RData and is used in the calculation of the IBTS0 index below.
5. Source the script ./src/Index_calculation.r to calculate the IBTS0 (MIK) index from the database. The indices are written as a text file ./outputs/MIK_indices.txt

