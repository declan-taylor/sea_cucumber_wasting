# sea_cucumber_wasting
Code and data repository for our project testing the effects of heat stress on 
wasting in giant California sea cucumbers.

**Contributing Authors:**

Declan Taylor (Earth, Ocean, and Atmospheric Sciences, the University of 
British Columbia, declanta@student.ubc.ca)

Jonathan J. Farr (Fall program student, BMSC, jfarr@ualberta.ca)

## Reproducibility

Complete and annotated code that is necessary for reproducing our analyses is 
found in the `scripts` directory. There are 11 
warning messages that occur when running our code, which arise from how dplyr 
handles time data (and NA values in date/time columns). These do not affect the 
code or data analyses.

## Subdirectories
### /DS_archive

This project started as a Fall Program directed studies (DS) project by Declan 
and Jonathan at the Bamfield Marine Sciences Centre (BMSC). This directory 
contains early iterations of code and other specific files for the DS course's 
submission in December 2021. All files in this directory are considered out of 
date.

### /data

Contains manipulated CSV files that were used in data exploration and analyses. 
BehaviourData_Final.csv, DailyLog_Final.csv and SizeData.csv contain the data 
we collected at BMSC. ONC_data contains temperature data from Ocean Networks 
Canada's mooring in Baynes Sound. SST_data contains satallite data of sea 
surface temperature in Nanoose Bay. The raw_data subdirectory contains raw 
unmanipulated data from the experiment in excel workbooks. Metadata for each 
excel file contained in the first sheet.

### /figures

Contains files used to create our figures. Subdirectory filenames reflect the 
figure location in the final paper. Final assembly of figures was done in Adobe 
Illustrator by Dan Wuitchik. The figures_scrips subdirectory holds scripts 
which were exclusivley used to create figures. The stats_scripts subdirectory 
holds scripts which involved data analysis. binaryVariables.R is a script which 
assempbles much of our data into tibble dataframes for analysis. The functions 
in this script create dataframes which are used extensivley throughout our 
analyses.
