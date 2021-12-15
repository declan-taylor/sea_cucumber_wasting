# DS_cucumber
Code repository for Declan and Jonathan's 2021 giant California sea cucumbers 
DS project.

**Contributing Authors:**

Jonathan J. Farr (Fall program student, BMSC, jfarr@ualberta.ca)

Declan Taylor (Fall program student, BMSC, declanta@student.ubc.ca)

## Reproducibility

Complete and annotated code that is necessary for reproducing our analyses is 
found in the RMarkdown File: FarrTaylor_code_DSADA2021.rmd. There are 11 
warning messages that occur when running our code, which arise from how dplyr 
handles time data (and NA values in date/time columns). These do not affect the 
code or data analyses.

## Subdirectories
### /paper

Contains the final manuscript (FarrTaylor_DSADA2021.docx and 
FarrTaylor_DSADA2021.pdf) and a subdirectory of archived manuscript drafts 
(/paper/archived_drafts). 

### /data

Contains manipulated CSV files that were used in data exploration and analyses. 
CSV files used by the final scripts and `FarrTaylor_code_DSADA2021.rmd` are 
BehaviourData_Final.csv, DailyLog_Final.csv and SizeData.csv. 

### /figures

Contains figures created using R. Filename reflects each image's location in 
the final paper.

### /raw_data 

Contains raw unmanipulated data from the experiment in excel workbooks. 
Metadata for each excel file contained in the first sheet.

### /stats_scripts_archive

Contains archived R scrips used to explore data, build dataframes, and conduct 
statistical analyses. Scripts were collated are in the main repo under 
FarrTaylor_code_DSADA2021.rmd for ease of review and replication.

