##
This repo produces a report (.pdf) detailing outputs from the STEP Learning Collaborative initiative across Connecticut. 

## Data
Two types of data currently used in the report: 1. Phonescreen data - downloadable from the Yale onedrive - documents all inquiries to the STEP LC initiative, and how they are processed; 2. redcap data - only information for those found to be eligible for the LC - downloadable from Yale RedCap. Both sources of data contain PHI and are therefore not made available. 

Maps/Spatial data - caveat - 03_RCViz_Map_eligibleXzip requires files outputted from a seperate directory (LearningCollabMap). The directory that produces these files can be found in ToniGibbsDean/LearningCollabMap. However, for ease of use, the outputted files can also be found and downloaded from here: https://drive.google.com/drive/u/0/folders/1IH6em5EMH7spLJI_anagusiwX4dIhUdf 

All patient data contains PHI and is therefore not made available - please contact me for further details. 

## Running the scripts
The .qmd file sources all other (r) files to produce the final .pdf. 

The renv has been locked - as such all packages needed for this repo to work should be installed when you open r in this directory. However, to ensure this process has worked, you may need to run:

renv::restore()