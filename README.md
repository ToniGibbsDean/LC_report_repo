##
This repo produces a report (.pdf) detailing outputs from the STEP Learning Collaborative initiative across Connecticut. 

## Data
Two types of data currently used in the report: 1. Phonescreen data - downloadable from the Yale onedrive - documents all inquiries to the STEP LC initiative, and how they are processed; 2. redcap data - only information for those found to be eligible for the LC - downloadable from Yale RedCap. Both sources of data contain PHI and are therefore not made available. 

Maps/Spatial data - caveat - 03_RCViz_Map_eligibleXzip requires files outputted from a seperate directory (LearningCollabMap). The directory that produces these files can be found in ToniGibbsDean/LearningCollabMap. However, for ease of use, the outputted files can also be found and downloaded from here: https://drive.google.com/drive/u/0/folders/1IH6em5EMH7spLJI_anagusiwX4dIhUdf 

All patient data contains PHI and is therefore not made available - please contact me for further details. 

## Running the scripts
The renv package has been used as an environment manager - as such all packages needed for this repo to work can be easily installed when you first download this directory. Simply, open a new terminal, change to this directory (LC_report_repo), open an instance of r, and you should see renv will bootstrap itself. Once this is complete you can use:

renv::restore()

This will ensure the right packages are installed, but may take a little time to run first time around. 

The .qmd file sources all other (r) files to produce the final .pdf. As such, you can simply run this file and it will produce the .pdf report. Keyboard shortcut for mac's to run the .qmd is command+shift+k - you will find the file in outputs.

The file structure of this repo is important to maintain. The r scripts sit in the code directory, whereas the markdown docs (.qmd and .yml) sit in the main directory. This is a necessary because markdown opens a terminal automatically in the main directory when running. You can control the formatting/titles etc., using the .yml

## Outputs
The .pdf will be outputted in the output directory. Using the standard approach, the code chunk outputs are also outputted into a folder that will be created by the markdown script. This can be found in the main directory and will be called STEPLC_report_files > figure-html. This folder will contain all the .png's from the code chunks. 