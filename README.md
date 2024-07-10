**REPLICATION INSTRUCTIONS**

_Skills Beget Skills: Addressing the Role of Mothers’ Occupation-Specific Skills on Children’s Developmental Process_

Alicia García-Sierra

Datasets:

1.	NLSY79 Child and Young Adult. Available to download freely from the NLS investigator. You can find all the relevant variables to download in the different tagsets files (‘Tagsets folder’). In each R Script I specify which tagset is needed.

2.	O*NET dataset. Available to download freely from: https://www.onetonline.org/ ->  O*NET Data  -> Skills (basic) -> Content.  Note that it can be downloaded in Excel format.

3.	Crosswalks. I employ several crosswalks to make occupations equivalent. Each one and its source is specified in the corresponding R Script.

Replication files: 

  00_MASTER_NLS.R: sets the environment 
  01_CHILDREN_DATA.R: prepares the data for the children
  02_IMPORT_OCCUPATIONS.R: imports the data on mothers’ occupations
  03_OCCUPATIONS_CROSSWALK.R: equivalises the different occupational classifications across decades
  04_MERGE_WITH_ONET.R: prepares O*NET data and merges it with the data on occupations
  05_MERGE_MOTHERS_CHILDREN.R: combines all the data from mother and children
  06_ADDITIONAL_CONTROLS.R: adds variables used in the extensions and robustness to the main data
  07_CLEANING.R: prepares the variables of interest of the dataset
  08_ANALYSES.R: runs the main models of the analyses
  09_ROBUSTNESS.R: runs all the different robustness checks
  10_FIGURES.R: produces the figures included in the manuscript
  
  Tagsets folder: includes six different tagsets that when uploaded to the NLS investigator produce the datasets of interest. Each R Script above specifies which of these tagsets should be used in each step. 
