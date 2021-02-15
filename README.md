# MBMF_Study2

All the R-codes, data and output used for this study. We are still recruiting participant so data collection is not finished yet. The code is under heavy maintenance and is not ready yet.

## Before launching

- R version 4.0.0 or later (mine: 4.0.3)
- RStudio version 1.4.x or later (mine: 1.4.1103)
- Please follow these instructions before launching any code in this folder: https://ccs-lab.github.io/hBayesDM/index.html (needed for the 7 parameter computations).
- Install all the packages proposed in *MBMFStudy2_Initialization.R* once. Once they are downloaded, their installation will be handled by *MBMFStudy2_Initialization.R*.

## Repository content
- The *MBMF_Study2.Rproj* which is the project. Open it before anything else to handle the working directory.
- The *AdditionalScripts* folder contains all the scripts used to get some small additional information.
- The *Analyse* folder contains all the scripts used to analyze the data.
- The *DataPreparation* folder contains all the script used to import the data, transform them, and prepare a final dataframe.
- The *DataPreparationHS.R* is an hyperscript used to launch all those in the data preparation folder.
- The *MBMFStudy2_Initialization.R* script contains all the packages and all the functions needed for the codes to run. It will be automatically launch with all the other scripts.
- The *Functions* folder contains all the function I wrote for this project. They are all automatically imported when needed.
- The *Output* folder contains all the output create by the *DataPreparation* scripts.
- The *Raw_Data* folder contains all the raw data, sorted by their source.

## Finished content already useable
I will update this part until the project is finished.
- *DataPreparation* is over and ready to use.

To Do:
- More recruitment.
- Analyses. The actual version is not over.
