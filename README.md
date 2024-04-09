# Repository for the "Estimating climate change loss and damage consistent with a social cost of greenhouse gasses" paper
A repository supporting project on estimating global loss and damage from emissions. This repo contains updated scripts to process, clean, analyze, and reproduce the figures in the loss and damage paper. The scripts are run through the github repository but raw data are stored in the BurkeLab Dropbox directory due to size limitations. The scripts written so that they pull the raw data from the dropbox and outpout intermediate dataframes used for figures into the github repo. The scripts used to plot the figures are then run from the github repo and directly pull the needed data from the github repo. Current preprint not publicly available. Public earlier drafts can be found here (https://www.nber.org/papers/w31658)

# Description
This repository provides the code and guidance necessary to run the analysis and produce the figures and in-text analysis in Burke et al., 2024 working paper. The structure of the scripts allow users to run the models under different assumptions and underlying conditions. This generally includes the **1)** FaIR emissions preturbation-temperature relationship, **2)** cell level warming ratio, the **3)** temperature-economic growth relationship. The structure of the folers is as follows:

- `FaIR\`
- `data\`
- `figures\`
- `scripts\`

## User requirements and data access

### User requirements
The software used for processing and analyzing the data is done through python (jupyter notebook) and R scripts. Users will need to have access to both software. Some of the R scripts are computationally intensive. As such we run those files as batch scripts utilizing Stanford Sherlock servers. 

### Data Access 
Raw data are publicly available and are obtained from the following links. We process the raw data in order to prepare them for analysis. The processed data can be accessed through the ECHOLab dropbox. 

## Analysis  
- The pipeline for producing this paper's results rely on output dataframes produced through the run_scripts_ld.R script. Inside this script the needed custom functions are sourced from scripts in the same folders. Each with thier own jobs.
- Some of the script are run through sherlock. These are batch scripts (R scripts) with shell files (.sh) specifiying the resources needed to run the script. 


