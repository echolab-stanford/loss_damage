# Repository for the "Estimating climate change loss and damage consistent with a social cost of greenhouse gasses" paper
A Repo supporting project on estimating global loss and damage from emissions. This repo contains updated scripts to process, clean, analyze, and reproduce the figures in the loss and damage paper. The scripts are run through the github repository but raw data are stored in the BurkeLab Dropbox directory due to size limitations. The scripts written so that they pull the raw data from the dropbox and outpout intermediate dataframes used for figures into the github repo. The scripts used to plot the figures are then run from the github repo and directly pull the needed data from the github repo. Current preprint not publicly available. Public earlier drafts can be found here (https://www.nber.org/papers/w31658)

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

### **1. prepare data**
#### a. FaIR temperature response to emissions preturbations
We first start by calculating the temperature response to emissions preturbations. To do so we utilize the FaIR v2.0.0 as explained in the paper. To produce the results in the paper we calculated temperature changes from emisison preturbations using the following scripts: 
- `FaIR/Install_fair .ipynb`
- `FaIR/2_calc_FaIR_deltat_1Gt_tCo2_2300.ipynb`
- `FaIR/2_calc_FaIR_deltat_bilateral.ipynb`
- `FaIR/2_calc_FaIR_deltat_cc.ipynb`
- `FaIR/2_calc_marginals1.ipynb`
- `FaIR/2_calc_pulse_marginals.ipynb`

#### b. Calculating grid level warming ratio 
To generate the grid level warming ratio (relative to global warming), we utilize 30 global CGM models to generate the grid level ratio. We then calculate the median ratio for each of the locations. 
- `scripts/1_r_cgm.R`

### **2. run the analysis**
We take the temperature change from emissions preturbing from FaIR and the warming ratio, and integrate it with our temperature-GDP panel to compute the damages resulting from the preturbation. All the scripts are stored under `scripts/` and are called in the main analysis script `scripts/run_scripts_ld.R`. The scripts containing the functions are 
- `scripts/2a_FaIR_deltaT_hist.R`
- `scripts/2b_FaIR_deltaT_hist_fut.R`
- `scripts/2c_FaIR_deltaT_hist_fut_disagg.R`
- `scripts/3a0_run_gdptemp_panel.R`
- `scripts/3a1_run_gdptemp_panel_bhmbs.R`
- `scripts/3a2_run_gdptemp_panel_5lags.R`
- `scripts/3b0_run_bhm_model.R`
- `scripts/3b1_run_gdptemp_panel_5lag.R`
- `scripts/3c0_calc_total_damages_bilateral.R`
- `scripts/3c1_calc_total_damages.R`
- `scripts/3c2_calc_total_damages_5lags.R`

### Figures and tables 
- Figure 2: 
    - scipts used: 
        - analysis: 
        - plotting:

    - data used: 
        - `~/BurkeLab Dropbox/projects/loss_damage/FaIR/`
        - `~/BurkeLab Dropbox/projects/loss_damage/data/processed/world_gdp_pop/`
        - `~/BurkeLab Dropbox/projects/loss_damage/data/processed/future_forecast/`
        - `~/BurkeLab Dropbox/projects/loss_damage/data/processed/bhm/`
        - `~/BurkeLab Dropbox/projects/loss_damage/data/processed/wdi_dat.rds`
        - `~/BurkeLab Dropbox/projects/loss_damage/data/processed/minmax_data.rds`
- Figure 3:
- Figure 4:
- Figure ED1:
- Figure ED5:
- Figure ED6:
- Figure ED8:
- Figure ED9:
- Figure ED10:
- Figure ED11:
- Figure ED12:
- Figure ED13:
- Figure ED14:
- Figure ED15:
- Figure ED16:
- Figure ED17:



