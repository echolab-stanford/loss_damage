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
- `Install fair .ipynb`
- `FaIR/2_calc_FaIR_deltat_1Gt_tCo2_2300.ipynb`
- `FaIR/2_calc_FaIR_deltat_bilateral.ipynb`
- `FaIR/2_calc_FaIR_deltat_cc.ipynb`
- `FaIR/2_calc_marginals1.ipynb`
- `FaIR/2_calc_pulse_marginals.ipynb`

After calculating the temperature response 

#### b. Calculating grid level warming ratio 
To generate the grid level warming ratio (relative to global warming), we utilize 30 global CGM models to generate the grid level ratio. We then calculate the median ratio for each of the locations. 
- `scripts/1_r_cgm.R`

### **2. run the analysis**
We take the temperature change from emissions preturbing from FaIR and the warming ratio, and integrate it with our temperature-GDP panel to compute the damages resulting from the preturbation. All the scripts are stored under `scripts/` and are called in the main analysis script `scripts/run_scripts_ld.R`. The scripts containing the functions are: 
- 3a0_run_gdptemp_panel.R
- 3a1_run_gdptemp_panel_bhmbs.R
- 3a2_run_gdptemp_panel_5lags.R
- 3b0_run_bhm_model.R
- 3b1_run_gdptemp_panel_5lag.R
- 3c0_calc_total_damages_bilateral.R
- 3c1_calc_total_damages.R
- 3c2_calc_total_damages_5lags.R

### Figures scripts

## Output and figures 

## Processes 

### Reproduce figures from the paper 

- Figure 2:
    - before running the scripts sync the following files in your desktop:
        - `~/BurkeLab Dropbox/projects/loss_damage/FaIR/`
        - `~/BurkeLab Dropbox/projects/loss_damage/data/processed/world_gdp_pop/`
        - `~/BurkeLab Dropbox/projects/loss_damage/data/processed/future_forecast/`
        - `~/BurkeLab Dropbox/projects/loss_damage/data/processed/bhm/`
        - `~/BurkeLab Dropbox/projects/loss_damage/data/processed/wdi_dat.rds`
        - `~/BurkeLab Dropbox/projects/loss_damage/data/processed/minmax_data.rds`
        - downloads the github 'loss_damage' repo from the lab directory (5lag_pipeline branch NOT main)
    - panels a-b:
        - output dataframes through run_scripts_ld.R found under "~/GitHub/loss_damage/scripts/working/analysis/run_scripts_ld.R"
            1. set up the directories to access the input data and to output the outcome data by syncing the above folders
            2. run the set up code chunks and read the main dataframes (lines 22-93). Those lines will produce the following:
                a. country-year data with population from the WDI
                b. a median raster from the 30 CGMs ensemble
                c. population raster to calculate weighted ratios
                d. a country level global shapefile
                e. a dataframe with minimum and maximum observed growth to bound future growth                
            3. run (line 190) to produce the fair temperature response to emissions preturbation. Specifically, this will return $' \delta{T}_t '$ used in equation (12)
            4. run (line 230) to read in the SSP3.70 scenario forecast data
            5. run (line 253-255) to read the GDP-temperature panel. This data will be used to calculate the damages attributed to the emissions preturbation 
            6. run (line 285) to read in the number of years to loop over to produce the damages per emissions in a given year 
            7. Finally, run lines (292-304) this will run the custom function that would calculate the damages following equations (12-16) and then the last linee is to output the dataset. The paramaters to specify:
                - NOTE: the script that contains the function that calculate the damages is '3c2_calc_total_damages_5lags.R'
                a. warming ratio raster
                b. fair temperature response to emissions preturbation 
                c. year(s) to loop over to calculate the damages per that year 
                d. year k when damages start to accumulate 
                e. future growth forecast per the SSP3.70 
                f. GDP-temperature panel 
                g. temperature data used. Default is ERA. Alternative is CRU but pipeline is not set up to switch yet (as of 03/14/24)
                h. settlement year. Default is 2020. This is important for discount rates
                i. indicating whether to turn on the growth-post-2100 poarameter. Default is 'F' to turn on just change that to '0'
                j. indicating whether to turn on teh adaptation mechanism. Default is 'F', to turn it on change to 'T'
                k. indicating whether to use the temperature-growth coeffeceints from base model or bootstrapped estimates
        - run figure preparation and plotting scripts
            - run the script fig2a_b_c_d.R found under "~/GitHub/loss_damage/scripts/working/figures/preparing_data/fig2a_b_c_d.R"
                - Before running the scripts make sure to adjust the 'run_date' paramter uptop so that the script can pull the data you have just produced above for panels a-d. This script will save the dataset in the github repo `~/GitHub/loss_damage/data/figures/{run_date}/`.
            - now run the scripts 'fig2a_b_plot.R' and can be found here:  `~/GitHub/loss_damage/scripts/working/figures/plotting_data/`. Also make sure you have updated the 'run_date' parameter up top. 



### Reproduce data under different paramters 

