# Repository for the "Estimating climate change loss and damage consistent with a social cost of greenhouse gasses" paper
A Repo supporting project on estimating global loss and damage from emissions. This repo contains updated scripts to process, clean, analyze, and reproduce the figures in the loss and damage paper. The scripts are run through the github repository but raw data are stored in the BurkeLab Dropbox directory due to size limitations. The scripts written so that they pull the raw data from the dropbox and outpout intermediate dataframes used for figures into the github repo. The scripts used to plot the figures are then run from the github repo and directly pull the needed data from the github repo. Current preprint not publicly available. Public earlier drafts can be found here (https://www.nber.org/papers/w31658)

# Description

Broadly, the workflow incorporates the simulation and estimation of four general processes. **1) temperature changed due to preturbed emissions**, **2) local warming ratio globally**, **3) temperature-growth response function**, and **4) generating country-year level dataset of temperature changes and growth impacts**. The aformentioned steps allow us then to calculate hoistorical and/or future damages due to historical emissions. The code workflow for the paper is divided into 3 main steps.

1. generating the global warming ratio (at the pixel level) using the CGM models.
2. Generating changes in temperature due to full emissions scenario (current historical emissions) and preturbed emissions using the FaIR model.
3. generating country-year level datasets with temperature under full emissions and under preturbed emissions, BHM model coeffecients under different models (5-lag, levels model with rebound), and computing discounted damages under preturbed scenario. 

## User suitability 
You can use your PC to generate temperature changes from the FaIR model. In order to generate the numbers reported in figures (), we suggest using a remote server with the required number of CPUs. The software needed to process and analyze the data are R and Python. Multuple Rstudio Versions were used (the latest being 2024.12.1+563 (2024.12.1+563)) for the R scripts, and Jupyter Notebook (via Anacondda base environment) was used for the Python part of the scripts.

# Workflow

## 0. Preamble

To install and call the needed libraries to call user-created functions, and to set up the directories for processed data and outputted data ([0_read_libs.R](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/scripts/working/analysis/0_read_libs.R)).


## 1. Generating the global warming ratio 
In this step, we generate a global spatially continuous dataset of warming ratio under a list of 30 GCMs. The warming ratio is calculated by dividing the change in temperature in a given pixel by the global change in temperature per the GCM. Finally, we take the median value across all GCMs. This pixel level ratio then is agrgegated to the country level by taking the population weighted average. You can navigate to the script ([1_r_cgm.R](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/scripts/working/analysis/1_r_cgm.R)).

## 2. Generating changes in temperature from FaIR
In this step we generate the temperature changes under the full emissions scenario (historical emissions) and the preturbed emissions scenario using the FaIR v2.0 model (Finite Amplitude Impulse Response simple climate model). Below are the detailed steps for installing FaIR and generating the temperature changes due to preturbed scenarios.

**NOTE**: To generate the below temperature change responses to emissions preturbation, we generate 1000 runs with random combinations of parameter values sampled from established distribution in the literature (see: Ashwin et al, 2019). We have sampled from the distribution and saved the sampled paramteres to be used across the different scripts. The specific file can be accessed here (~/BurkeLab Dropbox/projects/loss_damage/FaIR/fair_params/')

#### a. Installing FaIR 
To install fair, navigate to [install_fair.ipynb](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/FaIR/Install_fair%20.ipynb). Make sure [REQUIREMENTS.txt](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/FaIR/REQUIREMENTS.txt) is in your repository before running the install_fair.ipynb script. 

#### b. Generating full vs preturbed scenarios
Now that you have installed FaIR, you can import it in other scripts and call the functions for the various scenarios
###### I.  ([2_calc_FaIR_deltat_1Gt_tCO2_2300.ipynb](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/FaIR/2_calc_FaIR_deltat_1Gt_tCO2_2300.ipynb))
1 GtCO<sub>2</sub>/tCO<sub>2</sub> experiment. This experiment is used to generate temperature changes due to GtCO<sub>2</sub> or tCO<sub>2</sub> preturbation to historical emissions. The preturbation is taken from each year between 1990 and 2020. The temperature changes generated from preturbing 2020 emissions is used to calculate the SCC (Social Cost of Carbon).

- related figures ([fig2](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/figures/loss_damage_r1/fig2_post_illustrator.pdf), [fig3](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/figures/loss_damage_r1/fig3_post_illustrator.pdf),
[figED1](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/figures/loss_damage_r1/figED1_post_illustrator.pdf))

###### II.([2_calc_FaIR_deltat_bilateral.ipynb](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/FaIR/2_calc_FaIR_deltat_bilateral.ipynb))
country-level historical emissions preturbation (1990, 1980, and 1960 start years and consumption vs. production emissions) 
- related figures ([fig4](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/figures/loss_damage_r1/fig4.pdf), [figS4](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/figures/loss_damage_r1/figS4.pdf), [figS5](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/figures/loss_damage_r1/figS4_post_illustrator.pdf), [figS6](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/figures/loss_damage_r1/figS4_post_illustrator.pdf), [figS7](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/figures/loss_damage_r1/figS7_post_illustrator.pdf))

###### III. ([2_calc_FaIR_deltat_cc.ipynb](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/FaIR/2_calc_FaIR_deltat_cc.ipynb))
carbon capture experiment  
- related figures ([figED10](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/figures/loss_damage_r1/figED10.pdf))

###### IV. ([2_calc_pulse_marginals.ipynb](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/FaIR/2_calc_pulse_marginals.ipynb))
marginal emissions preturbation experiment 
- related figures ([figS12](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/figures/loss_damage_r1/figS12_post_illustrator.pdf))

#### c. Calculating the change in temperature between both scenarios
Now that we have generated the annual change in temperature in our historical full emissions scenario and our preturbed scenario, we can take the median change in temperature between the two scenarios.

###### I. ([2a_FaIR_deltaT_hist.R](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/scripts/working/analysis/2a_FaIR_deltaT_hist.R)) 
- related figures ([figED1](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/figures/loss_damage_r1/figED1_post_illustrator.pdf))
###### II. ([2b_FaIR_deltaT_hist_fut.R](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/scripts/working/analysis/2b_FaIR_deltaT_hist_fut.R))
- related figures ([figED5](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/figures/loss_damage_r1/figED5.png),
[figED7](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/figures/loss_damage_r1/figED7_post_illustrator.pdf),
[figED8](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/figures/loss_damage_r1/figED8.png))
###### III. ([2c_FaIR_deltaT_hist_fut_disagg.R](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/scripts/working/analysis/2c_FaIR_deltaT_hist_fut_disagg.R)) 
- related figures ([figED6](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/figures/loss_damage_r1/figED6_post_illustrator.pdf),
[fig2e](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/figures/loss_damage_r1/fig2e.pdf), 
[figS2](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/figures/loss_damage_r1/figS2_post_illustrator.pdf),
[figS3](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/figures/loss_damage_r1/figS3.pdf))

## 3. Generating country-year level dataset, BHM model coeffecients under different models & computing discounted damages
#### a.  
###### 0. ([3a0_run_gdptemp_panel.R](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/scripts/working/analysis/3a0_run_gdptemp_panel.R)) 
###### 1.([3a1_run_gdptemp_panel_bhmbs.R](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/scripts/working/analysis/3a1_run_gdptemp_panel_bhmbs.R)) 
###### 2.([3a2_run_gdptemp_panel_5lags.R](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/scripts/working/analysis/3a2_run_gdptemp_panel_5lags.R)) 
This script is used to generate the gdp-temp panel to be used for analysis.

#### b.  
###### 0. ([3b0_run_bhm_model.R](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/scripts/working/analysis/3b0_run_bhm_model.R)) 
This script is used to geneerate the BHM 0-lag model estimates.
###### 1. ([3b1_run_gdptemp_panel_5lag.R](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/scripts/working/analysis/3b1_run_gdptemp_panel_5lag.R)) 
This script is used to geneerate the BHM 5-lag model estimates.
#### c.  
###### 0.([3c0_calc_total_damages_bilateral.R](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/scripts/working/analysis/3c0_calc_total_damages_bilateral.R)) 
- related figures ([fig4](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/figures/loss_damage_r1/fig4.pdf), [figS4](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/figures/loss_damage_r1/figS4.pdf), [figS5](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/figures/loss_damage_r1/figS4.pdf), [figS6](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/figures/loss_damage_r1/figS4.pdf), [figS7](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/figures/loss_damage_r1/figS7.pdf))

###### 1.([3c1_calc_total_damages.R](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/scripts/working/analysis/3c1_calc_total_damages.R)) 
- related figures ([figED8](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/figures/loss_damage_r1/figED8.pdf))

###### 2.([3c2_calc_total_damages_5lags.R](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/scripts/working/analysis/3c2_calc_total_damages_5lags.R))
- related figures ([fig2](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/figures/loss_damage_r1/fig2.pdf), 
[figED5](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/figures/loss_damage_r1/figED5.png), 
[figED7](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/figures/loss_damage_r1/figED7_post_illustrator.pdf))
- ###### i. ([3c2i_calc_total_damages_lags.R](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/scripts/working/analysis/3c2i_calc_total_damages_lags.R)) 
- ###### ii. ([3c2ii_calc_total_damages_5lags_w_rebound.R](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/scripts/working/analysis/3c2ii_calc_total_damages_5lags_w_rebound.R)) 
- related figures ([figED8](https://github.com/echolab-stanford/loss_damage/blob/5lag_pipeline_r2/figures/loss_damage_r1/figED8.png))

## Scripts 
- The pipeline for producing this paper's results rely on output dataframes produced through the run_scripts_ld.R script. Inside this script the needed custom functions are sourced from scripts in the same folders. Each with thier own job.
- Some of the script are run through Stanford's Sherlock servers. These are batch scripts (R scripts) with shell files (.sh) specifying the resources needed from the servers to run the script. 

### Processing scripts 

### Analysis scripts 
- run_scripts_ld.R
- 0_read_libs.R
- 1_r_cgm.R
- 2a_FaIR_deltaT_hist.R
- 2b_FaIR_deltaT_hist_fut.R
- 2c_FaIR_deltaT_hist_fut_disagg.R
- 3a0_run_gdptemp_panel.R
- 3a1_run_gdptemp_panel_bhmbs.R
- 3a2_run_gdptemp_panel_5lags.R
- 3b0_run_bhm_model.R
- 3b1_run_gdptemp_panel_5lag.R
- 3c0_calc_total_damages_bilateral.R
- 3c1_calc_total_damages.R
- 3c2_calc_total_damages_5lags.R
- 3c2i_calc_total_damages_lags.R
- 3c2ii_calc_total_damages_5lags_w_rebound.R

### Batch scripts 
- run_5lag_k80_scc_sherlock2.R
- run_5lag_scc_sherlock2.R

### Figures preparation scripts
- fig2a_b_c_d_ED5_ED7.R	
- figED1.R	
- figED8.R
- fig2e.R	
- figED10.R		
- figED9.R
- fig3_S2_S3.R	
- figED6.R	
- figS12.R
- fig4_S4_S5_S6_S7.R	
- figED6c.R

### Figures plotting scripts
- fig2a_b_ED5_plot.R	
- fig4_S4_S5_S6_S7_plot.R	
- figED6c_plot.R
- fig2c_d_ED7_plot.R	
- figED1_plot.R		
- figED8_plot.R
- fig2e_plot.R		
- figED10_plot.R		
- figED9_plot.R
- fig3_S2_S3_plot.R	
- figED6_plot.R		
- figS12_plot.R

## Processes 

### Reproduce figures from the paper 
To reproduce the figures from the paper, you can either re run the scripts preparing the data "[prepating_data](https://github.com/echolab-stanford/loss_damage/tree/5lag_pipeline_r2/scripts/working/figures/preparing_data/)" to be plotted and the scripts plotting the figures themselves "[plotting_data](https://github.com/echolab-stanford/loss_damage/tree/5lag_pipeline_r2/scripts/working/figures/plotting_data/)". 

### Reproduce data under different paramters 
You can also regenerate the data underlying the figures by tweaking some of the paramters which are specified in the built in functions from the scripts above. For example you can generate an alternative FaIR preturbation as demonstrated in the FaIR scripts above and use the resulting dataset to process the change in temperature due to that preturbation. 

## Data

- The country-year level GDP data is from the World Bank.
    - The World Bank. World development indicators., 2022. URL http://data.worldbank.
org/data-catalog/world-development-indicators.
-  SSP data is downloaded from the IIASA.
    - Riahi, K., van Vuuren, D. P., Kriegler, E., Edmonds, J., O’Neill, B. C., Fujimori, S., Bauer, N., Calvin, K., Dellink, R., Fricko, O., Lutz, W., Popp, A., Cuaresma, J. C., KC, S., Leimbach, M., Jiang, L., Kram, T., Rao, S., Emmerling, J., … Tavoni, M. (2017). The Shared Socioeconomic Pathways and their energy, land use, and greenhouse gas emissions implications: An overview. Global Environmental Change, 42, 153–168. https://doi.org/10.1016/j.gloenvcha.2016.05.009
- Carbon Majors database 
    - Paul Griffin and CR Heede. The carbon majors database. CDP carbon majors report 2017,
14, 2017.
    - Richard Heede. Tracing anthropogenic carbon dioxide and methane emissions to fossil fuel
and cement producers, 1854–2010. Climatic change, 122(1):229–241, 2014.
- Country-level carbon data 
    - Global Carbon Budget. Global carbon budget 2022. Earth System Science Data, 14:4811–
4900, 2022.
- ERA5-Land data
    - Joaqu´ın Mu˜noz-Sabater, Emanuel Dutra, Anna Agust´ı-Panareda, Cl´ement Albergel,
Gabriele Arduini, Gianpaolo Balsamo, Souhail Boussetta, Margarita Choulga, Shaun Harrigan,
Hans Hersbach, et al. Era5-land: A state-of-the-art global reanalysis dataset for land
applications. Earth system science data, 13(9):4349–4383, 2021.
- Companies' revenues 
    - Macrotrends LLC. Macrotrends. https://www.macrotrends.net/, 2025. Accessed:
2025-01.
- Population data 
    - CIESIN. Gridded population of the world, version 4 (gpwv4): Population count, revision
11., 2018.
## Output and figures 
Plots and tables can be found under the [figures folder](https://github.com/echolab-stanford/loss_damage/tree/5lag_pipeline_r2/figures/loss_damage_r1). Some of the raw outputted 
