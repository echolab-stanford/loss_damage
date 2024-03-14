# Repository for the "Estimating climate change loss and damage consistent with a social cost of greenhouse gasses" paper
A Repo supporting project on estimating global loss and damage from emissions. This repo contains updated scripts to process, clean, analyze, and reproduce the figures in the loss and damage paper.

## Set up Python and R

## Scripts 

- The pipeline for producing this paper's results rely on output dataframes produced through the run_scripts.R script. Inside this script teh needed custom functions are sourced from scripts in the same folders. Each with thier own job.
- Some of the script are run through sherlock. These are batch scripts (R scripts) with shell files (.sh) specifiying the resources needed to run the script. 

### Processing scripts 

### Analysis scripts 

-  run_scripts.R
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

### Figures scripts

## Output and figures 

## Processes 

### Reproduce figures from the paper 

- Figure 2:
  -- output dataframes through run_scripts.R
      1) run the set up code chunks and read the main dataframes (lines 22-92)
      2) run (line 185) to produce the fair temperature response to emissions preturbation. Specifically, this will return $' \delta{T}_t '$
      3)    

### Reproduce data underlying figures in the paper

### Reproduce data under different paramters 

