# Repository for the "Estimating climate change loss and damage consistent with a social cost of greenhouse gasses" paper
A Repo supporting project on estimating global loss and damage from emissions. This repo contains updated scripts to process, clean, analyze, and reproduce the figures in the loss and damage paper. The scripts are run through the github repository but raw data are stored in the BurkeLab Dropbox directory due to size limitations. The scripts written so that they pull the raw data from the dropbox and outpout intermediate dataframes used for figures into the github repo. The scripts used to plot the figures are then run from the github repo and directly pull the needed data from the github repo. 

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
    - panels a-d:
        - output dataframes through run_scripts.R
            1. set up the directorie to access the input data and to output the outcome data 
            2. run the set up code chunks and read the main dataframes (lines 22-92). Those lines will produce the following:
                a. country-year data with population from the WDI
                b. a median raster from the 30 CGMs ensemble
                c. population raster to calculate weighted ratios
                d. a country level global shapefile
                e. a dataframe with minimum and maximum observed growth to bound future growth                
            3. run (line 185) to produce the fair temperature response to emissions preturbation. Specifically, this will return $ \delta{T}_t $ used in equation (12)
            4. run (line 225) to read in the SSP3.70 scenario forecast data
            5. run (line 248-250) to read the GDP-temperature panel. This data will be used to calculate the damages attributed to the emissions preturbation 
            6. run (line 266) to read in the BHM temperature-growth regression output
            7. run (line 287) to read in the number of years to loop over to produce the damages per emissions in a given year 
            8. Finally, run lines (294-307) this will run the custom function that would calculate the damages following equations (12-16) and then the last linee is to output the dataset. The paramaters to specify:
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



### Reproduce data under different paramters 

