# Scripts and run instructions

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



