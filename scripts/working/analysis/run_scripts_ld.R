##############################################################################
# Mustafa Zahid, May 27th, 2022
# This script has been and will be updated throughout the project.  This code 
# will be up in github and any changes to the code will be committed to the 
# repo in github. This is the master script and it sources the functions from 
# other scripts in the same directory. the initial part of the script contains 
# a fair amount of processing. As such I will save the processed data from this 
# script and just read it whenever I need to run the code. The processing code 
# will be commented out 
# Input(s): Libraries, created functions, raw CGM model rasters, world bank 
# data, Penn world table data, processed temperature response data (FaIR), 
# world population raster, world country-borders shapefile 
# output(s): country-year panel with total damages from different emissions 
# perturbations (past and future), emitter-harmed-year panel for bilateral 
# damages, 
# Last edited: November 2024
##############################################################################

################################################################################
################# PRE0: Clear env., read libs, source functions ################
################################################################################
remove(list=ls())
gc()
sf::sf_use_s2(FALSE)
setwd("~/GitHub/loss_damage")

#ADJUST THE RUN_DATE BEFORE RUNNING THE SCRIPT 
run_date <- "loss_damage_r1_mustafa_rep_temp"

# read in the needed libraries 
source("scripts/working/analysis/0_read_libs.R")
# function for calculating warming ratio CGMs
source("scripts/working/analysis/1_r_cgm.R")
# functions for computing deltaT form fair
source("scripts/working/analysis/2a_FaIR_deltaT_hist.R")
source("scripts/working/analysis/2b_FaIR_deltaT_hist_fut.R")
source("scripts/working/analysis/2c_FaIR_deltaT_hist_fut_disagg.R")
# functions for prepping gdp-temp panel and for computing damages
source("scripts/working/analysis/3a0_run_gdptemp_panel.R")
source("scripts/working/analysis/3a1_run_gdptemp_panel_bhmbs.R")
source("scripts/working/analysis/3a2_run_gdptemp_panel_5lags.R")
source("scripts/working/analysis/3a2i_run_gdptemp_panel_lags.R")
source("scripts/working/analysis/3b0_run_bhm_model.R")
source("scripts/working/analysis/3b1_run_gdptemp_panel_5lag.R")
source("scripts/working/analysis/3c0_calc_total_damages_bilateral.R")
source("scripts/working/analysis/3c1_calc_total_damages.R")
source("scripts/working/analysis/3c2_calc_total_damages_5lags.R")
source("scripts/working/analysis/3c2i_calc_total_damages_lags.R")

# let us set the path so we can read in the input data 
setwd(dropbox_path)

################################################################################
###################### PRE1: Read needed data for analysis #####################
################################################################################
# population country-year dataset
pop_wdi <- readRDS("data/processed/world_gdp_pop/pop_wdi.rds")

# read in the warming ratio raster
median_raster <- raster("data/processed/r_cgm/median_raster.tiff")

# we need to aggregate the deltat to the country level by weighting by pop
# so let us read the pop raster and resample it to match coordinates and 
# convert to a dataframe and then join
pop <- raster(paste0(raw_path, "population/gpw_v4_population_count_rev11_2010_1_deg.tif"))
pop <- readAll(pop)
pop <- resample(pop, median_raster)

# world shapefile 
world <- spData::world
world <- st_as_sf(world)
world <- subset(world, name_long != "Antarctica")
world$ISO3 <- countrycode::countrycode(sourcevar = world$iso_a2,origin = "iso2c",
                                       destination = "iso3c"
)
world <- subset(world, !is.na(ISO3))

# now let us read in the min and max historical growth numbers so we can bound 
# future growth
minmax_data <- readRDS("data/processed/minmax_data.rds")

# now read in the wdi data to get population estimates 
wdi_dat <- readRDS("data/processed/wdi_dat.rds")

#################################################################################
##################### PART I: Calculate CGM Warming Ratio #######################
#################################################################################
# calculate warming ration from CGM models. Specify the needed cgm models and 
#read the model names so we can use to call models and rename output
#
#cgm_guide <- read_excel("~/GitHub/loss_damage/scripts/working/analysis/cgm_model_guide.xlsx")
##
### Now we will create a warming ratio raster per each of the models
#for (i in unique(cgm_guide$cgm_model)){
#  tic()
#  raster_deltaT_calced1 <- calculate_grid_warming_ratio("ssp370", "historical", i)
#  assign(paste0("raster_deltaT_", i), raster_deltaT_calced1)
#  toc()
#}
#

# ok let us read the processed raster and list of rasters for warming ratio
#mean_r_raster <- raster("data/processed/r_cgm/ratio_raster_avgs.tif")


# ok let us read in the list of all CGM ratio rasters
load("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/r_cgm/list_r_rasters_20230822.RData")
#write_rds(list_r_rasters, "~/BurkeLab Dropbox/Projects/loss_damage/data/processed/r_cgm/list_r_rasters_20230822.rds")
#
## now we have a raster for each of the models where we have grid level warming 
## ratio relative to global warming 
#
## let us put the rasters in a list in case we want to loop over them
#list_r_rasters <- list(`raster_deltaT_ACCESS-CM2_r1i1p1f1`,
#                       `raster_deltaT_ACCESS-ESM1-5_r1i1p1f1`,
#                       `raster_deltaT_AWI-CM-1-1-MR_r1i1p1f1`,
#                       `raster_deltaT_BCC-CSM2-MR_r1i1p1f1`,
#                       raster_deltaT_CanESM5_r1i1p1f1,
#                       `raster_deltaT_CAS-ESM2-0_r1i1p1f1`,
#                       raster_deltaT_CESM2_r10i1p1f1,
#                       `raster_deltaT_CESM2-WACCM_r1i1p1f1`,
#                       `raster_deltaT_CMCC-CM2-SR5_r1i1p1f1`,
#                       `raster_deltaT_CMCC-ESM2_r1i1p1f1`,
#                       `raster_deltaT_CNRM-CM6-1_r1i1p1f2`,
#                       `raster_deltaT_CNRM-CM6-1-HR_r1i1p1f2`,
#                       `raster_deltaT_FGOALS-f3-L_r1i1p1f1`,
#                       `raster_deltaT_FGOALS-g3_r1i1p1f1`,
#                       `raster_deltaT_GFDL-ESM4_r1i1p1f1`,
#                       `raster_deltaT_GISS-E2-1-G_r1i1p1f2`,
#                       `raster_deltaT_IITM-ESM_r1i1p1f1`,
#                       `raster_deltaT_INM-CM4-8_r1i1p1f1`,
#                       `raster_deltaT_INM-CM5-0_r1i1p1f1`,
#                       `raster_deltaT_IPSL-CM5A2-INCA_r1i1p1f1`,
#                       `raster_deltaT_IPSL-CM6A-LR_r1i1p1f1`,
#                       `raster_deltaT_KACE-1-0-G_r1i1p1f1`,
#                       `raster_deltaT_MIROC-ES2L_r1i1p1f2`,
#                       raster_deltaT_MIROC6_r1i1p1f1,
#                       `raster_deltaT_MPI-ESM1-2-LR_r10i1p1f1`,
#                       `raster_deltaT_MRI-ESM2-0_r1i1p1f1`,
#                       `raster_deltaT_NorESM2-LM_r1i1p1f1`,
#                       `raster_deltaT_NorESM2-MM_r1i1p1f1`,
#                       raster_deltaT_TaiESM1_r1i1p1f1,
#                       `raster_deltaT_UKESM1-0-LL_r1i1p1f2`)
#
#stack_r_rasters <- stack(list_r_rasters)
#median_raster <- calc(stack_r_rasters, median)
#writeRaster(median_raster, "~/BurkeLab Dropbox/Projects/loss_damage/data/processed/r_cgm/median_raster.tiff")

#save raster 
#writeRaster(master_raster, "data/processed/r_cgm/ratio_raster_avgs.tif")
#save(list_r_rasters, file = "~/BurkeLab Dropbox/Projects/loss_damage/sherlock_files_060223/list_r_rasters_20230822.RData")
#save(list_r_rasters, file = "data/processed/r_cgm/list_r_rasters_20230822.RData")

# ok let us read the processed raster and list of rasters for warming ratio
#mean_r_raster <- raster("data/processed/r_cgm/ratio_raster_avgs.tif")

################################################################################
##################### PART II: Read and compute delta T ########################
################################################################################
# we have a set of experiments that we have ran through FaIR to recover deltat 
# from each emissions preturbation. Below we go through the different experiments. 
# The temperature response data were obtained by running FaIR through jupyter notebook.

####################### Experiment (1G/tCO2/yr): ########################
# this experiment is run to estimate the temperature effects of pulsing 
# 1GtCO2 or 1tCO2 at a given year. This will be used to calculate the 
# total damages by certain emitters

# 1tCO2 
## temperature response through 2100 
fair_exps_1tco2_2100_k90 <- process_exp_data_hist_fut("20230523", "1tCO2_hist_2100", 1990, aggregating = T) # figED9, figED10
fair_exps_1tco2_2100_k80 <- process_exp_data_hist_fut("20230821", "1tCO2_hist_2100", 1980, aggregating = T) # fig3, figED12, figED13
## temperature response through 2300 
fair_exps_1tco2_2300_k90 <- process_exp_data_hist_fut("20230523", "1tCO2_hist_2300", 1990, aggregating = T) # figED10, figED11
### temperature response dis-aggregated. In other words all runs. 
fair_exps_1gtco2_disagg_2300 <- process_disagg_exp_data("20230809","1tCO2_hist_2300", 1990) # figED11
#write_rds(fair_exps_1gtco2_disagg_2300, "~/BurkeLab Dropbox/Projects/loss_damage/sherlock_files_060223/fair_exps_disagg_20230822.rds")

# 1GtCO2 
## temperature response through 2100 
fair_exps_1gtco2_2100_k90 <- process_exp_data_hist_fut("20230523", "1GtCO2_hist_2100", 1990, aggregating = T) # fig2ab, fig2cd, figED5, figED7, #figS3
### temperature response dis-aggregated. In other words all runs. 
fair_exps_1gtco2_disagg_2100 <- process_disagg_exp_data("20230523","1GtCO2_hist_2100", 1990) #fig2e_i, fig2e_j, fig2e_k
fair_exps_1gtco2_disagg_k80_2100 <- process_disagg_exp_data("20230821","1tCO2_hist_2100", 1980) # figED12
#write_rds(fair_exps_1gtco2_disagg_k80_2100, "~/BurkeLab Dropbox/Projects/loss_damage/sherlock_files_060223/fair_exps_disagg_k80_20230821.rds")


####################### Experiment (Carbon Capture): ########################
# this experiment is to estimate the damages if we are to capture 1 tCO2 
# years after emitting it
fair_exps_cc <- process_exp_data_hist_fut("20230822", "cc_hist", 2020, aggregating = T) # figED19cd
# we will need this data saved for plotting figED19
write_rds(fair_exps_cc, paste0(output_path, "/fair_exps_cc.rds"))

################ Experiment (Country-level emissions): #################
# this experiment is to estimate the country level damages attributed to each 
# of the countries.
# for year_k = 1980
fair_exps_isos_k80 <- process_exp_data_hist("20230523", "hist_bi_v2022", 1980, aggregating = T) # figED15
# for year_k = 1990
fair_exps_isos_k90 <- process_exp_data_hist("20230523", "hist_bi_v2022", 1990, aggregating = T) # fig4
#for year_k = 1990 and only consumption emissions
fair_exps_isos_k90_consump <- process_exp_data_hist("20230523", "hist_biconsump_v2022", 1990, aggregating = T) # figED17
#for year_k = 1990 and only production emissions
fair_exps_isos_k90_prod <- process_exp_data_hist("20230523", "hist_biprod_v2022", 1990, aggregating = T) # figED18
# for year_k = 1960 (11/2024 addition)
fair_exps_isos_k60 <- process_exp_data_hist("20241112", "hist_bi_2100", 1960, aggregating = T) # figED16

####################### Experiment (1/10/1000/1M/1G/10G/100G/tCO2/yr): ######################## figED9a
# this experiment is run to estimate the temperature effects of pulsing 
# 1GtCO2 or 1tCO2 at a given year.
#fair_exps_10tco2_2100_k90 <- process_exp_data_hist_fut("20230807","10tCO2_hist_2300",1990,aggregating = T)
fair_exps_1000tco2_2100_k90 <- process_exp_data_hist_fut("20230807","1000tCO2_hist_2300",1990,aggregating = T) 
fair_exps_1Mtco2_2100_k90 <- process_exp_data_hist_fut("20230807","1MtCO2_hist_2300",1990,aggregating = T)
fair_exps_10Gtco2_2100_k90 <- process_exp_data_hist_fut("20230807","10GtCO2_hist_2300",1990,aggregating = T) 
fair_exps_100Gtco2_2100_k90 <- process_exp_data_hist_fut("20230807","100GtCO2_hist_2300",1990,aggregating = T) 

####################### Experiment (30%, 50%, 70%, and 90% of emissions): ######################### figED9b
# this experiment is to run the emissions damage calculation for 1gtco2 under 
# different baseline emissions
fair_exps_isos_usa_k90_10pct <- process_exp_data_hist("20231206", "hist_bi_2100_10pct", 1990, aggregating = T)
fair_exps_isos_k90_10pct <- process_exp_data_hist("20231207", "hist_bi_10pct_2100", 1990, aggregating = T)

fair_exps_isos_usa_k90_30pct <- process_exp_data_hist("20231206", "hist_bi_2100_30pct", 1990, aggregating = T)
fair_exps_isos_k90_30pct <- process_exp_data_hist("20231207", "hist_bi_30pct_2100", 1990, aggregating = T)

fair_exps_isos_usa_k90_50pct <- process_exp_data_hist("20231206", "hist_bi_2100_50pct", 1990, aggregating = T)
fair_exps_isos_k90_50pct <- process_exp_data_hist("20231207", "hist_bi_50pct_2100", 1990, aggregating = T)

fair_exps_isos_usa_k90_70pct <- process_exp_data_hist("20231206", "hist_bi_2100_70pct", 1990, aggregating = T)
fair_exps_isos_k90_70pct <- process_exp_data_hist("20231207", "hist_bi_70pct_2100", 1990, aggregating = T)

################################################################################
##################### PART III: Calculate Total Damages ########################
################################################################################
## The first section of this part is to read the future forecast growth numbers 
## from the IPCC SSP3.7 scenario
# first, we start by reading the future forecast dataset
future_forecast_ssp370 <- readRDS("data/processed/future_forecast/future_forecast_ssp370.rds")
# through 2300 with 2100 numbers 
future_forecast_ssp370_2300 <- readRDS("data/processed/future_forecast/future_forecast_ssp370_2300.rds")
# through 2300 with 1%
future_forecast_ssp370_2300_1pct <- readRDS("data/processed/future_forecast/future_forecast_ssp370_2300_1pct.rds")
# through 2300 with 2%
future_forecast_ssp370_2300_2pct <- readRDS("data/processed/future_forecast/future_forecast_ssp370_2300_2pct.rds")

####################### generate country-year panel: #########################
# ok let us read the processed country-year panel data frames 
# for calculations with year k = 1990 and ending in 2100 
gdp_temp_data_k90 <- readRDS("data/processed/world_gdp_pop/gdp_temp_data_k90.rds")
# k = 1990 and ending in 2300 
gdp_temp_data_k90_2300 <- readRDS("data/processed/world_gdp_pop/gdp_temp_data_k90_2300.rds")
# k = 1990 and ending in 2300 with post 2100 growth rates fixed at 1%
gdp_temp_data_k90_2300_1pct <- readRDS("data/processed/world_gdp_pop/gdp_temp_data_k90_2300_1pct.rds")
# k = 1990 and ending in 2300 with post 2100 growth rates fixed at 2%
gdp_temp_data_k90_2300_2pct <- readRDS("data/processed/world_gdp_pop/gdp_temp_data_k90_2300_2pct.rds")
# for calculations with year k = 1980 and ending in 2100 
gdp_temp_data_k80 <- readRDS("data/processed/world_gdp_pop/gdp_temp_data_k80.rds")
# k = 1980 and ending in 2300
gdp_temp_data_k80_2300 <- readRDS("data/processed/world_gdp_pop/gdp_temp_data_k80_2300.rds")
# for calculations with year k = 1990 and ending in 2300 with lagged temperatures
gdp_temp_data_5lags_2300 <- readRDS("data/processed/world_gdp_pop/gdp_temp_data_5lags_2300.rds")
# now limited to 2100 
gdp_temp_data_5lags_2100 <- subset(gdp_temp_data_5lags_2300, year < 2101)
# for calculations with year k = 1960 and ending in 2100 
gdp_temp_data_k60 <- readRDS("data/processed/world_gdp_pop/gdp_temp_data_k60.rds")


# now let us create a dataset that includes all lagged temp and precip up to 10
# years behind to run our supplemental numbers for SCC under lower higher lags 
# than 5 
gdp_temp_data_10lags_2100 <- gdp_temp_data_5lags_2100
for (lag in 6:10) {
  gdp_temp_data_10lags_2100[[paste0("era_mwtemp_l", lag)]] <- plm::lag(gdp_temp_data_10lags_2100$era_mwtemp, lag)
  gdp_temp_data_10lags_2100[[paste0("era_mwprecip_l", lag)]] <- plm::lag(gdp_temp_data_10lags_2100$era_mwprecip, lag)
}
# also to the k - 1980 dataset 
gdp_temp_data_5lags_k80 <- gdp_temp_data_k80
for (lag in 1:5) {
  gdp_temp_data_5lags_k80[[paste0("era_mwtemp_l", lag)]] <- plm::lag(gdp_temp_data_5lags_k80$era_mwtemp, lag)
  gdp_temp_data_5lags_k80[[paste0("era_mwprecip_l", lag)]] <- plm::lag(gdp_temp_data_5lags_k80$era_mwprecip, lag)
}


# let us make sure that growth is bounded so that cumulative growth is calcuated
# sensibly (there only 2 observations w/growth <-1)
#gdp_temp_data_k80$diff_lgdp_for_damages[gdp_temp_data_k80$diff_lgdp_for_damages< -1] <- -0.99999999999
#gdp_temp_data_k90$diff_lgdp_for_damages[gdp_temp_data_k90$diff_lgdp_for_damages< -1] <- -0.99999999999


################### generate country-year regression model: ##################
# generating the pooled base model 
#bhm_era_reg <- run_bhm_model_reg("pooled")
#save(bhm_era_reg, file = "data/processed/bhm/bhm_era_reg.RData")
load("data/processed/bhm/bhm_era_reg.RData")

# generating the pooled lagged model regression
#bhm_era_reg_5lag_1 <- run_bhm_model_reg_lag5("pooled")
#save(bhm_era_reg_5lag, file = "data/processed/bhm/bhm_era_reg_5lag.RData")
load("data/processed/bhm/bhm_era_reg_5lag.RData")

# let us load the bhm coeffecients for different lagged models 
bhm_coefs <- readRDS("data/processed/bhm/distributedlag_differentlags.rds")

##############################################################################
############### calculate the total damages for each scenario ################
##############################################################################
# we will go over the different scenarios used in the paper. The below 
# code will produce the data we will report in the paper, as well as the 
# datasets we will use to visualize. 

##################### 1GtCO2/tCO2yr experiment ###########################
# The data produced under this section is used for the following 
# figures 
################################################################################ Figures 3a, 3b, s3, supplemental under diff lags

# first we need to set up the set of experimenet years to loop over inside the 
# custom-made function
years_of_exps_1990_2020 <- c(1990:2020)
years_of_exps_1980_2020 <- c(1980:2020)
years_of_exps_1980_2022 <- c(1980:2022)
years_of_exps_1990_2022 <- c(1990:2022)
years_of_exps_2020_2100 <- c(2020:2100)

# ok let us start with the 1gtco2 experiment (6 mins)  # fig2ab, fig2cd, figED5, figED7
total_damages_1gtco2_k90 <- calculate_damages_pulse_5lag(median_raster,
                                                         fair_exps_1gtco2_2100_k90, 
                                                         years_of_exps_1990_2020,
                                                         1990,
                                                         future_forecast_ssp370,
                                                         gdp_temp_data_5lags_2100,
                                                         "ERA",
                                                         2020,
                                                         F,
                                                         F,
                                                         F)

write_rds(total_damages_1gtco2_k90, paste0("data/output/", 
                                           run_date, 
                                           "/total_damages_1gtco2_1990_2020.rds"))

# now let us run the above number but under decreasing and increasing number of  figS3
# lags to cover the range 1:10 
##1lag 
total_damages_1gtco2_k90_5lag <- calculate_damages_pulse_lags(median_raster,
                                                              fair_exps_1gtco2_2100_k90, 
                                                              2020,
                                                              1990,
                                                              future_forecast_ssp370,
                                                              gdp_temp_data_10lags_2100,
                                                              "ERA",
                                                              2020,
                                                              F,
                                                              F,
                                                              F,
                                                              subset(bhm_coefs, lag == 5))


total_damages_1gtco2_k90_1lag <- calculate_damages_pulse_lags(median_raster,
                                                         fair_exps_1gtco2_2100_k90, 
                                                         2020,
                                                         1990,
                                                         future_forecast_ssp370,
                                                         gdp_temp_data_10lags_2100,
                                                         "ERA",
                                                         2020,
                                                         F,
                                                         F,
                                                         F,
                                                         subset(bhm_coefs, lag == 1))

##2lag 
total_damages_1gtco2_k90_2lag <- calculate_damages_pulse_lags(median_raster,
                                                              fair_exps_1gtco2_2100_k90, 
                                                              2020,
                                                              1990,
                                                              future_forecast_ssp370,
                                                              gdp_temp_data_10lags_2100,
                                                              "ERA",
                                                              2020,
                                                              F,
                                                              F,
                                                              F,
                                                              subset(bhm_coefs, lag == 2))
##3lag 
total_damages_1gtco2_k90_3lag <- calculate_damages_pulse_lags(median_raster,
                                                              fair_exps_1gtco2_2100_k90, 
                                                              2020,
                                                              1990,
                                                              future_forecast_ssp370,
                                                              gdp_temp_data_10lags_2100,
                                                              "ERA",
                                                              2020,
                                                              F,
                                                              F,
                                                              F,
                                                              subset(bhm_coefs, lag == 3))
##4lag 
total_damages_1gtco2_k90_4lag <- calculate_damages_pulse_lags(median_raster,
                                                              fair_exps_1gtco2_2100_k90, 
                                                              2020,
                                                              1990,
                                                              future_forecast_ssp370,
                                                              gdp_temp_data_10lags_2100,
                                                              "ERA",
                                                              2020,
                                                              F,
                                                              F,
                                                              F,
                                                              subset(bhm_coefs, lag == 4))

##6lag 
total_damages_1gtco2_k90_6lag <- calculate_damages_pulse_lags(median_raster,
                                                              fair_exps_1gtco2_2100_k90, 
                                                              2020,
                                                              1990,
                                                              future_forecast_ssp370,
                                                              gdp_temp_data_10lags_2100,
                                                              "ERA",
                                                              2020,
                                                              F,
                                                              F,
                                                              F,
                                                              subset(bhm_coefs, lag == 6))
##7lag 
total_damages_1gtco2_k90_7lag <- calculate_damages_pulse_lags(median_raster,
                                                              fair_exps_1gtco2_2100_k90, 
                                                              2020,
                                                              1990,
                                                              future_forecast_ssp370,
                                                              gdp_temp_data_10lags_2100,
                                                              "ERA",
                                                              2020,
                                                              F,
                                                              F,
                                                              F,
                                                              subset(bhm_coefs, lag == 7))
##8lag 
total_damages_1gtco2_k90_8lag <- calculate_damages_pulse_lags(median_raster,
                                                              fair_exps_1gtco2_2100_k90, 
                                                              2020,
                                                              1990,
                                                              future_forecast_ssp370,
                                                              gdp_temp_data_10lags_2100,
                                                              "ERA",
                                                              2020,
                                                              F,
                                                              F,
                                                              F,
                                                              subset(bhm_coefs, lag == 8))
##9lag 
total_damages_1gtco2_k90_9lag <- calculate_damages_pulse_lags(median_raster,
                                                              fair_exps_1gtco2_2100_k90, 
                                                              2020,
                                                              1990,
                                                              future_forecast_ssp370,
                                                              gdp_temp_data_10lags_2100,
                                                              "ERA",
                                                              2020,
                                                              F,
                                                              F,
                                                              F,
                                                              subset(bhm_coefs, lag == 9))
##10lag 
total_damages_1gtco2_k90_10lag <- calculate_damages_pulse_lags(median_raster,
                                                              fair_exps_1gtco2_2100_k90, 
                                                              2020,
                                                              1990,
                                                              future_forecast_ssp370,
                                                              gdp_temp_data_10lags_2100,
                                                              "ERA",
                                                              2020,
                                                              F,
                                                              F,
                                                              F,
                                                              subset(bhm_coefs, lag == 10))


#let us output the processed dataset with the calculated damages
write_rds(total_damages_1gtco2_k90_1lag, paste0("data/output/", 
                                           run_date, 
                                           "/total_damages_1gtco2_1lag_1990_2020.rds"))
write_rds(total_damages_1gtco2_k90_2lag, paste0("data/output/", 
                                                run_date, 
                                                "/total_damages_1gtco2_2lag_1990_2020.rds"))
write_rds(total_damages_1gtco2_k90_3lag, paste0("data/output/", 
                                                run_date, 
                                                "/total_damages_1gtco2_3lag_1990_2020.rds"))
write_rds(total_damages_1gtco2_k90_4lag, paste0("data/output/", 
                                                run_date, 
                                                "/total_damages_1gtco2_4lag_1990_2020.rds"))
write_rds(total_damages_1gtco2_k90_5lag, paste0("data/output/", 
                                                run_date, 
                                                "/total_damages_1gtco2_5lag_1990_2020.rds"))
write_rds(total_damages_1gtco2_k90_6lag, paste0("data/output/", 
                                                run_date, 
                                                "/total_damages_1gtco2_6lag_1990_2020.rds"))
write_rds(total_damages_1gtco2_k90_7lag, paste0("data/output/", 
                                                run_date, 
                                                "/total_damages_1gtco2_7lag_1990_2020.rds"))
write_rds(total_damages_1gtco2_k90_8lag, paste0("data/output/", 
                                                run_date, 
                                                "/total_damages_1gtco2_8lag_1990_2020.rds"))
write_rds(total_damages_1gtco2_k90_9lag, paste0("data/output/", 
                                                run_date, 
                                                "/total_damages_1gtco2_9lag_1990_2020.rds"))
write_rds(total_damages_1gtco2_k90_10lag, paste0("data/output/", 
                                                run_date, 
                                                "/total_damages_1gtco2_10lag_1990_2020.rds"))




################################################################################  # fig3, figED12, figED13
total_damages_1tco2_k80 <- calculate_damages_pulse_5lag(median_raster,
                                                        fair_exps_1tco2_2100_k80, 
                                                        years_of_exps_1980_2022,
                                                        1980,
                                                        future_forecast_ssp370,
                                                        gdp_temp_data_5lags_k80,
                                                        "ERA",
                                                        2020, 
                                                        F, 
                                                        F, 
                                                        F)

write_rds(total_damages_1tco2_k80, paste0("data/output/", 
                                       run_date, 
                                       "/total_damages_1tco2_k80.rds"))


##################### 1/10/1000/1M/1G/10G/100G/tCO2yr experiment ########################### figED9
# The data produced under this section is used for the following 
# figures 
# ok let us start with the 1tco2 experiment 
total_damages_1tco2_k90 <- calculate_damages_pulse_5lag(median_raster,
                                                        fair_exps_1tco2_2100_k90, 
                                                        1990,
                                                        1990,
                                                        future_forecast_ssp370,
                                                        gdp_temp_data_5lags_2100,
                                                        "ERA",
                                                        2020,
                                                        F,
                                                        F,
                                                        F)

# ok let us start with the 10tco2 experiment 
#total_damages_10tco2_k90 <- calculate_damages_pulse(median_raster,
#                                                    fair_exps_10tco2_2100_k90, 
#                                                    1990,
#                                                    1990,
#                                                    future_forecast_ssp370,
#                                                    gdp_temp_data_k90,
#                                                    "ERA",
#                                                    bhm_era_reg,
#                                                    F,
#                                                    "no",
#                                                    "no",
#                                                    2020, F)
# ok let us start with the 1000tco2 experiment
total_damages_1000tco2_k90 <- calculate_damages_pulse_5lag(median_raster,
                                                           fair_exps_1000tco2_2100_k90, 
                                                           1990,
                                                           1990,
                                                           future_forecast_ssp370,
                                                           gdp_temp_data_5lags_2100,
                                                           "ERA",
                                                           2020,
                                                           F,
                                                           F,
                                                           F)


# ok let us start with the 1mtco2 experiment 
total_damages_1mtco2_k90 <- calculate_damages_pulse_5lag(median_raster,
                                                         fair_exps_1Mtco2_2100_k90, 
                                                         1990,
                                                         1990,
                                                         future_forecast_ssp370,
                                                         gdp_temp_data_5lags_2100,
                                                         "ERA",
                                                         2020,
                                                         F,
                                                         F,
                                                         F)

# ok let us start with the 1gtco2 experiment 
# ok let us start with the 1gtco2 experiment (6 mins)  # fig2ab, fig2cd, fig3a, fig3b, 
total_damages_1gtco2_k90 <- calculate_damages_pulse_5lag(median_raster,
                                                         fair_exps_1gtco2_2100_k90, 
                                                         years_of_exps_1990_2020,
                                                         1990,
                                                         future_forecast_ssp370,
                                                         gdp_temp_data_5lags_2100,
                                                         "ERA",
                                                         2020,
                                                         F,
                                                         F,
                                                         F)

total_damages_1gtco2_k90 <- calculate_damages_pulse_5lag(median_raster,
                                                         fair_exps_1gtco2_2100_k90, 
                                                         1990,
                                                         1990,
                                                         future_forecast_ssp370,
                                                         gdp_temp_data_5lags_2100,
                                                         "ERA",
                                                         2020,
                                                         F,
                                                         F,
                                                         F)
# ok let us start with the 10gtco2 experiment
total_damages_10gtco2_k90 <- calculate_damages_pulse_5lag(median_raster,
                                                          fair_exps_10Gtco2_2100_k90, 
                                                          1990,
                                                          1990,
                                                          future_forecast_ssp370,
                                                          gdp_temp_data_5lags_2100,
                                                          "ERA",
                                                          2020,
                                                          F,
                                                          F,
                                                          F)
# ok let us start with the 100gtco2 experiment 
total_damages_100gtco2_k90 <- calculate_damages_pulse_5lag(median_raster,
                                                           fair_exps_100Gtco2_2100_k90, 
                                                           1990,
                                                           1990,
                                                           future_forecast_ssp370,
                                                           gdp_temp_data_5lags_2100,
                                                           "ERA",
                                                           2020,
                                                           F,
                                                           F,
                                                           F)

write_rds(total_damages_1tco2_k90, paste0("data/output/", run_date, "/total_damages_1tco2_k90_compare.rds"))
#write_rds(total_damages_10tco2_k90, paste0("data/output/", run_date, "/total_damages_10tco2_k90_compare.rds"))
write_rds(total_damages_1000tco2_k90, paste0("data/output/", run_date, "/total_damages_1000tco2_k90_compare.rds"))
write_rds(total_damages_1mtco2_k90, paste0("data/output/", run_date, "/total_damages_1mtco2_k90_compare.rds"))
write_rds(total_damages_1gtco2_k90, paste0("data/output/", run_date, "/total_damages_1gtco2_k90_compare.rds"))
write_rds(total_damages_10gtco2_k90, paste0("data/output/", run_date, "/total_damages_10gtco2_k90_compare.rds"))
write_rds(total_damages_100gtco2_k90, paste0("data/output/", run_date, "/total_damages_100gtco2_k90_compare.rds"))



######################## SCC Uncertainty Sources ############################ figED11
######################## Response function uncertainty
# we need to begin with generating country panel with the bootstraps
# NOTE: currently we produce this data using sherlock (the stanford server)
#num_cores <- detectCores() - 1
#registerDoParallel(num_cores)
##
##
### now let us generate the total damages by bootstrap loop
#pooledbs$coef_id <- 1:nrow(pooledbs)
#pooledbs$merge_id <- 1
#gdp_temp_data_k90_2300$merge_id <- 1
##
##
#tic()
### parallelize the loop using foreach (~ 58 minutes) - run this code on a server 
### where you can exploit multiple CPUs
#
#registerDoParallel(6)
#
#total_damages_1gtco2_bhm <- foreach(i=1:1000, .combine="rbind")%dopar%{
#  
#  laggedbs_i <- subset(laggedbs, coef_id == sample(unique(laggedbs$coef_id), 1))
#  #gdp_temp_data_i <- gdp_temp_data_5lags_2300
#  #gdp_temp_data_i <- left_join(gdp_temp_data_i, 
#  #                             laggedbs_i, 
#  #                             by = c("merge_id"))
#  
#  damages_i <- calculate_damages_pulse_5lag(median_raster, 
#                                       fair_exps_1tco2_2300_k90,
#                                       2020,
#                                       1990,
#                                       future_forecast_ssp370_2300,
#                                       gdp_temp_data_5lags_2300,
#                                       "ERA",
#                                       2020,
#                                       0,
#                                       F,
#                                       T)
#  
#  return(damages_i)
#}
#toc()
#
#write_rds(total_damages_1gtco2_bhm, paste0(output_path, "/total_damages_1gtco2_bhm.rds"))


######################## CGM Models uncertainty 
# generate empty list of dataframes to be filled in woth processed dataframes 
processed_dfs <- list()
for (i in 1:length(list_r_rasters)){
  tic()
  damages_i <- calculate_damages_pulse_5lag(list_r_rasters[[i]],
                                            fair_exps_1tco2_2300_k90,
                                            2020,
                                            1990,
                                            future_forecast_ssp370_2300,
                                            gdp_temp_data_5lags_2300,
                                            "ERA",
                                            2020,
                                            0,
                                            F, 
                                            F)
  damages_i$cgm_id <- i
  processed_dfs[[i]] <- damages_i 
  toc()
}

# now let us bring all dataframes into one 
total_damages_1gtco2_cgm <- do.call(rbind, processed_dfs)

# write the dataset
write_rds(total_damages_1gtco2_cgm, paste0(output_path, "/total_damages_1gtco2_cgm.rds"))
######################## FaIR uncertainty 
# in order to calculate the total damages under different FaIR runs (~ 15 mins)
# set up parallel backend
registerDoParallel(cores = 5)
######################## FaIR uncertainty 
# in order to calculate the total damages under different FaIR runs (~ 15 mins)
tic()
total_damages_1gtco2_fair <- foreach(i = 1:length(unique(fair_exps_1gtco2_disagg_2300$num_loop)), 
                                     .combine = "rbind") %dopar% {
                                       fair_i <- subset(fair_exps_1gtco2_disagg_2300, num_loop == i)
                                       damages_i <- calculate_damages_pulse_5lag(median_raster,
                                                                                 fair_i,
                                                                                 2020,
                                                                                 1990,
                                                                                 future_forecast_ssp370_2300,
                                                                                 gdp_temp_data_5lags_2300,
                                                                                 "ERA",
                                                                                 2020,
                                                                                 0, 
                                                                                 F, 
                                                                                 F)
                                       damages_i$fair_id <- i
                                       #write_rds(damages_i, paste0("~/Desktop/test/damages_", i, ".rds"))
                                       return(damages_i)
                                     }
toc()
# write the dataset
write_rds(total_damages_1gtco2_fair, paste0(output_path, "/total_damages_1gtco2_fair.rds"))

######################## SCC Total Uncertainty ############################
# now we need to calculate the total uncertainty. In order to execute this 
# task we need to sample from each of our sources of uncertainty and 
# calculated the resulting total damages. The total uncertainty is run
# through sherlock (Stanford's server) given the computationally intensive 
# requirements
######################### Country-level bidamages ############################
# now we canlculate the country level damages attributed to each of the countries 

################################################################################ figED15
# we start with k = 1980 
gdp_temp_data_k80_2020 <- subset(gdp_temp_data_k80, year <= 2020)
total_damages_k80 <- calculate_bidamages_bilateral(median_raster, 
                                                   fair_exps_isos_k80, 
                                                   unique(fair_exps_isos_k80$experiment_iso),
                                                   1980, 
                                                   future_forecast_ssp370,
                                                   gdp_temp_data_k80_2020,
                                                   bhm_era_reg_5lag,
                                                   2020)


# write teh dataframe in to the output arm of teh directory 
#write_rds(total_damages_k80, paste0(output_path, "/total_damages_k80_v2022.rds"))

################################################################################ fig4
# now let us do k = 1990 
gdp_temp_data_k90_2020 <- subset(gdp_temp_data_k90, year <= 2020)
total_damages_k90 <- calculate_bidamages_bilateral(median_raster, 
                                                   fair_exps_isos_k90, 
                                                   unique(fair_exps_isos_k90$experiment_iso),
                                                   1990, 
                                                   future_forecast_ssp370,
                                                   gdp_temp_data_k90_2020,
                                                   bhm_era_reg_5lag,
                                                   2020)

# write the dataframe
#write_rds(total_damages_k90, paste0(output_path, "/total_damages_k90_v2022.rds"))

################################################################################ figED17
# now let us just do consumption emissions 
#gdp_temp_data_k90 <- subset(gdp_temp_data_k90, year <= 2020) 
fair_exps_isos_k90_consump <- subset(fair_exps_isos_k90_consump, !is.na(median_deltat))
total_damages_k90_consump <- calculate_bidamages_bilateral(median_raster, 
                                                   fair_exps_isos_k90_consump, 
                                                   "USA",#unique(fair_exps_isos_k90_consump$experiment_iso),
                                                   1990, 
                                                   future_forecast_ssp370,
                                                   gdp_temp_data_k90_2020,
                                                   bhm_era_reg_5lag,
                                                   2020)
#write_rds(total_damages_k90_consump, paste0(output_path, "/total_damages_k90_consump_v2022.rds"))

################################################################################ figED18
# now let us just do production emissions 
fair_exps_isos_k90_prod <- subset(fair_exps_isos_k90_prod, !is.na(median_deltat))
total_damages_k90_prod <- calculate_bidamages_bilateral(median_raster, 
                                                        fair_exps_isos_k90_prod, 
                                                        "USA",#unique(fair_exps_isos_k90_prod$experiment_iso),
                                                        1990, 
                                                        future_forecast_ssp370,
                                                        gdp_temp_data_k90_2020,
                                                        bhm_era_reg_5lag,
                                                        2020)
write_rds(total_damages_k90_prod, paste0(output_path, "/total_damages_k90_prod_v2022.rds"))

################################################################################ figED16
# let's do k = 1960 
gdp_temp_data_k60_2020 <- subset(gdp_temp_data_k60, year <= 2020)
# we need to back-extrapolate the dataset to 1960 to calculate damages
gdp_temp_data_k60_2020 <- gdp_temp_data_k60_2020 %>% 
  dplyr::group_by(ISO3) %>%
  # Calculate year-over-year changes and take the average change for each country
  dplyr::mutate(
    annual_change = c(NA, diff(NY.GDP.PCAP.KD_for_damages)),
    avg_annual_change = mean(annual_change, na.rm = TRUE)
  ) %>%
  # Identify first available year and value, handle missing cases
  dplyr::mutate(
    first_non_na_year = min(year[!is.na(NY.GDP.PCAP.KD_for_damages)], na.rm = TRUE),
    first_non_na_value = ifelse(is.finite(first_non_na_year), 
                                NY.GDP.PCAP.KD_for_damages[year == first_non_na_year], 
                                NA_real_)
  ) %>%
  # Back-extrapolate with non-negative constraint
  dplyr::mutate(
    NY.GDP.PCAP.KD_for_damages = if_else(
      year < first_non_na_year & is.na(NY.GDP.PCAP.KD_for_damages), 
      pmax(first_non_na_value - avg_annual_change * (first_non_na_year - year), 0),
      NY.GDP.PCAP.KD_for_damages
    )
  ) %>%
  # Update the first non-zero value after the back-extrapolation
  dplyr::mutate(
    first_non_zero_value_after_back_extrapolation = 
      min(NY.GDP.PCAP.KD_for_damages[NY.GDP.PCAP.KD_for_damages > 0], na.rm = TRUE),
    # Replace 0 values with the first non-zero value after the extrapolation
    NY.GDP.PCAP.KD_for_damages = if_else(
      NY.GDP.PCAP.KD_for_damages == 0, 
      first_non_zero_value_after_back_extrapolation, 
      NY.GDP.PCAP.KD_for_damages
    )
  ) %>%
  ungroup()

gdp_temp_data_k60_2020 <- subset(gdp_temp_data_k60_2020, year > 1959 & year < 2021)
total_damages_k60 <- calculate_bidamages_bilateral(median_raster, 
                                                   fair_exps_isos_k60, 
                                                   "USA",#unique(fair_exps_isos_k60$experiment_iso),
                                                   1960, 
                                                   future_forecast_ssp370,
                                                   gdp_temp_data_k60_2020,
                                                   bhm_era_reg_5lag,
                                                   2020)
#write_rds(total_damages_k60, paste0(output_path, "/total_damages_k60_v2022.rds"))


############# 30%,50%,70%,90% emissions baseline experiment #################### figED9

## first we need to calculate delta T and damages under baseline emissions
### 10%
total_damages_k90_10pct <- calculate_bidamages_bilateral(median_raster, 
                                                   fair_exps_isos_k90_10pct, 
                                                   "pct",
                                                   1990, 
                                                   future_forecast_ssp370,
                                                   gdp_temp_data_k90_2020,
                                                   bhm_era_reg_5lag,
                                                   2020)
total_damages_k90_10pct$era_mwtemp <- total_damages_k90_10pct$era_mwtemp - total_damages_k90_10pct$deltat
total_damages_k90_10pct <- total_damages_k90_10pct %>% 
  dplyr::select(c("year", "ISO3", "era_mwtemp"))
gdp_temp_data_k90_2020_10pct <- gdp_temp_data_k90_2020 %>% 
  dplyr::select(-c("era_mwtemp")) %>% 
  dplyr::left_join(.,total_damages_k90_10pct, by = c("year", "ISO3"))
gdp_temp_data_k90_2020_10pct$response_tempactual_era <- ((gdp_temp_data_k90_2020_10pct$era_mwtemp)*(coef(bhm_era_reg_5lag)[1] + coef(bhm_era_reg_5lag)[3] + coef(bhm_era_reg_5lag)[5] + coef(bhm_era_reg_5lag)[7] + coef(bhm_era_reg_5lag)[9] + coef(bhm_era_reg_5lag)[11])) +
  (((gdp_temp_data_k90_2020_10pct$era_mwtemp)^2)*(coef(bhm_era_reg_5lag)[2] + coef(bhm_era_reg_5lag)[4] + coef(bhm_era_reg_5lag)[6] + coef(bhm_era_reg_5lag)[8] + coef(bhm_era_reg_5lag)[10] + coef(bhm_era_reg_5lag)[12])) 
usa_damages_10pct <- calculate_bidamages_bilateral(median_raster, 
                                                   fair_exps_isos_usa_k90_10pct, 
                                                   "USA",
                                                   1990, 
                                                   future_forecast_ssp370,
                                                   gdp_temp_data_k90_2020_10pct,
                                                   bhm_era_reg_5lag,
                                                   2020)
### 30% 
total_damages_k90_30pct <- calculate_bidamages_bilateral(median_raster, 
                                                   fair_exps_isos_k90_30pct, 
                                                   "pct",
                                                   1990, 
                                                   future_forecast_ssp370,
                                                   gdp_temp_data_k90_2020,
                                                   bhm_era_reg_5lag,
                                                   2020)
total_damages_k90_30pct$era_mwtemp <- total_damages_k90_30pct$era_mwtemp - total_damages_k90_30pct$deltat
total_damages_k90_30pct <- total_damages_k90_30pct %>% 
  dplyr::select(c("year", "ISO3", "era_mwtemp"))
gdp_temp_data_k90_2020_30pct <- gdp_temp_data_k90_2020 %>% 
  dplyr::select(-c("era_mwtemp")) %>% 
  dplyr::left_join(.,total_damages_k90_30pct, by = c("year", "ISO3"))
gdp_temp_data_k90_2020_30pct$response_tempactual_era <- ((gdp_temp_data_k90_2020_30pct$era_mwtemp)*(coef(bhm_era_reg_5lag)[1] + coef(bhm_era_reg_5lag)[3] + coef(bhm_era_reg_5lag)[5] + coef(bhm_era_reg_5lag)[7] + coef(bhm_era_reg_5lag)[9] + coef(bhm_era_reg_5lag)[11])) +
  (((gdp_temp_data_k90_2020_30pct$era_mwtemp)^2)*(coef(bhm_era_reg_5lag)[2] + coef(bhm_era_reg_5lag)[4] + coef(bhm_era_reg_5lag)[6] + coef(bhm_era_reg_5lag)[8] + coef(bhm_era_reg_5lag)[10] + coef(bhm_era_reg_5lag)[12])) 
usa_damages_30pct <- calculate_bidamages_bilateral(median_raster, 
                                                   fair_exps_isos_usa_k90_30pct, 
                                                   "USA",
                                                   1990, 
                                                   future_forecast_ssp370,
                                                   gdp_temp_data_k90_2020_30pct,
                                                   bhm_era_reg_5lag,
                                                   2020)

### 50% 
total_damages_k90_50pct <- calculate_bidamages_bilateral(median_raster, 
                                                         fair_exps_isos_k90_50pct, 
                                                         "pct",
                                                         1990, 
                                                         future_forecast_ssp370,
                                                         gdp_temp_data_k90_2020,
                                                         bhm_era_reg_5lag,
                                                         2020)
total_damages_k90_50pct$era_mwtemp <- total_damages_k90_50pct$era_mwtemp - total_damages_k90_50pct$deltat
total_damages_k90_50pct <- total_damages_k90_50pct %>% 
  dplyr::select(c("year", "ISO3", "era_mwtemp"))
gdp_temp_data_k90_2020_50pct <- gdp_temp_data_k90_2020 %>% 
  dplyr::select(-c("era_mwtemp")) %>% 
  dplyr::left_join(.,total_damages_k90_50pct, by = c("year", "ISO3"))
gdp_temp_data_k90_2020_50pct$response_tempactual_era <- ((gdp_temp_data_k90_2020_50pct$era_mwtemp)*(coef(bhm_era_reg_5lag)[1] + coef(bhm_era_reg_5lag)[3] + coef(bhm_era_reg_5lag)[5] + coef(bhm_era_reg_5lag)[7] + coef(bhm_era_reg_5lag)[9] + coef(bhm_era_reg_5lag)[11])) +
  (((gdp_temp_data_k90_2020_50pct$era_mwtemp)^2)*(coef(bhm_era_reg_5lag)[2] + coef(bhm_era_reg_5lag)[4] + coef(bhm_era_reg_5lag)[6] + coef(bhm_era_reg_5lag)[8] + coef(bhm_era_reg_5lag)[10] + coef(bhm_era_reg_5lag)[12])) 
usa_damages_50pct <- calculate_bidamages_bilateral(median_raster, 
                                                   fair_exps_isos_usa_k90_50pct, 
                                                   "USA",
                                                   1990, 
                                                   future_forecast_ssp370,
                                                   gdp_temp_data_k90_2020_50pct,
                                                   bhm_era_reg_5lag,
                                                   2020)

### 70% 
total_damages_k90_70pct <- calculate_bidamages_bilateral(median_raster, 
                                                         fair_exps_isos_k90_70pct, 
                                                         "pct",
                                                         1990, 
                                                         future_forecast_ssp370,
                                                         gdp_temp_data_k90_2020,
                                                         bhm_era_reg_5lag,
                                                         2020)

total_damages_k90_70pct$era_mwtemp <- total_damages_k90_70pct$era_mwtemp - total_damages_k90_70pct$deltat
total_damages_k90_70pct <- total_damages_k90_70pct %>% 
  dplyr::select(c("year", "ISO3", "era_mwtemp"))
gdp_temp_data_k90_2020_70pct <- gdp_temp_data_k90_2020 %>% 
  dplyr::select(-c("era_mwtemp")) %>% 
  dplyr::left_join(.,total_damages_k90_70pct, by = c("year", "ISO3"))
gdp_temp_data_k90_2020_70pct$response_tempactual_era <- ((gdp_temp_data_k90_2020_70pct$era_mwtemp)*(coef(bhm_era_reg_5lag)[1] + coef(bhm_era_reg_5lag)[3] + coef(bhm_era_reg_5lag)[5] + coef(bhm_era_reg_5lag)[7] + coef(bhm_era_reg_5lag)[9] + coef(bhm_era_reg_5lag)[11])) +
  (((gdp_temp_data_k90_2020_70pct$era_mwtemp)^2)*(coef(bhm_era_reg_5lag)[2] + coef(bhm_era_reg_5lag)[4] + coef(bhm_era_reg_5lag)[6] + coef(bhm_era_reg_5lag)[8] + coef(bhm_era_reg_5lag)[10] + coef(bhm_era_reg_5lag)[12])) 
usa_damages_70pct <- calculate_bidamages_bilateral(median_raster, 
                                                   fair_exps_isos_usa_k90_70pct, 
                                                   "USA",
                                                   1990, 
                                                   future_forecast_ssp370,
                                                   gdp_temp_data_k90_2020_70pct,
                                                   bhm_era_reg_5lag,
                                                   2020)


## Now let us write out the datasets 
write_rds(usa_damages_10pct, paste0(output_path, "/usa_damages_10pct.rds"))
write_rds(usa_damages_30pct, paste0(output_path, "/usa_damages_30pct.rds"))
write_rds(usa_damages_50pct, paste0(output_path, "/usa_damages_50pct.rds"))
write_rds(usa_damages_70pct, paste0(output_path, "/usa_damages_70pct.rds"))



######################## SCC Under Diff Scenarios ############################
################################################################################ figED10
scc_2300_2100_growth <- calculate_damages_pulse(median_raster,
                                                fair_exps_1tco2_2300_k90,
                                                2020,
                                                1990,
                                                future_forecast_ssp370_2300,
                                                gdp_temp_data_k90_2300,
                                                "ERA",
                                                bhm_era_reg,
                                                F,
                                                "NO",
                                                1 ,
                                                2020,
                                                F)
write_rds(scc_2300_2100_growth, paste0(output_path, "/scc_2300_2100_growth.rds"))

scc_2300_1pct_growth <- calculate_damages_pulse(median_raster,
                                                fair_exps_1tco2_2300_k90,
                                                2020,
                                                1990,
                                                future_forecast_ssp370_2300_1pct,
                                                gdp_temp_data_k90_2300_1pct,
                                                "ERA",
                                                bhm_era_reg,
                                                F,
                                                "NO",
                                                1 ,
                                                2020, 
                                                F)
write_rds(scc_2300_1pct_growth, paste0(output_path, "/scc_2300_1pct_growth.rds"))

scc_2300_2pct_growth <- calculate_damages_pulse(median_raster,
                                                fair_exps_1tco2_2300_k90,
                                                2020,
                                                1990,
                                                future_forecast_ssp370_2300_2pct,
                                                gdp_temp_data_k90_2300_2pct,
                                                "ERA",
                                                bhm_era_reg,
                                                F,
                                                "NO",
                                                1 ,
                                                2020, 
                                                F)
write_rds(scc_2300_2pct_growth, paste0(output_path, "/scc_2300_2pct_growth.rds"))

# now clamping 
scc_2300_clamped_growth <- calculate_damages_pulse(median_raster,
                                                   fair_exps_1tco2_2300_k90,
                                                   2020,
                                                   1990,
                                                   future_forecast_ssp370_2300,
                                                   gdp_temp_data_k90_2300,
                                                   "ERA",
                                                   bhm_era_reg,
                                                   F, 
                                                   "clamp_growth",
                                                   1,
                                                   2020, 
                                                   F)
write_rds(scc_2300_clamped_growth, paste0(output_path, "/scc_2300_clamped_growth.rds"))

# now 5 lag 
scc_2100_2100_5lag <-  calculate_damages_pulse_5lag(median_raster,
                                                   fair_exps_1gtco2_2100_k90,
                                                   2020,
                                                   1990,
                                                   future_forecast_ssp370,
                                                   gdp_temp_data_5lags_2100,
                                                   "ERA",
                                                   2020, 
                                                   F,
                                                   F,
                                                   F)

write_rds(scc_2100_2100_5lag, paste0(output_path, "/scc_2100_2100_5lag.rds"))

# now 5 lag through 2300
scc_2300_2100_5lag <- calculate_damages_pulse_5lag(median_raster,
                                                   fair_exps_1tco2_2300_k90,
                                                   2020,
                                                   1990,
                                                   future_forecast_ssp370_2300,
                                                   gdp_temp_data_5lags_2300,
                                                   "ERA",
                                                   2020,
                                                   1, # 1 to keep growth at >0 post 2100 and 0 otherwise
                                                   F,
                                                   F)

write_rds(scc_2300_2100_5lag, paste0(output_path, "/scc_2300_2100_5lag.rds"))

scc_2300_nog_post_2100 <- calculate_damages_pulse(median_raster,
                                                  fair_exps_1tco2_2300_k90,
                                                  2020,
                                                  1990,
                                                  future_forecast_ssp370_2300,
                                                  gdp_temp_data_k90_2300,
                                                  "ERA", 
                                                  bhm_era_reg,
                                                  F, 
                                                  "no",
                                                  0,
                                                  2020, 
                                                  F)
write_rds(scc_2300_nog_post_2100, paste0(output_path, "/scc_2300_nog_post_2100.rds"))

scc_2300_nogrowth <- calculate_damages_pulse_5lag(median_raster,
                                                  fair_exps_1tco2_2300_k90,
                                                  2020,
                                                  1990,
                                                  future_forecast_ssp370_2300,
                                                  gdp_temp_data_5lags_2300,
                                                  "ERA",
                                                  2020,
                                                  0,
                                                  F, 
                                                  F)
sum(scc_2300_nogrowth$weighted_damages2_scld, na.rm = T)
write_rds(scc_2300_nogrowth, paste0(output_path, "/scc_2300_2100_5lag_nog.rds"))


scc_2100_2100_5lag_adaptation <- calculate_damages_pulse_5lag(median_raster,
                                                              fair_exps_1tco2_2300_k90,
                                                              2020,
                                                              1990,
                                                              future_forecast_ssp370_2300,
                                                              gdp_temp_data_5lags_2100,
                                                              "ERA",
                                                              2020,
                                                              "no",
                                                              T, 
                                                              F)

write_rds(scc_2100_2100_5lag_adaptation, paste0(output_path, "/scc_2100_2100_5lag_adaptation.rds"))

scc_2300_2100_5lag_adaptation <- calculate_damages_pulse_5lag(median_raster,
                                                              fair_exps_1tco2_2300_k90,
                                                              2020,
                                                              1990,
                                                              future_forecast_ssp370_2300,
                                                              gdp_temp_data_5lags_2300,
                                                              "ERA",
                                                              2020,
                                                              "no",
                                                              T, 
                                                              F)
write_rds(scc_2300_2100_5lag_adaptation, paste0(output_path, "/scc_2300_2100_5lag_adaptation.rds"))

# now no effects post 2100 (this is just the original normal run)
scc_2100 <- calculate_damages_pulse(median_raster,
                                    fair_exps_1tco2_2100_k90,
                                    2020,
                                    1990,
                                    future_forecast_ssp370,
                                    gdp_temp_data_k90,
                                    "ERA", 
                                    bhm_era_reg,
                                    F, 
                                    "no",
                                    "no",
                                    2020, 
                                    F)
write_rds(scc_2100, paste0(output_path, "/scc_2100.rds"))

scc_2100_adaptation <- calculate_damages_pulse(median_raster,
                                               fair_exps_1tco2_2100_k90,
                                               2020,
                                               1990,
                                               future_forecast_ssp370,
                                               gdp_temp_data_5lags_2100,
                                               "ERA", 
                                               bhm_era_reg,
                                               F, 
                                               "no",
                                               "no",
                                               2020, 
                                               T)
write_rds(scc_2100_adaptation, paste0(output_path, "/scc_2100_adaptation.rds"))

scc_2100_2300_adaptation <- calculate_damages_pulse(median_raster,
                                                    fair_exps_1tco2_2300_k90,
                                                    2020,
                                                    1990,
                                                    future_forecast_ssp370_2300,
                                                    gdp_temp_data_k90_2300,
                                                    "ERA", 
                                                    bhm_era_reg,
                                                    F, 
                                                    "no",
                                                    "no",
                                                    2020, 
                                                    T)
write_rds(scc_2100_2300_adaptation, paste0(output_path, "/scc_2300_adaptation.rds"))

##################### Carbon Capture experiment ########################### figED19
total_damages_cc <- calculate_damages_pulse(median_raster,
                                            fair_exps_cc,
                                            years_of_exps_2020_2100,
                                            2020,
                                            future_forecast_ssp370, 
                                            gdp_temp_data_k90, 
                                            "ERA",
                                            bhm_era_reg,
                                            F,
                                            "no", 
                                            "no",
                                            2020, 
                                            F)

# write the dataframe into the output arm of the directory
write_rds(total_damages_cc, paste0(output_path, "/total_damages_cc.rds"))

# end of script 
