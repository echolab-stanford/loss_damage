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
# Last edited: March 2024
##############################################################################

################################################################################
################# PRE0: Clear env., read libs, source functions ################
################################################################################
remove(list=ls())
gc()
sf::sf_use_s2(FALSE)
setwd("~/GitHub/loss_damage")

# You have two options. The first option is to replicate the study (i.e. run the 
# the scripts to produce the data underlying the study). The other option is to 
# produce new data with updated parameters, such as fair parameters, year of 
# start of damages, among others 
replicate <- F# change T to F if you want to create your own data  
if (replicate == T){
  run_date <- "20230523"
}
if (replicate == F){
  run_date <- gsub("-","",Sys.Date())
}
#run_date <- "20230821"

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
source("scripts/working/analysis/3b0_run_bhm_model.R")
source("scripts/working/analysis/3b1_run_gdptemp_panel_5lag.R")
source("scripts/working/analysis/3c0_calc_total_damages_bilateral.R")
source("scripts/working/analysis/3c1_calc_total_damages.R")
source("scripts/working/analysis/3c2_calc_total_damages_5lags.R")

# let us set the path so we can read in the input data 
setwd(dropbox_path)

################################################################################
###################### PRE1: Read needed data for analysis #####################
################################################################################
# population country-year dataset
pop_wdi <- readRDS("data/processed/world_gdp_pop/pop_wdi.rds")

# ok let us read the processed raster and list of rasters for warming ratio
#mean_r_raster <- raster("data/processed/r_cgm/ratio_raster_avgs.tif")
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
fair_exps_1tco2_2100_k90 <- process_exp_data_hist_fut("20230523", "1tCO2_hist_2100", 1990, aggregating = T) # fig2e_i, figED7_a figED7_d figED7_J
fair_exps_1tco2_2100_k80 <- process_exp_data_hist_fut("20230821", "1tCO2_hist_2100", 1980, aggregating = T) # figED8, fig3c, 
## temperature response through 2300 
fair_exps_1tco2_2300_k90 <- process_exp_data_hist_fut("20230523", "1tCO2_hist_2300", 1990, aggregating = T) # fig3a, fig3b, figED7_i, figED7_j, figED7_k, figED7_l, figED7_m, figED7_n, figED7_o, figED13_i, figED13_k, figED13_l
### temperature response dis-aggregated. In other words all runs. 
fair_exps_1gtco2_disagg_2300 <- process_disagg_exp_data("20230809","1tCO2_hist_2300", 1990) # figED13_j

# 1GtCO2 
## temperature response through 2100 
fair_exps_1gtco2_2100_k90 <- process_exp_data_hist_fut("20230523", "1GtCO2_hist_2100", 1990, aggregating = T) # fig2ab, figcd, figED4, figED9b, 
### temperature response dis-aggregated. In other words all runs. 
fair_exps_1gtco2_disagg_2100 <- process_disagg_exp_data("20230523","1GtCO2_hist_2100", 1990) #fig2e_i, fig2e_j, fig2e_k
#write_rds(fair_exps_1gtco2_disagg_2300, "~/BurkeLab Dropbox/Projects/loss_damage/sherlock_files_060223/fair_exps_disagg_20230822.rds")

####################### Experiment (Carbon Capture): ########################
# this experiment is to estimate the damages if we are to capture 1 tCO2 
# years after emitting it
fair_exps_cc <- process_exp_data_hist_fut("20230822", "cc_hist", 2020, aggregating = T) # figED9cd
# we will need this data saved for plotting cc figure (S6)
#write_rds(fair_exps_cc, paste0(output_path, "/fair_exps_cc.rds"))

################ Experiment (Country-level emissions): #################
# this experiment is to estimate the country level damages attributed to each 
# of the countries.
# for year_k = 1980
fair_exps_isos_k80 <- process_exp_data_hist("20230523", "hist_bi_v2022", 1980, aggregating = T) # figED10
# for year_k = 1990
fair_exps_isos_k90 <- process_exp_data_hist("20230523", "hist_bi_v2022", 1990, aggregating = T) # fig4
#for year_k = 1990 and only consumption emissions
fair_exps_isos_k90_consump <- process_exp_data_hist("20230523", "hist_biconsump_v2022", 1990, aggregating = T) # figED11
#for year_k = 1990 and only production emissions
fair_exps_isos_k90_prod <- process_exp_data_hist("20230523", "hist_biprod_v2022", 1990, aggregating = T) # figED12

####################### Experiment (1/10/1000/1M/1G/10G/100G/tCO2/yr): ########################
# this experiment is run to estimate the temperature effects of pulsing 
# 1GtCO2 or 1tCO2 at a given year.
#fair_exps_10tco2_2100_k90 <- process_exp_data_hist_fut("20230807","10tCO2_hist_2300",1990,aggregating = T)
fair_exps_1000tco2_2100_k90 <- process_exp_data_hist_fut("20230807","1000tCO2_hist_2300",1990,aggregating = T) # figED6_i
fair_exps_1Mtco2_2100_k90 <- process_exp_data_hist_fut("20230807","1MtCO2_hist_2300",1990,aggregating = T) # figED6_j
fair_exps_10Gtco2_2100_k90 <- process_exp_data_hist_fut("20230807","10GtCO2_hist_2300",1990,aggregating = T) # figED6_l
fair_exps_100Gtco2_2100_k90 <- process_exp_data_hist_fut("20230807","100GtCO2_hist_2300",1990,aggregating = T) # figED6_m

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

gdp_temp_data_5lags_2300$diff_lgdp_for_damages[is.na(gdp_temp_data_5lags_2300$diff_lgdp_for_damages) & (gdp_temp_data_5lags_2300$year > 2014 & gdp_temp_data_5lags_2300$year <2021)] <- 0
gdp_temp_data_5lags_2300$diff_lgdp_for_damages[is.na(gdp_temp_data_5lags_2300$diff_lgdp_for_damages) & (gdp_temp_data_5lags_2300$year == 1990)]  <- 0

write_rds(gdp_temp_data_5lags_2300, "~/Desktop/gdp_temp_data_5lags_2300+20240311.rds")

gdp_temp_data_5lags_2100$diff_lgdp_for_damages[is.na(gdp_temp_data_5lags_2100$diff_lgdp_for_damages) & (gdp_temp_data_5lags_2100$year > 2014 & gdp_temp_data_5lags_2100$year <2021)] <- 0
gdp_temp_data_5lags_2100$diff_lgdp_for_damages[is.na(gdp_temp_data_5lags_2100$diff_lgdp_for_damages) & (gdp_temp_data_5lags_2100$year == 1990)]  <- 0
# before going on make sure canada and other countries' data are included 


################### generate country-year regression model: ##################
# generating the pooled base model 
#bhm_era_reg <- run_bhm_model_reg("pooled")
#save(bhm_era_reg, file = "data/processed/bhm/bhm_era_reg.RData")
load("data/processed/bhm/bhm_era_reg.RData")

# generating the pooled lagged model regression
#bhm_era_reg_5lag <- run_bhm_model_reg_lag5("pooled")
#save(bhm_era_reg, file = "data/processed/bhm/bhm_era_reg.RData")

-(coef(bhm_era_reg_5lag)[1]+
  coef(bhm_era_reg_5lag)[3] + 
coef(bhm_era_reg_5lag)[5] +
coef(bhm_era_reg_5lag)[7] +
coef(bhm_era_reg_5lag)[9] +
coef(bhm_era_reg_5lag)[11]) / (2*(coef(bhm_era_reg_5lag)[2]+
                                    coef(bhm_era_reg_5lag)[4] + 
                                    coef(bhm_era_reg_5lag)[6] +
                                    coef(bhm_era_reg_5lag)[8] +
                                    coef(bhm_era_reg_5lag)[10] +
                                    coef(bhm_era_reg_5lag)[12]))

##############################################################################
############### calculate the total damages for each scenario ################
##############################################################################

# we will go over the different scenarios used in the paper. The below 
# code will produce the data we will report in the paper, as well as the 
# datasets we will use to visualize. 
##################### 1GtCO2/tCO2yr experiment ###########################
# The data produced under this section is used for the following 
# figures 

################################################################################ Figures 3a, 3b, s3

# first we need to set up the set of experimenet years to loop over inside the 
# custom-made function
years_of_exps_1990_2020 <- c(1990:2020)
years_of_exps_1980_2020 <- c(1980:2020)
years_of_exps_1980_2022 <- c(1980:2022)
years_of_exps_1990_2022 <- c(1990:2022)
years_of_exps_2020_2100 <- c(2020:2100)

# ok let us start with the 1gtco2 experiment (6 mins)  # fig2ab, fig2cd, fig3a, fig3b
total_damages_1gtco2_k90 <- calculate_damages_pulse(median_raster,
                                                    fair_exps_1gtco2_2100_k90, 
                                                    years_of_exps_1990_2022,
                                                    1990,
                                                    future_forecast_ssp370,
                                                    gdp_temp_data_k90,
                                                    "ERA",
                                                    bhm_era_reg,
                                                    F,
                                                    "no",
                                                    "no",
                                                    2020)

write_rds(total_damages_1gtco2_k90, paste0("data/output/", 
                                           run_date, 
                                           "/total_damages_1gtco2_1990_2022.rds"))

################################################################################  # fig3c
total_damages_1tco2_k80 <- calculate_damages_pulse(median_raster,
                                                   fair_exps_1tco2_2100_k80, 
                                                   years_of_exps_1980_2022,
                                                   1980,
                                                   future_forecast_ssp370,
                                                   gdp_temp_data_k80,
                                                   "ERA",
                                                   bhm_era_reg,
                                                   F, 
                                                   "no",
                                                   "no",
                                                   2020)
write_rds(total_damages_1tco2_k80, paste0("data/output/", 
                                       run_date, 
                                       "/total_damages_1tco2_1980_2022.rds"))


##################### 1/10/1000/1M/1G/10G/100G/tCO2yr experiment ########################### figED6
# The data produced under this section is used for the following 
# figures 
# ok let us start with the 1tco2 experiment 
total_damages_1tco2_k90 <- calculate_damages_pulse(median_raster,
                                                    fair_exps_1tco2_2100_k90, 
                                                    1990,
                                                    1990,
                                                    future_forecast_ssp370,
                                                    gdp_temp_data_k90,
                                                    "ERA",
                                                    bhm_era_reg,
                                                    F,
                                                    "no",
                                                    "no",
                                                    2020)

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
#                                                    2020)
# ok let us start with the 1000tco2 experiment
total_damages_1000tco2_k90 <- calculate_damages_pulse(median_raster,
                                                    fair_exps_1000tco2_2100_k90, 
                                                    1990,
                                                    1990,
                                                    future_forecast_ssp370,
                                                    gdp_temp_data_k90,
                                                    "ERA",
                                                    bhm_era_reg,
                                                    F,
                                                    "no",
                                                    "no",
                                                    2020)
# ok let us start with the 1mtco2 experiment 
total_damages_1mtco2_k90 <- calculate_damages_pulse(median_raster,
                                                    fair_exps_1Mtco2_2100_k90, 
                                                    1990,
                                                    1990,
                                                    future_forecast_ssp370,
                                                    gdp_temp_data_k90,
                                                    "ERA",
                                                    bhm_era_reg,
                                                    F,
                                                    "no",
                                                    "no",
                                                    2020)
# ok let us start with the 1gtco2 experiment 
total_damages_1gtco2_k90 <- calculate_damages_pulse(median_raster,
                                                    fair_exps_1gtco2_2100_k90, 
                                                    1990,
                                                    1990,
                                                    future_forecast_ssp370,
                                                    gdp_temp_data_k90,
                                                    "ERA",
                                                    bhm_era_reg,
                                                    F,
                                                    "no",
                                                    "no",
                                                    2020)
# ok let us start with the 10gtco2 experiment
total_damages_10gtco2_k90 <- calculate_damages_pulse(median_raster,
                                                    fair_exps_10Gtco2_2100_k90, 
                                                    1990,
                                                    1990,
                                                    future_forecast_ssp370,
                                                    gdp_temp_data_k90,
                                                    "ERA",
                                                    bhm_era_reg,
                                                    F,
                                                    "no",
                                                    "no",
                                                    2020)
# ok let us start with the 100gtco2 experiment 
total_damages_100gtco2_k90 <- calculate_damages_pulse(median_raster,
                                                    fair_exps_100Gtco2_2100_k90, 
                                                    1990,
                                                    1990,
                                                    future_forecast_ssp370,
                                                    gdp_temp_data_k90,
                                                    "ERA",
                                                    bhm_era_reg,
                                                    F,
                                                    "no",
                                                    "no",
                                                    2020)

write_rds(total_damages_1tco2_k90, paste0("data/output/", run_date, "/total_damages_1tco2_k90_compare.rds"))
#write_rds(total_damages_10tco2_k90, paste0("data/output/", run_date, "/total_damages_10tco2_k90_compare.rds"))
write_rds(total_damages_1000tco2_k90, paste0("data/output/", run_date, "/total_damages_1000tco2_k90_compare.rds"))
write_rds(total_damages_1mtco2_k90, paste0("data/output/", run_date, "/total_damages_1mtco2_k90_compare.rds"))
write_rds(total_damages_1gtco2_k90, paste0("data/output/", run_date, "/total_damages_1gtco2_k90_compare.rds"))
write_rds(total_damages_10gtco2_k90, paste0("data/output/", run_date, "/total_damages_10gtco2_k90_compare.rds"))
write_rds(total_damages_100gtco2_k90, paste0("data/output/", run_date, "/total_damages_100gtco2_k90_compare.rds"))


######################## SCC Uncertainty Sources ############################
######################## Response function uncertainty
# we need to begin with generating country panel with the bootstraps
#num_cores <- detectCores() - 1
#registerDoParallel(num_cores)
#
#
## now let us generate the total damages by bootstrap loop
#pooledbs$coef_id <- 1:nrow(pooledbs)
#pooledbs$merge_id <- 1
#gdp_temp_data_k90_2300$merge_id <- 1
#
#
#tic()
## parallelize the loop using foreach (~ 58 minutes) - run this code on a server 
## where you can exploit multiple CPUs
#total_damages_1gtco2_bhm <- foreach(i=1:1000, .combine="rbind")%dopar%{
#
#  pooledbs_i <- subset(pooledbs, coef_id == i)
#  gdp_temp_data_i <- gdp_temp_data_k90_2300
#  gdp_temp_data_i <- left_join(gdp_temp_data_i, 
#                               pooledbs_i, 
#                               by = c("merge_id"))
#  
#  damages_i <- calculate_damages_pulse(mean_r_raster, 
#                                       fair_exps_1gtco2_2300_k90,
#                                       2020,
#                                       1990,
#                                       future_forecast_ssp370_2300,
#                                       gdp_temp_data_i,
#                                       "ERA",
#                                       bhm_era_reg,
#                                       T, 
#                                       "NO",
#                                       0,
#                                       2020)
#   
#  return(damages_i)
# }
#toc()
#
#write_rds(total_damages_1gtco2_bhm, paste0(output_path, "/total_damages_1gtco2_bhm.rds"))


######################## CGM Models uncertainty #figED13
# generate empty list of dataframes to be filled in woth processed dataframes 
processed_dfs <- list()
for (i in 1:length(list_r_rasters)){
  tic()
  damages_i <- calculate_damages_pulse(list_r_rasters[[i]],
                                       fair_exps_1tco2_2300_k90,
                                       2020,
                                       1990,
                                       future_forecast_ssp370_2300,
                                       gdp_temp_data_k90_2300,
                                       "ERA",
                                       bhm_era_reg,
                                       F,
                                       "NO",
                                       0,
                                       2020)
  
  damages_i$cgm_id <- i
  processed_dfs[[i]] <- damages_i 
  toc()
}

# now let us bring all dataframes into one 
total_damages_1gtco2_cgm <- do.call(rbind, processed_dfs)

# write the dataset
write_rds(total_damages_1gtco2_cgm, paste0(output_path, "/total_damages_1gtco2_cgm.rds"))

######################## FaIR uncertainty #figED13
# in order to calculate the total damages under different FaIR runs (~ 15 mins)
tic()
total_damages_1gtco2_fair <- foreach(i = 1:length(unique(fair_exps_1gtco2_disagg_2300$num_loop)), 
              .combine = "rbind") %dopar% {
  fair_i <- subset(fair_exps_1gtco2_disagg_2300, num_loop == i)
  damages_i <- calculate_damages_pulse(median_raster,
                                       fair_i,
                                       2020,
                                       1990,
                                       future_forecast_ssp370_2300,
                                       gdp_temp_data_k90_2300,
                                       "ERA",
                                       bhm_era_reg,
                                       F,
                                       "NO",
                                       0,
                                       2020)
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
# calculated the resulting total damages. 
#tic()
#total_damages_1gtco2_total <- foreach(i = 601:1000, .combine = "rbind") %dopar% {
#  fair_exps_1gtc_disagg_i <- subset(fair_exps_1gtco2_disagg_2100, 
#                                    num_loop == sample(unique(fair_exps_1gtco2_disagg_2100$num_loop), 1))
#  pooledbs_i <- subset(pooledbs, coef_id == i)
#  gdp_temp_data_sbstd <- left_join(gdp_temp_data_k90, 
#                                   pooledbs_i, 
#                                   by = c("merge_id"))
#  damages_i <- calculate_damages_pulse(list_r_rasters[[sample(1:29, 1)]],
#                                                fair_exps_1gtc_disagg_i, 
#                                                2020, 
#                                                1990,
#                                                future_forecast_ssp370,
#                                                gdp_temp_data_sbstd,
#                                                "ERA",
#                                                bhm_era_reg,
#                                                T,
#                                                "no",
#                                                "no",
#                                                2020)
#  
#  damages_i$iter_id <- i
#  return(damages_i)
#  
#}
#toc()
#
#write_rds(total_damages_1gtco2_total, paste0(output_path, run_date,"/total_damages_1gtco2_total3.rds"))

######################### Country-level bidamages ############################
# now we canlculate the country level damages attributed to each of the countries 

################################################################################ Figures 1, Sankeys (4, S?)

# we start with k = 1980 #figED10
gdp_temp_data_k80_2020 <- subset(gdp_temp_data_k80, year <= 2020)
total_damages_k80 <- calculate_bidamages_bilateral(median_raster, 
                                                   fair_exps_isos_k80, 
                                                   unique(fair_exps_isos_k80$experiment_iso),
                                                   1980, 
                                                   future_forecast_ssp370,
                                                   gdp_temp_data_k80_2020,
                                                   bhm_era_reg,
                                                   2020)

# write teh dataframe in to the output arm of teh directory 
#write_rds(total_damages_k80, "data/output/041023/total_damages_k80_v2022.rds")
#write_rds(total_damages_k80, "data/output/060223/total_damages_k80_v2022.rds")
write_rds(total_damages_k80, paste0(output_path, "/total_damages_k80_v2022.rds"))

# now let us do k = 1990 #fig4
gdp_temp_data_k90_2020 <- subset(gdp_temp_data_k90, year <= 2020)
total_damages_k90 <- calculate_bidamages_bilateral(median_raster, 
                                                   fair_exps_isos_k90, 
                                                   unique(fair_exps_isos_k90$experiment_iso),
                                                   1990, 
                                                   future_forecast_ssp370,
                                                   gdp_temp_data_k90_2020,
                                                   bhm_era_reg,
                                                   2020)

# write the dataframe
#write_rds(total_damages_k90, "data/output/060223/total_damages_k90_v2022.rds")
#write_rds(total_damages_k90, "data/output/041023/total_damages_k90_v2022.rds")
write_rds(total_damages_k90, paste0(output_path, "/total_damages_k90_v2022.rds"))

# now let us just do consumption emissions 
#gdp_temp_data_k90 <- subset(gdp_temp_data_k90, year <= 2020) #figED11
fair_exps_isos_k90_consump <- subset(fair_exps_isos_k90_consump, !is.na(median_deltat))
total_damages_k90_consump <- calculate_bidamages_bilateral(median_raster, 
                                                   fair_exps_isos_k90_consump, 
                                                   unique(fair_exps_isos_k90_consump$experiment_iso),
                                                   1990, 
                                                   future_forecast_ssp370,
                                                   gdp_temp_data_k90_2020,
                                                   bhm_era_reg,
                                                   2020)

#write_rds(total_damages_k90_consump, "data/output/060223/total_damages_k90_consump_v2022.rds")
write_rds(total_damages_k90_consump, paste0(output_path, "/total_damages_k90_consump_v2022.rds"))

#sum(total_damages_k90$weighted_damages2[total_damages_k90$emitter == "USA" & total_damages_k90$weighted_damages2 < 0], na.rm = T)

# now let us just do production emissions #figED12
#gdp_temp_data_k90 <- subset(gdp_temp_data_k90, year <= 2020)
fair_exps_isos_k90_prod <- subset(fair_exps_isos_k90_prod, !is.na(median_deltat))
total_damages_k90_prod <- calculate_bidamages_bilateral(median_raster, 
                                                        fair_exps_isos_k90_prod, 
                                                        unique(fair_exps_isos_k90_prod$experiment_iso),
                                                        1990, 
                                                        future_forecast_ssp370,
                                                        gdp_temp_data_k90_2020,
                                                        bhm_era_reg,
                                                        2020)

#write_rds(total_damages_k90_prod, "data/output/060223/total_damages_k90_prod_v2022.rds")
write_rds(total_damages_k90_prod, paste0(output_path, "/total_damages_k90_prod_v2022.rds"))

######################## SCC Under Diff Scenarios ############################

################################################################################ figED7

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
                                                2020)

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
                                                2020)
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
                                                2020)
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
                                                   2020)
write_rds(scc_2300_clamped_growth, paste0(output_path, "/scc_2300_clamped_growth.rds"))

# now 5 lag 
scc_2100_2100_5lag <- calculate_damages_pulse_5lag(median_raster,
                                                   fair_exps_1tco2_2300_k90,
                                                   2020,
                                                   1990,
                                                   future_forecast_ssp370,
                                                   gdp_temp_data_5lags_2100,
                                                   "ERA",
                                                   2020)


write_rds(scc_2100_2100_5lag, paste0(output_path, "/scc_2100_2100_5lag.rds"))

# now 5 lag through 2300
scc_2300_2100_5lag <- calculate_damages_pulse_5lag(median_raster,
                                                   fair_exps_1tco2_2300_k90,
                                                   2020,
                                                   1990,
                                                   future_forecast_ssp370_2300,
                                                   gdp_temp_data_5lags_2300,
                                                   "ERA",
                                                   2020)

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
                                                  2020)

write_rds(scc_2300_nog_post_2100, paste0(output_path, "/scc_2300_nog_post_2100.rds"))

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
                                    2020)

write_rds(scc_2100, paste0(output_path, "/scc_2100.rds"))

##################### Carbon Capture experiment ###########################
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
                                            2020)

#sum(total_damages_cc$weighted_damages2_scld, na.rm = T)/1000000000
# we need to re run the cc becasue it is not exactly 204 

# write the dataframe into the output arm of the directory
write_rds(total_damages_cc, paste0(output_path, "/total_damages_cc.rds"))

# end of script 
