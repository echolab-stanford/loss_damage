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
# Last edited: June 2023
##############################################################################

################################################################################
################# PRE0: Clear env., read libs, source functions ################
################################################################################
remove(list=ls())
gc()
sf::sf_use_s2(FALSE)
setwd("~/GitHub/loss_damage")

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
source("scripts/working/analysis/3c0_calc_total_damages_bilateral.R")
source("scripts/working/analysis/3c1_calc_total_damages.R")
source("scripts/working/analysis/3c2_calc_total_damages_5lags.R")

setwd(dropbox_path)

################################################################################
###################### PRE1: Read needed data for analysis #####################
################################################################################
# population country-year dataset
pop_wdi <- readRDS("data/processed/world_gdp_pop/pop_wdi.rds")

# ok let us read the processed raster and list of rasters for warming ratio
mean_r_raster <- raster("data/processed/r_cgm/ratio_raster_avgs.tif")
list_r_rasters <- readRDS("data/processed/r_cgm/ratio_raster_list.rds")

# we need to aggregate the deltat to the country level by weighting by pop
# so let us read the pop raster and resample it to match coordinates and 
# convert to a dataframe and then join
pop <- raster("data/raw/population/gpw_v4_population_count_rev11_2010_1_deg.tif")
pop <- readAll(pop)
pop <- resample(pop, mean_r_raster)

# world shapefile 
world <- spData::world
world <- st_as_sf(world)
world <- subset(world, name_long != "Antarctica")
world$ISO3 <- countrycode::countrycode(sourcevar = world$iso_a2,origin = "iso2c",
                                       destination = "iso3c"
)
world <- subset(world, !is.na(ISO3))
#plot(world["geom"])


#################################################################################
##################### PART I: Calculate CGM Warming Ratio #######################
#################################################################################
# calculate deltaTs. Specify the needed cgm models and read the model names so we 
# can use to call models and rename output

#cgm_guide <- read_excel("scripts/working/analysis/cgm_model_guide.xlsx")

# Now we will create a warming ratio raster per each of the models
#for (i in unique(cgm_guide$cgm_model)){
#  tic()
#  raster_deltaT_calced1 <- calculate_grid_warming_ratio("historical", "ssp370", i)
#  assign(paste0("raster_deltaT_", i), raster_deltaT_calced1)
#  toc()
#}

# now we have a raster for each of the models where we have grid level warming 
# ratio relative to global warming 

# let us create one raster that is the average of all the rasters
# master_raster <- mean(`raster_deltaT_ACCESS-CM2_r1i1p1f1`,
#                      `raster_deltaT_ACCESS-ESM1-5_r1i1p1f1`,
#                      `raster_deltaT_AWI-CM-1-1-MR_r1i1p1f1`,
#                      `raster_deltaT_BCC-CSM2-MR_r1i1p1f1`,
#                      raster_deltaT_CanESM5_r1i1p1f1,
#                      `raster_deltaT_CAS-ESM2-0_r1i1p1f1`,
#                      raster_deltaT_CESM2_r10i1p1f1,#
#                      `raster_deltaT_CESM2-WACCM_r1i1p1f1`,
#                      `raster_deltaT_CMCC-CM2-SR5_r1i1p1f1`,
#                      `raster_deltaT_CMCC-ESM2_r1i1p1f1`,
#                      `raster_deltaT_CNRM-CM6-1_r1i1p1f2`,
#                      `raster_deltaT_CNRM-CM6-1-HR_r1i1p1f2`,
#                      `raster_deltaT_FGOALS-f3-L_r1i1p1f1`,
#                      `raster_deltaT_FGOALS-g3_r1i1p1f1`,
#                      `raster_deltaT_GFDL-ESM4_r1i1p1f1`,
#                      `raster_deltaT_GISS-E2-1-G_r1i1p1f2`,
#                      `raster_deltaT_IITM-ESM_r1i1p1f1`,
#                      `raster_deltaT_INM-CM4-8_r1i1p1f1`,
#                      `raster_deltaT_IPSL-CM5A2-INCA_r1i1p1f1`,
#                      `raster_deltaT_IPSL-CM6A-LR_r1i1p1f1`,
#                      `raster_deltaT_KACE-1-0-G_r1i1p1f1`,
#                      `raster_deltaT_MIROC-ES2L_r1i1p1f2`,
#                      raster_deltaT_MIROC6_r1i1p1f1,
#                      `raster_deltaT_MPI-ESM1-2-LR_r10i1p1f1`,
#                      `raster_deltaT_MRI-ESM2-0_r1i1p1f1`,
#                      `raster_deltaT_NorESM2-LM_r1i1p1f1`,
#                      `raster_deltaT_NorESM2-MM_r1i1p1f1`,
#                      raster_deltaT_TaiESM1_r1i1p1f1,
#                      `raster_deltaT_UKESM1-0-LL_r1i1p1f2`)#

# let us put the rasters in a list in case we want to loop over them
#list_rasters <- list(`raster_deltaT_ACCESS-CM2_r1i1p1f1`,
#                     `raster_deltaT_ACCESS-ESM1-5_r1i1p1f1`,
#                     `raster_deltaT_AWI-CM-1-1-MR_r1i1p1f1`,
#                     `raster_deltaT_BCC-CSM2-MR_r1i1p1f1`,
#                     raster_deltaT_CanESM5_r1i1p1f1,
#                     `raster_deltaT_CAS-ESM2-0_r1i1p1f1`,
#                     raster_deltaT_CESM2_r10i1p1f1,
#                     `raster_deltaT_CESM2-WACCM_r1i1p1f1`,
#                     `raster_deltaT_CMCC-CM2-SR5_r1i1p1f1`,
#                     `raster_deltaT_CMCC-ESM2_r1i1p1f1`,
#                     `raster_deltaT_CNRM-CM6-1_r1i1p1f2`,
#                     `raster_deltaT_CNRM-CM6-1-HR_r1i1p1f2`,
#                     `raster_deltaT_FGOALS-f3-L_r1i1p1f1`,
#                     `raster_deltaT_FGOALS-g3_r1i1p1f1`,
#                     `raster_deltaT_GFDL-ESM4_r1i1p1f1`,
#                     `raster_deltaT_GISS-E2-1-G_r1i1p1f2`,
#                     `raster_deltaT_IITM-ESM_r1i1p1f1`,
#                     `raster_deltaT_INM-CM4-8_r1i1p1f1`,
#                     `raster_deltaT_IPSL-CM5A2-INCA_r1i1p1f1`,
#                     `raster_deltaT_IPSL-CM6A-LR_r1i1p1f1`,
#                     `raster_deltaT_KACE-1-0-G_r1i1p1f1`,
#                     `raster_deltaT_MIROC-ES2L_r1i1p1f2`,
#                     raster_deltaT_MIROC6_r1i1p1f1,
#                     `raster_deltaT_MPI-ESM1-2-LR_r10i1p1f1`,
#                     `raster_deltaT_MRI-ESM2-0_r1i1p1f1`,
#                     `raster_deltaT_NorESM2-LM_r1i1p1f1`,
#                     `raster_deltaT_NorESM2-MM_r1i1p1f1`,
#                     raster_deltaT_TaiESM1_r1i1p1f1,
#                     `raster_deltaT_UKESM1-0-LL_r1i1p1f2`)
#
# save raster 
#writeRaster(master_raster, "data/figs/fig1/ratio_raster_avgs.tif")
#writeRaster(master_raster, "data/processed/r_cgm/ratio_raster_avgs.tif")
#raster::writeRaster(master_raster, "sherlock_files/data/pre_processed/master_raster.tif")
#save(list_rasters, file = "sherlock_files/data/pre_processed/list_rasters.RData")
#saveRDS(list_rasters, "data/processed/r_cgm/ratio_raster_list.rds")

# ok let us read the processed raster and list of rasters for warming ratio
#mean_r_raster <- raster("data/processed/r_cgm/ratio_raster_avgs.tif")
#list_r_rasters <- readRDS("data/processed/r_cgm/ratio_raster_list.rds")

################################################################################
##################### PART II: Read and compute delta T ########################
################################################################################
# we have a set of experiments that we have ran through FaIR to recover deltat 
# from each emissions preturbation. Below we go through the different experiments. 
# The temperature response data were obtained by running FaIR through jupyter notebook.

####################### Experiment (1G/tCO2/yr): ########################
# this experiment is run to estimate the temperature effects of pulsing 
# 1GtCO2 or 1tCO2 at a given year.

## 1tCO2 
# temperature response through 2100 
fair_exps_1tco2_2100 <- process_exp_data_hist_fut("20230523", "1tCO2_hist_2100", 1990, aggregating = T)
# temperature response through 2300 
fair_exps_1tco2_2300 <- process_exp_data_hist_fut("20230523", "1tCO2_hist_2300", 1990, aggregating = T)
# now let us calc deltat and return all the runs so that we can run 
# uncertainty analysis
#fair_exps_1tco2_disagg <- process_disagg_exp_data("20230523","1tCO2_hist_2300", 1990)
fair_exps_1gtco2_disagg_2100 <- process_disagg_exp_data("20230523","1GtCO2_hist_2100", 1990)
# now we need to run the experiment for 1gtco2 instead of 1tco2
fair_exps_1gtco2_2100 <- process_exp_data_hist_fut("20230523","1GtCO2_hist_2100", 1990, aggregating = T)

####################### Experiment (Carbon Capture): ########################
# this experiment is to estimate the damages if we are to capture 1 tCO2 
# years after emitting it
fair_exps_cc <- process_exp_data_hist_fut("20230410", "cc_hist", 2020, aggregating = T)

####################### Experiment (Carbon debt): ######################
# this experiment is to estimate the deltaT resulting from emissions 
# sources of certain known emitters (individuals/entities)
# Alternatively, we can use the numbers we get from the tco2/yr experiment
# to look at the carbon debt. The latter is the cuuren approach. As such, we 
# are going to stick with just running 1tco2 experiment 

################ Experiment (Country-level emissions): #################
# this experiment is to estimate the country level damages attributed to each 
# of the countries.

# for year_k = 1980
fair_exps_isos_k80 <- process_exp_data_hist("20230411", "hist_bi_v2022", 1980, aggregating = T)
# for year_k = 1990
fair_exps_isos_k90 <- process_exp_data_hist("20230411", "hist_bi_v2022", 1990, aggregating = T)
#for year_k = 1990 and only consumption emissions
fair_exps_isos_k90_consump <- process_exp_data_hist("20230411", "hist_biconsump_v2022", 1990, aggregating = T)
#for year_k = 1990 and only production emissions
fair_exps_isos_k90_prod <- process_exp_data_hist("20230411", "hist_biprod_v2022", 1990, aggregating = T)

################################################################################
##################### PART III: Calculate Total Damages ########################
################################################################################

## The first section of this part is to read the world bank augmented gdp 
## panel dataset and combine it with the temperature-year panel

# first, we start by reading the future forecast dataset
future_forecast_ssp370 <- readRDS("data/processed/future_forecast/future_forecast_ssp370.rds")
# through 2300 with 2100 numbers 
future_forecast_ssp370_2300 <- readRDS("data/processed/future_forecast/future_forecast_ssp370_2300.rds")
# through 2300 with 1%
future_forecast_ssp370_2300_1pct <- readRDS("data/processed/future_forecast/future_forecast_ssp370_2300_1pct.rds")
# through 2300 with 2%
future_forecast_ssp370_2300_2pct <- readRDS("data/processed/future_forecast/future_forecast_ssp370_2300_2pct.rds")

####################### generate country-year panel: #########################
# Now we need to generate the country-year panel data frames 
#for k = 1990
#gdp_temp_data_k90 <- generate_gdptemp_panel("pooled", 
#                                        future_forecast_ssp370, 
#                                        "1990", 
#                                        "ERA")
#write_rds(gdp_temp_data_k90, "data/processed/world_gdp_pop/gdp_temp_data_k90.rds")


#for k = 1990 & into 2300 at 2100 growth rates
#gdp_temp_data_k90_2300 <- generate_gdptemp_panel("pooled", 
#                                            future_forecast_ssp370_2300, 
#                                            "1990", 
#                                            "ERA")
#

#write_rds(gdp_temp_data_k90_2300, "data/processed/world_gdp_pop/gdp_temp_data_k90_2300.rds")
#for k = 1990 & into 2300 at 1% grwoth rate
#gdp_temp_data_k90_2300_1pct <- generate_gdptemp_panel("pooled", 
#                                                 future_forecast_ssp370_2300_1pct, 
#                                                 "1990", 
#                                                 "ERA")

#write_rds(gdp_temp_data_k90_2300_1pct, "data/processed/world_gdp_pop/gdp_temp_data_k90_2300_1pct.rds")

#for k = 1990 & into 2300 at 2% grwoth rate
#gdp_temp_data_k90_2300_2pct <- generate_gdptemp_panel("pooled", 
#                                                 future_forecast_ssp370_2300_2pct, 
#                                                 "1990", 
#                                                 "ERA")
##write_rds(gdp_temp_data_k90_2300_2pct, "data/processed/world_gdp_pop/gdp_temp_data_k90_2300_2pct.rds")

#for k = 1980 
#gdp_temp_data_k80 <- generate_gdptemp_panel("pooled", 
#                                            future_forecast_ssp370, 
#                                            "1980", 
#                                            "ERA")
#write_rds(gdp_temp_data_k80, "data/processed/world_gdp_pop/gdp_temp_data_k80.rds")

#gdp_temp_data_k80_2300 <- generate_gdptemp_panel("pooled", 
#                                            future_forecast_ssp370_2300, 
#                                            "1980", 
#                                            "ERA")
#write_rds(gdp_temp_data_k80_2300, "data/processed/world_gdp_pop/gdp_temp_data_k80_2300.rds")

#gdp_temp_data_5lags <- generate_gdptemp_panel_5lags("pooled", 
#                                        future_forecast_ssp370, 
#                                        "1990", 
#                                        "ERA")

#write_rds(gdp_temp_data_5lags, "data/processed/world_gdp_pop/gdp_temp_data_5lags.rds")


# ok let us read the processed country-year panel data frames 
gdp_temp_data_k90 <- readRDS("data/processed/world_gdp_pop/gdp_temp_data_k90.rds")
gdp_temp_data_k90_2300 <- readRDS("data/processed/world_gdp_pop/gdp_temp_data_k90_2300.rds")
gdp_temp_data_k90_2300_1pct <- readRDS("data/processed/world_gdp_pop/gdp_temp_data_k90_2300_1pct.rds")
gdp_temp_data_k90_2300_2pct <- readRDS("data/processed/world_gdp_pop/gdp_temp_data_k90_2300_2pct.rds")
gdp_temp_data_k80 <- readRDS("data/processed/world_gdp_pop/gdp_temp_data_k80.rds")
gdp_temp_data_k80_2300 <- readRDS("data/processed/world_gdp_pop/gdp_temp_data_k80_2300.rds")
gdp_temp_data_5lags <- readRDS("data/processed/world_gdp_pop/gdp_temp_data_5lags.rds")

################# generate country-year bootstrapped panel: ##################
pooledbs <- as.data.frame(readRDS("data/processed/bhm/pooledregression_boostraps_era.rds"))
#gdp_temp_data_bhmbs <- generate_gdptemp_panel_bhmbs("pooled", future_forecast_ssp370_cru, 1990, "CRU")
#write_rds(gdp_temp_data_bhmbs, "data/processed/world_gdp_pop/gdp_temp_data_bhmbs.rds")
#gdp_temp_data_bhmbs <- generate_gdptemp_panel_bhmbs("pooled",
#                                                    future_forecast_ssp370,
#                                                    1990,
#                                                    "ERA")

gdp_temp_data_bhmbs <- readRDS("data/processed/world_gdp_pop/gdp_temp_data_bhmbs.rds")

################### generate country-year regression model: ##################
# as well as the regression model
#bhm_era_reg <- run_bhm_model_reg("pooled")

#save(bhm_era_reg, file = "data/processed/bhm/bhm_era_reg.RData")
load("data/processed/bhm/bhm_era_reg.RData")

############### generate country-year regression model: richvpoor ##############
# now we do the same but with the rich/poor. Here rich/poor is constructed by 
# looking at average of each country's GDP  relative to median of full sample
# panel...
#gdp_temp_data <- run_bhm_model("richpoor")
# model...
#bhm_cru_reg <- run_bhm_model_reg("richpoor")

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


# ok let us start with the 1gtco2 experiment (6 mins)
total_damages_1gtco2 <- calculate_damages_pulse(mean_r_raster,
                                            fair_exps_1gtco2_2100, 
                                            years_of_exps_1990_2020,
                                            1990,
                                            future_forecast_ssp370,
                                            gdp_temp_data_k90,
                                            "ERA",
                                            bhm_era_reg,
                                            "no",
                                            "no",
                                            2020)

write_rds(total_damages_1gtco2, paste0("data/processed/", 
                                       gsub("-", "", Sys.Date()), 
                                       "/total_damages_1gtco2_1990_2020.rds"))



################################################################################ Figures 3c

######################## SCC Uncertainty Sources ############################
######################## Response function uncertainty
# we need to begin with generating country panel with the bootstraps
num_cores <- detectCores() - 1
registerDoParallel(num_cores)


# now let us generate the total damages by bootstrap loop
pooledbs$coef_id <- 1:nrow(pooledbs)
pooledbs$merge_id <- 1
gdp_temp_data_k90$merge_id <- 1

tic()
# parallelize the loop using foreach (~ 33 minutes) - run this code on a server 
# where you can exploit multiple CPUs
total_damages_1gtco2_bhm <- foreach(i=1:300, .combine="rbind")%dopar%{

  pooledbs_i <- subset(pooledbs, coef_id == i)
  gdp_temp_data_i <- gdp_temp_data_k90
  gdp_temp_data_i <- left_join(gdp_temp_data_i, 
                               pooledbs_i, 
                               by = c("merge_id"))
  
  damages_i <- calculate_damages_pulse(mean_r_raster, 
                                       fair_exps_1gtco2_2100,
                                       2020,
                                       1990,
                                       future_forecast_ssp370,
                                       gdp_temp_data_i,
                                       "ERA",
                                       bhm_era_reg,
                                       "NO",
                                       "NO",
                                       2020)
  
  return(damages_i)
 }
toc()

write_rds(total_damages_1gtco2_bhm, paste0(output_path, "total_damages_1gtco2_bhm.rds"))

######################## CGM Models uncertainty
# generate empty list of dataframes to be filled in woth processed dataframes
processed_dfs <- list()
for (i in 1:length(list_r_rasters)){
  tic()
  damages_i <- calculate_damages_pulse(list_r_rasters[[i]],
                                       fair_exps_1gtco2_2100,
                                       2020,
                                       1990,
                                       future_forecast_ssp370,
                                       gdp_temp_data_k90,
                                       "ERA",
                                       bhm_era_reg,
                                       "NO",
                                       "NO",
                                       2020)
  
  damages_i$cgm_id <- i
  processed_dfs[[i]] <- damages_i 
  toc()
}

# now let us bring all dataframes into one 
total_damages_1gtco2_cgm <- do.call(rbind, processed_dfs)

# write the dataset
write_rds(total_damages_1gtco2_cgm, paste0(output_path, "total_damages_1gtco2_cgm.rds"))

######################## FaIR uncertainty
# in order to calculate the total damages under different FaIR runs

total_damages_1gtco2_fair <- foreach(i = 1:length(unique(fair_exps_1gtco2_disagg_2100$num_loop)), 
              .combine = "rbind") %dopar% {
  fair_i <- subset(fair_exps_1gtco2_disagg_2100, num_loop == i)
  damages_i <- calculate_damages_pulse(mean_r_raster,
                                       fair_i,
                                       2020,
                                       1990,
                                       future_forecast_ssp370,
                                       gdp_temp_data_k90,
                                       "ERA",
                                       bhm_era_reg,
                                       "NO",
                                       "NO",
                                       2020)
  damages_i$fair_id <- i
  #write_rds(damages_i, paste0("~/Desktop/test/damages_", i, ".rds"))
  return(damages_i)
              }

# write the dataset
write_rds(total_damages_1gtco2_fair, paste0(output_path, "total_damages_1gtco2_fair.rds"))

######################## SCC Total Uncertainty ############################
# now we need to calculate the total uncertainty. In order to execute this 
# task we need to sample from each of our sources of uncertainty and 
# calculated the resulting total damages. 
tic()
total_damages_1gtco2_total <- foreach(i = 1:300, .combine = "rbind") %dopar% {
  fair_exps_1gtc_disagg_i <- subset(fair_exps_1gtco2_disagg_2100, 
                                    num_loop == sample(unique(fair_exps_1gtco2_disagg_2100$num_loop), 1))
  pooledbs_i <- subset(pooledbs, coef_id == i)
  gdp_temp_data_sbstd <- left_join(gdp_temp_data_k90, 
                                   pooledbs_i, 
                                   by = c("merge_id"))
  damages_i <- calculate_damages_pulse(list_r_rasters[[sample(1:29, 1)]],
                                                fair_exps_1gtc_disagg_i, 
                                                2020, 
                                                1990,
                                                future_forecast_ssp370,
                                                gdp_temp_data_sbstd,
                                                "ERA",
                                                bhm_era_reg,
                                                "no",
                                                "no",
                                                2020)
  
  damages_i$iter_id <- i
  return(damages_i)
  
}
toc()

write_rds(total_damages_1gtco2_total, paste0(output_path, "total_damages_1gtco2_total.rds"))

######################### Country-level bidamages ############################
# now we canlculate the country level damages attributed to each of the countries 

################################################################################ Figures 1, Sankeys (4, S?)


# we start with k = 1980 
gdp_temp_data_k80 <- subset(gdp_temp_data_k80, year <= 2020)
total_damages_k80 <- calculate_bidamages_bilateral(mean_r_raster, 
                                                   fair_exps_isos_k80, 
                                                   unique(fair_exps_isos_k80$experiment_iso),
                                                   1980, 
                                                   future_forecast_ssp370,
                                                   gdp_temp_data_k80,
                                                   bhm_era_reg,
                                                   2020)

# write teh dataframe in to the output arm of teh directory 
write_rds(total_damages_k80, "data/output/041023/total_damages_k80_v2022.rds")
write_rds(total_damages_k80, "data/output/060223/total_damages_k80_v2022.rds")

# now let us do k = 1990 
gdp_temp_data_k90 <- subset(gdp_temp_data_k90, year <= 2020)
total_damages_k90 <- calculate_bidamages_bilateral(mean_r_raster, 
                                                   fair_exps_isos_k90, 
                                                   unique(fair_exps_isos_k90$experiment_iso),
                                                   1990, 
                                                   future_forecast_ssp370,
                                                   gdp_temp_data_k90,
                                                   bhm_era_reg,
                                                   2020)

# write the dataframe
write_rds(total_damages_k90, "data/output/060223/total_damages_k90_v2022.rds")
write_rds(total_damages_k90, "data/output/041023/total_damages_k90_v2022.rds")

# now let us just do consumption emissions 
gdp_temp_data_k90 <- subset(gdp_temp_data_k90, year <= 2020)
fair_exps_isos_k90_consump <- subset(fair_exps_isos_k90_consump, !is.na(median_deltat))
total_damages_k90_consump <- calculate_bidamages_bilateral(mean_r_raster, 
                                                   fair_exps_isos_k90_consump, 
                                                   unique(fair_exps_isos_k90_consump$experiment_iso),
                                                   1990, 
                                                   future_forecast_ssp370,
                                                   gdp_temp_data_k90,
                                                   bhm_era_reg,
                                                   2020)

write_rds(total_damages_k90_consump, "data/output/060223/total_damages_k90_consump_v2022.rds")

#sum(total_damages_k90$weighted_damages2[total_damages_k90$emitter == "USA" & total_damages_k90$weighted_damages2 < 0], na.rm = T)

# now let us just do consumption emissions 
gdp_temp_data_k90 <- subset(gdp_temp_data_k90, year <= 2020)
fair_exps_isos_k90_prod <- subset(fair_exps_isos_k90_prod, !is.na(median_deltat))
total_damages_k90_prod <- calculate_bidamages_bilateral(mean_r_raster, 
                                                        fair_exps_isos_k90_prod, 
                                                        unique(fair_exps_isos_k90_prod$experiment_iso),
                                                        1990, 
                                                        future_forecast_ssp370,
                                                        gdp_temp_data_k90,
                                                        bhm_era_reg,
                                                        2020)

write_rds(total_damages_k90_prod, "data/output/060223/total_damages_k90_prod_v2022.rds")

######################## SCC Under Diff Scenarios ############################

################################################################################ Figures S4

scc_2300_2100_growth <- calculate_bidamages(mean_r_raster,
                                            fair_exps_1tCO2_2300,
                                            2020,
                                            1990,
                                            future_forecast_ssp370_2300,
                                            gdp_temp_data_k90_2300,
                                            "ERA",
                                            bhm_era_reg,
                                            "NO",
                                            1 ,
                                            2020)
write_rds(scc_2300_2100_growth, paste0(output_path, "scc_2300_2100_growth.rds"))

scc_2300_1pct_growth <- calculate_bidamages(mean_r_raster,
                                            fair_exps_1tCO2_2300,
                                            2020,
                                            1990,
                                            future_forecast_ssp370_2300_1pct,
                                            gdp_temp_data_k90_2300_1pct,
                                            "ERA",
                                            bhm_era_reg,
                                            "NO",
                                            1 ,
                                            2020)
write_rds(scc_2300_1pct_growth, paste0(output_path, "scc_2300_1pct_growth.rds"))

scc_2300_2pct_growth <- calculate_bidamages(mean_r_raster,
                                            fair_exps_1tCO2_2300,
                                            2020,
                                            1990,
                                            future_forecast_ssp370_2300_2pct,
                                            gdp_temp_data_k90_2300_2pct,
                                            "ERA",
                                            bhm_era_reg,
                                            "NO",
                                            1 ,
                                            2020)
write_rds(scc_2300_2pct_growth, paste0(output_path, "scc_2300_2pct_growth.rds"))



# now clamping 
scc_2300_clamped_growth <- calculate_bidamages(mean_r_raster,
                                               fair_exps_1tCO2_2300,
                                               2020,
                                               1990,
                                               future_forecast_ssp370_2300,
                                               gdp_temp_data_k90_2300,
                                               "ERA",
                                               bhm_era_reg,
                                               "clamp_growth",
                                               1,
                                               2020)
write_rds(scc_2300_clamped_growth, paste0(output_path, "scc_2300_clamped_growth.rds"))


# now 5 lag (this one is lag, let us see how this will go)
scc_2300_2100_5lag <- calculate_bidamages_5lag(mean_r_raster,
                                               fair_exps_1tCO2_2300,
                                               2020,
                                               1990,
                                               future_forecast_ssp370_2300,
                                               gdp_temp_data_5lags,
                                               "ERA",
                                               2020)
write_rds(scc_2300_2100_5lag, paste0(output_path, "scc_2300_2100_5lag.rds"))


scc_2300_nog_post_2100 <- calculate_bidamages(mean_r_raster,
                                              fair_exps_1tCO2_2300,
                                              2020,
                                              1990,
                                              future_forecast_ssp370_2300,
                                              gdp_temp_data_k90_2300,
                                              "ERA", 
                                              bhm_era_reg,
                                              "no",
                                              0,
                                              2020)
write_rds(scc_2300_nog_post_2100, paste0(output_path, "scc_2300_nog_post_2100.rds"))

# now no effects post 2100 (this is just the original normal run)
scc_2100 <- calculate_bidamages(mean_r_raster,
                                fair_exps_1tCO2_2100,
                                2020,
                                1990,
                                future_forecast_ssp370,
                                gdp_temp_data_k90,
                                "ERA", 
                                bhm_era_reg,
                                "no",
                                "no",
                                2020)
write_rds(scc_2300_nog_post_2100, paste0(output_path, "scc_2100.rds"))



##################### Carbon Capture experiment ###########################
total_damages_cc <- calculate_damages_pulse(mean_r_raster,
                                            fair_exps_cc,
                                            2100,
                                            2020,
                                            future_forecast_ssp370, 
                                            gdp_temp_data_k90, 
                                            "ERA",
                                            bhm_era_reg,
                                            "no", 
                                            "no",
                                            2020)

# write the dataframe into the output arm of the directory
write_rds(total_damages_cc, paste0(output_path, "total_damages_cc.rds"))

# end of script 
