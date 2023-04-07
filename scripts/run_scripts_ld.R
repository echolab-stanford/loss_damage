##############################################################################
# Mustafa Zahid, May 27th, 2022
# This script has been and will be updated throughout the project.  This code 
# will be up in github and any changes to teh code will be committed to the 
# repo in github
# This is the master script and it sources the functions from other scripts in 
# the same directory.
# the initial part of the script contains a fair amount of processing. As such 
# I will save the processed data from this script and just read it whenever 
# I need to run the code. The processing code will be commented out 
##############################################################################

################################################################################
################ PRE: Clear env., read libs, create macro data #################
################################################################################
remove(list=ls())
gc()
sf::sf_use_s2(FALSE)

#read in the needed libraries 
source("~/BurkeLab Dropbox/Projects/loss_damage/scripts/working/0_read_libs.R")

# read world bank data to be used later on in calculation for GDP and POP
#wdi_dat <- wb_data(indicator = c("NY.GDP.PCAP.KD", "SP.POP.TOTL", "NY.GDP.PCAP.CD", "NY.GDP.PCAP.PP.KD"),
                   #                   country = "countries_only",
                   #                   start_date = 1960, end_date = 2020,
                   #                   return_wide = T)

#write_rds(wdi_dat, "~/Desktop/wdi_dat.rds")
# rename columns
#colnames(wdi_dat)[4] <- "year"

# before processing and using the GDP numbers, we need to rebase the numbers 
# from 2015 numbers to 2020. In the next step we will want to  fill in missing years 
# with Penn World Table data. The Penn World Table end in 2019 and as such we 
# cannot base Penn table data to 2020 from 2017 (the original base). So what 
# we are going to do is rebase WB data to 2017, then merge data fill in missing 
# and then rebase to 2020
# first let us get the 2017 index by dividing each country's current 2017 value 
# by the constant 2015 equivalent 
#wdi_dat <- wdi_dat %>% dplyr::group_by(iso3c) %>% 
#  dplyr::mutate(index_2017 = NY.GDP.PCAP.CD[year == "2017"] / NY.GDP.PCAP.KD[year == "2017"])

# now that we have the index we can rescale the gdp numbers to be in 2017 USD 
# we are going to do that by multuplying our 2017 ratio by the 2015 constant 
# USD value. I'm not going to give the new scaled value a different name 
# since the scripts are already written using the 2015 constant name
#wdi_dat$NY.GDP.PCAP.KD <- wdi_dat$NY.GDP.PCAP.KD * wdi_dat$index_2017

# select columns for a specific pop-country-year datsset
#pop_wdi <- wdi_dat %>% dplyr::select(c("iso3c", "SP.POP.TOTL", "year")) 

# Now the world bank data has missing values for some countries. For example. 
# Ethiopia seem to be missing a couple of decades of data 
#library("pwt10")

# let us read the pwt data. 
#pwt_dta <- pwt10::pwt10.0
# select the needed data 
#pwt_dta <- pwt_dta %>% dplyr::select(c("isocode", "year", "rgdpna", "pop"))


#wdi_dat$ppp_d <- wdi_dat$NY.GDP.PCAP.KD / wdi_dat$NY.GDP.PCAP.PP.KD


# rename the columns before merging 
#colnames(pwt_dta) <- c("iso3c", "year", "pwt_gdp", "pwt_pop")

# now we will need to merge both datasets 
#wdi_dat <- left_join(wdi_dat,
#                    pwt_dta,
#                    by = c("iso3c", "year"))


# now let us assign to the country it is ppp
#wdi_dat <- wdi_dat %>% dplyr::group_by(iso3c) %>% 
  #  dplyr::mutate(ppp_d = mean(ppp_d, na.rm = T))

#wdi_dat$pwt_gdppcap <- (wdi_dat$pwt_gdp)/ (wdi_dat$pwt_pop)
#wdi_dat$pwt_gdppcap <- wdi_dat$pwt_gdppcap * wdi_dat$ppp_d

#plot(wdi_dat$NY.GDP.PCAP.PP.KD, (wdi_dat$pwt_gdp/wdi_dat$pwt_pop))

# now that we have both merged, let us fill in the missing cells 
#wdi_dat$NY.GDP.PCAP.KD[is.na(wdi_dat$NY.GDP.PCAP.KD)] <- wdi_dat$pwt_gdppcap[is.na(wdi_dat$NY.GDP.PCAP.KD)]


# now let us rescale data so that it is expressed in 2020 dollars 
#wdi_dat <- wdi_dat %>% dplyr::group_by(iso3c) %>% 
  #  dplyr::mutate(index_2020 = NY.GDP.PCAP.CD[year == "2020"] / NY.GDP.PCAP.KD[year == "2020"])

#wdi_dat$NY.GDP.PCAP.KD <- wdi_dat$NY.GDP.PCAP.KD * wdi_dat$index_2020


#write_rds(wdi_dat, "~/BurkeLab Dropbox/Projects/loss_damage/data/processed/wdi_dat.rds")
#write_rds(pop_wdi, "~/BurkeLab Dropbox/Projects/loss_damage/data/processed/pop_wdi.rds")

# read the processed data 
wdi_dat <- readRDS("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/world_gdp_pop/wdi_dat.rds")
pop_wdi <- readRDS("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/world_gdp_pop/pop_wdi.rds")

#################################################################################
##################### PART I: Calculate CGM Warming Ratio #######################
#################################################################################
##calculate deltaTs 
#source("~/BurkeLab Dropbox/Projects/loss_damage/scripts/working/1_r_cgm.R")
#
## specify the needed cgm models and read the model names so we can use to 
## call models and rename output
#library("readxl")
#cgm_guide <- read_excel("~/BurkeLab Dropbox/Projects/loss_damage/scripts/working/cgm_model_guide.xlsx")
#
## Now we will create a warming ratio raster per each of the models
#for (i in unique(cgm_guide$cgm_model)){
#  tic()
#  raster_deltaT_calced1 <- calculate_grid_warming_ratio("historical", "ssp370", i)
#  assign(paste0("raster_deltaT_", i), raster_deltaT_calced1)
#  toc()
#}
#
# now we have a raster for each of the models where we have grid level relative 
# warming ratio
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
#writeRaster(master_raster, "~/Burkelab Dropbox/Projects/loss_damage/data/figs/fig1/ratio_raster_avgs.tif")
#writeRaster(master_raster, "~/Burkelab Dropbox/Projects/loss_damage/data/processed/r_cgm/ratio_raster_avgs.tif")
#saveRDS(list_rasters, "~/Burkelab Dropbox/Projects/loss_damage/data/processed/r_cgm/ratio_raster_list.rds")

# now let us remove the individual rasters so that we can empty the environment 
#remove(`raster_deltaT_ACCESS-CM2_r1i1p1f1`,
#       `raster_deltaT_ACCESS-ESM1-5_r1i1p1f1`,
#       `raster_deltaT_AWI-CM-1-1-MR_r1i1p1f1`,
#       `raster_deltaT_BCC-CSM2-MR_r1i1p1f1`,
#       raster_deltaT_CanESM5_r1i1p1f1,
#       `raster_deltaT_CAS-ESM2-0_r1i1p1f1`,
#       raster_deltaT_CESM2_r10i1p1f1,
#       `raster_deltaT_CESM2-WACCM_r1i1p1f1`,
#       `raster_deltaT_CMCC-CM2-SR5_r1i1p1f1`,
#       `raster_deltaT_CMCC-ESM2_r1i1p1f1`,
#       `raster_deltaT_CNRM-CM6-1_r1i1p1f2`,
#       `raster_deltaT_CNRM-CM6-1-HR_r1i1p1f2`,
#       `raster_deltaT_FGOALS-f3-L_r1i1p1f1`,
#       `raster_deltaT_FGOALS-g3_r1i1p1f1`,
#       `raster_deltaT_GFDL-ESM4_r1i1p1f1`,
#       `raster_deltaT_GISS-E2-1-G_r1i1p1f2`,
#       `raster_deltaT_IITM-ESM_r1i1p1f1`,
#       `raster_deltaT_INM-CM4-8_r1i1p1f1`,
#       `raster_deltaT_IPSL-CM5A2-INCA_r1i1p1f1`,
#       `raster_deltaT_IPSL-CM6A-LR_r1i1p1f1`,
#       `raster_deltaT_KACE-1-0-G_r1i1p1f1`,
#       `raster_deltaT_MIROC-ES2L_r1i1p1f2`,
#       raster_deltaT_MIROC6_r1i1p1f1,
#       `raster_deltaT_MPI-ESM1-2-LR_r10i1p1f1`,
#       `raster_deltaT_MRI-ESM2-0_r1i1p1f1`,
#       `raster_deltaT_NorESM2-LM_r1i1p1f1`,
#       `raster_deltaT_NorESM2-MM_r1i1p1f1`,
#       raster_deltaT_TaiESM1_r1i1p1f1,
#       `raster_deltaT_UKESM1-0-LL_r1i1p1f2`)

#raster::writeRaster(master_raster, "~/BurkeLab Dropbox/Projects/loss_damage/sherlock_files/data/pre_processed/master_raster.tif")
#save(list_rasters, file = "~/BurkeLab Dropbox/Projects/loss_damage/sherlock_files/data/pre_processed/list_rasters.RData")
mean_r_raster <- raster("~/Burkelab Dropbox/Projects/loss_damage/data/processed/r_cgm/ratio_raster_avgs.tif")
list_r_rasters <- readRDS("~/Burkelab Dropbox/Projects/loss_damage/data/processed/r_cgm/ratio_raster_list.rds")

################################################################################
##################### PART II: Read and compute delta T ########################
################################################################################
source("~/BurkeLab Dropbox/Projects/loss_damage/scripts/working/2a_FaIR_deltaT_hist.R")
source("~/BurkeLab Dropbox/Projects/loss_damage/scripts/working/2b_FaIR_deltaT_hist_fut.R")
source("~/BurkeLab Dropbox/Projects/loss_damage/scripts/working/2c_FaIR_deltaT_hist_fut_disagg.R")

# we have a set of experiments that we need to run. Below we go through the 
# different experiments. The experiments data that we need to process were
# obtained by running FaIR through jupyter notebook 
# The function has two arguments. The first is the FaIR exeriment.For example,
# below is the "hist_fut" experiment, this is the run that includes past and 
#future impacts up to 2100
####################### Experiment (1G/tCO2/yr): ########################
# this experiment is run to estimate the temperature effects of pulsing 
# 1GtCO2 at a given year
# set up a list of experiments to loop over. Here we have main experiment, 
# the experiment with lucf emissions (1990-2020), and finally methane (1990-2020)
fair_exps_1tco2 <- process_exp_data_hist_fut("1tCO2_hist_fut_main", 1980)

# now let us calc deltat when were running damages out to 2300
fair_exps_1tco2_2300 <- process_exp_data_hist_fut("1tCO2_hist_2300", 1990)

# now let us calc deltat and return all the runs so that we can run 
# uncertainty analysis
fair_exps_1tco2_disagg <- process_disagg_exp_data("1tCO2_hist_fut_main", 1990)

# now we need to run the experiment for 1gtco2 instead of 1tco2
fair_exps_1GtCO2 <- process_exp_data_hist_fut("1GtCO2_hist_fut_main", 1990)

####################### Experiment (Green GDP): ########################
# this experiment is to estimate the green GDP. In other words, the 
# official GDP but corrected for externalities in both directions.
# for year_k = 1980
fair_exps_k80_yriso <- process_exp_data_hist_fut("hist_fut_yriso", 1980)
# for year_k = 1990
fair_exps_k90_yriso <- process_exp_data_hist_fut("hist_fut_yriso", 1990)

####################### Experiment (Carbon Capture): ########################
fair_exps_cc <- process_exp_data_hist_fut("new_cc_hist", 2020)

############# Experiment (Future SSP Scenarios emissions): #############
# this experiment is to estimate the temperature changes that are result 
# of the SSP future emissions trajectories. We do this by running FaIR 
# with full emissions (including the SSP emissions) and we run the 
# another experiment where we stop emissions in 2020 (only observed) 
# whatever resulting deltaT is attributed to future emissions
fair_exps_ssp370 <- process_exp_data_hist_fut("ssp370", 1980)
fair_exps_ssp245 <- process_exp_data_hist_fut("ssp245", 1980)
fair_exps_ssp126 <- process_exp_data_hist_fut("ssp126", 1980)
fair_exps_ssp119 <- process_exp_data_hist_fut("ssp119", 1980)

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
fair_exps_isos_k80 <- process_exp_data_hist("hist_bi_v2022", 1980, aggregating = T)

# for year_k = 1990
fair_exps_isos_k90 <- process_exp_data_hist("hist_bi_v2022", 1990, aggregating = T)

#for year_k = 1990 and only consumption emissions
fair_exps_isos_k90_consump <- process_exp_data_hist("hist_biconsump_v2022", 1990, aggregating = T)

##################### Experiment (disagg for runs): ######################
# in order to obtain deltat estimates from fair we nned to first calculate  
# deltat under various climatic parameters as specified by established 
# distributions in the literature

################################################################################
##################### PART III: Calculate Total Damages ########################
################################################################################
source("~/BurkeLab Dropbox/Projects/loss_damage/scripts/working/3a_run_gdptemp_panel.R")
source("~/BurkeLab Dropbox/Projects/loss_damage/scripts/working/3a1_run_gdptemp_panel_bhmbs.R")
source("~/BurkeLab Dropbox/Projects/loss_damage/scripts/working/3a2_run_gdptemp_panel.R")
source("~/BurkeLab Dropbox/Projects/loss_damage/scripts/working/3b_run_bhm_model.R")
source("~/BurkeLab Dropbox/Projects/loss_damage/scripts/working/3c_calc_total_damages_bilateral.R")
source("~/BurkeLab Dropbox/Projects/loss_damage/scripts/working/3c1_calc_total_damages.R")
source("~/BurkeLab Dropbox/Projects/loss_damage/scripts/working/3c2_calc_total_damages_bootstrap.R")
source("~/BurkeLab Dropbox/Projects/loss_damage/scripts/working/3c3_calc_total_damages_richpoorbhm.R")

# before running the scripts let us read the future forecast data
#future_forecast_ssp370 <- readRDS("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/world_gdp_pop/gdp_eratemp_2021_2100_ssp370.rds")
#future_forecast_ssp370 <- future_forecast_ssp370 %>% dplyr::select(c("ISO3", "year", "era_mwtemp", "lgdp_d", "pop"))
#colnames(future_forecast_ssp370)[4] <- "diff_lgdp"
#future_forecast_ssp370 <- subset(future_forecast_ssp370, year >2020)

# now let us create a sub dataset that extends growth and temperature data onto 
# 2300 
#future_forecast_ssp370_2300 <- future_forecast_ssp370 %>% 
  #  dplyr::group_by(ISO3) %>% 
  #  complete(year = full_seq(2101:2300, 1)) %>% 
  #  fill(year)

# what we do as default is set the growth and pop numbers at thier 2100 levels
#future_forecast_ssp370_2300 <- future_forecast_ssp370_2300 %>% 
  #  dplyr::group_by(ISO3) %>% 
  #dplyr::mutate(growth_2100 = diff_lgdp[year == 2100],
                #                pop_2100 = pop[year == 2100])
#future_forecast_ssp370_2300$diff_lgdp[is.na(future_forecast_ssp370_2300$diff_lgdp)] <- future_forecast_ssp370_2300$growth_2100[is.na(future_forecast_ssp370_2300$diff_lgdp)]
#future_forecast_ssp370_2300$pop[is.na(future_forecast_ssp370_2300$pop)] <- future_forecast_ssp370_2300$pop_2100[is.na(future_forecast_ssp370_2300$pop)]

#write_rds(future_forecast_ssp370_1, "~/BurkeLab Dropbox/Projects/loss_damage/sherlock_files/data/pre_processed/gdp_eratemp_2021_2300_ssp370.rds")
#write_rds(future_forecast_ssp370_2300, "~/BurkeLab Dropbox/Projects/loss_damage/data/processed/future_forecast/future_forecast_ssp370_2300.rds")

#future_forecast_ssp370_2300_1pct <- future_forecast_ssp370 %>% 
#  group_by(ISO3) %>% 
#  complete(year = full_seq(2101:2300, 1)) %>% 
#  fill(year)
#future_forecast_ssp370_2300_1pct <- future_forecast_ssp370_2300_1pct %>% 
#  dplyr::group_by(ISO3) %>% 
#  dplyr::mutate(growth_2100 = diff_lgdp[year == 2100],
#                pop_2100 = pop[year == 2100])
#future_forecast_ssp370_2300_1pct$diff_lgdp[is.na(future_forecast_ssp370_2300_1pct$diff_lgdp)] <- 0.01
#future_forecast_ssp370_2300_1pct$pop[is.na(future_forecast_ssp370_2300_1pct$pop)] <- future_forecast_ssp370_2300_1pct$pop_2100[is.na(future_forecast_ssp370_2300_1pct$pop)]
#
#future_forecast_ssp370_2300_2pct <- future_forecast_ssp370 %>% 
#  group_by(ISO3) %>% 
#  complete(year = full_seq(2101:2300, 1)) %>% 
#  fill(year)
#future_forecast_ssp370_2300_2pct <- future_forecast_ssp370_2300_2pct %>% 
#  dplyr::group_by(ISO3) %>% 
#  dplyr::mutate(growth_2100 = diff_lgdp[year == 2100],
                #                pop_2100 = pop[year == 2100])
#future_forecast_ssp370_2300_2pct$diff_lgdp[is.na(future_forecast_ssp370_2300_2pct$diff_lgdp)] <- 0.02
#future_forecast_ssp370_2300_2pct$pop[is.na(future_forecast_ssp370_2300_2pct$pop)] <- future_forecast_ssp370_2300_2pct$pop_2100[is.na(future_forecast_ssp370_2300_2pct$pop)]

#write_rds(future_forecast_ssp370_2300_1pct, "~/BurkeLab Dropbox/Projects/loss_damage/data/processed/future_forecast/future_forecast_ssp370_2300_1pct.rds")
#write_rds(future_forecast_ssp370_2300_2pct, "~/BurkeLab Dropbox/Projects/loss_damage/data/processed/future_forecast/future_forecast_ssp370_2300_2pct.rds")

future_forecast_ssp370_2300_1pct <- readRDS("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/future_forecast/future_forecast_ssp370_2300_1pct.rds")
future_forecast_ssp370_2300_2pct <- readRDS("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/future_forecast/future_forecast_ssp370_2300_2pct.rds")
future_forecast_ssp370_2300 <- readRDS("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/future_forecast/future_forecast_ssp370_2300.rds")

# now let us do the alt run where we have CRU as the temp dataset instead of era
#future_forecast_ssp370_cru <- readRDS("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/world_gdp_pop/gdp_crutemp_2021_2100_ssp370.rds")
#future_forecast_ssp370_cru <- future_forecast_ssp370_cru %>% dplyr::select(c("ISO3", "year", "cru_mwtemp", "lgdp_d", "pop"))
#colnames(future_forecast_ssp370_cru)[4] <- "diff_lgdp"
#future_forecast_ssp370_cru <- subset(future_forecast_ssp370_cru, year >2020)
#
#future_forecast_ssp245 <- readRDS("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/world_gdp_pop/gdp_eratemp_2021_2100_ssp245.rds")
#future_forecast_ssp245 <- future_forecast_ssp245 %>% dplyr::select(c("ISO3", "year", "era_mwtemp", "lgdp_d", "pop"))
#colnames(future_forecast_ssp245)[4] <- "diff_lgdp"
#future_forecast_ssp245 <- subset(future_forecast_ssp245, year >2020)
#
#future_forecast_ssp126 <- readRDS("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/world_gdp_pop/gdp_eratemp_2021_2100_ssp126.rds")
#future_forecast_ssp126 <- future_forecast_ssp126 %>% dplyr::select(c("ISO3", "year", "era_mwtemp", "lgdp_d", "pop"))
#colnames(future_forecast_ssp126)[4] <- "diff_lgdp"
#future_forecast_ssp126 <- subset(future_forecast_ssp126, year >2020)
#
#future_forecast_ssp119 <- readRDS("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/world_gdp_pop/gdp_eratemp_2021_2100_ssp119.rds")
#future_forecast_ssp119 <- future_forecast_ssp119 %>% dplyr::select(c("ISO3", "year", "era_mwtemp", "lgdp_d", "pop"))
#colnames(future_forecast_ssp119)[4] <- "diff_lgdp"
#future_forecast_ssp119 <- subset(future_forecast_ssp119, year >2020)

#write_rds(future_forecast_ssp370_cru, "~/BurkeLab Dropbox/Projects/loss_damage/data/processed/future_forecast/future_forecast_ssp370_cru.rds")
#write_rds(future_forecast_ssp245, "~/BurkeLab Dropbox/Projects/loss_damage/data/processed/future_forecast/future_forecast_ssp245.rds")
#write_rds(future_forecast_ssp126, "~/BurkeLab Dropbox/Projects/loss_damage/data/processed/future_forecast/future_forecast_ssp126.rds")
#write_rds(future_forecast_ssp119, "~/BurkeLab Dropbox/Projects/loss_damage/data/processed/future_forecast/future_forecast_ssp119.rds")

future_forecast_ssp370_cru <- readRDS("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/future_forecast/future_forecast_ssp370_cru.rds")
future_forecast_ssp245 <- readRDS("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/future_forecast/future_forecast_ssp245.rds")
future_forecast_ssp126 <- readRDS("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/future_forecast/future_forecast_ssp126.rds")
future_forecast_ssp119 <- readRDS("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/future_forecast/future_forecast_ssp119.rds")

####################### generate country-year panel: #########################
# first we generate the GDP-temp country-year level panel, 
gdp_temp_data_k90 <- generate_gdptemp_panel("pooled", 
                                        future_forecast_ssp370, 
                                        "1990", 
                                        "ERA")
gdp_temp_data_k90_2300 <- generate_gdptemp_panel("pooled", 
                                                 future_forecast_ssp370_2300, 
                                                 "1990", 
                                                 "ERA")
gdp_temp_data_k90_2300_1pct <- generate_gdptemp_panel("pooled", 
                                                 future_forecast_ssp370_2300_1pct, 
                                                 "1990", 
                                                 "ERA")
gdp_temp_data_k90_2300_2pct <- generate_gdptemp_panel("pooled", 
                                                 future_forecast_ssp370_2300_2pct, 
                                                 "1990", 
                                                 "ERA")


gdp_temp_data_k80 <- generate_gdptemp_panel("pooled", 
                                            future_forecast_ssp370, 
                                            "1980", 
                                            "ERA")

gdp_temp_data_k80_2300 <- generate_gdptemp_panel("pooled", 
                                            future_forecast_ssp370_2300, 
                                            "1980", 
                                            "ERA")

#plot(gdp_temp_data_k80_2300$year, gdp_temp_data_k80_2300$diff_lgdP)


gdp_temp_data_5lags <- generate_gdptemp_panel_5lags("pooled", 
                                        future_forecast_ssp370_2300, 
                                        "1990", 
                                        "ERA")

#write_rds(gdp_temp_data, "~/BurkeLab Dropbox/Projects/loss_damage/sherlock_files/data/pre_processed/gdp_temp_data.rds")

################# generate country-year bootstrapped panel: ##################
pooledbs <- as.data.frame(readRDS("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/pooledregression_boostraps_era.rds"))
gdp_temp_data_bhmbs <- run_bhm_model("pooled", future_forecast_ssp370_cru, 1990, "CRU")

################### generate country-year regression model: ##################
# as well as the regression model
bhm_era_reg <- run_bhm_model_reg("pooled")

#bhm_era_reg$coefficients[1] / (-2*(bhm_era_reg$coefficients[2]))

#save(bhm_era_reg, file = "~/BurkeLab Dropbox/Projects/loss_damage/sherlock_files/data/pre_processed/bhm_era_reg.RData")
# code lines for bootstrapped bhm
#pooledbs <- as.data.frame(readRDS("~/Desktop/distributedlag_bootstraps_era.rds"))
pooledbs <- as.data.frame(readRDS("~/Desktop/loss_damages/pooledregression_boostraps_era.rds"))
#pooledbs <- readRDS("~/Desktop/pooledregression_bootstraps_cru.rds")
#pooledbs <- pooledbs %>% dplyr::select(c("V7", "V8"))
#colnames(pooledbs)[1]<- "temp"
#colnames(pooledbs)[2]<- "temp2"
#gdp_temp_data <- run_bhm_model("pooled", future_forecast_df, 1990)
#gdp_temp_datac <- run_bhm_model("pooled", future_forecast_df, 1990)
#gdp_temp_data <- gdp_temp_datac
#gdp_temp_datab <- run_bhm_model("pooled", future_forecast_df)
#bhm_cru_reg <- run_bhm_model_reg("pooled")

# now we do the same but with the rich/poor. Here rich/poor is constructed by 
# looking at average of each country's GDP  relative to median of full sample
# panel...
#gdp_temp_data <- run_bhm_model("richpoor")
# model...
#bhm_cru_reg <- run_bhm_model_reg("richpoor")
##############################################################################
############### calculate the total damages for each scenario ################
##############################################################################
# we need to aggregate the deltat to the country level by weighting by pop
# so let us read the pop raster and resample it to match coordinates and 
# convert to a dataframe and then join
pop <- raster("~/BurkeLab Dropbox/Projects/loss_damage/data/raw/population/gpw_v4_population_count_rev11_2010_1_deg.tif")
pop <- readAll(pop)
pop <- resample(pop, master_raster)
popdf <- as.data.frame(as.matrix(rasterToPoints(pop)))

#write_rds(popdf, "~/BurkeLab Dropbox/Projects/loss_damage/sherlock_files/data/pre_processed/popdf.rds")
# the other thing we need is to get a country id for each lon-lat combo
data("wrld_simpl")
world <- wrld_simpl[,3]
world <- st_as_sf(world)

#write_sf(world, "~/BurkeLab Dropbox/Projects/loss_damage/sherlock_files/data/pre_processed/world.shp")
# now let us go one by one
##################### 1GtCO2/yr experiment ###########################
# first we need to set up the set of experimenet years to loop over inside the 
# custom-mmade function
list_of_exps <- c(1990:2023)
colnames(fair_exps_1GtCO2)[7] <- "deltaT_preturb"
colnames(fair_exps_1GtCO2)[6] <- "deltaT_fullemms"

total_damages_1gtCO2 <- calculate_bidamages(master_raster,
                                            fair_exps_1GtCO2, 
                                            list_of_exps, 
                                            1990,
                                            future_forecast_ssp370,
                                            gdp_temp_data,
                                            "ERA")

total_damages_1gtCO2 <- calculate_bidamages(master_raster,
                                            fair_exps_1tco2_2300, 
                                            list_of_exps, 
                                            1990,
                                            future_forecast_ssp370_2300,
                                            gdp_temp_data_k90_2300,
                                            "ERA",
                                            "NO",
                                            1)


#write the output data 
#write_rds(total_damages_1gtCO2, "~/BurkeLab Dropbox/projects/loss_damage/data/processed/output/1gtco2_yr_exp_damages.rds")
#sum(total_damages_1gtCO2$weighted_damages2_scld[total_damages_1gtCO2$emitter == 2020], na.rm = T) / 1000000000

#list_of_exps <- c(1990:2020)
#total_damages_1gtCO2 <- calculate_bidamages(master_raster,
 #                                           fair_exps_1GtCO2_updated, 
  #                                          list_of_exps, 
   #                                         1990,
    #                                        future_forecast_ssp370,
     #                                       gdp_temp_data)

#write the output data 
#write_rds(total_damages_1gtCO2, "~/BurkeLab Dropbox/projects/loss_damage/data/processed/output/1gtco2_yr_exp_damages.rds")
#sum(total_damages_1gtC$weighted_damages2_scld[total_damages_1gtC$emitter == 2020], na.rm = T)


######################## SCC Uncertainty Sources ############################
######################## Response function uncertainty
# we need to begin with generating country panel with the bootstraps
gdp_temp_data_bhmbs <- generate_gdptemp_panel_bhmbs("pooled",
                                                    future_forecast_ssp370,
                                                    1990,
                                                    "ERA")

# now let us generate the total damages by bootstrap loop
total_damages_1tCO2 <- calculate_bidamages_bhmbs(master_raster, 
                                                 fair_exps_1tco2,
                                                 2020,
                                                 1990,
                                                 future_forecast_ssp370,
                                                 gdp_temp_data_bhmbs)

total_damages_1tCO2 <- calculate_bidamages(master_raster, 
                                                 fair_exps_1tco2,
                                                 c(1990:2023),
                                                 1990,
                                                 future_forecast_ssp370,
                                                 gdp_temp_data_k90,
                                                "ERA")

total_damages_1tCO2 <- calculate_bidamages(master_raster, 
                                           fair_exps_1tco2,
                                           c(1990:2023),
                                           1990,
                                           future_forecast_ssp370,
                                           gdp_temp_data_k90,
                                           "ERA",
                                           "no",
                                           "normal")


total_damages_cc <- calculate_bidamages(master_raster, 
                                           fair_exps_cc,
                                           c(2036:2100),
                                           1980,
                                           future_forecast_ssp370_2300,
                                           gdp_temp_data_k80_2300,
                                           "ERA",
                                           bhm_era_reg,
                                           "no",
                                           "normal")

#sum(total_damages_1tCO2$weighted_damages2_scld, na.rm= T)


#calculate_bidamages()



#sum(total_damages_1tCO2$weighted_damages2_scld[total_damages_1tCO2$coef_id == 100], na.rm = T)
# write the dataset 
#write_rds(total_damages_1tCO2, "~/BurkeLab Dropbox/projects/loss_damage/data/processed/output/1tco2_scc_bhm_uncertainty.rds")

######################## CGM Models uncertainty
# generate empty list of dataframes to be filled in woth processed dataframes
processed_dfs <- list()
for (i in 1:length(list_rasters)){
  tic()
  damages_i <- calculate_bidamages(list_rasters[[i]],
                                   fair_exps_1tco2,
                                   2020,
                                   1990,
                                   future_forecast_ssp370,
                                   gdp_temp_data)
  damages_i$cgm_id <- i
  processed_dfs[[i]] <- damages_i 
  toc()
}

# now let us bring all dataframes into one 
total_damages_1tCO2_cgm <- do.call(rbind, processed_dfs)

#sum(total_damages_1tCO2_cgm$weighted_damages2_scld[total_damages_1tCO2_cgm$fair_id == 2], na.rm = T)
# write the dataset
#write_rds(total_damages_1tCO2_cgm, "~/BurkeLab Dropbox/projects/loss_damage/data/processed/output/1tco2_scc_cgm_uncertainty.rds")

######################## FaIR uncertainty
# in order to calculate the total damages under different FaIR runs
processed_dfs <- list()
for (i in 1:length(unique(fair_exps_1tco2_disagg$num_loop))){
  tic()
  fair_i <- subset(fair_exps_1tco2_disagg, num_loop == i)
  damages_i <- calculate_bidamages(master_raster,
                                   fair_i,
                                   2020,
                                   1990,
                                   future_forecast_ssp370,
                                   gdp_temp_data)
  damages_i$fair_id <- i
  processed_dfs[[i]] <- damages_i 
  toc()
}

# now let us bring all dataframes into one 
total_damages_1tCO2_fair <- do.call(rbind, processed_dfs)

#sum(total_damages_1tCO2_fair$weighted_damages2_scld[total_damages_1tCO2_fair$fair_id == 76], na.rm = T)
# write the dataset
#write_rds(total_damages_1tCO2_fair, "~/BurkeLab Dropbox/projects/loss_damage/data/processed/output/1tco2_scc_fair_uncertainty.rds")

######################## SCC Total Uncertainty ############################
# now we need to calculate the total uncertainty. In order to execute this 
# task we need to sample from each of our sources of uncertainty and 
# calculated the resulting total damages. 
for (i in 1:1000) {
  tic()
  
  fair_exps_1gtc_disagg_i <- subset(fair_exps_1tco2_disagg, 
                                    num_loop == sample(unique(fair_exps_1tco2_disagg$num_loop), 1))
  gdp_temp_data_sbstd <- subset(gdp_temp_data_bhmbs, coef_id == sample(1:1000, 1))
  #gdp_temp_data_sbstd <- subset(gdp_temp_data, coef_id == 1)
  total_damages_1gtC <- calculate_bidamages(list_rasters[[sample(1:29, 1)]],
                                            fair_exps_1gtc_disagg_i, 
                                            2020, 
                                            1990,
                                            future_forecast_ssp370,
                                            gdp_temp_data_sbstd)
  #sum(total_damages_1gtC$total_damages2, na.rm = T)
  #list_of_estimates <- rbind(list_of_estimates,
  #                          total_damages_1gtC)
  #hist(list_of_estimates$total_damages2)
  write_rds(total_damages_1gtC, 
            paste0("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/net_scc_base_111822/net_scc_est_", i, ".rds"))
  #list_of_estimates[[i]] <- total_damages_1gtC
  toc()
}

######################## CRU
for (i in 938:1000) {
  tic()
  
  fair_exps_1gtc_disagg_i <- subset(fair_exps_1tco2_disagg, 
                                    num_loop == sample(unique(fair_exps_1tco2_disagg$num_loop), 1))
  gdp_temp_data_sbstd <- subset(gdp_temp_data_bhmbs, coef_id == sample(1:1000, 1))
  #gdp_temp_data_sbstd <- subset(gdp_temp_data, coef_id == 1)
  total_damages_1gtC <- calculate_bidamages(list_rasters[[sample(1:29, 1)]],
                                            fair_exps_1gtc_disagg_i, 
                                            2020, 
                                            1990,
                                            future_forecast_ssp370_cru,
                                            gdp_temp_data_sbstd, 
                                            "CRU")
  #sum(total_damages_1gtC$total_damages2, na.rm = T)
  #list_of_estimates <- rbind(list_of_estimates,
  #                          total_damages_1gtC)
  #hist(list_of_estimates$total_damages2)
  write_rds(total_damages_1gtC, 
            paste0("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/net_scc_cru_112822/net_scc_est_", i, ".rds"))
  #list_of_estimates[[i]] <- total_damages_1gtC
  toc()
}


######################## Green GDP calculation ############################
# this calculation is to generate the numbers necesary for the green gdp 
# figure in paper. to estimate the green GDP. In other words, the 
# official GDP but corrected for externalities in both directions.
# for year_k = 1980
# The experiment was run for 4 set of countries, usa, japan, china, and 
# germany
list_of_exps <- unique(fair_exps_k90_yriso$experiment_iso)

# NOW calculate the total damages
total_damages_ggdp <- calculate_bidamages_ggdp(master_raster,
                                               fair_exps_k90_yriso, 
                                               list_of_exps, 
                                               1990,
                                               future_forecast_ssp370,
                                               gdp_temp_data)

# write the dataset 
write_rds(total_damages_ggdp, 
          "~/BurkeLab Dropbox/projects/loss_damage/data/processed/output/total_damages_ggdp.rds")

#sum(total_damages_ggdp$weighted_damages2_scld[total_damages_ggdp$exp_yr == 2000 & total_damages_ggdp$emitter == "USA"], na.rm = T)

######################### Country-level bidamages ############################


gdp_temp_data_k80 <- subset(gdp_temp_data_k80, year <= 2020)

total_damages_k80 <- calculate_bidamages_bilateral(master_raster, 
                                                   fair_exps_isos_k80, 
                                                   unique(fair_exps_isos_k80$experiment_iso),
                                                   1980, 
                                                   future_forecast_ssp370,
                                                   gdp_temp_data_k80,
                                                   bhm_era_reg)

write_rds(total_damages_k80, "~/Desktop/total_damages_k80_v2022.rds")

bhm_era_reg$coefficients[1]/(-2*bhm_era_reg$coefficients[2])


gdp_temp_data_k90 <- subset(gdp_temp_data_k90, year <= 2020)

total_damages_k90 <- calculate_bidamages_bilateral(master_raster, 
                                                   fair_exps_isos_k90, 
                                                   unique(fair_exps_isos_k90$experiment_iso),
                                                   1990, 
                                                   future_forecast_ssp370,
                                                   gdp_temp_data_k90,
                                                   bhm_era_reg)

write_rds(total_damages_k90, "~/Desktop/total_damages_k90_v2022.rds")

gdp_temp_data_k90 <- subset(gdp_temp_data_k90, year <= 2020)

fair_exps_isos_k90_consump <- subset(fair_exps_isos_k90_consump, !is.na(median_deltat))
total_damages_k90_consump <- calculate_bidamages_bilateral(master_raster, 
                                                   fair_exps_isos_k90_consump, 
                                                   unique(fair_exps_isos_k90_consump$experiment_iso),
                                                   1990, 
                                                   future_forecast_ssp370,
                                                   gdp_temp_data_k90,
                                                   bhm_era_reg)

write_rds(total_damages_k90_consump, "~/Desktop/total_damages_k90_consump_v2022.rds")

sum(total_damages_k90_consump$weighted_damages2[total_damages_k90_consump$emitter == "USA" & total_damages_k90_consump$weighted_damages2 < 0 ], na.rm = T)
sum(total_damages_k90$weighted_damages2[total_damages_k90$emitter == "USA" & total_damages_k80$weighted_damages2 < 0], na.rm = T)
sum(total_damages_k80$weighted_damages2[total_damages_k80$emitter == "AUS" & total_damages_k80$weighted_damages2 < 0 ], na.rm = T)

write_rds(total_damages_k90, "~/Desktop/total_damages_k90_v2022.rds")

bhm_era_reg$coefficients[1]/(-2*bhm_era_reg$coefficients[2])

ex <- readRDS("~/Desktop/FSE_projects/loss_damages/total_bidamages_k80_0223.rds")

sum(ex$weighted_damages2[ex$emitter == "USA" & ex$weighted_damages2 < 0 ], na.rm = T)

total_damages_k80 <- calculate_bidamages_bilateral(master_raster, 
                                                   fair_exps_isos_k80, 
                                                   "USA",
                                                   1980, 
                                                   future_forecast_ssp370,
                                                   gdp_temp_data_k80,
                                                   bhm_era_reg)




head(total_damages_k80)

sum(total_damages_k80$weighted_damages2[total_damages_k80$emitter == "USA" & total_damages_k80$year <= 2020 & total_damages_k80$weighted_damages2 < 0 ], na.rm = T)


gdp_temp_data_k90 <- subset(gdp_temp_data_k90, year <= 2020)

write_rds(total_damages_k80, "~/Desktop/total_damages_k809_020623.rds")
write_rds(total_damages_k90, "~/Desktop/total_damages_k90_020623.rds")

fair_exps_isos_k90 <- subset(fair_exps_isos_k90, year <= 2020)

total_damages_k90 <- calculate_bidamages_bilateral(master_raster, 
                                                   fair_exps_isos_k90, 
                                                   unique(fair_exps_isos_k90$experiment_iso),
                                                   1990, 
                                                   future_forecast_ssp370,
                                                   gdp_temp_data_k90,
                                                   bhm_era_reg)


#write_rds(total_damages_k80, "~/BurkeLab Dropbox/Projects/loss_damage/data/figs/fig1/total_damages_k80.rds")
#write_rds(fair_exps_isos_k80, "~/BurkeLab Dropbox/Projects/loss_damage/data/figs/fig1/fair_exps_isos_k80.rds")

colnames(fair_exps_isos_k80)[7] <- "deltaT_preturb"
colnames(fair_exps_1GtCO2)[6] <- "deltaT_fullemms"

calculate_bidamages()
max(total_damages$year)
sum(total_damages_k80$weighted_damages2[total_damages_k80$year <= 2020 & total_damages_k80$weighted_damages2 > 0 ], na.rm = T)

total_damages_neg <- subset(total_damages, weighted_damages2 < 0 )
total_damages_neg <- subset(total_damages_neg, year < 2021 )
total_damages_usa <- subset(total_damages, emitter == "USA")

sum(total_damages_neg$weighted_damages2, na.rm = T)
######################## Impact of future gdp-ssp ############################

# now we can calculate the bilateral damages. Here, list_of_exps indicate
#the countries turning off of emissions. In case we want all the countries, 
# we set list_of_exps = unique(fair_exps$experiment_iso) (i.e. all ISOs)
list_of_exps <- unique(fair_exps$experiment_iso)
list_of_exps <- unique(fair_exps_1gtc$experiment_iso)
list_of_exps <- unique(fair_exps_carbon_debt$experiment_iso)
list_of_exps <- unique(fair_exps_k80_ssp119$experiment_iso)

# if you want to run damages owed by USA only
#list_of_exps <- "USA"

list_of_exps <- unique(fair_exps_k80$experiment_iso)

fair_loop_id <- as.data.frame(unique(fair_exps_1gtc_disagg$num_loop)) 
colnames(fair_loop_id)[1] <- "num_loop"
fair_loop_id$id  <- sample(1:90, replace = F)

sample(fair_loop_id$id, 1)

help(hist)
list_of_exps <- c("USA", "CHN", "DEU", "JPN")
list_of_exps <- unique(fair_exps_k80$experiment_iso)
# NOW calculate the total damages
total_damages_k80 <- calculate_bidamages(master_raster,
                                         fair_exps_k80, 
                                         list_of_exps, 
                                         1980,
                                         future_forecast_df,
                                         gdp_temp_data)

sum(total_damages_k80$weighted_damages2_scld, na.rm = T)
write_rds(total_damages_k80, "~/BurkeLab Dropbox/Projects/loss_damage/data/processed/total_bidamages_k80.rds")

# NOW calculate the total damages
total_damages_k90 <- calculate_bidamages(master_raster,
                                         fair_exps_k90, 
                                         list_of_exps, 
                                         1990,
                                         future_forecast_df,
                                         gdp_temp_data)

write_rds(total_damages_k90, "~/BurkeLab Dropbox/Projects/loss_damage/data/processed/total_bidamages_k90.rds")

ex <- calculate_bidamages_bilateral(master_raster,
                                    fair_exps_isos_k80,
                                    "USA",
                                    1980,
                                    future_forecast_ssp370)

usa <- subset(ex, year <= 2020)
sum(usa$weighted_damages2, na.rm = T)

#write_rds(total_damages, "~/BurkeLab Dropbox/Projects/loss_damage/data/processed/bidamages_k80_hist_fut.rds")

fair_exps_1gtc_disagg <- subset(fair_exps_1gtc_disagg, num_loop > 85)

# now total _damages from the 1gtc experiment 
list_of_exps <- c(2020)
numloop <- 1
num_rast <- 1
finished_rasters <- list()
for (numloop in unique(fair_exps_1gtc_disagg$num_loop)){
  tic()
  fair_exps_1gtc_disagg_i <- subset(fair_exps_1gtc_disagg, num_loop == numloop)
  for (num_rast in 1:29){
    total_damages_1gtC <- calculate_bidamages(list_rasters[[num_rast]],
                                              fair_exps_1gtc_disagg_i, 
                                              list_of_exps, 
                                              1980,
                                              future_forecast_df)
    
    #raster_name <- substr(name(raster), 1,10)
    
    run <- paste0(num_rast, '_', numloop)
    
    #finished_rasters[[run]] <- total_damages_1gtC
    
    write_rds(total_damages_1gtC, 
              paste0("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/estimates_uncertainty/total_damages_cgm", 
                     num_rast, "_fair", numloop, ".rds"))
    toc()
    print(paste0("finished with ", run))
  }
}
list_of_exps <- c(2020)
for (numloop in unique(fair_exps_1gtc_disagg$num_loop)){
  tic()
  fair_exps_1gtc_disagg_i <- subset(fair_exps_1gtc_disagg, num_loop == numloop)
  for (num_rast in 1:29){
    total_damages_1gtC <- calculate_bidamages(list_rasters[[num_rast]],
                                              fair_exps_1gtc_disagg_i, 
                                              list_of_exps, 
                                              1980,
                                              future_forecast_df)
    
    #raster_name <- substr(name(raster), 1,10)
    
    run <- paste0(num_rast, '_', numloop)
    
    #finished_rasters[[run]] <- total_damages_1gtC
    
    write_rds(total_damages_1gtC, 
              paste0("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/estimates_uncertainty_cru/total_damages_cru_cgm", 
                     num_rast, "_fair", numloop, ".rds"))
    toc()
    print(paste0("finished with ", run))
  }
}


# now total _damages from the 1gtc experiment 
list_of_exps <- c(2020)

fair_exps_1gtc_disagg_ssp245 <- subset(fair_exps_1gtc_disagg_ssp245, num_loop > 22)

for (numloop in unique(fair_exps_1gtc_disagg_ssp245$num_loop)){
  tic()
  fair_exps_1gtc_disagg_i <- subset(fair_exps_1gtc_disagg_ssp245, num_loop == numloop)
  for (num_rast in 1:29){
    total_damages_1gtC <- calculate_bidamages(list_rasters[[num_rast]],
                                              fair_exps_1gtc_disagg_i, 
                                              list_of_exps, 
                                              1980,
                                              future_forecast_df)
    
    #raster_name <- substr(name(raster), 1,10)
    
    run <- paste0(num_rast, '_', numloop)
    
    #finished_rasters[[run]] <- total_damages_1gtC
    
    write_rds(total_damages_1gtC, 
              paste0("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/uncertainty_est_ssp245/total_damages_erassp2_cgm", 
                     num_rast, "_fair", numloop, ".rds"))
    toc()
    print(paste0("finished with ", run))
  }
}

fair_exps_1gtc_disagg_ssp126 <- subset(fair_exps_1gtc_disagg_ssp126, num_loop > 78)
for (numloop in unique(fair_exps_1gtc_disagg_ssp126$num_loop)){
  tic()
  fair_exps_1gtc_disagg_i <- subset(fair_exps_1gtc_disagg_ssp126, num_loop == numloop)
  for (num_rast in 1:29){
    total_damages_1gtC <- calculate_bidamages(list_rasters[[num_rast]],
                                              fair_exps_1gtc_disagg_i, 
                                              list_of_exps, 
                                              1980,
                                              future_forecast_df)
    
    #raster_name <- substr(name(raster), 1,10)
    
    run <- paste0(num_rast, '_', numloop)
    
    #finished_rasters[[run]] <- total_damages_1gtC
    
    write_rds(total_damages_1gtC, 
              paste0("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/uncertainty_est_ssp126/total_damages_erassp1_cgm", 
                     num_rast, "_fair", numloop, ".rds"))
    toc()
    print(paste0("finished with ", run))
  }
}


list_of_cgm_dfs <- list()

list_of_exps <- c(2020)
for (i in 1:length(list_rasters)){
  tic()
  total_damages_1gtC <- calculate_bidamages(list_rasters[[i]],
                                            fair_exps_1gtc, 
                                            list_of_exps, 
                                            1990,
                                            future_forecast_df,
                                            gdp_temp_datab)
  
  total_damages_1gtC$cgm_i <- paste0("cgm_", i)
  
  list_of_cgm_dfs[[i]] <- total_damages_1gtC
  toc()
}

master_raster <- do.call(mean, list_rasters)
# uncertainty by BHM 
total_damages_uncertainty_bhm <- calculate_bidamages(master_raster,
                                                     fair_exps_1gtc, 
                                                     list_of_exps, 
                                                     1990,
                                                     future_forecast_df,
                                                     gdp_temp_datac)

# uncertainty by FaIR 
list_of_dfs_fair <- list()
for (numloop in unique(fair_exps_1gtc_disagg$num_loop)){
  tic()
  fair_exps_1gtc_disagg_i <- subset(fair_exps_1gtc_disagg, num_loop == numloop)
  
  total_damages_1gtC <- calculate_bidamages(master_raster,
                                            fair_exps_1gtc_disagg_i,
                                            list_of_exps,
                                            1990,
                                            future_forecast_df,
                                            gdp_temp_datab)
  
  total_damages_1gtC$fair_i <- paste0("fair_", numloop)
  
  list_of_dfs_fair[[numloop]] <- total_damages_1gtC
  toc()
}


write_rds(total_damages_1gtC, "~/Desktop/1gtcyr_exp.rds")
write_rds(total_damages_1gtC, "~/Desktop/1tcyr_exp.rds")

total_damages_1gtC <- calculate_bidamages(master_raster,
                                          fair_exps_1gtc, 
                                          list_of_exps, 
                                          1980,
                                          future_forecast_df,
                                          gdp_temp_data)
total_damages_1tC <- calculate_bidamages(master_raster,
                                         fair_exps_1tc, 
                                         list_of_exps, 
                                         1980,
                                         future_forecast_df,
                                         gdp_temp_data)

ex <- left_join(fair_exps_1gtc, fair_exps_1tc, by = c("experiment_iso", "year"))

plot(ex$median_deltat.x, (ex$median_deltat.y*1000000000))

ex$median_deltat.y <- ex$median_deltat.y * 1000000000
ex$diff <- ex$median_deltat.x - (ex$median_deltat.y)

exavg <- ex %>% dplyr::group_by(experiment_iso) %>% dplyr::summarise(avg_diff = mean(diff, na.rm = T))

plot(exavg$experiment_iso, exavg$avg_diff)



sum(total_damages_1gtC$weighted_damages2_scld[total_damages_1gtC$emitter == 2022], na.rm = T) / -1000000000
sum(total_damages_1tC$weighted_damages2_scld[total_damages_1tC$emitter == 2022], na.rm = T) / -1



total_damages_1gtC <- calculate_bidamages(master_raster,
                                          fair_exps_1gtc, 
                                          list_of_exps, 
                                          1990,
                                          future_forecast_df,
                                          gdp_temp_data)


list_of_exps <- unique(fair_exps_carbon_debt_indent$experiment_iso)
total_damages_carbon_debt <- calculate_bidamages(master_raster,
                                                 fair_exps_carbon_debt_indent, 
                                                 list_of_exps, 
                                                 1990,
                                                 future_forecast_df,
                                                 gdp_temp_data)

sum(total_damages_carbon_debt$weighted_damages2_scld[total_damages_carbon_debt$emitter == "Jeff Bezos's jet"], na.rm = T)

sum(total_damages_carbon_debt$weighted_damages2_scld[total_damages_carbon_debt$emitter == "Elon Musk's  jet"], na.rm = T)

sum1_1gtc <- sum(total_damages_carbon_debt$weighted_damages2_scld[total_damages_carbon_debt$emitter == ""], na.rm = T) 
list_of_exps
list_of_exps <- c(2020)

fair_exps_1gtc_disagg <- subset(fair_exps_1gtc_disagg, num_loop > 23)
for (numloop in unique(fair_exps_1gtc_disagg$num_loop)){
  tic()
  fair_exps_1gtc_disagg_i <- subset(fair_exps_1gtc_disagg, num_loop == numloop)
  for (num_rast in 1:29){
    total_damages_1gtC <- calculate_bidamages(list_rasters[[num_rast]],
                                              fair_exps_1gtc_disagg_i, 
                                              list_of_exps, 
                                              1980,
                                              future_forecast_df)
    
    #raster_name <- substr(name(raster), 1,10)
    
    run <- paste0(num_rast, '_', numloop)
    
    #finished_rasters[[run]] <- total_damages_1gtC
    
    write_rds(total_damages_1gtC, 
              paste0("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/estimates_uncertainty_lag5/total_damages_l5_cgm", 
                     num_rast, "_fair", numloop, ".rds"))
    toc()
    print(paste0("finished with ", run))
  }
}
#write_rds(total_damages_1gtC, "~/BurkeLab Dropbox/Projects/loss_damage/data/processed/bidamages_k80_1GtC_hist_fut.rds")

class(gdp_temp_data$year)

gdp_temp_data$year <- as.numeric(as.character(gdp_temp_data$year))

# NOW calculate the total damages
total_damages_k80_ssp119 <- calculate_bidamages(master_raster,
                                                fair_exps_k80_ssp119, 
                                                list_of_exps, 
                                                1980,
                                                future_forecast_df)
total_damages_k80_ssp126 <- calculate_bidamages(master_raster,
                                                fair_exps_k80_ssp126, 
                                                list_of_exps, 
                                                1980,
                                                future_forecast_df)

total_damages_k80_ssp245 <- calculate_bidamages(master_raster,
                                                fair_exps_k80_ssp245, 
                                                list_of_exps, 
                                                1980,
                                                future_forecast_df)


write_rds(total_damages_k80_ssp119, "~/BurkeLab Dropbox/Projects/loss_damage/data/processed/total_bidamages_k80_ssp119.rds")

# with methane
#ch4_total_damages <- calculate_bidamages(raster_cgm_ratio, ch4_fair_exps)
#with LUCF emissions
#lucf_total_damages <- calculate_bidamages(raster_cgm_ratio, lucf_fair_exps,
#                                         list_of_exps = list_of_exps)

#write_rds(ch4_total_damages, "~/BurkeLab Dropbox/Projects/loss_damage/data/processed/ch4_total_damages.rds")
#write_rds(total_damages, "~/BurkeLab Dropbox/Projects/loss_damage/data/processed/total_damages.rds")
#write_rds(lucf_total_damages, "~/BurkeLab Dropbox/Projects/loss_damage/data/processed/lucf_total_damages.rds")


# Now we do the same but but for the rich /poor model. calculate total 
#bi-damages for each of the experiments (rich/poor bhm)
#total_damages_richpoor <- calculate_bidamages_richpoor(raster_cgm_ratio, 
#                                                    fair_exps)
#ch4_total_damages_richpoor <- calculate_bidamages_richpoor(raster_cgm_ratio, 
#                                                         ch4_fair_exps)
#lucf_total_damages_richpoor <- calculate_bidamages_richpoor(raster_cgm_ratio, 
#                                                           lucf_fair_exps)


# total damages for ssp 119 if all emmissions post 2020 were 0 
list_of_exps <- unique(fair_exps_k80_ssp119$experiment_iso)
list_of_exps <- c("USA", "CHN")
total_damages_k80_noemms_ssp119 <- calculate_bidamages(master_raster,
                                                       fair_exps_k80_ssp119, 
                                                       list_of_exps, 
                                                       1980,
                                                       future_forecast_df)



fair_exps <- readRDS("~/Desktop/fair_exps_emms.rds")

list_of_exps <- c("all")

fair_exps

total_damages_test <- calculate_bidamages(master_raster,
                                          fair_exps,
                                          list_of_exps,
                                          1980,
                                          future_forecast_df)

write_rds(total_damages_test, "~/Desktop/damages_emms.rds")

total_damages_test

summary(total_damages_test$SP.POP.TOTL)
summary(total_damages_test$NY.GDP.PCAP.KD)

damages_neg <- left_join(total_dama,
                         ssp_gdp_pop,
                         by = c("ISO3", 
                                "year" = "variable"))

total_damages_k80_ssp119 <- total_damages_k80_ssp119 %>% 
  dplyr::mutate(pop = case_when(is.na(pop) ~ SP.POP.TOTL,
                                TRUE ~ pop),
                NY.GDP.PCAP.KD = case_when(is.na(NY.GDP.PCAP.KD) ~ gdp_pc_intrpl,
                                           TRUE ~ NY.GDP.PCAP.KD))



list_of_exps <- unique(fair_exps_carbon_debt_op$experiment_iso)
total_damages_carbon_debt <- calculate_bidamages(master_raster,
                                                 fair_exps_carbon_debt_op, 
                                                 list_of_exps, 
                                                 1980,
                                                 future_forecast_df,
                                                 gdp_temp_data)

unique(total_damages_carbon_debt$emitter)
listofdamagedf <- list()
i <- 1980
for (i in 1990:2020){
  tic()
  fair_exps_i <- subset(fair_exps_k90_yriso, exp_yr == i)
  fair_exps_i <- fair_exps_i %>%  dplyr::select(c("year", "experiment_iso", "median_deltat", "exp_yr"))
  total_damages_i <- calculate_bidamages(master_raster,
                                         fair_exps_i,
                                         list_of_exps = c("USA", "DEU", "CHN"),
                                         1990,
                                         future_forecast_df,
                                         gdp_temp_data,
                                         i)
  
  total_damages_i$experiment_yr <- paste0("year_", i)
  listofdamagedf[[i - 1989]] <- total_damages_i
  
  
  print(paste0("done with ", i, " and it took "))
  toc()
}

total_damages_i <- calculate_bidamages(master_raster,
                                       fair_exps_k90_yriso,
                                       "USA",
                                       1990,
                                       future_forecast_df,
                                       gdp_temp_data,
                                       1991)

total_damages_i <- calculate_bidamages(master_raster,
                                       fair_exps_1gtc,
                                       "1991",
                                       1990,
                                       future_forecast_df,
                                       gdp_temp_data,
                                       1991)

sum(total_damages_i$weighted_damages2_scld, na.rm = T) / 1370000000

sum(total_damages_i$weighted_damages2_scld, na.rm = T) / 1000000000


all_damages <- do.call(rbind, listofdamagedf)

all_damages <- subset(all_damages, experiment_yr == "year_2020") 

unique(all_damages$experiment_yr)

all_damages_pos <- subset(all_damages, weighted_damages2_scld >=0)
all_damages_neg <- subset(all_damages, weighted_damages2_scld <=0)

# now let us summarize by year of damage
all_damages_pos <- all_damages_pos %>% dplyr::group_by(emitter, experiment_yr) %>% 
  dplyr::summarise(total_damages_dr2 = sum(weighted_damages2_scld, na.rm = T))
all_damages_neg <- all_damages_neg %>% dplyr::group_by(emitter, experiment_yr) %>% 
  dplyr::summarise(total_damages_dr2 = sum(weighted_damages2_scld, na.rm = T))


# now let us summarize by year of damage
all_damages <- all_damages %>% dplyr::group_by(emitter, experiment_yr) %>% 
  dplyr::summarise(total_damages_dr2 = sum(weighted_damages2_scld, na.rm = T))

all_damages$year <- as.numeric(as.character(substr(all_damages$experiment_yr, 6,9)))

all_damages <- all_damages %>% dplyr::group_by(year, emitter, experiment_yr) %>% 
  dplyr::summarise(total_damages_dr2 = sum(weighted_damages2_scld, na.rm = T))

all_damages_by_year <- all_damages %>% dplyr::group_by(experiment_yr, emitter) %>% 
  dplyr::summarise(total_damages_dr2 = sum(total_damages_dr2, na.rm = T))


##################### Carbon Capture experiment ###########################
total_damages_cc <- calculate_bidamages(master_raster,
                                        fair_exps_cc,
                                        c(2020:2100),
                                        2020,
                                        future_forecast_ssp370_2300, 
                                        gdp_temp_data_k80_2300, 
                                        "ERA",
                                        bhm_era_reg,
                                        "no", 
                                        "no")

################################################################################
######################### PART IV: Visualize data   ############################
################################################################################
source("~/BurkeLab Dropbox/Projects/loss_damage/scripts/working/5_visualize_flow_sankey.R")

## Fig 1 
## 1GtC/yr experiment plot 
## 1GtC/yr experiment table
## Green GDP 
## SCC total uncertainty table
## SCC contribution to uncertainty by source
## Sankey 
## Damages as pct. of GDP under future scenarios

## Sankeys
# here is an example
b <- visualize_bidamages("1980", total_damages)

#end of script


