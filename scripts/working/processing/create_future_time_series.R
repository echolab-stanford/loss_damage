##############################################################################
# Mustafa Zahid, August 27th, 2022
# This R script generates the time series for future forecast of both gdp 
# growth rates, as well as temperature. The gdp forecast is obtained by 
# interpolating growth rates from the 5-year intervals of forecasted GDP 
# according to the SSP370 model run. The temperature is obtained by adding 
# FaiR temperature response from forecasted future emissions to the average of 
# temperature variable in the last 5 years of available observed data
# Input(s): Raw SSP data and processed conutry-year panel data for 1960-2009
# as well as Fair temperature response to historical and future SSP370 emissions
# Output(s): Processed 2021-2100 time series with temperature and gdp growth
##############################################################################

##############################################################################
##### PART pre-I: clear environment, load packages, and set directories ######
##############################################################################
rm(list = ls())
gc()

##############################################################################
########################## PART a: SSP370 GDPs ###############################
##############################################################################
  ##############################################################################
  ######################### PART I: read the data ##############################
  ##############################################################################
  # read SSP gdp data 
  ssp_gdp <- readxl::read_xlsx("~/Desktop/loss_damages/IIASA_SSP_GDP.xlsx")
  ssp_pop <- readxl::read_xlsx("~/Desktop/loss_damages/IIASA_SSP_population.xlsx")
  
  # now let us bring in the gdp-temp-year dataset 
  gdp_temp_data <- readRDS("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/temp_gdp_world_panel.rds")
  
  ##############################################################################
  ######################## PART II: clean the data #############################
  ##############################################################################
  
  # select needed vars 
  ssp_gdp <- ssp_gdp %>% dplyr::select(-c("Model", "Variable", "Unit", "Notes"))
  ssp_pop <- ssp_pop %>% dplyr::select(-c("Model", "Variable", "Unit", "Notes"))
  
  # now reshape
  ssp_gdp_long <- reshape2::melt(ssp_gdp)
  ssp_pop_long <- reshape2::melt(ssp_pop)
  
  # now bring them both together 
  ssp_gdp_pop <- left_join(ssp_gdp_long,
                           ssp_pop_long,
                           by = c("Scenario",
                                  "Region",
                                  "variable"))
  colnames(ssp_gdp_pop)[4] <- "gdp"
  colnames(ssp_gdp_pop)[5] <- "pop"
  
  ssp_gdp_pop$year <- as.numeric(as.character(ssp_gdp_pop$variable))
  
  ssp_gdp_pop <- subset(ssp_gdp_pop, Scenario == "SSP3")
  
  sum(ssp_gdp_pop$gdp, na.rm = T)
  
  # now let us scale variables and calculate per capita 
  ssp_gdp_pop$gdp <- ssp_gdp_pop$gdp * 1000000000
  ssp_gdp_pop$pop <- ssp_gdp_pop$pop * 1000000
  
  # now calculate per capita 
  ssp_gdp_pop$gdp_pc <- ssp_gdp_pop$gdp / ssp_gdp_pop$pop
  
  # remove na from this df
  ssp_gdp_pop <- na.omit(ssp_gdp_pop)
  
  # keep ssp scenarios 
  ssp_gdp_pop <- subset(ssp_gdp_pop, Scenario == "SSP1" |
                          Scenario == "SSP2" | Scenario == "SSP3" |
                          Scenario == "SSP4" | Scenario == "SSP5")
  
  # create an ID column so that we can interpolate
  ssp_gdp_pop$id <- paste0(ssp_gdp_pop$Scenario, "_", ssp_gdp_pop$Region)
  
  # set up the year variable
  ssp_gdp_pop$variable <- as.numeric(as.character(ssp_gdp_pop$variable))

  # now generate average annual growth rate between each 5-year interval
  ssp_gdp_pop <- ssp_gdp_pop %>% dplyr::group_by(id) %>% 
    dplyr::mutate(gdp_pc_t1 = lag(gdp_pc, 1),
                  growth = (gdp_pc /gdp_pc_t1)^(1/5) - 1,
                  pop = pop)
  
  # extend the data to go up to 2100 annually 
  ssp_gdp_pop <- ssp_gdp_pop %>% group_by(id) %>% 
    complete(Region, variable = 2010:2100)  %>% 
    mutate(gdp_pc_intrpl = zoo::na.approx(gdp_pc, na.rm = FALSE),
           pop_intrpl = zoo::na.approx(pop, na.rm = FALSE))
  
  ssp_gdp_pop <- ssp_gdp_pop %>% dplyr::select(c("ISO3",
                                                 "model_run",
                                                 "gdp_pc_intrpl",
                                                 "variable",
                                                 "pop_intrpl"))
  
  
  
  # Now we fill in the missing extended years by assigning average growth rate 
  # of the interval that the year falls in 
  for (i in c(2009,2014,2019,2024,2029,2034,
              2039,2044,2049,2054,2059,2064,
              2069,2074,2079,2084,2089,2094)){
    ssp_gdp_pop <- ssp_gdp_pop %>% 
      dplyr::mutate(growth = case_when(variable <i+6 & variable >i ~ growth[variable == i+6],
                                       TRUE ~ growth),
                    pop = case_when(variable <i+6 & variable >i ~ pop[variable == i+6],
                                    TRUE ~ pop))
  }
  
  # rename some data 
  # now let us bring both datasets together and plot them
  ssp_gdp_pop$ISO3 <- substr(ssp_gdp_pop$id, 6, 8)
  ssp_gdp_pop$model_run <- substr(ssp_gdp_pop$id, 1, 4)
  colnames(ssp_gdp_pop)[3] <- "year"
  colnames(ssp_gdp_pop)[9] <- "lgdp_d"
  
##############################################################################
########################## PART b: SSP370 Temps ###############################
##############################################################################
  ##############################################################################
  ###################### PART I: read/clean the FaIR data ######################
  ##############################################################################
  fair_temp <- read_csv("~/BurkeLab Dropbox/Projects/loss_damage/FaIR/fair_temp_resp_1750_2100.csv")
  
  # clear first two rows
  fair_temp <- fair_temp[-c(1:2),]
  
  # numerize the response variable
  fair_temp$Test <- as.numeric(fair_temp$Test)
  
  # generate year variable
  fair_temp$year <- as.numeric(fair_temp$Scenario) + 1750
  
  # now take the median of all the runs
  fair_temp_median <- fair_temp %>% dplyr::group_by(year) %>% 
    dplyr::summarise(median_resp = median(Test, na.rm = T))
  
  # plot to gutt check
  plot(fair_temp_median$year, fair_temp_median$median_resp, type = "l")
  
  # what we need is to recover delta T from 2020 
  fair_temp_median$median_resp_d <- fair_temp_median$median_resp - fair_temp_median$median_resp[fair_temp_median$year == 2020]
  
  # now just leave years after 2020 (future years)
  fair_temp_median <- subset(fair_temp_median, year > 2019)
  
  # ok what we need now is to take average of last 5 years of dataset
  country_level <- gdp_temp_data %>% subset(., year >= 2014 & year <2020) %>% 
    dplyr::group_by(ISO3) %>% 
    dplyr::summarise(mean_temp = median(era_mwtemp, na.rm = T))
  
  # now we need to bring two datasets together: country level average of last 5
  # years, as well as FaIR temperature response to emissions
  # before we do that we need to create a corresponding variable to merge on
  country_level$merge <- 1
  fair_temp_median$merge <- 1
  
  # now let bring them both together 
  future_temp <- left_join(country_level, fair_temp_median)
  
  # now add up temperature response to average temperature
  future_temp$era_mwtemp <- future_temp$mean_temp + future_temp$median_resp_d

  plot(future_temp$year, future_temp$median_resp_d)  
##############################################################################
########################## PART c: SSP370 Temps ###############################
##############################################################################
  # now let us bring in both future forecast ogf temperature and of gdp growth
  future_forecast <- left_join(future_temp,
                               ssp_gdp_pop,
                               by = c("year",
                                      "ISO3"))
  
  # we can save whatever data we need. For now we will write out the SSP370 data
  future_forecast <- future_forecast %>%  subset(., model_run == "SSP3") %>% 
    write_rds(., "~/BurkeLab Dropbox/Projects/loss_damage/data/processed/gdp_eratemp_2021_2100_ssp370.rds")

  
# end of script  
  
  # now let us extend teh data to 2300 with the different scenarios 
  
  #future_forecast_ssp370 <- readRDS("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/world_gdp_pop/gdp_eratemp_2021_2100_ssp370.rds")
  #future_forecast_ssp370 <- future_forecast_ssp370 %>% dplyr::select(c("ISO3", "year", "era_mwtemp", "lgdp_d", "pop"))
  #colnames(future_forecast_ssp370)[4] <- "diff_lgdp"
  #future_forecast_ssp370 <- subset(future_forecast_ssp370, year >2020)
  
  #write_rds(future_forecast_ssp370, "~/BurkeLab Dropbox/Projects/loss_damage/data/processed/future_forecast/future_forecast_ssp370_2300.rds")
  
  # now let us create a sub dataset that extends growth and temperature data onto 
  # 2300 
  future_forecast_ssp370_2300 <- future_forecast_ssp370 %>% 
    dplyr::group_by(ISO3) %>% 
    complete(year = full_seq(2101:2300, 1)) %>% 
    fill(year)
  
  # what we do as default is set the growth and pop numbers at thier 2100 levels
  future_forecast_ssp370_2300 <- future_forecast_ssp370_2300 %>% 
    dplyr::group_by(ISO3) %>% 
    dplyr::mutate(growth_2100 = diff_lgdp[year == 2100],
                  pop_2100 = pop[year == 2100])
  future_forecast_ssp370_2300$diff_lgdp[is.na(future_forecast_ssp370_2300$diff_lgdp)] <- future_forecast_ssp370_2300$growth_2100[is.na(future_forecast_ssp370_2300$diff_lgdp)]
  future_forecast_ssp370_2300$pop[is.na(future_forecast_ssp370_2300$pop)] <- future_forecast_ssp370_2300$pop_2100[is.na(future_forecast_ssp370_2300$pop)]
  
  #write_rds(future_forecast_ssp370_1, "~/BurkeLab Dropbox/Projects/loss_damage/sherlock_files/data/pre_processed/gdp_eratemp_2021_2300_ssp370.rds")
  #write_rds(future_forecast_ssp370_2300, "~/BurkeLab Dropbox/Projects/loss_damage/data/processed/future_forecast/future_forecast_ssp370_2300.rds")
  
  future_forecast_ssp370_2300_1pct <- future_forecast_ssp370 %>% 
    group_by(ISO3) %>% 
    complete(year = full_seq(2101:2300, 1)) %>% 
    fill(year)
  future_forecast_ssp370_2300_1pct <- future_forecast_ssp370_2300_1pct %>% 
    dplyr::group_by(ISO3) %>% 
    dplyr::mutate(growth_2100 = diff_lgdp[year == 2100],
                  pop_2100 = pop[year == 2100])
  future_forecast_ssp370_2300_1pct$diff_lgdp[is.na(future_forecast_ssp370_2300_1pct$diff_lgdp)] <- 0.01
  future_forecast_ssp370_2300_1pct$pop[is.na(future_forecast_ssp370_2300_1pct$pop)] <- future_forecast_ssp370_2300_1pct$pop_2100[is.na(future_forecast_ssp370_2300_1pct$pop)]
  
  future_forecast_ssp370_2300_2pct <- future_forecast_ssp370 %>% 
    group_by(ISO3) %>% 
    complete(year = full_seq(2101:2300, 1)) %>% 
    fill(year)
  future_forecast_ssp370_2300_2pct <- future_forecast_ssp370_2300_2pct %>% 
    dplyr::group_by(ISO3) %>% 
    dplyr::mutate(growth_2100 = diff_lgdp[year == 2100],
                  pop_2100 = pop[year == 2100])
  future_forecast_ssp370_2300_2pct$diff_lgdp[is.na(future_forecast_ssp370_2300_2pct$diff_lgdp)] <- 0.02
  future_forecast_ssp370_2300_2pct$pop[is.na(future_forecast_ssp370_2300_2pct$pop)] <- future_forecast_ssp370_2300_2pct$pop_2100[is.na(future_forecast_ssp370_2300_2pct$pop)]
  
  #write_rds(future_forecast_ssp370_2300_1pct, "~/BurkeLab Dropbox/Projects/loss_damage/data/processed/future_forecast/future_forecast_ssp370_2300_1pct.rds")
  #write_rds(future_forecast_ssp370_2300_2pct, "~/BurkeLab Dropbox/Projects/loss_damage/data/processed/future_forecast/future_forecast_ssp370_2300_2pct.rds")
  
  
  future_forecast_ssp370_2300 <- readRDS("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/future_forecast/future_forecast_ssp370_2300.rds")
  
  future_forecast_ssp370_2300_1pct <- readRDS("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/future_forecast/future_forecast_ssp370_2300_1pct.rds")
  future_forecast_ssp370_2300_2pct <- readRDS("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/future_forecast/future_forecast_ssp370_2300_2pct.rds")
  #future_forecast_ssp370_2300 <- readRDS("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/future_forecast/future_forecast_ssp370_2300.rds")
  
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
  


  