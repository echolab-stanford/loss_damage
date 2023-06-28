##############################################################################
# Mustafa Zahid, December 8th, 2021
# This R script processes the income gdp data from the world bank, as well as 
# the temperature and precipitation data from ERA5 and CRU data 
# Input(s): Two .nc files for average temperatures as well as precipitation 
# Output(s): a cleaned panel with temp and income data 
##############################################################################

rm(list=ls())

# download the needed packages
require(pacman)
p_load(raster, 
       maptools, 
       dplyr, 
       rio, 
       ggplot2, 
       sf, 
       grid, 
       gridExtra, 
       plm, 
       fixest, 
       data.table, 
       foreign,
       ncdf4)

#install.packages("WDI")
#install.packages("wbstats")
library(WDI)
library(wbstats)

##############################################################################
######################## PART I: Process the income data #################kl####
##############################################################################
#new_wdi_cache <- WDIcache() 
#WDIsearch("gdp.*ppp* *capita.*US\\$", cache = new_wdi_cache)
#wdi_dat <- WDI(indicator = c("NY.GDP.PCAP.PP.KD", "NY.GDP.PCAP.KD",
#                             "NY.GDP.MKTP.CD", "SP.DYN.LE00.IN", 
 #                            "SP.DYN.IMRT.IN", "SP.POP.TOTL"), 
  #             start = 1960, 
   #            end = 2021,
    #           extra = TRUE) 
#names(wdi_dat)
#length(unique(wdi_dat$iso3c))
gdp_temp_data <- readRDS("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/temp_gdp_world_panel.rds")
gdp_temp_data <- readRDS("~/Desktop/FSE_Projects/loss_damages/temp_gdp_world_panel.rds")
#write_rds(gdp_temp_data, "~/BurkeLab Dropbox/Projects/loss_damage/data/processed/temp_gdp_world_panel.rds")
gdp_temp_data <- gdp_temp_data %>% dplyr::select(c("ISO3", "year", "era_mwtemp", "era_mwprecip"))

#devtools::install_github("vincentarelbundock/WDI")
wdi_dat <- wb_data(indicator = c("NY.GDP.PCAP.KD", "SP.POP.TOTL", "NY.GDP.PCAP.CD", "NY.GDP.PCAP.PP.KD"),
                   country = "countries_only",
                   start_date = 1960, end_date = 2020,
                   return_wide = T)

#wdi_dat <- rbind(wdi_dat,wdi_dat1)
colnames(wdi_dat)[4] <- "year"

wdi_dat <- wdi_dat %>% dplyr::select("iso3c", "year", "SP.POP.TOTL")

gdp_temp_data <- left_join(gdp_temp_data,
                           wdi_dat,
                           by = c("ISO3" = "iso3c",
                                  "year" = "year"))


# before processing and using the GDP numbers, we need to rebase the numbers 
# from 2015 numbers to 2020. In the next step we will want to  fill in missing years 
# with Penn World Table data. The Penn World Table end in 2019 and as such we 
# cannot base Penn table data to 2020 from 2017 (the original base). So what 
# we are going to do is rebase WB data to 2017, then merge data fill in missing 
# and then rebase to 2020
# first let us get the 2017 index by dividing each country's current 2017 value 
# by the constant 2015 equivalent 
wdi_dat <- wdi_dat %>% dplyr::group_by(iso3c) %>% 
  dplyr::mutate(index_2017 = NY.GDP.PCAP.CD[year == "2017"] / NY.GDP.PCAP.KD[year == "2017"])

# now that we have the index we can rescale the gdp numbers to be in 2017 USD 
# we are going to do that by multuplying our 2017 ratio by the 2015 constant 
# USD value. I'm not going to give the new scaled value a different name 
# since the scripts are already written using the 2015 constant name
wdi_dat$NY.GDP.PCAP.KD <- wdi_dat$NY.GDP.PCAP.KD * wdi_dat$index_2017

# select columns for a specific pop-country-year datsset
pop_wdi <- wdi_dat %>% dplyr::select(c("iso3c", "SP.POP.TOTL", "year")) 

# Now the world bank data has missing values for some countries. For example. 
# Ethiopia seem to be missing a couple of decades of data 
library("pwt10")

# let us read the pwt data. 
pwt_dta <- pwt10::pwt10.0
# select the needed data 
pwt_dta <- pwt_dta %>% dplyr::select(c("isocode", "year", "rgdpna", "pop"))


wdi_dat$ppp_d <- wdi_dat$NY.GDP.PCAP.KD / wdi_dat$NY.GDP.PCAP.PP.KD


# rename the columns before merging 
colnames(pwt_dta) <- c("iso3c", "year", "pwt_gdp", "pwt_pop")

# now we will need to merge both datasets 
wdi_dat <- left_join(wdi_dat,
                     pwt_dta,
                     by = c("iso3c", "year"))


# now let us assign to the country it is ppp
wdi_dat <- wdi_dat %>% dplyr::group_by(iso3c) %>% 
  dplyr::mutate(ppp_d = mean(ppp_d, na.rm = T))

wdi_dat$pwt_gdppcap <- (wdi_dat$pwt_gdp)/ (wdi_dat$pwt_pop)
wdi_dat$pwt_gdppcap <- wdi_dat$pwt_gdppcap * wdi_dat$ppp_d

#plot(wdi_dat$NY.GDP.PCAP.PP.KD, (wdi_dat$pwt_gdp/wdi_dat$pwt_pop))

# now that we have both merged, let us fill in the missing cells 
wdi_dat$NY.GDP.PCAP.KD[is.na(wdi_dat$NY.GDP.PCAP.KD)] <- wdi_dat$pwt_gdppcap[is.na(wdi_dat$NY.GDP.PCAP.KD)]


# now let us rescale data so that it is expressed in 2020 dollars 
wdi_dat <- wdi_dat %>% dplyr::group_by(iso3c) %>% 
  dplyr::mutate(index_2020 = NY.GDP.PCAP.CD[year == "2020"] / NY.GDP.PCAP.KD[year == "2020"])

wdi_dat$NY.GDP.PCAP.KD <- wdi_dat$NY.GDP.PCAP.KD * wdi_dat$index_2020

# let us get Yemen and Venezuela 
wdi_dat$NY.GDP.PCAP.KD[wdi_dat$iso3c == "YEM" | 
                         wdi_dat$iso3c == "VEN" | 
                         wdi_dat$iso3c == "TKM"] <- wdi_dat$NY.GDP.PCAP.CD[wdi_dat$iso3c == "YEM" | 
                                                                             wdi_dat$iso3c == "VEN" | 
                                                                             wdi_dat$iso3c == "TKM"]

wdi_dat$NY.GDP.PCAP.KD[(wdi_dat$iso3c == "KAZ" & is.na(wdi_dat$NY.GDP.PCAP.KD))] <- wdi_dat$NY.GDP.PCAP.CD[(wdi_dat$iso3c == "KAZ" & is.na(wdi_dat$NY.GDP.PCAP.KD))]

wdi_dat$NY.GDP.PCAP.KD[(wdi_dat$iso3c == "KAZ") & wdi_dat$year == 1989] <- wdi_dat$NY.GDP.PCAP.KD[(wdi_dat$iso3c == "KAZ") & wdi_dat$year == 1990]

wdi_dat_adjusted <- wdi_dat

#devtools::install_github("vincentarelbundock/WDI")
wdi_dat <- wb_data(indicator = c("NY.GDP.PCAP.KD", "SP.POP.TOTL", "NY.GDP.PCAP.CD", "NY.GDP.PCAP.PP.KD"),
                   country = "countries_only",
                   start_date = 1960, end_date = 2020,
                   return_wide = T)

#wdi_dat <- rbind(wdi_dat,wdi_dat1)
colnames(wdi_dat)[4] <- "year"


# now let us rescale data so that it is expressed in 2020 dollars 
wdi_dat <- wdi_dat %>% dplyr::group_by(iso3c) %>% 
  dplyr::mutate(index_2020 = NY.GDP.PCAP.CD[year == "2020"] / NY.GDP.PCAP.KD[year == "2020"])

wdi_dat$NY.GDP.PCAP.KD <- wdi_dat$NY.GDP.PCAP.KD * wdi_dat$index_2020

plot(wdi_dat$year[wdi_dat$iso3c == "ETH"],
     wdi_dat$NY.GDP.PCAP.KD[wdi_dat$iso3c == "ETH"])

colnames(wdi_dat_adjusted)[6] <- paste0(colnames(wdi_dat_adjusted)[6], "_for_damages")

colnames(wdi_dat) 

wdi_dat <- wdi_dat %>% dplyr::select(c("iso3c", "year", "NY.GDP.PCAP.KD"))

wdi_dat_adjusted <- left_join(wdi_dat_adjusted,
                              wdi_dat,
                              by = c("iso3c", "year"))

plot(wdi_dat_adjusted$year[wdi_dat$iso3c == "ETH"],
     wdi_dat_adjusted$NY.GDP.PCAP.KD_for_damages[wdi_dat$iso3c == "ETH"])

lines(wdi_dat_adjusted$year[wdi_dat$iso3c == "ETH"],
      wdi_dat_adjusted$NY.GDP.PCAP.KD[wdi_dat$iso3c == "ETH"],
      type = "l", col = "red")

plot(wdi_dat_adjusted$year[wdi_dat$iso3c == "VEN"],
      wdi_dat_adjusted$NY.GDP.PCAP.KD_for_damages[wdi_dat$iso3c == "VEN"],
      type = "l", col = "red")

plot(wdi_dat_adjusted$year[wdi_dat$iso3c == "RUS"],
     wdi_dat_adjusted$NY.GDP.PCAP.KD_for_damages[wdi_dat$iso3c == "RUS"],
     type = "l", col = "red")


gdp_temp_data <- left_join(gdp_temp_data,
                           wdi_dat_adjusted,
                           by = c("ISO3" = "iso3c",
                                  "year" = "year"))

head(gdp_temp_data)
write_rds(gdp_temp_data, "~/BurkeLab Dropbox/Projects/loss_damage/data/processed/temp_gdp_world_panel_052223.rds")


##############################################################################
######################## PART II: Process the TEMP data ######################
##############################################################################
############################## 1) read the data ##############################
##############################################################################
### ERA5 monthly averaged data, 1981--2021/09
  # 2m temperature, Kelvin degrees
  era_temp_81_20 <- raster::brick("~/BurkeLab Dropbox/Data/ERA5/Global/2m_temperature_2021/era5land_temp_1981_2020.nc")
  era_temp_81_20 <- raster::aggregate(era_temp_81_20, fact = 5, fun = "mean")
  era_temp_81_20 <- rotate(era_temp_81_20)
  

  era_temp_60_80 <- raster::brick("~/BurkeLab Dropbox/Data/ERA5/Global/2m_temperature_2021/era5land_temp_1960_1980.nc")
  era_temp_60_80 <- raster::aggregate(era_temp_60_80, fact = 5, fun = "mean")
  era_temp_60_80 <- rotate(era_temp_60_80)
  
  era_temp <- raster::stack(era_temp_60_80, 
                            era_temp_81_20)
 # era_temp <- rotate(era_temp)
  
  # precipitation, meters
  era_precip_60_80 <- raster::brick("~/BurkeLab Dropbox/Data/ERA5/Global/total_precip/era5land_precip_1960_1980.nc")
  era_precip_60_80 <- raster::aggregate(era_precip_60_80, fact = 5, fun = "mean")
  # the source data extent has the x go from 0 to 360 instead of -180 to 180
  era_precip_60_80 <- rotate(era_precip_60_80)
  
  era_precip_81_20 <- raster::brick("~/BurkeLab Dropbox/Data/ERA5/Global/total_precip/era5land_precip_1981_2020.nc")
  era_precip_81_20 <- raster::aggregate(era_precip_81_20, fact = 5, fun = "mean")
  # the source data extent has the x go from 0 to 360 instead of -180 to 180
  era_precip_81_20 <- rotate(era_precip_81_20)
  #bring them together
  era_precip <- raster::stack(era_precip_60_80,
                              era_precip_81_20)

### CRU, 1901--2020
  #monthly average  temperature, Celcius degrees
  cru_temp <- raster::brick("~/BurkeLab Dropbox/Data/CRU/temp/cru_ts4.05.1901.2020.tmp.dat.nc")
  
  #precipitation, millimetres per month
  cru_precip <- raster::brick("~/BurkeLab Dropbox/Data/CRU/precip/cru_ts4.05.1901.2020.pre.dat.nc")
  
# in order to aggregate up pop-weighted country level averages, we need to 
# import gridded pop data 
  pop_grids <- raster("~/BurkeLab Dropbox/Projects/loss_damage/data/raw/population/gpw_v4_population_count_rev11_2010_1_deg.tif")
  pop_grids <- readAll(pop_grids)
  
  ### country level shapefile to aggregate variables to the country level 
  data(wrld_simpl)
  country_shape <- wrld_simpl
  country_shape_sf <- st_as_sf(country_shape) 
  country_df <- cbind.data.frame(1:nrow(country_shape@data), 
                                 country_shape@data$ISO3, 
                                 country_shape@data$NAME)
  colnames(country_df) <- c("country_id", "ISO3")

##############################################################################
######################## 2) process and clean the data #######################
##############################################################################
### PREPARE ERA5 DATA
  # era_grid is a single raster layer of era (since we only want cell numbers)
  era_grid <- era_temp[[1]]
  #era_grid <- rotate(era_grid)
  # resample GPW raster with ERA grid
  era_pop_grid <- raster::resample(pop_grids, era_grid) 
  #era_pop_grid[era_pop_grid < 0] <- 0
  
  
  
  #process weighted mean at the country level
  era_temp_avgs <- exactextractr::exact_extract(era_temp, 
                                       country_shape_sf,
                                       'weighted_mean',
                                       weights = era_pop_grid,
                                       default_weight = 0,
                                       append_cols = 'ISO3')
  #now longate the dataset 
  era_temp_avgs_long <- melt(era_temp_avgs, id = c("ISO3"))
  
  ### PREPARE ERA5 DATA
  # era_grid is a single raster layer of era (since we only want cell numbers)
  era_grid <- era_precip[[1]]
  # resample GPW raster with ERA grid
  era_pop_grid <- raster::resample(pop_grids, era_grid) 
  
  #Now process the era precip data 1960-1980 
  era_precip_avgs <- exactextractr::exact_extract(era_precip_60_80, 
                                                   country_shape_sf,
                                                   'weighted_mean',
                                                   weights = era_pop_grid,
                                                   default_weight = 0,
                                                   append_cols = 'ISO3')
  #Now process the era precip data 1981-2020 
  era_precip_avgs1 <- exactextractr::exact_extract(era_precip_81_20, 
                                                country_shape_sf,
                                                'weighted_mean',
                                                weights = era_pop_grid,
                                                default_weight = 0,
                                                append_cols = 'ISO3')
  # now we can longate the data
  era_precip_avgs_long <- reshape2::melt(era_precip_avgs, id = c("ISO3"))
  era_precip_avgs1_long <- reshape2::melt(era_precip_avgs1, id = c("ISO3"))
  #now bring them together
  era_precip_long <- rbind(era_precip_avgs_long,
                           era_precip_avgs1_long)

### REPEAT FOR CRU DATA
  # cru_grid is a single raster layer of era (since we only want cell numbers)
  cru_grid <- cru_temp[[1]]
  # resample GPW raster with ERA grid
  cru_pop_grid <- raster::resample(pop_grids, cru_grid) 
  
  #process weighted mean at the country level
  cru_temp_avgs <- exactextractr::exact_extract(cru_temp, 
                                                country_shape_sf,
                                                'weighted_mean',
                                                weights = cru_pop_grid,
                                                default_weight = 0,
                                                append_cols = 'ISO3')
  #longating the dataframe
  cru_temp_avgs_long <- reshape2::melt(cru_temp_avgs, id = c("ISO3"))
  
  #Now process the cru precip data 1901-2020
  cru_precip_avgs <- exactextractr::exact_extract(cru_precip, 
                                                country_shape_sf,
                                                'weighted_mean',
                                                weights = cru_pop_grid,
                                                default_weight = 0,
                                                append_cols = 'ISO3')
  
  #longating the dataframe
  cru_precip_avgs_long <- reshape2::melt(cru_precip_avgs, id = c("ISO3"))

##############################################################################
################## PART III: Create temp-income panel data ###################
##############################################################################
# Bring all data together 
  # lets average and rename before joining 
  era_temp_df <- era_temp_avgs_long %>% dplyr::rename(era_mwtemp = value,
                                                         time = variable)
  
  head(era_temp_df)
  era_temp_averages <- era_temp_df %>% 
    mutate(year = substr(time, 16, 19)) %>% 
    dplyr::group_by(ISO3, year) %>% 
    dplyr::summarise(era_mwtemp = mean(era_mwtemp, na.rm = T), 
                     .groups = "keep")
  
  era_precip_df <- era_precip_long %>% dplyr::rename(era_mwprecip = value,
                                                         time = variable)
  
  era_precip_averages <- era_precip_df %>% 
    mutate(year = substr(time, 16, 19)) %>% 
    dplyr::group_by(ISO3, year) %>% 
    dplyr::summarise(era_mwprecip = mean(era_mwprecip, na.rm = T), 
                     .groups = "keep")
  
  cru_temp_df <- cru_temp_avgs_long %>% 
    dplyr::rename(cru_mwtemp = value,
                  time = variable)
  
  cru_temp_averages <- cru_temp_df %>% 
    mutate(year = substr(time, 16, 19)) %>% 
    dplyr::group_by(ISO3, year) %>% 
    dplyr::summarise(cru_mwtemp = mean(cru_mwtemp, na.rm = T), 
                     .groups = "keep")
  
  cru_precip_df <- cru_precip_avgs_long %>% dplyr::rename(cru_mwprecip = value,
                  time = variable)
  
  cru_precip_averages <- cru_precip_df %>% 
    mutate(year = substr(time, 16, 19)) %>% 
    dplyr::group_by(ISO3, year) %>% 
    dplyr::summarise(cru_mwprecip = mean(cru_mwprecip, na.rm = T), 
                     .groups = "keep")
  
  era_data <- left_join(era_temp_averages,
            era_precip_averages,
            by = c("year", "ISO3"))
  
  cru_data <- left_join(cru_temp_averages,
                        cru_precip_averages,
                        by = c("year", "ISO3"))
  
  # UDel Temp vars 
  udeltemp <- raster::stack("~/Downloads/air.mon.mean.v501.nc")
  
  udeltemp <- raster::rotate(udeltemp)
  
  pop_grids <- raster::raster("~/BurkeLab Dropbox/Data/population/processed_rasters/WORLDPOP_2015_GLOBE_1K_projected.tif")
  
  udel_grid <- udeltemp[[1]]
  plot(udel_grid)
  # resample GPW raster with ERA grid
  era_pop_grid <- raster::resample(pop_grids, udel_grid) 
  
  #process weighted mean at the country level
  udel_temp_avgs <- exactextractr::exact_extract(udeltemp, 
                                                 country_shape_sf,
                                                 'weighted_mean',
                                                 weights = era_pop_grid,
                                                 default_weight = 0,
                                                 append_cols = 'ISO3')
  
  udel_temp_avgs_long <- reshape2::melt(udel_temp_avgs, id = c("ISO3"))
  
  
  udelprecip <- raster::stack("~/Downloads/precip.mon.total.v501.nc")
  
  udelprecip <- raster::rotate(udelprecip)
  
  pop_grids <- raster::raster("~/BurkeLab Dropbox/Data/population/processed_rasters/WORLDPOP_2015_GLOBE_1K_projected.tif")
  
  udel_grid <- udeltemp[[1]]
  
  # resample GPW raster with ERA grid
  era_pop_grid <- raster::resample(pop_grids, udel_grid) 
  
  #process weighted mean at the country level
  udel_precip_avgs <- exactextractr::exact_extract(udelprecip, 
                                                   country_shape_sf,
                                                   'weighted_mean',
                                                   weights = era_pop_grid,
                                                   default_weight = 0,
                                                   append_cols = 'ISO3')
  
  udel_precip_avgs_long <- reshape2::melt(udel_precip_avgs, id = c("ISO3"))
  
  colnames(udel_temp_avgs_long)[3] <- "udel_temp"
  colnames(udel_temp_avgs_long)[2] <- "year"
  
  udel_temp_avgs_long$year <- substr(udel_temp_avgs_long$year, 16,19)
  
  udel_temp_avgs <- udel_temp_avgs_long %>% dplyr::group_by(year, ISO3) %>%
    dplyr::summarise(udel_temp = mean(udel_temp, na.rm = T), .groups = "keep")
  
  colnames(udel_precip_avgs_long)[3] <- "udel_precip"
  colnames(udel_precip_avgs_long)[2] <- "year"
  
  udel_precip_avgs_long$year <- substr(udel_precip_avgs_long$year, 16,19)
  
  udel_precip_avgs <- udel_precip_avgs_long %>% dplyr::group_by(year, ISO3) %>% 
    dplyr::summarise(udel_precip = mean(udel_precip, na.rm = T), .groups = "keep")
  
  
  udel_data <- left_join(udel_temp_avgs,
                        udel_precip_avgs,
                        by = c("year", "ISO3"))
  
  
  
  udel_climate$year <- substr(udel_climate$year, 16,19)
  
  udel_climate <- left_join(udel_temp_avgs,
                            udel_precip_avgs,
                            by = c("year",
                                   "ISO3"))
  
  udel_climate$year <- as.numeric(as.character(udel_climate$year))
  data$year1 <- as.numeric(as.character(data$year))
  
  data_udel <- left_join(data, udel_climate,
                     by = c("year1" = "year",
                            "ISO3" = "ISO3"))
  
  write_csv(data_udel, "~/Desktop/loss_damages/data1.csv")
  
  
  
  # NOW BRING ALL TOGETHER 
  all_sets_df <- left_join(cru_data,
                           era_data,
                           by = c("year", "ISO3"))
  
  all_sets_df <- left_join(all_sets_df,
                           udel_data,
                           by = c("year", "ISO3"))

  
  #adjust units for era data (convert from kelvin to celsius)
  all_sets_df$era_mwtemp <- all_sets_df$era_mwtemp - 273.15
  #convert precipitation from meter to millimeter
  all_sets_df$era_mwprecip <- all_sets_df$era_mwprecip *1000
  tail(all_sets_df)
  #now bring the gdp data in
  all_sets_df$year <- as.numeric(all_sets_df$year)
  all_sets_df <- left_join(all_sets_df,
                           wdi_dat,
                           by = c("year" = "year",
                                  "ISO3" = "iso3c"))
  ### split sample on median and test rich vs poor countries
  avg_wealth <- all_sets_df %>% dplyr::group_by(ISO3) %>% 
    dplyr::summarise(avg_gdppc = mean(NY.GDP.PCAP.KD, na.rm = T)) 
  
  avg_wealth$rich <- 0
  avg_wealth$rich[avg_wealth$avg_gdppc > median(avg_wealth$avg_gdppc, na.rm = T)] <- 1
  avg_wealth <- avg_wealth %>% dplyr::select(ISO3, rich)
  all_sets_df <- left_join(all_sets_df, avg_wealth)
  rich <- all_sets_df %>% subset(rich == 1)
  poor <- all_sets_df %>% subset(rich == 0)
  
  ### split sample at middle year and test early period vs late period
  all_sets_df$Elate <- 0
  all_sets_df$Elate[as.numeric(all_sets_df$year) + 1959 > 1999.5] <- 1
  all_sets_df$Clate <- 0
  all_sets_df$Clate[as.numeric(all_sets_df$year) + 1959 > 1990.5] <- 1
  Elate <- all_sets_df %>% subset(Elate == 1)
  Eearly <- all_sets_df %>% subset(Elate == 0)
  Clate <- all_sets_df %>% subset(Clate == 1)
  Cearly <- all_sets_df %>% subset(Clate == 0)
  
  readr::write_rds(all_sets_df, "~/Desktop/temp_gdp_world_panel.rds")
  readr::write_csv(all_sets_df, "~/Desktop/temp_gdp_world_panel.csv")
  
  readr::write_csv(all_sets_df, "~/BurkeLab Dropbox/projects/loss_damage/data/processed/temp_gdp_world_panel_110322.csv")
  
  
  all_sets_df1 <- all_sets_df %>% ungroup(.) %>% 
    dplyr::select(c("cru_mwtemp", 
                                                  "era_mwtemp",
                                                  "udel_temp"))
  
  all_sets_df1a <- subset(all_sets_df1, !is.na(era_mwtemp))
  all_sets_df1a <- subset(all_sets_df1a, udel_temp != "NaN")
  
  plot(all_sets_df1$udel_temp, all_sets_df1$era_mwtemp)
  points(all_sets_df1$cru_mwtemp, all_sets_df1$udel_temp, col = 
         "red")
  
  
  
  ## wdi data 
  
  # read world bank data to be used later on in calculation for GDP and POP
  wdi_dat <- wb_data(indicator = c("NY.GDP.PCAP.KD", "SP.POP.TOTL", "NY.GDP.PCAP.CD", "NY.GDP.PCAP.PP.KD"),
                     country = "countries_only",
                     start_date = 1960, end_date = 2020,
                     return_wide = T)
  
  #write_rds(wdi_dat, "~/Desktop/wdi_dat.rds")
  # rename columns
  colnames(wdi_dat)[4] <- "year"
  
  # before processing and using the GDP numbers, we need to rebase the numbers 
  # from 2015 numbers to 2020. In the next step we will want to  fill in missing years 
  # with Penn World Table data. The Penn World Table end in 2019 and as such we 
  # cannot base Penn table data to 2020 from 2017 (the original base). So what 
  # we are going to do is rebase WB data to 2017, then merge data fill in missing 
  # and then rebase to 2020
  # first let us get the 2017 index by dividing each country's current 2017 value 
  # by the constant 2015 equivalent 
  wdi_dat <- wdi_dat %>% dplyr::group_by(iso3c) %>% 
    dplyr::mutate(index_2017 = NY.GDP.PCAP.CD[year == "2017"] / NY.GDP.PCAP.KD[year == "2017"])
  
  # now that we have the index we can rescale the gdp numbers to be in 2017 USD 
  # we are going to do that by multuplying our 2017 ratio by the 2015 constant 
  # USD value. I'm not going to give the new scaled value a different name 
  # since the scripts are already written using the 2015 constant name
  wdi_dat$NY.GDP.PCAP.KD <- wdi_dat$NY.GDP.PCAP.KD * wdi_dat$index_2017
  
  # select columns for a specific pop-country-year datsset
  #pop_wdi <- wdi_dat %>% dplyr::select(c("iso3c", "SP.POP.TOTL", "year")) 
  
  # Now the world bank data has missing values for some countries. For example. 
  # Ethiopia seem to be missing a couple of decades of data 
  library("pwt10")
  
  # let us read the pwt data. 
  pwt_dta <- pwt10::pwt10.0
  # select the needed data 
  pwt_dta <- pwt_dta %>% dplyr::select(c("isocode", "year", "rgdpna", "pop"))
  
  
  wdi_dat$ppp_d <- wdi_dat$NY.GDP.PCAP.KD / wdi_dat$NY.GDP.PCAP.PP.KD
  
  # rename the columns before merging 
  colnames(pwt_dta) <- c("iso3c", "year", "pwt_gdp", "pwt_pop")
  
  # now we will need to merge both datasets 
  wdi_dat <- left_join(wdi_dat,
                       pwt_dta,
                       by = c("iso3c", "year"))
  # now let us assign to the country it is ppp
  wdi_dat <- wdi_dat %>% dplyr::group_by(iso3c) %>% 
    dplyr::mutate(ppp_d = mean(ppp_d, na.rm = T))
  wdi_dat$pwt_gdppcap <- (wdi_dat$pwt_gdp)/ (wdi_dat$pwt_pop)
  wdi_dat$pwt_gdppcap <- wdi_dat$pwt_gdppcap * wdi_dat$ppp_d
  plot(wdi_dat$NY.GDP.PCAP.PP.KD, (wdi_dat$pwt_gdp/wdi_dat$pwt_pop))
  # now that we have both merged, let us fill in the missing cells 
  wdi_dat$NY.GDP.PCAP.KD[is.na(wdi_dat$NY.GDP.PCAP.KD)] <- wdi_dat$pwt_gdppcap[is.na(wdi_dat$NY.GDP.PCAP.KD)]
  
  
  # now let us rescale data so that it is expressed in 2020 dollars 
  wdi_dat <- wdi_dat %>% dplyr::group_by(iso3c) %>% 
    dplyr::mutate(index_2020 = NY.GDP.PCAP.CD[year == "2020"] / NY.GDP.PCAP.KD[year == "2020"])
  
  wdi_dat$NY.GDP.PCAP.KD <- wdi_dat$NY.GDP.PCAP.KD * wdi_dat$index_2020
  
  # let us get Yemen and Venezuela 
  wdi_dat$NY.GDP.PCAP.KD[wdi_dat$iso3c == "YEM" | 
                           wdi_dat$iso3c == "VEN" | 
                           wdi_dat$iso3c == "TKM"] <- wdi_dat$NY.GDP.PCAP.CD[wdi_dat$iso3c == "YEM" | 
                                                                               wdi_dat$iso3c == "VEN" | 
                                                                               wdi_dat$iso3c == "TKM"]
  wdi_dat$NY.GDP.PCAP.KD[(wdi_dat$iso3c == "KAZ" & is.na(wdi_dat$NY.GDP.PCAP.KD))] <- wdi_dat$NY.GDP.PCAP.CD[(wdi_dat$iso3c == "KAZ" & is.na(wdi_dat$NY.GDP.PCAP.KD))]
  wdi_dat$NY.GDP.PCAP.KD[(wdi_dat$iso3c == "KAZ") & wdi_dat$year == 1989] <- wdi_dat$NY.GDP.PCAP.KD[(wdi_dat$iso3c == "KAZ") & wdi_dat$year == 1990]
  
  colnames(wdi_dat)
  
  for(i in unique(wdi_dat$ISO3)){
    wdi_dat$diff_lgdp_for_damages[wdi_dat$ISO3 == i & is.na(wdi_dat$diff_lgdp_for_damages) & wdi_dat$year == 1990] <- 0
  }
  
  for(i in unique(wdi_dat$ISO3)){
    if (i == "TKM" | i == "VEN" | i == "ERI" | i == "YEM"){
      wdi_dat$diff_lgdp_for_damages[wdi_dat$ISO3 == i & is.na(wdi_dat$diff_lgdp_for_damages) & (wdi_dat$year > 2014 & wdi_dat$year <2021) ] <- 0
    }
    
  }
  
  for(i in unique(wdi_dat$ISO3)){
    if (i == "LBY"){
      wdi_dat$diff_lgdp_for_damages[wdi_dat$ISO3 == i & is.na(wdi_dat$diff_lgdp_for_damages) & wdi_dat$year < 2000] <- 0
    }
  }
  
  #write_rds(wdi_dat, "~/BurkeLab Dropbox/Projects/loss_damage/data/processed/world_gdp_pop/wdi_data.rds")
  #write_rds(pop_wdi, "~/BurkeLab Dropbox/Projects/loss_damage/data/processed/pop_wdi.rds")
  
  # read the processed data 
  #wdi_dat <- readRDS("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/world_gdp_pop/wdi_data.rds")
  
  #pop_wdi <- readRDS("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/world_gdp_pop/pop_wdi.rds")
  
  
  
  