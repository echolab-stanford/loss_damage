##############################################################################
# Mustafa Zahid, January 7th, 2023
# This R script reads the data and prepares the necessary data to plots figure
# ED1. Figure ED1 demonstrates the different steps taken to calculate teh damages
#############################################################################
remove(list=ls())
gc()
sf::sf_use_s2(FALSE)
setwd("~/GitHub/loss_damage")

replicate <- F# change T to F if you want to create your own data  
if (replicate == T){
  run_date <- "20230523"
}
if (replicate == F){
  run_date <- gsub("-","",Sys.Date())
}

run_date <- "20230821"

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
###############        PART I: read and prepare the data      ##################
################################################################################
################################################################################
################################################################################
# 1a 

#read data 
emissions_dataset <- read_csv(paste0(dropbox_path, "data/raw/emissions/GCB2021v34_MtCO2_flat.csv"))

#select needed columns
emissions_dataset <- emissions_dataset %>% 
  dplyr::select(c("ISO 3166-1 alpha-3","Year","Total"))

emissions_dataset$Year <- as.numeric(emissions_dataset$Year)

# we are assuming perturbation here is shutting off US emissions 
# to do that we need to subset US numbers and joing them into main
us_emissions <- emissions_dataset %>% 
  subset(`ISO 3166-1 alpha-3` == "USA" & Year >= 1900)

us_emissions$Total[us_emissions$Year < 1990] <- 0
# now we want to merge US 1990 numbers to total world numbers 
world_emissions <- emissions_dataset %>% 
  subset(`ISO 3166-1 alpha-3` == "WLD")

world_minus_us_emms <- left_join(world_emissions,
                                 us_emissions,
                                 by = c("Year"))

world_minus_us_emms$Total.y[is.na(world_minus_us_emms$Total.y)] <- 0

world_minus_us_emms$Total.x <- world_minus_us_emms$Total.x - world_minus_us_emms$Total.y

# select the columns we need 
world_minus_us_emms <- world_minus_us_emms %>% dplyr::select(c("Year", "Total.x"))
colnames(world_minus_us_emms)[2] <- "total"
colnames(world_minus_us_emms)[1] <- "year"

colnames(world_emissions)[2] <- "year"
colnames(world_emissions)[3] <- "total"

world_emissions$total <- (world_emissions$total / 1000)/3.67
world_minus_us_emms$total <- (world_minus_us_emms$total / 1000)/3.67

#This data is ready for plotting 
write_rds(world_minus_us_emms, paste0(fig_prepped_dta, run_date,"world_minus_us_emms.rds"))
write_rds(world_emissions, paste0(fig_prepped_dta, run_date,"world_emissions.rds"))

################################################################################
################################################################################
# 1b

# now let us process the FaIR temperature response to emissions by running the 
# created function 
#gtco2_effect <- process_disagg_exp_data("1GtCO2_hist_2300", 1990)

fair_exps_isos_k90 <- process_exp_data_hist("20230523", "hist_bi_2100", 
                                                1990, aggregating = F)


# alt: have 1b as the deltaT coming from the emissions of us 
fair_exps_isos_k90_usa <- subset(fair_exps_isos_k90, experiment_iso == "USA")

# OK NOW LET US GET max and min
# now caculate bounds 
gtc1_fair_exp <- fair_exps_isos_k90_usa %>% dplyr::group_by(year) %>% 
  dplyr::mutate(p_05 = quantile(deltaT, 0.10, na.rm = T),
                p_95 = quantile(deltaT, 0.90, na.rm = T))

# now subset the dataset to keep values within bounds
gtc1_fair_exp <- subset(gtc1_fair_exp, deltaT > p_05 & deltaT < p_95)

# now let us calculate the average min and max
gtc1_fair_exp <- gtc1_fair_exp %>% dplyr::group_by(experiment_iso, year) %>% 
  dplyr::summarise(min = min(deltaT, na.rm = T),
                   median = median(deltaT, na.rm = T),
                   max = max(deltaT, na.rm = T))

# now reorder data to prep for plotting 
gtc1_fair_exp <- gtc1_fair_exp[order(gtc1_fair_exp$year),] 

# dataset is ready for plotting
gtc1_fair_exp <- as.list(as.data.frame(gtc1_fair_exp))
gtc1_fair_exp <- as.data.frame(gtc1_fair_exp)

write_rds(gtc1_fair_exp, paste0(fig_prepped_dta, run_date,"gtc1_fair_exp.rds"))

################################################################################
################################################################################
# 1c
raster_cgm <- raster(paste0(dropbox_path, "data/processed/r_cgm/median_raster.tiff"))


world <- spData::world
world <- st_as_sf(world)
world <- subset(world, name_long != "Antarctica")
world$ISO3 <- countrycode::countrycode(sourcevar = world$iso_a2,origin = "iso2c",
                                       destination = "iso3c"
)

world <- subset(world, !is.na(ISO3))

library(terra)
world <- terra::vect(world)
r <- terra::rast(raster_cgm)
r2 <- terra::disagg(r, 15)
v <- world

us_fair_median_dt <- subset(fair_exps_isos_k90, experiment_iso == "USA" & year == 2020)

fair_exps_isos_k90_usa_2020 <- us_fair_median_dt %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(median_deltat = median(deltaT, ma.rm = T))

fair_exps_isos_k90_usa_2020 <- subset(fair_exps_isos_k90_usa_2020, year == 2020)

deltat_cgm <- r2 * fair_exps_isos_k90_usa_2020$median_deltat
#deltat_cgm <- r2
x <- crop(deltat_cgm, ext(world) + 0.1)
y <- terra::mask(x, world)
y <- mean(y)

# data is ready 
writeVector(v, paste0(fig_prepped_dta, run_date,"v.shp"), overwrite= TRUE)
writeRaster(y, paste0(fig_prepped_dta, run_date,"y.tiff"), overwrite= TRUE)


################################################################################
################################################################################
# 1d

# we need to aggregate the deltat to the country level by weighting by pop
# so let us read the pop raster and resample it to match coordinates and 
# convert to a dataframe and then join
pop <- raster(paste0(raw_path, "population/gpw_v4_population_count_rev11_2010_1_deg.tif"))
pop <- readAll(pop)
pop <- resample(pop, raster_cgm)
#popdf <- as.data.frame(as.matrix(rasterToPoints(pop)))

#write_rds(popdf, "~/BurkeLab Dropbox/Projects/loss_damage/sherlock_files/data/pre_processed/popdf.rds")
# the other thing we need is to get a country id for each lon-lat combo
world <- spData::world
world <- st_as_sf(world)
world <- subset(world, name_long != "Antarctica")
world$ISO3 <- countrycode::countrycode(sourcevar = world$iso_a2,origin = "iso2c",
                                       destination = "iso3c"
)

fair_exps_isos_k90 <- process_exp_data_hist("20230523", "hist_bi_v2022", 1990, aggregating = T)

gdp_temp_data_k90 <- readRDS(paste0(dropbox_path, "/data/processed/world_gdp_pop/gdp_temp_data_k90.rds"))
load(paste0(dropbox_path, "/data/processed/bhm/bhm_era_reg.RData"))
pop_wdi <- readRDS(paste0(dropbox_path, "/data/processed/world_gdp_pop/pop_wdi.rds"))
future_forecast_ssp370 <- readRDS(paste0(dropbox_path, "/data/processed/future_forecast/future_forecast_ssp370.rds"))


total_damages_k90_usa <- calculate_bidamages_bilateral(raster_cgm,
                                                       fair_exps_isos_k90,
                                                       "USA",
                                                       1990,
                                                       future_forecast = future_forecast_ssp370,
                                                       gdp_temp_dataset = gdp_temp_data_k90,
                                                       bhm_model = bhm_era_reg,
                                                       2020)

total_damages_k90_usa <- subset(total_damages_k90_usa, emitter == "USA" & ISO3 == "BRA")

annual_observed <- total_damages_k90_usa %>% dplyr::group_by(year) %>% 
  dplyr::summarise(observed = mean(era_mwtemp, na.rm = T),
                   corrected = mean(era_mwtemp - deltat, na.rm = T))

annual_observed <- subset(annual_observed, year <= 2020)

annual_observed$observed - annual_observed$corrected

# ready for plotting 
write_rds(annual_observed, paste0(fig_prepped_dta,run_date, "annual_observed.rds"))

################################################################################
################################################################################
# 1e
usa_bra <- subset(total_damages_k90_usa, ISO3 == "BRA")

usa_bra$observed_gdp <- usa_bra$NY.GDP.PCAP.KD * usa_bra$SP.POP.TOTL
usa_bra$counterfactual_gdp <- usa_bra$observed_gdp - usa_bra$weighted_damages2

usa_bra <- usa_bra %>% dplyr::select(c("year", "observed_gdp", "counterfactual_gdp",
                                       "weighted_damages2"))

#1e
usa_bra$gdp1 <- usa_bra$observed_gdp / 1000000000
usa_bra$adjusted_gdp1 <- usa_bra$counterfactual_gdp / 1000000000

# ready for plotting
write_rds(usa_bra, paste0(fig_prepped_dta,run_date, "usa_bra.rds"))

################################################################################
################################################################################
# 1F

usa_bra1 <- subset(usa_bra, year <= 2020)
sum_usa_bra <- usa_bra1 %>% ungroup() %>% 
  dplyr::mutate(cumsum2 = cumsum(weighted_damages2 * -1))

sum_usa_bra$cumsum2 <- (sum_usa_bra$cumsum2) / 1000000000 

# ready for plotting 
write_rds(sum_usa_bra, paste0(fig_prepped_dta,run_date, "sum_usa_bra.rds"))

# end of script 
