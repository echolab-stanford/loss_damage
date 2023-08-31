##############################################################################
# Mustafa Zahid, January 7th, 2023
# This R script reads the data and prepares the necessary data to plots figure
# 1. Figure 1 demonstrates the different steps taken to calculate teh damages
#############################################################################
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

us_emissions$Total[us_emissions$Year < 1980] <- 0
# now we want to merge US 1980 numbers to total world numbers 
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

################################################################################
################################################################################
# 1b

# now let us process the FaIR temperature response to emissions by running the 
# created function 
#gtco2_effect <- process_disagg_exp_data("1GtCO2_hist_2300", 1980)

fair_exps_isos_k80 <- process_exp_data_hist_fut("20230411", "hist_biusa_v2022", 1980, aggregating = F)

# alt: have 1b as the deltaT coming from the emissions of us 
fair_exps_isos_k80_usa <- subset(fair_exps_isos_k80, experiment_iso == "USA")

# OK NOW LET US GET max and min
# now caculate bounds 
gtc1_fair_exp <- fair_exps_isos_k80_usa %>% dplyr::group_by(year) %>% 
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


################################################################################
################################################################################
# 1c
raster_cgm <- raster("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/r_cgm/ratio_raster_avgs.tif")


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

us_fair_median_dt <- subset(fair_exps_isos_k80, experiment_iso == "USA" & year == 2020)

fair_exps_isos_k80_usa_2020 <- us_fair_median_dt %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(median_deltat = median(deltaT, ma.rm = T))

fair_exps_isos_k80_usa_2020 <- subset(fair_exps_isos_k80_usa_2020, year == 2020)

deltat_cgm <- r2 * fair_exps_isos_k80_usa_2020$median_deltat
#deltat_cgm <- r2
x <- crop(deltat_cgm, ext(world) + 0.1)
y <- terra::mask(x, world)
y <- mean(y)

#plot(asinh(y), col= rev(heat.colors(n = 20)), legend=F, axes = FALSE, boxes = T, mar=c(1, 4, 2, 1))
#lines(v, lwd =0.8)
#


################################################################################
################################################################################
# 1d

# we need to aggregate the deltat to the country level by weighting by pop
# so let us read the pop raster and resample it to match coordinates and 
# convert to a dataframe and then join
pop <- raster(paste0(dropbox_path, "data/raw/population/gpw_v4_population_count_rev11_2010_1_deg.tif"))
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

fair_exps_isos_k80 <- process_exp_data_hist("20230411", "hist_bi_v2022", 1980, aggregating = T)

gdp_temp_data_k80 <- readRDS(paste0(dropbox_path, "/data/processed/world_gdp_pop/gdp_temp_data_k80.rds"))
load(paste0(dropbox_path, "/data/processed/bhm/bhm_era_reg.RData"))
pop_wdi <- readRDS(paste0(dropbox_path, "/data/processed/world_gdp_pop/pop_wdi.rds"))
future_forecast_ssp370 <- readRDS(paste0(dropbox_path, "/data/processed/future_forecast/future_forecast_ssp370.rds"))


total_damages_k80_usa <- calculate_bidamages_bilateral(raster_cgm,
                                                       fair_exps_isos_k80,
                                                       "USA",
                                                       1980,
                                                       future_forecast = future_forecast_ssp370,
                                                       gdp_temp_dataset = gdp_temp_data_k80,
                                                       bhm_model = bhm_era_reg,
                                                       2020)

total_damages_k80_usa <- subset(total_damages_k80_usa, emitter == "USA" & ISO3 == "BRA")

annual_observed <- total_damages_k80_usa %>% dplyr::group_by(year) %>% 
  dplyr::summarise(observed = mean(era_mwtemp, na.rm = T),
                   corrected = mean(era_mwtemp - deltat, na.rm = T))

annual_observed <- subset(annual_observed, year <= 2020)

annual_observed$observed - annual_observed$corrected


# ready for plotting 
################################################################################
################################################################################
# 1e
usa_bra <- subset(total_damages_k80_usa, ISO3 == "BRA")

usa_bra$observed_gdp <- usa_bra$NY.GDP.PCAP.KD * usa_bra$SP.POP.TOTL
usa_bra$counterfactual_gdp <- usa_bra$observed_gdp - usa_bra$weighted_damages2

usa_bra <- usa_bra %>% dplyr::select(c("year", "observed_gdp", "counterfactual_gdp",
                                       "weighted_damages2"))

#1e
usa_bra$gdp1 <- usa_bra$observed_gdp / 1000000000
usa_bra$adjusted_gdp1 <- usa_bra$counterfactual_gdp / 1000000000
par(mar = c(6,8,4,2))  
plot(x = usa_bra$year, y = usa_bra$gdp1,
     type = "l", lty = 1, xlim = range(c(1980, 2020)),
     xlab = "Year", ylab = "Real GDP (in Bil of $USD) \n",
     las = 1, lwd = 2, family = "Arial", cex.lab = 2.5, cex.axis = 1.85, 
     frame.plot = F, col = "red") 

# 3) add info for the other fishery
lines(x = usa_bra$year, y = usa_bra$adjusted_gdp1, 
      type = "l", lty = 1, lwd = 2, pch = 16, col = "black")


segments(2000, 1000, 2002, 1000, lwd = 2, col = "black")
segments(2000, 900, 2002, 900, col = "red", lty = 1, lwd = 2)

text(2003,1000, " Oberved GDP", cex = 0.85, adj = 0)
text(2003,900, " GDP abscent US Emissions \n1980 onward", cex = 0.85, adj = 0)

title("e", adj = 0, line =1, cex.main = 2.5)


# ready for plotting


################################################################################
################################################################################
# 1F

usa_bra1 <- subset(usa_bra, year <= 2020)
sum_usa_bra <- usa_bra1 %>% ungroup() %>% 
  dplyr::mutate(cumsum2 = cumsum(weighted_damages2 * -1))

#sum_usa_bra_only$cumsum2 <- tm.cumsum

#par(mar = c(4,4,2,2)) 

dev.off()

# visualize all together  
################################################################################
################################################################################


mat <- matrix(c(2,1,2,1),nrow=2, ncol = 3, byrow=T)
layout(mat)
ax = 1.5  #scaling for axes
par(mar=c(4,4,2,1))

dev.off()
par(mfrow=c(2,3))


#1a
par(mar = c(4,10,4,2))  
plot(x = world_emissions$year, y = world_emissions$total,
     type = "l", lty = 1, xlim = range(c(1900, 2020)),
     xlab = "Year", ylab = "CO2 Emissions (GtCO2)\n",
     las = 1, lwd = 2, cex.axis = 1.85, cex.lab = 2, 
     frame.plot = F) 

title("a", adj = 0, line =1, cex.main = 2.5)
# las = 1 says "turn the y-axis tick labels to be horizontal"
# 3) add info for the other fishery
lines(x = world_minus_us_emms$year, y = world_minus_us_emms$total, 
      type = "l", lty = 2, lwd = 2, pch = 16, col = "red")
# 4) add the legend
segments(1975, 2.2, 1980, 2.2, lwd = 2)
segments(1975, 1, 1980, 1, col = "red", lty = 2, lwd = 2)

text(1981,2.2, " Full Emissions", cex = 1.25, adj = 0)
text(1981,1, " Full Emissions minus \n US Emissions 1980 Onward", cex = 1.25, 
     adj = 0)

# legend("bottomleft", legend = c("Full Emissions",
#                  "Full Emissions w/out USA \nemissions 1980 onward"), 
#      lty = c(1,2), lwd = c(2,2), bty = "n", 
#     col = c("black", "red"), cex = 0.85)

#write_rds(world_emissions, "~/Desktop/world_emissions.rds")
#world_emissions <- read_rds("~/Desktop/world_emissions.rds")
#write_rds(world_minus_us_emms, "~/Desktop/world_minus_us_emms.rds")
#world_minus_us_emms <- read_rds("~/Desktop/world_minus_us_emms.rds")



################################################################################
################################################################################
# 1b
tail(gtc1_fair_exp)
#gtc1_fair_exp <- subset(gtc1_fair_exp, year <= 2100)
par(mar = c(4,10,4,2))  
plot(x = gtc1_fair_exp[,2], y = gtc1_fair_exp[,4],
     type = "l", lty = 1, xlim = range(c(1980,2100)),
     ylim = range(0, 0.3),
     xlab = "Year", ylab = "ΔGMST (C°) \n",
     las = 1, lwd = 2, cex.lab= 2, cex.axis = 1.85, 
     frame.plot = F)  
# las = 1 says "turn the y-axis tick labels to be horizontal"

#newdata <- unlist(newdata)

polygon(c(gtc1_fair_exp[,2],rev(gtc1_fair_exp[,2])),c(gtc1_fair_exp[,3],rev(gtc1_fair_exp[,5])),
        col="grey", border = NA)

lines(x = gtc1_fair_exp[,2], y = gtc1_fair_exp[,4],
      type = "l", lty = 1, xlim = range(c(1980,2300)),
      ylim = range(0, 0.004),
      xlab = "Year", ylab = "Temperature (c°)",
      las = 1, lwd = 2, cex.lab = 2.5, cex.axis = 1.85)  # las = 1 says "turn the y-axis tick labels to be horizontal"

title("b", adj = 0, line =1, cex.main = 2.5)


################################################################################
################################################################################
# 1c

par(mar = c(4,4,10,2))  
#dev.off()
plot((y), col= rev(heat.colors(n = 20)), legend = T, 
     plg=list(shrink=0.85, cex=.8, x = "bottomleft", title = "\nttielk"), 
     axes = FALSE, boxes = T, mar=c(5, 2, 2, 4))

lines(v, lwd =0.8)

title("\n c", adj = 0, line =1, cex.main = 2.5)




################################################################################
################################################################################
# 1d



par(mar = c(4,10,4,2))  
plot(x = annual_observed$year, y = annual_observed$observed,
     type = "l", lty = 1, xlim = range(c(1980, 2020)),
     ylim = range(c(22,23.5)),
     xlab = "Year", ylab = "Temperature (C°) \n",
     las = 1, lwd = 2, cex.lab = 2, cex.axis = 1.85, 
     frame.plot = F)  
# las = 1 says "turn the y-axis tick labels to be horizontal"
# 3) add info for the other fishery
lines(x = annual_observed$year, y = annual_observed$corrected, 
      type = "l", lty = 1, lwd = 2, cex.axis = 1.85, cex.lab = 2, 
      pch = 16, col = "red")

segments(2000, 22.15, 2002, 22.15, lwd = 2)
segments(2000, 22.05, 2002, 22.05, col = "red", lty = 1, lwd = 2)

text(2003,22.15, "Observed temperature (C°)", cex = 1.25, adj = 0)
text(2003,22.05, "Observed temperature \nminus ΔT", cex = 1.25, adj = 0)

title("d", adj = 0, line =1, cex.main = 2.5)

################################################################################
################################################################################
# 1e

par(mar = c(4,10,4,2))  
plot(x = usa_bra$year, y = usa_bra$adjusted_gdp1,
     type = "l", lty = 1, xlim = range(c(1980, 2020)),
     xlab = "Year", ylab = "Real GDP (in Bil of $USD) \n",
     las = 1, lwd = 2, #family = "Arial", 
     cex.lab = 1.85, cex.axis = 1.85, 
     frame.plot = F, col = "red") 

# 3) add info for the other fishery
lines(x = usa_bra$year, y = usa_bra$gdp1, 
      type = "l", lty = 1, lwd = 2, pch = 16, col = "black")


segments(2000, 900, 2002, 900, lwd = 2, col = "black")
segments(2000, 800, 2002, 800, col = "red", lty = 1, lwd = 2)

text(2003,900, " Oberved GDP", cex = 1.25, adj = 0)
text(2003,800, " GDP abscent US Emissions \n1980 onward", cex = 1.25, adj = 0)

title("e", adj = 0, line =1, cex.main = 2.5)

################################################################################
################################################################################
#1f

par(mar = c(4,10,4,2))  
#sum_usa_bra$cumsum2 <- (sum_usa_bra$cumsum2) / 1000000000 
#sum_usa_bra$cumsum2 <- (sum_usa_bra$cumsum2) * 1000000000 
plot(x = sum_usa_bra$year, y = sum_usa_bra$cumsum2,
     type = "l", lty = 1, xlim = range(c(1980, 2020)),
     ylim = range(0,500),
     xlab = "Year", ylab =  "Cumulative Damages \n(in Billions of $USD)\n",
     las = 1, lwd = 2, cex.lab = 2, cex.axis = 1.85, 
     frame.plot = F) 
title("f", adj = 0, line = 1, cex.main = 2.5)




# end of script 
