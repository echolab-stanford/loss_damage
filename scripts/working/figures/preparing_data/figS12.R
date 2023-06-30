##############################################################################
# Mustafa Zahid, June 30th, 2023
# This script is used to prepare the data needed to plot figure S12. 
##############################################################################
remove(list=ls())
gc()
sf::sf_use_s2(FALSE)
setwd("~/GitHub/loss_damage")

# read in the needed libraries 
source("scripts/working/analysis/0_read_libs.R")

setwd(dropbox_path)
#############################################################################
#############################################################################
# read data 
total_damages_1gtco2 <- readRDS(paste0(output_path, 
                                       "20230628/total_damages_1gtco2_1990_2020.rds"))

#############################################################################
#############################################################################
# prep data 
# then we need to subset the 1990 damages 
total_damages_1gtco2_1990 <- subset(total_damages_1gtco2, emitter == 1990)

# now let us calculate the total net impacts for each country 
total_damages_1gtco2_1990_iso <- total_damages_1gtco2_1990 %>% 
  dplyr::group_by(ISO3, year) %>% 
  dplyr::summarise(damages = sum(weighted_damages2_scld, na.rm = T))

# ok now let us divide damages into through 2020 and 2021-2100 
total_damages_1gtco2_1990_iso <- total_damages_1gtco2_1990_iso %>% 
  dplyr::mutate(year_cat = case_when(year <= 2020 ~ "1990-2020",
                                     year >2020 ~ "2021-2100"))

# ok now let us sum up by the teime frame of damages and country 
total_damages_1gtco2_1990_iso <- total_damages_1gtco2_1990_iso %>% 
  dplyr::group_by(ISO3, year_cat) %>% 
  dplyr::summarise(damages = sum(damages, na.rm = T))

# ok now let us cast missings 
total_damages_1gtco2_1990_iso$damages[total_damages_1gtco2_1990_iso$damages == 0] <- NA

damages_1990_2020 <- subset(total_damages_1gtco2_1990_iso, year_cat == "1990-2020")
damages_2021_2100 <- subset(total_damages_1gtco2_1990_iso, year_cat == "2021-2100")

# ok now let us read in the word shapefile 
# world shapefile 
world <- spData::world
world <- st_as_sf(world)
world <- subset(world, name_long != "Antarctica")
world$ISO3 <- countrycode::countrycode(sourcevar = world$iso_a2,origin = "iso2c",
                                       destination = "iso3c"
)
world <- subset(world, !is.na(ISO3))
  

### data is ready to plot. We can write them into directory
write_sf(world, paste0(fig_prepped_dta, "20230629/world.shp"))
write_rds(damages_1990_2020, paste0(fig_prepped_dta, "20230629/1gtco2_damages_1990_2020.rds"))
write_rds(damages_2021_2100, paste0(fig_prepped_dta, "20230629/1gtco2_damages_2020_2100.rds"))

# end of script 
