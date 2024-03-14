##############################################################################
# Mustafa Zahid, January 7th, 2023
# This R script reads the data and prepares the necessary data to plots figure
# 2. Figure 2 
#############################################################################
remove(list=ls())
gc()
sf::sf_use_s2(FALSE)
setwd("~/GitHub/loss_damage")

#replicate <- F# change T to F if you want to create your own data  
#if (replicate == T){
#  run_date <- "20230523"
#}
#if (replicate == F){
#  run_date <- gsub("-","",Sys.Date())
#}

run_date <- "20240314"

# read in the needed libraries 
source("scripts/working/analysis/0_read_libs.R")

setwd(dropbox_path)
#############################################################################
#############################################################################
# read data 
total_damages_1gtco2_cd <- readRDS(paste0(output_path,"/total_damages_1gtco2_1990_2020.rds"))
total_damages_1gtco2 <- readRDS(paste0(output_path, "/total_damages_1gtco2_1990_2020.rds"))

#############################################################################
#############################################################################
################################################################################ panels a, b
total_damages_1gtC1 <- subset(total_damages_1gtco2, emitter <=  2020 & emitter >1989)

total_damages_1gtC1 <- total_damages_1gtC1 %>% 
  dplyr::mutate(year_cat = case_when(year < 2020 ~ "1990-2020",
                                     year >2020 ~ "2021-2100"))

total_damages_by_pulse <- total_damages_1gtC1 %>% 
  dplyr::group_by(emitter, year_cat) %>% 
  dplyr::summarise(total_damages_dr1_5 = sum(weighted_damages1_5_scld, na.rm = T),
                   total_damages_dr2 = sum(weighted_damages2_scld, na.rm = T),
                   total_damages_dr3 = sum(weighted_damages3_scld, na.rm = T),
                   total_damages_dr5 = sum(weighted_damages5_scld, na.rm = T),
                   total_damages_dr7 = sum(weighted_damages7_scld, na.rm = T),
                   .groups= "keep")

total_damages_by_pulse <- total_damages_by_pulse %>% dplyr::select(c("year_cat", 
                                                                     "emitter",
                                                                     "total_damages_dr1_5",
                                                                     "total_damages_dr2",
                                                                     "total_damages_dr3",
                                                                     "total_damages_dr5",
                                                                     "total_damages_dr7"))

total_damages_by_pulse$total_damages_dr1_5 <- total_damages_by_pulse$total_damages_dr1_5 * (1 /1000000000)
total_damages_by_pulse$total_damages_dr2 <- total_damages_by_pulse$total_damages_dr2 * (1 /1000000000)
total_damages_by_pulse$total_damages_dr3 <- total_damages_by_pulse$total_damages_dr3 * (1 /1000000000)
total_damages_by_pulse$total_damages_dr5 <- total_damages_by_pulse$total_damages_dr5 * (1 /1000000000)
total_damages_by_pulse$total_damages_dr7 <- total_damages_by_pulse$total_damages_dr7 * (1 /1000000000)

total_damages_by_pulse_2020 <- subset(total_damages_by_pulse, year_cat == "1990-2020")

total_damages_by_pulse_2020_1_5 <- total_damages_by_pulse_2020 %>% 
  dplyr::select(c("emitter", "total_damages_dr1_5")) %>% 
  dplyr::mutate(discount_rate = "1.5%")

colnames(total_damages_by_pulse_2020_1_5)[3] <- "total_damages"

total_damages_by_pulse_2020_2 <- total_damages_by_pulse_2020 %>% 
  dplyr::select(c("emitter", "total_damages_dr2")) %>% 
  dplyr::mutate(discount_rate = "2%")

colnames(total_damages_by_pulse_2020_2)[3] <- "total_damages"

total_damages_by_pulse_2020_3 <- total_damages_by_pulse_2020 %>% 
  dplyr::select(c("emitter", "total_damages_dr3")) %>% 
  dplyr::mutate(discount_rate = "3%")

colnames(total_damages_by_pulse_2020_3)[3] <- "total_damages"

total_damages_by_pulse_2020_5 <- total_damages_by_pulse_2020 %>% 
  dplyr::select(c("emitter", "total_damages_dr5")) %>% 
  dplyr::mutate(discount_rate = "5%")

colnames(total_damages_by_pulse_2020_5)[3] <- "total_damages"

total_damages_by_pulse_2020_7 <- total_damages_by_pulse_2020 %>% 
  dplyr::select(c("emitter", "total_damages_dr7")) %>% 
  dplyr::mutate(discount_rate = "7%")

colnames(total_damages_by_pulse_2020_7)[3] <- "total_damages"

total_damages_by_pulse_2020_all <- rbind(total_damages_by_pulse_2020_1_5,
                                         total_damages_by_pulse_2020_2,
                                         total_damages_by_pulse_2020_3,
                                         total_damages_by_pulse_2020_5,
                                         total_damages_by_pulse_2020_7)

#####


total_damages_by_pulse_2100 <- subset(total_damages_by_pulse, year_cat == "2021-2100")

total_damages_by_pulse_2100_1_5 <- total_damages_by_pulse_2100 %>% 
  dplyr::select(c("emitter", "total_damages_dr1_5")) %>% 
  dplyr::mutate(discount_rate = "1.5%")

colnames(total_damages_by_pulse_2100_1_5)[3] <- "total_damages"


total_damages_by_pulse_2100_2 <- total_damages_by_pulse_2100 %>% 
  dplyr::select(c("emitter", "total_damages_dr2")) %>% 
  dplyr::mutate(discount_rate = "2%")

colnames(total_damages_by_pulse_2100_2)[3] <- "total_damages"

total_damages_by_pulse_2100_3 <- total_damages_by_pulse_2100 %>% 
  dplyr::select(c("emitter", "total_damages_dr3")) %>% 
  dplyr::mutate(discount_rate = "3%")

colnames(total_damages_by_pulse_2100_3)[3] <- "total_damages"

total_damages_by_pulse_2100_5 <- total_damages_by_pulse_2100 %>% 
  dplyr::select(c("emitter", "total_damages_dr5")) %>% 
  dplyr::mutate(discount_rate = "5%")

colnames(total_damages_by_pulse_2100_5)[3] <- "total_damages"

total_damages_by_pulse_2100_7 <- total_damages_by_pulse_2100 %>% 
  dplyr::select(c("emitter", "total_damages_dr7")) %>% 
  dplyr::mutate(discount_rate = "7%")

colnames(total_damages_by_pulse_2100_7)[3] <- "total_damages"

total_damages_by_pulse_2100_all <- rbind(total_damages_by_pulse_2100_1_5,
                                         total_damages_by_pulse_2100_2,
                                         total_damages_by_pulse_2100_3,
                                         total_damages_by_pulse_2100_5,
                                         total_damages_by_pulse_2100_7)


total_damages_by_pulse_2100 <- ungroup(total_damages_by_pulse_2100)

test_df_for_table_2100 <- total_damages_by_pulse_2100 %>% dplyr::select(c("emitter",
                                                                          "total_damages_dr1_5",
                                                                          "total_damages_dr2",
                                                                          "total_damages_dr3",
                                                                          "total_damages_dr5",
                                                                          "total_damages_dr7"))

colnames(test_df_for_table_2100)[2] <- "total_damages_2100_dr1_5"
colnames(test_df_for_table_2100)[3] <- "total_damages_2100_dr2"
colnames(test_df_for_table_2100)[4] <- "total_damages_2100_dr3"
colnames(test_df_for_table_2100)[5] <- "total_damages_2100_dr5"
colnames(test_df_for_table_2100)[6] <- "total_damages_2100_dr7"

test_df_for_table_2100$total_damages_2100_dr1_5 <- round(test_df_for_table_2100$total_damages_2100_dr1_5, 0)
test_df_for_table_2100$total_damages_2100_dr2 <- round(test_df_for_table_2100$total_damages_2100_dr2, 0)
test_df_for_table_2100$total_damages_2100_dr3 <- round(test_df_for_table_2100$total_damages_2100_dr3, 0)
test_df_for_table_2100$total_damages_2100_dr5 <- round(test_df_for_table_2100$total_damages_2100_dr5, 0)
test_df_for_table_2100$total_damages_2100_dr7 <- round(test_df_for_table_2100$total_damages_2100_dr7, 0)


total_damages_by_pulse_2020 <- ungroup(total_damages_by_pulse_2020)

test_df_for_table_2020 <- total_damages_by_pulse_2020 %>% dplyr::select(c("emitter",
                                                                          "total_damages_dr1_5",
                                                                          "total_damages_dr2",
                                                                          "total_damages_dr3",
                                                                          "total_damages_dr5",
                                                                          "total_damages_dr7"))

colnames(test_df_for_table_2020)[2] <- "total_damages_2020_dr1_5"
colnames(test_df_for_table_2020)[3] <- "total_damages_2020_dr2"
colnames(test_df_for_table_2020)[4] <- "total_damages_2020_dr3"
colnames(test_df_for_table_2020)[5] <- "total_damages_2020_dr5"
colnames(test_df_for_table_2020)[6] <- "total_damages_2020_dr7"

test_df_for_table_2020$total_damages_2020_dr1_5[test_df_for_table_2020$emitter <2012] <- round(test_df_for_table_2020$total_damages_2020_dr1_5[test_df_for_table_2020$emitter <2012], 0)
test_df_for_table_2020$total_damages_2020_dr2[test_df_for_table_2020$emitter <2012] <- round(test_df_for_table_2020$total_damages_2020_dr2[test_df_for_table_2020$emitter <2012], 0)
test_df_for_table_2020$total_damages_2020_dr3[test_df_for_table_2020$emitter <2012] <- round(test_df_for_table_2020$total_damages_2020_dr3[test_df_for_table_2020$emitter <2012], 0)
test_df_for_table_2020$total_damages_2020_dr5[test_df_for_table_2020$emitter <2012] <- round(test_df_for_table_2020$total_damages_2020_dr5[test_df_for_table_2020$emitter <2012], 0)
test_df_for_table_2020$total_damages_2020_dr7[test_df_for_table_2020$emitter <2012] <- round(test_df_for_table_2020$total_damages_2020_dr7[test_df_for_table_2020$emitter <2012], 0)

test_df_for_table_2020$total_damages_2020_dr1_5[test_df_for_table_2020$emitter >= 2012 & test_df_for_table_2020$emitter < 2018] <- round(test_df_for_table_2020$total_damages_2020_dr1_5[test_df_for_table_2020$emitter >= 2012 & test_df_for_table_2020$emitter < 2018], 1)
test_df_for_table_2020$total_damages_2020_dr2[test_df_for_table_2020$emitter >= 2012 & test_df_for_table_2020$emitter < 2018] <- round(test_df_for_table_2020$total_damages_2020_dr2[test_df_for_table_2020$emitter >= 2012 & test_df_for_table_2020$emitter < 2018], 1)
test_df_for_table_2020$total_damages_2020_dr3[test_df_for_table_2020$emitter >= 2012 & test_df_for_table_2020$emitter < 2018] <- round(test_df_for_table_2020$total_damages_2020_dr3[test_df_for_table_2020$emitter >= 2012 & test_df_for_table_2020$emitter < 2018], 1)
test_df_for_table_2020$total_damages_2020_dr5[test_df_for_table_2020$emitter >= 2012 & test_df_for_table_2020$emitter < 2018] <- round(test_df_for_table_2020$total_damages_2020_dr5[test_df_for_table_2020$emitter >= 2012 & test_df_for_table_2020$emitter < 2018], 1)
test_df_for_table_2020$total_damages_2020_dr7[test_df_for_table_2020$emitter >= 2012 & test_df_for_table_2020$emitter < 2018] <- round(test_df_for_table_2020$total_damages_2020_dr7[test_df_for_table_2020$emitter >= 2012 & test_df_for_table_2020$emitter < 2018], 1)

test_df_for_table_2020$total_damages_2020_dr1_5[test_df_for_table_2020$emitter >= 2018 & test_df_for_table_2020$emitter < 2020] <- round(test_df_for_table_2020$total_damages_2020_dr1_5[test_df_for_table_2020$emitter >= 2018 & test_df_for_table_2020$emitter < 2020], 2)
test_df_for_table_2020$total_damages_2020_dr2[test_df_for_table_2020$emitter >= 2018 & test_df_for_table_2020$emitter < 2020] <- round(test_df_for_table_2020$total_damages_2020_dr2[test_df_for_table_2020$emitter >= 2018 & test_df_for_table_2020$emitter < 2020], 2)
test_df_for_table_2020$total_damages_2020_dr3[test_df_for_table_2020$emitter >= 2018 & test_df_for_table_2020$emitter < 2020] <- round(test_df_for_table_2020$total_damages_2020_dr3[test_df_for_table_2020$emitter >= 2018 & test_df_for_table_2020$emitter < 2020], 2)
test_df_for_table_2020$total_damages_2020_dr5[test_df_for_table_2020$emitter >= 2018 & test_df_for_table_2020$emitter < 2020] <- round(test_df_for_table_2020$total_damages_2020_dr5[test_df_for_table_2020$emitter >= 2018 & test_df_for_table_2020$emitter < 2020], 2)
test_df_for_table_2020$total_damages_2020_dr7[test_df_for_table_2020$emitter >= 2018 & test_df_for_table_2020$emitter < 2020] <- round(test_df_for_table_2020$total_damages_2020_dr7[test_df_for_table_2020$emitter >= 2018 & test_df_for_table_2020$emitter < 2020], 2)


test_df_for_table_2020$total_damages_2020_dr1_5[test_df_for_table_2020$emitter == 2020] <- round(test_df_for_table_2020$total_damages_2020_dr1_5[test_df_for_table_2020$emitter == 2020], 3)
test_df_for_table_2020$total_damages_2020_dr2[test_df_for_table_2020$emitter == 2020] <- round(test_df_for_table_2020$total_damages_2020_dr2[test_df_for_table_2020$emitter == 2020], 3)
test_df_for_table_2020$total_damages_2020_dr3[test_df_for_table_2020$emitter == 2020] <- round(test_df_for_table_2020$total_damages_2020_dr3[test_df_for_table_2020$emitter == 2020], 3)
test_df_for_table_2020$total_damages_2020_dr5[test_df_for_table_2020$emitter == 2020] <- round(test_df_for_table_2020$total_damages_2020_dr5[test_df_for_table_2020$emitter == 2020], 3)
test_df_for_table_2020$total_damages_2020_dr7[test_df_for_table_2020$emitter == 2020] <- round(test_df_for_table_2020$total_damages_2020_dr7[test_df_for_table_2020$emitter == 2020], 3)

test_df_for_table_2020$total_damages_2020_dr1_5 <- as.character(test_df_for_table_2020$total_damages_2020_dr1_5)
test_df_for_table_2020$total_damages_2020_dr2 <- as.character(test_df_for_table_2020$total_damages_2020_dr2)
test_df_for_table_2020$total_damages_2020_dr3 <- as.character(test_df_for_table_2020$total_damages_2020_dr3)
test_df_for_table_2020$total_damages_2020_dr5 <- as.character(test_df_for_table_2020$total_damages_2020_dr5)
test_df_for_table_2020$total_damages_2020_dr7 <- as.character(test_df_for_table_2020$total_damages_2020_dr7)

#### now bring them together 
test_df_for_table <- left_join(test_df_for_table_2020,
                               test_df_for_table_2100,
                               by = c("emitter"))

################################################################################ panels c, d
# prep data 
# then we need to subset the 1990 damages 
total_damages_1gtco2_1990 <- subset(total_damages_1gtco2_cd, emitter == 1990)

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
summary(total_damages_1gtco2_1990_iso$damages)
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
setwd("~/GitHub/loss_damage")
write_rds(test_df_for_table, paste0(fig_prepped_dta, run_date, "/test_df_for_table.rds"))
write_rds(total_damages_by_pulse_2020_all, paste0(fig_prepped_dta, run_date, "/total_damages_by_pulse_2020.rds"))
write_rds(total_damages_by_pulse_2100_all, paste0(fig_prepped_dta, run_date, "/total_damages_by_pulse_2100.rds"))
write_sf(world, paste0(fig_prepped_dta, run_date, "/world.shp"))
write_rds(damages_1990_2020, paste0(fig_prepped_dta, run_date, "/1gtco2_damages_1990_2020.rds"))
write_rds(damages_2021_2100, paste0(fig_prepped_dta, run_date, "/1gtco2_damages_2020_2100.rds"))


# end of script 