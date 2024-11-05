##############################################################################
# Mustafa Zahid, March 14, 2024
# This R script reads the data and prepares the necessary data to plots figure
# ED6. 
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
# read in the needed libraries 
source("scripts/working/analysis/0_read_libs.R")

run_date <- "20240311_6"

setwd("~/BurkeLab Dropbox/projects/loss_damage")

getwd()
#############################################################################
#############################################################################
# read data in chuncks to not overwhelm R
list_files <- list.files(path = paste0(getwd(),"/data/output/20240311_6"),
                         pattern = "scc_", 
                         full.names = T)
# 1st chunk
listofdfs <- mclapply(list_files[1:200], readRDS, mc.cores = 5)
data <- do.call(rbind, listofdfs)
data1 <- data %>%
  dplyr::group_by(sim_id, emitter, year) %>%
  dplyr::summarise(damages = sum(weighted_damages2_scld, na.rm = TRUE))

# 2nd chunk
listofdfs <- mclapply(list_files[201:400], readRDS, mc.cores = 5)
data <- do.call(rbind, listofdfs)
data2 <- data %>%
  dplyr::group_by(sim_id, emitter, year) %>%
  dplyr::summarise(damages = sum(weighted_damages2_scld, na.rm = TRUE))

# 3rd chunk
listofdfs <- mclapply(list_files[401:600], readRDS, mc.cores = 5)
data <- do.call(rbind, listofdfs)
data3 <- data %>%
  dplyr::group_by(sim_id, emitter, year) %>%
  dplyr::summarise(damages = sum(weighted_damages2_scld, na.rm = TRUE))

# 4th chunk
listofdfs <- mclapply(list_files[601:800], readRDS, mc.cores = 5)
data <- do.call(rbind, listofdfs)
data4 <- data %>%
  dplyr::group_by(sim_id, emitter, year) %>%
  dplyr::summarise(damages = sum(weighted_damages2_scld, na.rm = TRUE))

# 5th chunk
listofdfs <- mclapply(list_files[801:1000], readRDS, mc.cores = 5)
data <- do.call(rbind, listofdfs)
data5 <- data %>%
  dplyr::group_by(sim_id, emitter, year) %>%
  dplyr::summarise(damages = sum(weighted_damages2_scld, na.rm = TRUE))

# now let us bring it all together
data <- rbind(data1,
              data2,
              data3, 
              data4,
              data5)

#############################################################################
#############################################################################
# prep data for plotting
# divide data into hdco and fdco
data_1990_2020 <- subset(data, year > 1989 & year <2021)
data_2021_2100 <- subset(data, year > 2020)

# ok now let us get the median and range by year of emmission
data_1990_2020 <- data_1990_2020 %>% 
  dplyr::group_by(emitter, sim_id) %>% 
  dplyr::summarise(damages = sum(damages,na.rm = T))

data_2021_2100 <- data_2021_2100 %>% 
  dplyr::group_by(emitter, sim_id) %>% 
  dplyr::summarise(damages = sum(damages,na.rm = T))

# assign the labels
data_1990_2020$scenario <- "1990-2020"
data_2021_2100$scenario <- "2021-2100"

# finally calculate the moments 
data_1990_2020_moments <- data_1990_2020 %>% 
  dplyr::group_by(emitter) %>% 
  dplyr::summarise(median = median(damages),
                   p_25 =  quantile(damages, c(.25, .75))[1],
                   p_75 =  quantile(damages, c(.25, .75))[2],
                   p_10 = quantile(damages, c(.10, .90))[1],
                   p_90 = quantile(damages, c(.10, .90))[2],
                   p_05 = quantile(damages, c(.05, .95))[1],
                   p_95 = quantile(damages, c(.05, .95))[2])

data_2021_2100_moments <- data_2021_2100 %>% 
  dplyr::group_by(emitter) %>% 
  dplyr::summarise(median = median(damages),
                   p_25 =  quantile(damages, c(.25, .75))[1],
                   p_75 =  quantile(damages, c(.25, .75))[2],
                   p_10 = quantile(damages, c(.10, .90))[1],
                   p_90 = quantile(damages, c(.10, .90))[2],
                   p_05 = quantile(damages, c(.05, .95))[1],
                   p_95 = quantile(damages, c(.05, .95))[2])

#############################################################################
#############################################################################
# save out the datasets
setwd("~/GitHub/loss_damage/")
write_rds(data_1990_2020_moments, "data/figures/20241104/data_1990_2020_moments.rds")
write_rds(data_2021_2100_moments, "data/figures/20241104/data_2021_2100_moments.rds")

# end of script