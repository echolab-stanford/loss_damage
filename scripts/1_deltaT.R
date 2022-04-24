##############################################################################
# Mustafa Zahid, January 10th, 2022
# This R script reads the CMIP historical and natural models. The natural 
# CMIP models represent temperature in teh counterfactural world with no human 
# forcings. On the other hand, the historical data represent change in temper-
# -ature in teh presence of all forcings
# Input(s): CGM data .nc files from various experiments (1850-2020)
# Output(s): A country-year dataset that has the change in temperature due to 
# all forcings.
##############################################################################

##############################################################################
##################### PART pre-I: Read needed datasets #######################
##############################################################################
# read the pop raster 
pop <- raster("~/BurkeLab Dropbox/Projects/loss_damage/data/raw/population/gpw_v4_population_count_rev11_2010_1_deg.tif")
pop <- readAll(pop)

# read in the world polygon 
data("wrld_simpl")
world_poly <- st_as_sf(wrld_simpl)
                            
##############################################################################
############## PART I: Extract country-level weighted averages ###############
##############################################################################
#read the function that will process the data and calculate the weighted average
# temp
list_of_dfs <- list()
extract_avg_weight <- function(forcing, experiment_id, num_years, k) {
 # establish empty list to store individual rasters in
 list_of_rasters <- list()
 for (i in 1:num_years) {
    tic()
    rasters <- readAll(raster::brick(paste0("~/BurkeLab Dropbox/Projects/loss_damage/data/raw/cmip6/tas_Amon_", 
                                            forcing,"_", experiment_id,
                                            "_ncecat_ann_mean.nc"),
                                     varname = "tas",
                                     level = i))
    rasters <- rotate(rasters)
    rasters <- raster::resample(rasters, pop)
    #rasters <- raster::calc(rasters, fun = mean)
    year <- i + 1849
    #names(rasters) <- paste0(year, "_average")
    # process historical runs
    rasters_weighted_avgs <- exactextractr::exact_extract(rasters,
                                                          world_poly,
                                                          "weighted_mean",
                                                          weights = pop,
                                                          default_weight = 0,
                                                          append_cols = c("ISO3",
                                                                          "POP2005")) 
    
    
    #reshape data from wide to long
    rasters_weighted_avgs <- reshape2::melt(rasters_weighted_avgs,
                                            id = c("ISO3", "POP2005"))
    
    rasters_weighted_avgs$variable <- paste0(rasters_weighted_avgs$variable, 
                                             ".",
                                             year)
    
    rasters_weighted_avgs$realization <- substr(rasters_weighted_avgs$variable,15,17)
    rasters_weighted_avgs$year <- str_sub(rasters_weighted_avgs$variable,-4,-1)
    rasters_weighted_avgs$temp_c <- rasters_weighted_avgs$value - 273.15
    
    across_all <- k
    
    if (across_all == "across_all") {
      rasters_weighted_avgs <- rasters_weighted_avgs %>% dplyr::group_by(year, ISO3) %>% 
        dplyr::summarise(temp_c = mean(temp_c, na.rm = T),
                         .groups = "keep")
    }
    
    
    list_of_dfs[[i]] <- rasters_weighted_avgs
    toc()
 }

  return(list_of_dfs)
}

##############################################################################
######################### PART II-a: Calculate deltas ########################
##############################################################################

calc_deltas <- function(ensembles, forcings, num_years, across_all) {
  # Now operate the function with the passed parameters
  tic()
  for (i in ensembles) {
    for (j in forcings) {
      weighted_avgs <- extract_avg_weight(j, i, num_years, across_all)
      weighted_avgs <- do.call(rbind,weighted_avgs)
      weighted_avgs$year <- as.numeric(weighted_avgs$year)
      #weighted_avgs$realization <- substr(weighted_avgs$variable,15,17)
      #calculate 10 year average for all choices of k 
      if (j == "historical") {
        colnames(weighted_avgs)[3] <- "temp_c_hist" 
        avg_55_64_hist <- weighted_avgs %>% 
          subset(year > 1954 & year < 1965) %>% 
          dplyr::group_by(ISO3) %>% 
          dplyr::summarize(avg_10_year_K60_hist = mean(temp_c_hist, na.rm = T),
                           .groups= "keep")
        avg_85_94_hist <- weighted_avgs %>% 
          subset(year > 1984 & year < 1995) %>% 
          dplyr::group_by(ISO3) %>% 
          dplyr::summarize(avg_10_year_K90_hist = mean(temp_c_hist, na.rm = T),
                           .groups= "keep") 
        avg_75_84_hist <- weighted_avgs %>% 
          subset(year > 1974 & year < 1985) %>% 
          dplyr::group_by(ISO3) %>% 
          dplyr::summarize(avg_10_year_K80_hist = mean(temp_c_hist, na.rm = T),
                           .groups= "keep") 
        avg_05_14_hist <- weighted_avgs %>% 
          subset(year > 2004 & year < 2015) %>% 
          dplyr::group_by(ISO3) %>% 
          dplyr::summarize(avg_lst10_year_hist = mean(temp_c_hist, na.rm = T),
                           .groups= "keep") 
        
        weighted_avgs <- left_join(weighted_avgs,
                                   avg_55_64_hist)
        weighted_avgs <- left_join(weighted_avgs,
                                   avg_75_84_hist)
        weighted_avgs <- left_join(weighted_avgs,
                                   avg_85_94_hist)
        weighted_avgs <- left_join(weighted_avgs,
                                   avg_05_14_hist)
        
        weighted_avgs <- weighted_avgs %>% 
          dplyr::mutate(delta_k60_hist = avg_lst10_year_hist - avg_10_year_K60_hist,
                        delta_k80_hist = avg_lst10_year_hist - avg_10_year_K80_hist,
                        delta_k90_hist = avg_lst10_year_hist - avg_10_year_K90_hist) 
        
        
      } else if (j == "hist-nat") {
        colnames(weighted_avgs)[3] <- "temp_c_nat" 
        avg_55_64_nat <- weighted_avgs %>% 
          subset(year > 1954 & year < 1965) %>% 
          dplyr::group_by(ISO3) %>% 
          dplyr::summarize(avg_10_year_K60_nat = mean(temp_c_nat, na.rm = T),
                           .groups= "keep")
        avg_75_84_nat <- weighted_avgs %>% 
          subset(year > 1974 & year < 1985) %>% 
          dplyr::group_by(ISO3) %>% 
          dplyr::summarize(avg_10_year_K80_nat = mean(temp_c_nat, na.rm = T),
                           .groups= "keep")
        avg_85_94_nat <- weighted_avgs %>% 
          subset(year > 1984 & year < 1995) %>% 
          dplyr::group_by(ISO3) %>% 
          dplyr::summarize(avg_10_year_K90_nat = mean(temp_c_nat, na.rm = T),
                           .groups= "keep")
        avg_05_14_nat <- weighted_avgs %>% 
          subset(year > 2004 & year < 2015) %>% 
          dplyr::group_by(ISO3) %>% 
          dplyr::summarize(avg_lst10_year_nat = mean(temp_c_nat, na.rm = T),
                           .groups= "keep") 
        
        weighted_avgs <- left_join(weighted_avgs,
                                   avg_55_64_nat)
        weighted_avgs <- left_join(weighted_avgs,
                                   avg_75_84_nat)
        weighted_avgs <- left_join(weighted_avgs,
                                   avg_85_94_nat)
        weighted_avgs <- left_join(weighted_avgs,
                                   avg_05_14_nat)
        
        weighted_avgs <- weighted_avgs %>% 
          dplyr::mutate(delta_k60_nat = avg_lst10_year_nat - avg_10_year_K60_nat,
                        delta_k80_nat = avg_lst10_year_nat - avg_10_year_K80_nat,
                        delta_k90_nat = avg_lst10_year_nat - avg_10_year_K90_nat)
      }
      write_rds(weighted_avgs, paste0("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/",
                                      j, "_", i, "_", across_all ,"_" , "wavgs.rds"))
    }
  }
  toc()
}

##############################################################################
################### PART II-b: Calculate deltas /realization #################
##############################################################################
# calculating deltaTs by realization 
calc_deltas_realization <- function(ensembles, forcings, num_years, across_all) {
  # Now operate the function with the passed parameters
  tic()
  for (i in ensembles) {
    for (j in forcings) {
      weighted_avgs <- extract_avg_weight(j, i, num_years, across_all)
      weighted_avgs <- do.call(rbind,weighted_avgs)
      weighted_avgs$year <- as.numeric(weighted_avgs$year)
      weighted_avgs$realization <- substr(weighted_avgs$variable,15,17)
      #calculate 10 year average for all choices of k 
      if (j == "historical") {
        colnames(weighted_avgs)[7] <- "temp_c_hist" 
        avg_55_64_hist <- weighted_avgs %>% 
          subset(year > 1954 & year < 1965) %>% 
          dplyr::group_by(realization, ISO3) %>% 
          dplyr::summarize(avg_10_year_K60_hist = mean(temp_c_hist, na.rm = T),
                           .groups= "keep")
        avg_85_94_hist <- weighted_avgs %>% 
          subset(year > 1984 & year < 1995) %>% 
          dplyr::group_by(realization, ISO3) %>% 
          dplyr::summarize(avg_10_year_K90_hist = mean(temp_c_hist, na.rm = T),
                           .groups= "keep") 
        avg_75_84_hist <- weighted_avgs %>% 
          subset(year > 1974 & year < 1985) %>% 
          dplyr::group_by(realization, ISO3) %>% 
          dplyr::summarize(avg_10_year_K80_hist = mean(temp_c_hist, na.rm = T),
                           .groups= "keep") 
        avg_05_14_hist <- weighted_avgs %>% 
          subset(year > 2004 & year < 2015) %>% 
          dplyr::group_by(realization, ISO3) %>% 
          dplyr::summarize(avg_lst10_year_hist = mean(temp_c_hist, na.rm = T),
                           .groups= "keep") 
        
        weighted_avgs <- left_join(weighted_avgs,
                                   avg_55_64_hist)
        weighted_avgs <- left_join(weighted_avgs,
                                   avg_75_84_hist)
        weighted_avgs <- left_join(weighted_avgs,
                                   avg_85_94_hist)
        weighted_avgs <- left_join(weighted_avgs,
                                   avg_05_14_hist)
        
        weighted_avgs <- weighted_avgs %>% 
          dplyr::mutate(delta_k60_hist = avg_lst10_year_hist - avg_10_year_K60_hist,
                        delta_k80_hist = avg_lst10_year_hist - avg_10_year_K80_hist,
                        delta_k90_hist = avg_lst10_year_hist - avg_10_year_K90_hist) 
        
        
      } else if (j == "hist-nat") {
        colnames(weighted_avgs)[7] <- "temp_c_nat" 
        avg_55_64_nat <- weighted_avgs %>% 
          subset(year > 1954 & year < 1965) %>% 
          dplyr::group_by(realization, ISO3) %>% 
          dplyr::summarize(avg_10_year_K60_nat = mean(temp_c_nat, na.rm = T),
                           .groups= "keep")
        avg_75_84_nat <- weighted_avgs %>% 
          subset(year > 1974 & year < 1985) %>% 
          dplyr::group_by(realization, ISO3) %>% 
          dplyr::summarize(avg_10_year_K80_nat = mean(temp_c_nat, na.rm = T),
                           .groups= "keep")
        avg_85_94_nat <- weighted_avgs %>% 
          subset(year > 1984 & year < 1995) %>% 
          dplyr::group_by(realization, ISO3) %>% 
          dplyr::summarize(avg_10_year_K90_nat = mean(temp_c_nat, na.rm = T),
                           .groups= "keep")
        avg_05_14_nat <- weighted_avgs %>% 
          subset(year > 2004 & year < 2015) %>% 
          dplyr::group_by(realization, ISO3) %>% 
          dplyr::summarize(avg_lst10_year_nat = mean(temp_c_nat, na.rm = T),
                           .groups= "keep") 
        
        weighted_avgs <- left_join(weighted_avgs,
                                   avg_55_64_nat)
        weighted_avgs <- left_join(weighted_avgs,
                                   avg_75_84_nat)
        weighted_avgs <- left_join(weighted_avgs,
                                   avg_85_94_nat)
        weighted_avgs <- left_join(weighted_avgs,
                                   avg_05_14_nat)
        
        weighted_avgs <- weighted_avgs %>% 
          dplyr::mutate(delta_k60_nat = avg_lst10_year_nat - avg_10_year_K60_nat,
                        delta_k80_nat = avg_lst10_year_nat - avg_10_year_K80_nat,
                        delta_k90_nat = avg_lst10_year_nat - avg_10_year_K90_nat)
      }
      write_rds(weighted_avgs, paste0("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/", 
                                      j, "_", i, "_", across_all,"_", "wavgs.rds"))
    }
  }
  toc()
}

##############################################################################
########################## PART III: Compute delta T #########################
##############################################################################
# now bring the historical and natural data together
calc_deltaT <- function(hist, nat, realization){
  if (realization == "realization"){
    all <- left_join(hist, 
                     nat,
                     by = c("year",
                            "ISO3",
                            realization))
    
    all$deltaT_k60 <- (all$delta_k60_hist - all$delta_k60_nat)/54
    all$deltaT_k80 <- (all$delta_k80_hist - all$delta_k80_nat)/34
    all$deltaT_k90 <- (all$delta_k90_hist - all$delta_k90_nat)/24
    
    # Now let us calculate the deltaT by multiplying the average annual increase in 
    # temperature by the number of years in between k and year t 
    # add new years 
    dataframe <- data.frame(ISO3 = all$ISO3,
                            realization = all$realization,
                            deltaT_k60 = all$deltaT_k60,
                            deltaT_k80 = all$deltaT_k80,
                            deltaT_k90 = all$deltaT_k90
                            )
    
    dataframe1 <- dataframe %>% dplyr::group_by(ISO3, realization) %>% 
      dplyr::mutate(year = row_number()) %>% 
      subset(year <7)
    
    dataframe1$year <- dataframe1$year + 2014
    
    all <- rbind.fill(all, dataframe1)
    
    # first calculate the number of years in betwen k and tt
    all$years_since_k60 <- all$year - 1960
    all$years_since_k80 <- all$year - 1980
    all$years_since_k90 <- all$year - 1990
    
    # then multuply the distance from k by average annual temp increase 
    all$deltaT_1960 <- all$deltaT_k60*(all$years_since_k60)
    all$deltaT_1980 <- all$deltaT_k80*(all$years_since_k80)
    all$deltaT_1990 <- all$deltaT_k90*(all$years_since_k90)
  }
  else if (realization == "not_realization"){
    all <- left_join(hist, 
                     nat,
                     by = c("year",
                            "ISO3"))
    
    all$deltaT_k60 <- (all$delta_k60_hist - all$delta_k60_nat)/54
    all$deltaT_k80 <- (all$delta_k80_hist - all$delta_k80_nat)/34
    all$deltaT_k90 <- (all$delta_k90_hist - all$delta_k90_nat)/24
    
    # Now let us calculate the deltaT by multiplying the average annual increase in 
    # temperature by the number of years in between k and year t 
    # add new years 
    dataframe <- data.frame(ISO3 = all$ISO3,
                            deltaT_k60 = all$deltaT_k60,
                            deltaT_k80 = all$deltaT_k80,
                            deltaT_k90 = all$deltaT_k90
    )
    
    dataframe1 <- dataframe %>% dplyr::group_by(ISO3) %>% 
      dplyr::mutate(year = row_number()) %>% 
      subset(year <7)
    
    dataframe1$year <- dataframe1$year + 2014
    
    all <- rbind.fill(all, dataframe1)
    
    # first calculate the number of years in betwen k and tt
    all$years_since_k60 <- all$year - 1960
    all$years_since_k80 <- all$year - 1980
    all$years_since_k90 <- all$year - 1990
    
    # then multuply the distance from k by average annual temp increase 
    all$deltaT_1960 <- all$deltaT_k60*(all$years_since_k60)
    all$deltaT_1980 <- all$deltaT_k80*(all$years_since_k80)
    all$deltaT_1990 <- all$deltaT_k90*(all$years_since_k90)
  }
    return(all)
}


##############################################################################
########################### PART IV: Visualize delta T #######################
##############################################################################
#density curve for country-year-realization
list_of_deltaTs <- list("deltaT_1960",
                        "deltaT_1980",
                        "deltaT_1990")


# now let us create a function to plot the density of deltaT
plot_density <- function(dataset, year_i, plot_lvl) {
  deltaT <- dataset %>% dplyr::group_by(ISO3,year) %>% 
    dplyr::summarise(deltaT_1960 = mean(deltaT_1960, na.rm = T),
                     deltaT_1980 = mean(deltaT_1980, na.rm = T),
                     deltaT_1990 = mean(deltaT_1990, na.rm = T),
                     .groups = "keep")
  deltaT1 <- subset(deltaT, year > year_i)
  var <- paste0("deltaT_", year_i)
  density <- ggplot(deltaT1, aes_string(x = var)) + geom_density() + 
    scale_color_grey() + theme_classic() + geom_vline(xintercept = c(0)) +
    ggtitle(paste0("Distribution of Country-Year ΔT ", plot_lvl, " ", year_i, "-2020")) +
    xlab("ΔT") + ylim(c(0,2.5)) +xlim(c(-0.65, 2.65)) + 
    theme(title = element_text(family = "Times", size = 12))
}



# Now let us plot the temporal trend in delta T 
plot_trend_year_r <- function(yeari) {
  # now let us aggregate the country-year-realization to make it country-year 
  # only deltaTs
  
  #deltaT1 <- subset(deltaT_avg, year >= yeari)
  var <- paste0("deltaT_", yeari)
  deltaT_avg <- all %>% dplyr::group_by(year,realization) %>% 
    dplyr::summarise(deltaT_1960 = mean(deltaT_1960, na.rm = T),
                     deltaT_1980 = mean(deltaT_1980, na.rm = T),
                     deltaT_1990 = mean(deltaT_1990, na.rm = T),
                     .groups = "keep")
  deltaT_avg1 <- subset(deltaT_avg, year >= yeari)
  #plot
  plot <- ggplot(deltaT_avg1, aes(x = year, y = deltaT_avg1[[var]], group = realization, 
                                  color = realization)) + geom_line() + 
    #geom_smooth() + 
    theme_classic() + ylab("ΔT") +
    ggtitle(paste0("Annual Trends in ΔT by MIROC6 Model Realization ", yeari, "-2020")) + 
    theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
    geom_hline(yintercept = 0) + 
    theme(title = element_text(family = "Times", size = 12)) +  
    scale_x_discrete(breaks=seq(yeari,2020,by=10))
}




# Now let us plot the temporal trend in delta T 
plot_trend_year <- function(dataset, yeari, plot_lvl) {
  deltaT_avg <- dataset %>% dplyr::group_by(year,realization) %>% 
    dplyr::summarise(deltaT_1960 = mean(deltaT_1960, na.rm = T),
                     deltaT_1980 = mean(deltaT_1980, na.rm = T),
                     deltaT_1990 = mean(deltaT_1990, na.rm = T),
                     .groups = "keep")
  # now let us aggregate the country-year to make it country-year only deltaTs
  deltaT_avg_year <- deltaT_avg %>% dplyr::group_by(year) %>% 
    dplyr::summarise(min_deltaT_1960 = min(deltaT_1960, na.rm = T),
                     min_deltaT_1980 = min(deltaT_1980, na.rm = T),
                     min_deltaT_1990 = min(deltaT_1990, na.rm = T),
                     max_deltaT_1960 = max(deltaT_1960, na.rm = T),
                     max_deltaT_1980 = max(deltaT_1980, na.rm = T),
                     max_deltaT_1990 = max(deltaT_1990, na.rm = T),
                     deltaT_1960 = mean(deltaT_1960, na.rm = T),
                     deltaT_1980 = mean(deltaT_1980, na.rm = T),
                     deltaT_1990 = mean(deltaT_1990, na.rm = T),
                     .groups = "keep")
  #deltaT1 <- subset(deltaT_avg, year >= yeari)
  var <- paste0("deltaT_", yeari)
  varmin <- paste0("min_deltaT_", yeari)
  varmax <- paste0("max_deltaT_", yeari)
  deltaT_avg1 <- subset(deltaT_avg_year, year >= yeari)
  deltaT_avg1$year <- as.character(deltaT_avg1$year)
  #plot
  plot <- ggplot(deltaT_avg1, aes(x = year, y = deltaT_avg1[[var]], group = 1)) + 
    geom_line() + #geom_smooth() + 
    geom_ribbon(aes(ymin = deltaT_avg1[[varmin]], ymax = deltaT_avg1[[varmax]]), alpha = 0.15) +
    theme_classic() + ylab("ΔT") + 
    scale_x_discrete(breaks=seq(yeari,2020,by=10)) +
    ggtitle(paste0("Annual Trends in ΔT ", plot_lvl, " ",yeari, "-2020")) + 
   # theme(axis.text.x = element_text(angle = 45, hjust=1, family = "Times"),
    #      title = element_text(family = "Times", size = 12)) + 
    geom_hline(yintercept = 0) + ylim(c(0,1.75)) 
}


#end of script

