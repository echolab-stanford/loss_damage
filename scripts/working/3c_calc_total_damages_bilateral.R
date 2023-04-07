##############################################################################
# Mustafa Zahid, April 1st, 2022
# This script calculates the total damages owed by emitter country j to receiver 
# country i 
#############################################################################

#total_damages_k80 <- calculate_bidamages(master_raster,
#                                         fair_exps_k80, 
#                                         list_of_exps, 
#                                         1980,
#                                         future_forecast_df,
#                                         gdp_temp_data)

#ratio_raster <- master_raster#
#experiment_df <- fair_exps_1tco2_2300

#list_of_exps <- unique(fair_exps_1tco2_2300$experiment_iso)
#year_k <- 1990
#future_forecast <- future_forecast_ssp370_2300

# calculating damages owed by each country to another. It takes in arguments 
# for the warming ratio used, as well as the dataframe from FaIR temperature 
# response and the list of emitters (list_of_exps)
calculate_bidamages_bilateral <- function(ratio_raster, experiment_df, list_of_exps, 
                                year_k, future_forecast, gdp_temp_dataset, bhm_model){
  #read raster data for warming ratio 
  deltat_df <- as.data.frame(as.matrix(rasterToPoints(ratio_raster)))
  colnames(deltat_df)[3] <- "layer"
  
  # assign merge if to merge with fair data
  deltat_df$merge_id <- "1"
  
  # now assign the experiment data frame FROM FAIR
  fair_exps <- experiment_df 
  #fair_exps <- subset(fair_exps, num_loop == num_loop)
  #assign merge id 
  fair_exps$merge_id <- "1"
  
  fair_exps <- subset(fair_exps, !is.na(experiment_iso))
  # now bring them together
  deltat_calced_df <- left_join(deltat_df,
                                fair_exps,
                                by = c("merge_id"))
  
  
  # now multuply the grid level warming ratio by median temp response from FaIR
  deltat_calced_df$deltat <- deltat_calced_df$layer * deltat_calced_df$median_deltat
  
  # now let us take the columns we need from the fair experiment dataframe 
  deltaT_df <- deltat_calced_df %>% dplyr::select(c("x",
                                                    "y",
                                                    "deltat",
                                                    "year",
                                                    "experiment_iso"))
  
  # we need to aggregate the deltat to the country level by weighting by pop
  # so let us read the pop raster and resample it to match coordinates and 
  # convert to a dataframe and then join
  pop <- raster("~/BurkeLab Dropbox/Projects/loss_damage/data/raw/population/gpw_v4_population_count_rev11_2010_1_deg.tif")
  pop <- readAll(pop)
  pop <- resample(pop, ratio_raster)
  popdf <- as.data.frame(as.matrix(rasterToPoints(pop)))
  
  # the other thing we need is to get a country id for each lon-lat combo
  data("wrld_simpl")
  world <- wrld_simpl[,3]
  world <- st_as_sf(world)
  
  # now join base coordaintes with country ids and remove NAs
  base_coords <- deltat_df %>% dplyr::select(c("x", "y"))
  base_coords <- st_as_sf(base_coords, coords = c("x", "y"), 
                          crs = crs(ratio_raster))
  base_coords <- st_join(base_coords,world)
  base_coords <- base_coords %>%
    mutate(x = unlist(map(base_coords$geometry,1)),
           y = unlist(map(base_coords$geometry,2)))
  st_geometry(base_coords) <- NULL 
  base_coords <- subset(base_coords, !is.na(ISO3))
  
  # now let us get a unique dataframe of all experiemtns (i.e. countries
  # - shutting off emissions)
  #deltaT_df <- subset(deltaT_df, !is.na(experiment_iso))
  #fair_experiment_iso <- unique(deltaT_df$experiment_iso)
  pop <- wdi_dat %>% dplyr::select(c("iso3c", "SP.POP.TOTL", "year")) 
  
  # calculating model growth response after adjusting for the delta T by creating 
  # a function that takes in the temperature variable, as well as the model used
  calc_delta_g <- function(dataset, temp_var, model, deltaT, coef1, coef2) {
    response_tempnew <- ((temp_var - deltaT)*(coef1)) +
      (((temp_var - deltaT)^2)*(coef2)) 
    response_tempnew
  }
  i <- "all"
  # start an empty dataframe 
  mother_df <- data.frame()
  # now loop over the experiments and calculate total damages owed by each of 
  # the countries
  i <- "USA"
  for (i in list_of_exps){
    tic()
    # subset by keeping one experiment for each loop
    deltaT_df1 <- subset(deltaT_df, experiment_iso == i)
    # merge with coordinates and country ids
    joined_final_df <- left_join(deltaT_df1,
                                 base_coords,
                                 by = c("x", "y"))
    
    # keep non missing ISOs exoeriments
    joined_final_df <- subset(joined_final_df, !is.na(ISO3))
    # keep to a certain year
    #joined_final_df <- subset(joined_final_df, year < 2021)
    
    # now let us calculate the average deltaT by country-year
    joined_final_df <- joined_final_df %>% dplyr::group_by(ISO3, year) %>% 
      dplyr::summarise(deltat = mean(deltat, na.rm = T),
                       .groups = "keep")
    
    # now let us get the country-year panel
    gdp_temp_data1 <- gdp_temp_dataset
    gdp_temp_data1$year <- as.numeric(as.character(gdp_temp_data1$year))
    
    #gdp_temp_data1 <- subset(gdp_temp_data1, year >= 1980)
    # join country-year temp change with the country-year panel
    gdp_temp_data1 <- left_join(joined_final_df,
                                gdp_temp_data1,
                                by = c("ISO3", "year"))
    
    #  gdp_temp_data1$adjusted_temp <- gdp_temp_data1$era_mwtemp - gdp_temp_data1$deltat
    
    # dev.off()
    #  par(mfrow = c(1,1))
    
    #  hist(gdp_temp_data1$era_mwtemp, add =T)
    #  hist(gdp_temp_data1$adjusted_temp, col = "red", add = T)
    # calculate reposnse from added temperature
    gdp_temp_data1$response_tempnew <- calc_delta_g(gdp_temp_data1,
                                                    gdp_temp_data1$era_mwtemp,
                                                    bhm_model,
                                                    gdp_temp_data1$deltat,
                                                    coef(bhm_model)[1],
                                                    coef(bhm_model)[2])
    
    plot(gdp_temp_data1$era_mwtemp, gdp_temp_data1$response_tempactual_era, type = "l")
    lines(gdp_temp_data1$era_mwtemp, gdp_temp_data1$response_tempnew, type = "l",
         col = "red")
    
    # now let us calculate deltaG
    gdp_temp_data1$delta_g_era <- gdp_temp_data1$response_tempactual_era - gdp_temp_data1$response_tempnew
  
    
    maxtemp_2020 <- max(gdp_temp_data1$era_mwtemp[gdp_temp_data1$year < 2021], na.rm = T)
    gdp_temp_data1$max_delta_g_era <- gdp_temp_data1$delta_g_era[gdp_temp_data1$year < 2021 & gdp_temp_data1$era_mwtemp == maxtemp_2020 & !is.na(gdp_temp_data1$delta_g_era)]
    max_g <- gdp_temp_data1$delta_g_era[gdp_temp_data1$year < 2021 & gdp_temp_data1$era_mwtemp == maxtemp_2020 & !is.na(gdp_temp_data1$delta_g_era)]
    gdp_temp_data1$delta_g_era[gdp_temp_data1$year > 2020 & gdp_temp_data1$delta_g_era < max_g] <- max_g 

    # test1 <- gdp_temp_data1 %>% dplyr::select(c("response_tempactual_era", "response_tempnew","delta_g_era", "ISO3", "year"))
    # let us bring in the population data at the country-year level
    gdp_temp_data1 <- left_join(gdp_temp_data1,
                                pop_wdi,
                                by = c("ISO3" = "iso3c",
                                       "year" = "year"))
    
    gdp_temp_data1 <- gdp_temp_data1 %>% 
      dplyr::mutate(SP.POP.TOTL = case_when(is.na(SP.POP.TOTL) ~ pop,
                                            TRUE ~ SP.POP.TOTL))
    
    # unlist...
    gdp_temp_data1$delta_g_era <- unlist(gdp_temp_data1$delta_g_era)
    
    # now let us calculate adjusted growht rate by adding deltaG to observed growth
    gdp_temp_data1$adj_growth <- (gdp_temp_data1$delta_g_era + gdp_temp_data1$diff_lgdp_for_damages)
    
    #colnames(gdp_temp_data_k80)
    
    # let us add 1 to growth variables so we can calculate cumulative growth
    gdp_temp_data1$diff_lgdp_for_damages <- gdp_temp_data1$diff_lgdp_for_damages + 1
    gdp_temp_data1$adj_growth <- gdp_temp_data1$adj_growth + 1
    
    # let us set missing NAs to 1 so we can run the script without breaking 
    # becasue of NAs
    gdp_temp_data1$diff_lgdp_for_damages[is.na(gdp_temp_data1$diff_lgdp_for_damages)] <- 1
    gdp_temp_data1$adj_growth[is.na(gdp_temp_data1$adj_growth)] <- 1
    
    # we now want to set 0 growth to NA
    gdp_temp_data1$diff_lgdp_for_damages <- gdp_temp_data1$diff_lgdp_for_damages - 1
    gdp_temp_data1$diff_lgdp_for_damages[gdp_temp_data1$diff_lgdp_for_damages == 0.000] <- NA
    # then let us calculate adjusted growth rate
    gdp_temp_data1 <- gdp_temp_data1 %>% 
      dplyr::mutate(adj_growthx = delta_g_era + diff_lgdp_for_damages)
    # now let us add 1 so that we can calculate cumulative growth rate 
    damages_i_t3 <- gdp_temp_data1 %>% 
      dplyr::mutate(adj_growthxz = adj_growthx + 1)
    # now let us set GDP at year k as initial gdp number to calculate impact of 
    # added temoerature
    damages_i_t3 <- damages_i_t3 %>% dplyr::group_by(ISO3) %>% 
      dplyr::mutate(gdp_year = NY.GDP.PCAP.KD_for_damages[year == year_k])
    
    # now let us add 1 to observed growth so we can calculate cumulative
    # gdp growth
    damages_i_t3$diff_lgdp1 <- damages_i_t3$diff_lgdp_for_damages +1
    # now let us calculate cumulative GDP growth
    damages_i_t4 <- damages_i_t3 %>% dplyr::group_by(ISO3) %>% 
      dplyr::mutate(cum_adj_growthz = cumprod(adj_growthxz),
                    cum_growth_real = cumprod(diff_lgdp1))
    # finally comput edamages...
    damages_i_t4 <- damages_i_t4 %>% 
      dplyr::mutate(damages = (gdp_year * cum_adj_growthz) - (gdp_year * cum_growth_real))
    
    #summary(damages_i_t4$damages)
    
    # scale by population
    damages_i_t4$damages_pop <- damages_i_t4$damages * damages_i_t4$SP.POP.TOTL
    
    # calculate discounted damages for past and for future (note different 
    #- processes for discounting past damages and future damages)
    damages_i_t4 <- damages_i_t4 %>% 
      dplyr::mutate(t_since_k = year - year_k,
                    t_since_today = year - 2020,
                    weighted_damages2 = case_when(year <= 2020 ~ (damages_pop*((1+(0.02))^t_since_k)),
                                                  year > 2020 ~ (damages_pop*(1/(1+(0.02))^t_since_today))),
                    weighted_damages3 = case_when(year <= 2020 ~ (damages_pop*((1+(0.03))^t_since_k)),
                                                  year > 2020 ~ (damages_pop*(1/(1+(0.03))^t_since_today))),
                    weighted_damages5 = case_when(year <= 2020 ~ (damages_pop*((1+(0.05))^t_since_k)),
                                                  year > 2020 ~ (damages_pop*(1/(1+(0.05))^t_since_today))),
                    weighted_damages7 = case_when(year <= 2020 ~ (damages_pop*((1+(0.07))^t_since_k)),
                                                  year > 2020 ~ (damages_pop*(1/(1+(0.07))^t_since_today))))
    
    # keep pnly negative damages
    damages_neg <- subset(damages_i_t4, damages_pop < 0)
    #sum(damages_neg$weighted_damages2, na.rm = T)
    #sum(damages_i_t4$weighted_damages2, na.rm = T)
    #damages_neg$status <- paste0("data_", i)
    #damages_i_t4$status <- paste0("data_", i)
    # name th emitter
    damages_i_t4$emitter <- i
    
    #damages_i_t4$emitter <- i
    #test_dfs[[i]] <- damages_neg
    # add to data frame
    mother_df <- rbind(mother_df, damages_i_t4)
    toc()
  }
  return(mother_df)
  
}

#sum(damages_i_t4$weighted_damages2[damages_i_t4$weighted_damages2 < 0 & damages_i_t4$year < 2021], na.rm = T)
