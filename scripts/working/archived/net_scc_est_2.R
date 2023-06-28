

ratio_raster <- list_rasters[[1]]
experiment_df <-  fair_exps_1gtc_disagg_i
list_of_exps <-  list_of_exps
year_k <-  1990
future_forecast <-  future_forecast_df
gdp_temp_dataset <-  gdp_temp_data_sbstd


calculate_bidamages <- function(ratio_raster, experiment_df, list_of_exps, 
                                year_k, future_forecast, gdp_temp_dataset){
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
  
  #base_coords
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
  #i <- "all"
  i <- 2020
  # start an empty dataframe 
  mother_df <- data.frame()
  # now loop over the experiments and calculate total damages owed by each of 
  # the countries
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
                                                    bhm_era_reg,
                                                    gdp_temp_data1$deltat,
                                                    gdp_temp_data1$temp,
                                                    gdp_temp_data1$temp2)
    
    # now let us calculate deltaG
    gdp_temp_data1$delta_g_era <- gdp_temp_data1$response_tempactual_era - gdp_temp_data1$response_tempnew
    
    # test1 <- gdp_temp_data1 %>% dplyr::select(c("response_tempactual_era", "response_tempnew","delta_g_era", "ISO3", "year"))
    # let us bring in the population data at the country-year level
    gdp_temp_data1 <- left_join(gdp_temp_data1,
                                pop_wdi,
                                by = c("ISO3" = "iso3c",
                                       "year" = "year"))
    
    gdp_temp_data1 <- gdp_temp_data1 %>% 
      dplyr::mutate(SP.POP.TOTL = case_when(is.na(SP.POP.TOTL) ~ pop,
                                            TRUE ~ SP.POP.TOTL))
    
    
    
  #  identifier <- gdp_temp_data1 %>% 
   #   ungroup(.) %>%  
    #  dplyr::select(c("ISO3", "diff_lgdp", "year")) %>% 
    #  subset(.,year == 2021) %>% 
    #  dplyr::mutate(ssp_data = case_when(year >= 2021 & is.na(diff_lgdp) ~ "no",
                                 #        TRUE ~ "yes")) %>% 
  #  subset(., ssp_data == "yes") %>% 
  #   dplyr::select(c("ISO3"))
    
  #  gdp_temp_data1 <- left_join(identifier, gdp_temp_data1)
    
    # unlist...
    gdp_temp_data1$delta_g_era <- unlist(gdp_temp_data1$delta_g_era)
    
    # now let us calculate adjusted growht rate by adding deltaG to observed growth
    gdp_temp_data1$adj_growth <- (gdp_temp_data1$delta_g_era + gdp_temp_data1$diff_lgdp)
    
    # let us add 1 to growth variables so we can calculate cumulative growth
    gdp_temp_data1$diff_lgdp <- gdp_temp_data1$diff_lgdp + 1
    gdp_temp_data1$adj_growth <- gdp_temp_data1$adj_growth + 1
    
    # now let us set GDP at year k as initial gdp number to calculate impact of 
    # added temoerature
    gdp_temp_data1 <- gdp_temp_data1 %>% dplyr::group_by(ISO3) %>% 
      dplyr::mutate(gdp_year = NY.GDP.PCAP.KD[year == year_k])
    
    
    gdp_temp_data1 <- subset(gdp_temp_data1, year >= 2020)
    
    
    damages_i_t4 <- gdp_temp_data1 %>% dplyr::group_by(ISO3) %>% 
      dplyr::mutate(cum_adj_growthz = cumprod(adj_growth),
                    cum_growth_real = cumprod(diff_lgdp))
    
    # finally comput edamages...
    damages_i_t4 <- damages_i_t4 %>% 
      dplyr::mutate(damages = (gdp_year * cum_adj_growthz) - (gdp_year * cum_growth_real))
    
    # calculate discounted damages for past and for future (note different 
    #- processes for discounting past damages and future damages)
    damages_i_t4 <- damages_i_t4 %>% 
      dplyr::mutate(t_since_k = year - year_k,
                    t_since_today = year - 2020,
                    weighted_damages2 = case_when(year <= 2020 ~ (damages*((1+(0.02))^t_since_k)),
                                                  year > 2020 ~ (damages*(1/(1+(0.02))^t_since_today))),
                    weighted_damages3 = case_when(year <= 2020 ~ (damages*((1+(0.03))^t_since_k)),
                                                  year > 2020 ~ (damages*(1/(1+(0.03))^t_since_today))),
                    weighted_damages5 = case_when(year <= 2020 ~ (damages*((1+(0.05))^t_since_k)),
                                                  year > 2020 ~ (damages*(1/(1+(0.05))^t_since_today))),
                    weighted_damages7 = case_when(year <= 2020 ~ (damages*((1+(0.07))^t_since_k)),
                                                  year > 2020 ~ (damages*(1/(1+(0.07))^t_since_today))))
    
    damages_i_t4 <- subset(damages_i_t4, !is.na(coef_id))
    
    
    # scale by population
    damages_i_t4$damages_pop <- damages_i_t4$damages * damages_i_t4$SP.POP.TOTL
    damages_i_t4 <- damages_i_t4 %>% 
      dplyr::mutate(weighted_damages2_scld = weighted_damages2 *SP.POP.TOTL,
                    weighted_damages3_scld = weighted_damages3 *SP.POP.TOTL,
                    weighted_damages5_scld = weighted_damages5 *SP.POP.TOTL,
                    weighted_damages7_scld = weighted_damages7 *SP.POP.TOTL)
  
    damages_i_t4$emitter <- i
    
    damages_summ <- damages_i_t4 %>% dplyr::group_by(emitter, coef_id) %>% 
      dplyr::summarise(total_damages = sum(damages_pop, na.rm = T),
                       total_damages2 = sum(weighted_damages2_scld, na.rm = T),
                       total_damages3 = sum(weighted_damages3_scld, na.rm = T),
                       .groups = "keep")
  
    # add to data frame
    mother_df <- rbind(mother_df, damages_summ)

    toc()
  }
  return(mother_df)
  
}

list_of_exps <- c(2020)

# test 
test_cals <- calculate_bidamages(master_raster, experiment_df = fair_exps, list_of_exps = list_of_exps ,year_k = 1990, future_forecast = future_forecast_df,gdp_temp_dataset = gdp_temp_data_sbstd)