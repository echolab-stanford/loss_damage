##############################################################################
# Mustafa Zahid, April 1st, 2022
# This script calculates the total damages owed by emitter country j to receiver 
# country i 
#############################################################################


#now let's write the rich/poor separate response functions 
calc_delta_g_richpoor <- function(dataset, temp_var, model, deltaT, interact) {
  response_tempnew <- ((temp_var - deltaT)*(coef(model)[1])) +
    (((temp_var - deltaT)^2)*(coef(model)[2])) + (interact*(temp_var-deltaT)*(coef(model)[5])) +
    (interact*((temp_var - deltaT)^2)*(coef(model)[6]))
  response_tempnew
}

calculate_bidamages_richpoor <- function(ratio_raster, experiment_df, list_of_exps, year_k){
  deltat_df <- as.data.frame(as.matrix(rasterToPoints(ratio_raster)))
  # assign merge if to merge with fair data
  deltat_df$merge_id <- "1"
  
  # now assign the experiment data frame FROM FAIR
  fair_exps <- experiment_df 
  #assign merge id 
  fair_exps$merge_id <- "1"
  
  fair_exps <- subset(fair_exps, !is.na(experiment_iso))
  
  # now bring them together
  deltat_calced_df <- left_join(deltat_df,
                                fair_exps,
                                by = c("merge_id"))
  
  # now multuply the grid level warming ratio 
  deltat_calced_df$deltat <- deltat_calced_df$raster_deltat_ratio_cmip6 * deltat_calced_df$median_deltat
  
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
  pop <- resample(pop, raster_cgm_ratio)
  popdf <- as.data.frame(as.matrix(rasterToPoints(pop)))
  
  # the other thing we need is to get a country id for each lon-lat combo
  data("wrld_simpl")
  world <- wrld_simpl[,3]
  world <- st_as_sf(world)
  
  # now join base coordaintes with country ids and remove NAs
  base_coords <- deltat_df %>% dplyr::select(c("x", "y"))
  base_coords <- st_as_sf(base_coords, coords = c("x", "y"), 
                          crs = crs(raster_cgm_ratio))
  base_coords <- st_join(base_coords,world)
  base_coords <- base_coords %>%
    mutate(x = unlist(map(base_coords$geometry,1)),
           y = unlist(map(base_coords$geometry,2)))
  st_geometry(base_coords) <- NULL 
  base_coords <- subset(base_coords, !is.na(ISO3))
  
  # now let us get a unique dataframe of all experiemtns (i.e. countries
  # - shutting off emissions)
  #fair_experiment_iso <- unique(deltaT_df$experiment_iso)
  pop <- wdi_dat %>% dplyr::select(c("iso3c", "SP.POP.TOTL", "year")) 
  
  # start an empty dataframe 
  mother_df <- data.frame()
  # now loop over the experiments and calculate total damages owed by each of 
  # the countries
  for (i in list_of_exps){
    tic()
    # example 
    deltaT_df1 <- subset(deltaT_df, experiment_iso == i)
    joined_final_df <- left_join(deltaT_df1,
                                 base_coords,
                                 by = c("x", "y"))
    
    joined_final_df <- subset(joined_final_df, !is.na(ISO3))
    #joined_final_df <- subset(joined_final_df, year < 2021)
    
    joined_final_df <- joined_final_df %>% dplyr::group_by(ISO3, year) %>% 
      dplyr::summarise(deltat = mean(deltat, na.rm = T),
                       .groups = "keep")
    
    # Merge both datasets
    gdp_temp_data1 <- gdp_temp_data
    gdp_temp_data1$year <- as.numeric(as.character(gdp_temp_data1$year))
    gdp_temp_data1 <- left_join(joined_final_df,
                                gdp_temp_data1,
                                by = c("ISO3", "year"))
    
    
    gdp_temp_data1$response_tempnew <- calc_delta_g_richpoor(gdp_temp_data1,
                                                             gdp_temp_data1$era_mwtemp,
                                                             bhm_era_reg,
                                                             gdp_temp_data1$deltat,
                                                             gdp_temp_data1$poor)
    
    gdp_temp_data1$delta_g_era <- gdp_temp_data1$response_tempactual_era - 
      gdp_temp_data1$response_tempnew
    
    gdp_temp_data1$delta_g_era <- unlist(gdp_temp_data1$delta_g_era)
    
    gdp_temp_data1 <- left_join(gdp_temp_data1,
                                pop_wdi,
                                by = c("ISO3" = "iso3c",
                                       "year" = "year"))
    
    
    gdp_temp_data1$delta_g_era <- unlist(gdp_temp_data1$delta_g_era)
    
    gdp_temp_data1$adj_growth <- (gdp_temp_data1$delta_g_era + gdp_temp_data1$diff_lgdp)
    
    gdp_temp_data1$diff_lgdp <- gdp_temp_data1$diff_lgdp + 1
    
    gdp_temp_data1$adj_growth <- gdp_temp_data1$adj_growth + 1
    
    gdp_temp_data1$diff_lgdp[is.na(gdp_temp_data1$diff_lgdp)] <- 1
    gdp_temp_data1$adj_growth[is.na(gdp_temp_data1$adj_growth)] <- 1
    
    
    gdp_temp_data1$diff_lgdp <- gdp_temp_data1$diff_lgdp - 1
    gdp_temp_data1$diff_lgdp[gdp_temp_data1$diff_lgdp == 0.000] <- NA
    gdp_temp_data1 <- gdp_temp_data1 %>% 
      dplyr::mutate(adj_growthx = delta_g_era + diff_lgdp)
    damages_i_t3 <- gdp_temp_data1 %>% 
      dplyr::mutate(adj_growthxz = adj_growthx + 1)
    damages_i_t3 <- damages_i_t3 %>% dplyr::group_by(ISO3) %>% 
      dplyr::mutate(gdp_year = NY.GDP.PCAP.KD[year == year_k])
    damages_i_t3$diff_lgdp1 <- damages_i_t3$diff_lgdp +1
    damages_i_t4 <- damages_i_t3 %>% dplyr::group_by(ISO3) %>% 
      dplyr::mutate(cum_adj_growthz = cumprod(adj_growthxz),
                    cum_growth_real = cumprod(diff_lgdp1))
    damages_i_t4 <- damages_i_t4 %>% dplyr::mutate(damages = (gdp_year * cum_adj_growthz) - 
                                                     (gdp_year * cum_growth_real))
    damages_i_t4$damages_pop <- damages_i_t4$damages * damages_i_t4$SP.POP.TOTL
    damages_i_t4 <- damages_i_t4 %>% 
      dplyr::mutate(t_since_k = year - year_k,
                    t_since_today = 2020 - year,
                    weighted_damages2 = case_when(year <= 2020 ~ (damages_pop*((1+(0.02))^t_since_k)),
                                                  year > 2020 ~ (damages_pop*(1/(1+(0.02))^t_since_today))),
                    weighted_damages3 = case_when(year <= 2020 ~ (damages_pop*((1+(0.03))^t_since_k)),
                                                  year > 2020 ~ (damages_pop*(1/(1+(0.03))^t_since_today))),
                    weighted_damages5 = case_when(year <= 2020 ~ (damages_pop*((1+(0.05))^t_since_k)),
                                                  year > 2020 ~ (damages_pop*(1/(1+(0.05))^t_since_today))),
                    weighted_damages7 = case_when(year <= 2020 ~ (damages_pop*((1+(0.07))^t_since_k)),
                                                  year > 2020 ~ (damages_pop*(1/(1+(0.07))^t_since_today))))
    damages_neg <- subset(damages_i_t4, damages_pop < 0)
    #sum(damages_neg$weighted_damages2, na.rm = T)
    damages_i_t4$status <- paste0("data_", i)
    damages_i_t4$emitter <- i
    mother_df <- rbind(mother_df, damages_i_t4)
    toc()
  }
  return(mother_df)
}