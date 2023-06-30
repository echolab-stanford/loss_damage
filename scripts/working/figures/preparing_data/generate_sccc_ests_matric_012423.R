##############################################################################
# Mustafa Zahid, Jan 23, 2023
# This r script will produce the SCC estimates for various models under 
# different assumptions 
##############################################################################


# first let us readin the world bank gdp and classification data and 
# categorize the countries, take the averages, and finally record the 
# minimum and maximum for each of the income groups. Then we can 
# combine with the SSP growth data and clamp the growth on both ends 
# by income group. This will then go through the calculation script. There 
# we need to make sure we are also constraining the counterfactual grwoth 
# rate from giong beyond the set bounds 
income_class <- read_csv("~/Downloads/data-XHzgJ.csv")
income_class <- income_class %>% dplyr::select(c("Country", "Income group"))
income_class$Country[income_class$Country == "Turkey"] <- "Turkiye"
wdi_dat <- left_join(wdi_dat,
                     income_class,
                     by = c("country" = "Country"))
#colnames(wdi_dat)[4] <- "year"

wdi_dat <- subset(wdi_dat, year < 2021)
# create the growth variable and set the dataset as a pdataframe
wdi_dat$gdp_pc <- log(wdi_dat$NY.GDP.PCAP.KD)
wdi_dat <- plm::pdata.frame(wdi_dat, index = c("iso3c","year"))
wdi_dat$lgdp_pc <- plm::lag(wdi_dat$gdp_pc)
wdi_dat$diff_lgdp <- wdi_dat$gdp_pc - wdi_dat$lgdp_pc

wdi_dat <- wdi_dat %>% dplyr::group_by(iso3c) %>% 
  dplyr::mutate(missing = case_when(is.na(diff_lgdp) ~ 0,
                                    TRUE ~ 1))
wdi_dat <- wdi_dat %>% dplyr::group_by(iso3c) %>% 
  dplyr::mutate(missing_tot = sum(missing ,na.rm = T))
wdi_dat <- subset(wdi_dat, missing_tot >= 10)
average_data <- wdi_dat %>% 
  dplyr::group_by(iso3c, Income.group) %>% 
  dplyr::summarise(diff_lgdp = mean(diff_lgdp, na.rm = T))
minmax_data <- average_data %>% dplyr::group_by(Income.group) %>% 
  dplyr::mutate(min_dg = min(diff_lgdp, na.rm = T),
                   max_dg = max(diff_lgdp, na.rm = T),
                   p_90 = quantile(diff_lgdp, 0.90))


minmax_data <- minmax_data %>% dplyr::select(c("iso3c", "Income.group", 
                                               "min_dg", "max_dg", "p_90"))

# now let us merge in with the forecast data 
future_forecast_ssp370_2300_adj <- left_join(future_forecast_ssp370_2300,
                                             minmax_data,
                                             by = c("ISO3" = "iso3c"))

# now let us establish the clamping on observed growth rate. Basically 
# we are gonna say in our future projection sample we are not going to allow 
# growth to go out of bounds. 
future_forecast_ssp370_2300_adj <- future_forecast_ssp370_2300_adj %>% 
  dplyr::mutate(diff_lgdp = case_when(diff_lgdp > p_90 ~ p_90,
                                      diff_lgdp < min_dg ~ min_dg,
                                      TRUE ~ diff_lgdp))

hist(future_forecast_ssp370_2300_adj$diff_lgdp)
summary(future_forecast_ssp370_2300_adj$diff_lgdp)

# done clamping. now let us bring in the calculation function so we can 
# calculate the scc with the clamping

ratio_raster <- master_raster
experiment_df <- fair_exps_1tco2_2300
year_k <- 1990
future_forecast <- future_forecast_ssp370_2300
gdp_temp_dataset <- generate_gdptemp_panel_5lags("pooled",
                                                 future_forecast_ssp370_2300,
                                                 1990,
                                                 "ERA")

gdp_temp_dataset <- generate_gdptemp_panel("pooled",
                                            future_forecast_ssp370_2300_adj,
                                            1990,
                                            "ERA")
temp_dataset <- "ERA"

calculate_bidamages <- function(ratio_raster, experiment_df, list_of_exps, 
                                year_k, future_forecast, gdp_temp_dataset, 
                                temp_dataset){
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
  
  #  fair_exps <- subset(fair_exps, !is.na(experiment_iso))
  # now bring them together
  deltat_calced_df <- left_join(deltat_df,
                                fair_exps,
                                by = c("merge_id"))
  
  # now multuply the grid level warming ratio by median temp response from FaIR
  #deltat_calced_df$deltat <- deltat_calced_df$layer * deltat_calced_df$median_deltat
  deltat_calced_df$deltat_scld <- deltat_calced_df$layer * deltat_calced_df$median_deltat   
  deltat_calced_df$deltat_fullemms_scld <- deltat_calced_df$layer * deltat_calced_df$deltat_fullemms
  deltat_calced_df$deltat_preturb_scld <- deltat_calced_df$layer * deltat_calced_df$deltaT_preturb
  
  # now let us take the columns we need from the fair experiment dataframe 
  deltaT_df <- deltat_calced_df %>% dplyr::select(c("x",
                                                    "y",
                                                    "deltat_fullemms_scld",
                                                    "deltat_preturb_scld",
                                                    "deltat_scld",
                                                    "year",
                                                    "experiment_iso"))
  #  "exp_yr"))
  
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
  
  # start an empty dataframe 
  mother_df <- data.frame()
  i <- 2020
  i <- "USA"
  i <- "all0"
  # now loop over the experiments and calculate total damages owed by each of 
  # the countries
  for (i in list_of_exps){
    tic()
    
    # subset by keeping one experiment for each loop
    deltaT_df1 <- subset(deltaT_df, experiment_iso == i)
    
    #  deltaT_df1 <- deltaT_df
    # merge with coordinates and country ids
    joined_final_df <- left_join(deltaT_df1,
                                 base_coords,
                                 by = c("x", "y"))
    
    # keep non missing ISOs exoeriments
    joined_final_df <- subset(joined_final_df, !is.na(ISO3))
    # keep to a certain year
    #joined_final_df <- subset(joined_final_df, year < 2021)
    
    # now let us calculate the average deltaT by country-year
    #    joined_final_df <- joined_final_df %>% dplyr::group_by(ISO3, year) %>% 
    #     dplyr::summarise(deltat = mean(deltat, na.rm = T),
    #                     .groups = "keep")
    
    joined_final_df <- joined_final_df %>% dplyr::group_by(ISO3, year) %>% 
      dplyr::summarise(deltat_fullemms = mean(deltat_fullemms_scld, na.rm = T),
                       deltat_preturb = mean(deltat_preturb_scld, na.rm = T),
                       deltat = mean(deltat_scld, na.rm = T),
                       .groups = "keep")
    
    joined_final_df$deltat_preturb[joined_final_df$year < 2021] <- 0
    joined_final_df$deltat_fullemms[joined_final_df$year < 2021] <- 0
    
    # now let us get the country-year panel
    gdp_temp_data1 <- gdp_temp_dataset
    gdp_temp_data1$year <- as.numeric(as.character(gdp_temp_data1$year))
    
    #gdp_temp_data1 <- subset(gdp_temp_data1, year >= 1980)
    # join country-year temp change with the country-year panel
    
    gdp_temp_data1 <- left_join(joined_final_df,
                                gdp_temp_data1,
                                by = c("ISO3", "year"))
    
    # we need to create country level annual average for last 5 years of observed 
    # data 
    
    if (temp_dataset == "ERA"){
      gdp_temp_data1$era_mwtemp[gdp_temp_data1$year >2020] <- NA 
      #  if (i < 2020){
      gdp_temp_data1$era_mwtemp_preturb_pre2020 <- gdp_temp_data1$era_mwtemp - gdp_temp_data1$deltat
      gdp_temp_data1 <- gdp_temp_data1 %>% dplyr::group_by(ISO3) %>% 
        dplyr::mutate(avg_temp_2016_2020_preturb = mean(era_mwtemp_preturb_pre2020[year > 2015 & year <= 2020], na.rm = T))
    }
    if (temp_dataset == "CRU"){
      gdp_temp_data1$cru_mwtemp[gdp_temp_data1$year >2020] <- NA 
      #  if (i < 2020){
      gdp_temp_data1$cru_mwtemp_preturb_pre2020 <- gdp_temp_data1$cru_mwtemp - gdp_temp_data1$deltat
      gdp_temp_data1 <- gdp_temp_data1 %>% dplyr::group_by(ISO3) %>% 
        dplyr::mutate(avg_temp_2016_2020_preturb = mean(cru_mwtemp_preturb_pre2020[year > 2015 & year <= 2020], na.rm = T))
    }
    
    
    # plot(gdp_temp_data1$year[gdp_temp_data1$ISO3 == "USA"], gdp_temp_data1$deltat[gdp_temp_data1$ISO3== "SAU"], type = "l")
    #  plot(fair_exps_1GtCO2_updated$year[fair_exps_1GtCO2_updated$experiment_iso == 2020], fair_exps_1GtCO2_updated$median_deltat[fair_exps_1GtCO2_updated$experiment_iso == 2020])
    
    
    #  }
    
    #  gdp_temp_data1 <- gdp_temp_data1 %>% dplyr::group_by(ISO3) %>% 
    #   dplyr::mutate(avg_temp_2016_2020 = mean(era_mwtemp[year > 2015 & year <= 2020], na.rm = T))
    
    #  if (i >= 2020){
    #  gdp_temp_data1 <- gdp_temp_data1 %>%  dplyr::group_by(ISO3, year) %>% 
    #  dplyr::mutate(era_mwtemp_fullemms = case_when(!is.na(era_mwtemp) ~ era_mwtemp, 
    #                                            is.na(era_mwtemp) & year >2020 ~ avg_temp_2016_2020 + deltat_fullemms),
    #               era_mwtemp_preturb= case_when(!is.na(era_mwtemp) ~ era_mwtemp, 
    #                                             is.na(era_mwtemp) & year >2020 ~ avg_temp_2016_2020 + deltat_preturb))
    # }
    
    #  if (i >= 2020){
    #    gdp_temp_data1 <- gdp_temp_data1 %>%  dplyr::group_by(ISO3, year) %>% 
    #      dplyr::mutate(era_mwtemp_preturb= case_when(!is.na(era_mwtemp) ~ era_mwtemp, 
    #                                                  is.na(era_mwtemp) & year >2020 ~ avg_temp_2016_2020_preturb + deltat_preturb))
    #  }
    
    #  gdp_temp_data1$era_mwtemp_fullemms <- gdp_temp_data1$era_mwtemp_preturb + gdp_temp_data1$deltat 
    
    #  if (i < 2020) {
    #  gdp_temp_data1 <- gdp_temp_data1 %>%  dplyr::group_by(ISO3, year) %>% 
    #  dplyr::mutate(era_mwtemp_fullemms = case_when(!is.na(era_mwtemp) ~ era_mwtemp, 
    #                                                 is.na(era_mwtemp) & year >2020 ~ avg_temp_2016_2020 + deltat_fullemms),
    #                  era_mwtemp_preturb= case_when(!is.na(era_mwtemp) ~ era_mwtemp_preturb_pre2020, 
    #                                                 is.na(era_mwtemp) & year >2020 ~ avg_temp_2016_2020_preturb + deltat_preturb))
    #  }
    #  gdp_temp_data1$deltat <- gdp_temp_data1$era_mwtemp_ssp370 - gdp_temp_data1$era_mwtemp_noemms
    #  if (i < 2020) {
    
    if (temp_dataset == "ERA") {
      gdp_temp_data1 <- gdp_temp_data1 %>%  dplyr::group_by(ISO3, year) %>% 
        dplyr::mutate(
          era_mwtemp_preturb= case_when(!is.na(era_mwtemp) ~ era_mwtemp_preturb_pre2020, 
                                        is.na(era_mwtemp) & year >2020 ~ avg_temp_2016_2020_preturb + deltat_preturb))
    }
    if (temp_dataset == "CRU"){
      gdp_temp_data1 <- gdp_temp_data1 %>%  dplyr::group_by(ISO3, year) %>% 
        dplyr::mutate(
          cru_mwtemp_preturb= case_when(!is.na(cru_mwtemp) ~ cru_mwtemp_preturb_pre2020, 
                                        is.na(cru_mwtemp) & year >2020 ~ avg_temp_2016_2020_preturb + deltat_preturb))
    }
    #  }
    
    if (temp_dataset == "ERA"){
      gdp_temp_data1$era_mwtemp_fullemms <- gdp_temp_data1$era_mwtemp_preturb + gdp_temp_data1$deltat  
    }
    if (temp_dataset == "CRU"){
      gdp_temp_data1$cru_mwtemp_fullemms <- gdp_temp_data1$cru_mwtemp_preturb + gdp_temp_data1$deltat  
    }
    
    #  gdp_temp_data1$deltat <- gdp_temp_data1$era_mwtemp_ssp370 - gdp_temp_data1$era_mwtemp_noemms
    #  gdp_temp_data1$adjusted_temp <- gdp_temp_data1$era_mwtemp - gdp_temp_data1$deltat
    
    # dev.off()
    #  par(mfrow = c(1,1))
    
    #  hist(gdp_temp_data1$era_mwtemp, add =T)
    #  hist(gdp_temp_data1$adjusted_temp, col = "red", add = T)
    # calculate reposnse from added temperature
    # gdp_temp_data1$response_tempnew <- calc_delta_g(gdp_temp_data1,
    #                                                gdp_temp_data1$era_mwtemp,
    #                                               bhm_era_reg,
    #                                              gdp_temp_data1$deltat,
    #                                             coef(bhm_era_reg)[1],
    #                                            coef(bhm_era_reg)[2])
    
    
    bhm_model <- run_bhm_model_reg("pooled")
    
    gdp_temp_data1$temp <- coef(bhm_model)[1]
    gdp_temp_data1$temp2 <- coef(bhm_model)[2]
    
    gdp_temp_data1$era_mwtemp_fullemms[gdp_temp_data1$era_mwtemp_fullemms > 30 & !is.na(gdp_temp_data1$era_mwtemp_fullemms)] <- 30
    gdp_temp_data1$era_mwtemp_preturb[gdp_temp_data1$era_mwtemp_preturb > 30 & !is.na(gdp_temp_data1$era_mwtemp_preturb)] <- 30
    
    #plot(gdp_temp_data1$year, gdp_temp_data1$era_mwtemp_fullemms)
    #if (experiment == "ssp370" | experiment == "ssp370" | experiment == "ssp370" | experiment == "ssp370"){
    #    gdp_temp_data1$era_mwtemp_fullemms[gdp_temp_data1$era_mwtemp_fullemms > 30] <- 30
    #  }
    
    if (temp_dataset == "ERA"){
      gdp_temp_data1$resp_temp_fullemms <- (gdp_temp_data1$era_mwtemp_fullemms * gdp_temp_data1$temp) + 
        ((gdp_temp_data1$era_mwtemp_fullemms^2) * gdp_temp_data1$temp2)
      
      gdp_temp_data1$resp_temp_preturb <- (gdp_temp_data1$era_mwtemp_preturb * gdp_temp_data1$temp) + 
        ((gdp_temp_data1$era_mwtemp_preturb^2) * gdp_temp_data1$temp2)
      
      gdp_temp_data1$delta_g_era <- gdp_temp_data1$resp_temp_preturb - gdp_temp_data1$resp_temp_fullemms
      
      #  maxtemp_2020 <- max(gdp_temp_data1$era_mwtemp[gdp_temp_data1$year < 2021], na.rm = T)
      # gdp_temp_data1$max_delta_g_era <- gdp_temp_data1$delta_g_era[gdp_temp_data1$year < 2021 & gdp_temp_data1$era_mwtemp == maxtemp_2020 & !is.na(gdp_temp_data1$delta_g_era)]
      #max_g <- gdp_temp_data1$delta_g_era[gdp_temp_data1$year < 2021 & gdp_temp_data1$era_mwtemp == maxtemp_2020 & !is.na(gdp_temp_data1$delta_g_era)]
      #gdp_temp_data1$delta_g_era[gdp_temp_data1$year > 2020 & gdp_temp_data1$delta_g_era < max_g] <- max_g 
      
    }
    
    if (temp_dataset == "CRU"){
      gdp_temp_data1$resp_temp_fullemms <- (gdp_temp_data1$cru_mwtemp_fullemms * gdp_temp_data1$temp) + 
        ((gdp_temp_data1$cru_mwtemp_fullemms^2) * gdp_temp_data1$temp2)
      
      gdp_temp_data1$resp_temp_preturb <- (gdp_temp_data1$cru_mwtemp_preturb * gdp_temp_data1$temp) + 
        ((gdp_temp_data1$cru_mwtemp_preturb^2) * gdp_temp_data1$temp2)
      
      gdp_temp_data1$delta_g_cru <- gdp_temp_data1$resp_temp_preturb - gdp_temp_data1$resp_temp_fullemms
      
    }
    
    #summary(gdp_temp_data1$resp_temp_preturb)
    #summary(gdp_temp_data1$resp_temp_fullemms)
    #hist(gdp_temp_data1$delta_g_era)
    
    #gdp_temp_data1_vra <- subset(gdp_temp_data1, ISO3 == "BRA")
    #plot(gdp_temp_data1_vra$year, gdp_temp_data1_vra$era_mwtemp_fullemms - gdp_temp_data1_vra$era_mwtemp_preturb, type = "l")
    #lines(gdp_temp_data1_vra$year, gdp_temp_data1_vra$era_mwtemp_preturb, type = "l", col = "red")
    
    #gdp_temp_data1$temp,
    #gdp_temp_data1$temp2)
    
    # now let us calculate deltaG
    #gdp_temp_data1$delta_g_era <- gdp_temp_data1$response_tempactual_era - gdp_temp_data1$response_tempnew
    
    # test1 <- gdp_temp_data1 %>% dplyr::select(c("response_tempactual_era", "response_tempnew","delta_g_era", "ISO3", "year"))
    # let us bring in the population data at the country-year level
    gdp_temp_data1 <- left_join(gdp_temp_data1,
                                pop_wdi,
                                by = c("ISO3" = "iso3c",
                                       "year" = "year"))
    
    gdp_temp_data1 <- gdp_temp_data1 %>% 
      dplyr::mutate(SP.POP.TOTL = case_when(is.na(SP.POP.TOTL) ~ pop,
                                            TRUE ~ SP.POP.TOTL))
    
    identifier <- gdp_temp_data1 %>% 
      #  dplyr::group_by(ISO3, year) %>% 
      # dplyr::summarise(diff_lgdp = mean(diff_lgdp, na.rm = T),
      #                 .group = "keep") %>% 
      ungroup(.) %>%  
      dplyr::select(c("ISO3", "diff_lgdp", "year", "NY.GDP.PCAP.KD")) %>% 
      subset(.,year == year_k) %>% 
      dplyr::mutate(ssp_data = case_when(is.na(diff_lgdp) ~ "no",
                                         TRUE ~ "yes")) %>% 
      subset(., ssp_data == "yes") %>% 
      dplyr::select(c("ISO3"))
    
    gdp_temp_data1 <- left_join(identifier, gdp_temp_data1)
    
    # unlist...
    if (temp_dataset == "ERA"){
      gdp_temp_data1$delta_g_era <- unlist(gdp_temp_data1$delta_g_era)
      # now let us calculate adjusted growht rate by adding deltaG to observed growth
      gdp_temp_data1$adj_growth <- (gdp_temp_data1$delta_g_era + gdp_temp_data1$diff_lgdp)
      
    }
    if (temp_dataset == "CRU"){
      gdp_temp_data1$delta_g_cru <- unlist(gdp_temp_data1$delta_g_cru)
      # now let us calculate adjusted growht rate by adding deltaG to observed growth
      gdp_temp_data1$adj_growth <- (gdp_temp_data1$delta_g_cru + gdp_temp_data1$diff_lgdp)
      
    }
    
    
    # let us add 1 to growth variables so we can calculate cumulative growth
    gdp_temp_data1$diff_lgdp <- gdp_temp_data1$diff_lgdp + 1
    gdp_temp_data1$adj_growth <- gdp_temp_data1$adj_growth + 1
    #year_k <- 1990  
    # now let us set GDP at year k as initial gdp number to calculate impact of 
    # added temoerature
    # gdp_temp_data1 <- subset(gdp_temp_data1, !is.na(coef_id))
    #year_k <- 1990
    
    gdp_temp_data1 <- gdp_temp_data1 %>% dplyr::group_by(ISO3) %>% 
      dplyr::mutate(gdp_year = NY.GDP.PCAP.KD[year == year_k])
    
    gdp_temp_data1 <- subset(gdp_temp_data1, year >= year_k)
    
    damages_i_t4 <- gdp_temp_data1 %>% dplyr::group_by(ISO3) %>% 
      dplyr::mutate(cum_adj_growthz = cumprod(adj_growth),
                    cum_growth_real = cumprod(diff_lgdp))
    
    # finally comput edamages...
    #  damages_i_t4 <- damages_i_t4 %>% 
    #    dplyr::mutate(damages = (gdp_year * cum_adj_growthz) - (gdp_year * cum_growth_real))
    
    damages_i_t4 <- damages_i_t4 %>% 
      dplyr::mutate(gdp_noemms = (gdp_year * cum_adj_growthz),
                    gdp_ssp370 = (gdp_year * cum_growth_real),
                    damages = gdp_noemms - gdp_ssp370)
    
    
    #damages_i_t4$gdp <- (damages_i_t4$gdp_year * damages_i_t4$cum_growth_real)
    #experiment_year <- unique(damages_i_t4$exp_yr)
    damages_i_t4 <- damages_i_t4 %>% dplyr::group_by(year) %>% 
      dplyr::mutate(average_gdp = case_when(year <= 2020 ~ mean(NY.GDP.PCAP.KD, na.rm = T),
                                            year > 2020 ~ mean(gdp_ssp370, na.rm = T)))
    
    damages_i_t4$average_gdp_2020 = mean(damages_i_t4$NY.GDP.PCAP.KD[damages_i_t4$year == 2020], na.rm = T)
    

    # calculate discounted damages for past and for future (note different 
    #- processes for discounting past damages and future damages)
    damages_i_t4 <- damages_i_t4 %>% 
      dplyr::mutate(t_since_k = year - year_k,
                    t_since_today = year - 2020,
                    weighted_damages1 = case_when(year <= 2020 ~ (damages*((1+(0.01))^t_since_k)),
                                                  year > 2020 ~ (damages*(1/(1+(0.01))^t_since_today))),
                    weighted_damages2 = case_when(year <= 2020 ~ (damages*((1+(0.02))^t_since_k)),
                                                  year > 2020 ~ (damages*(1/(1+(0.02))^t_since_today))),
                    weighted_damages3 = case_when(year <= 2020 ~ (damages*((1+(0.03))^t_since_k)),
                                                  year > 2020 ~ (damages*(1/(1+(0.03))^t_since_today))),
                    weighted_damages5 = case_when(year <= 2020 ~ (damages*((1+(0.05))^t_since_k)),
                                                  year > 2020 ~ (damages*(1/(1+(0.05))^t_since_today))),
                    weighted_damages7 = case_when(year <= 2020 ~ (damages*((1+(0.07))^t_since_k)),
                                                  year > 2020 ~ (damages*(1/(1+(0.07))^t_since_today))),
                    weighted_damages_ramsey = case_when(year <= 2020 ~ (damages*((1+(0.02))^t_since_k)),
                                                        year > 2020 ~ damages*((1/(1.002^(t_since_today)))*((average_gdp/average_gdp_2020)^(-1.24)))),
                    ramsey_discount = ((1/(1.002^(t_since_today)))*((average_gdp/average_gdp_2020)^(-1.24))))
    #  damages_i_t4 <- damages_i_t4 %>% 
    #  dplyr::mutate(t_since_k = year - year_k,
    #        t_since_today = year - experiment_year,
    #         weighted_damages2 = case_when(year <= 2020 ~ (damages*((1+(0.02))^t_since_k)),
    #                                        year > 2020 ~ (damages*(1/(1+(0.02))^t_since_today))),
    #           weighted_damages3 = case_when(year <= 2020 ~ (damages*((1+(0.03))^t_since_k)),
    #                                          year > 2020 ~ (damages*(1/(1+(0.03))^t_since_today))),
    #             weighted_damages5 = case_when(year <= 2020 ~ (damages*((1+(0.05))^t_since_k)),
    #                                            year > 2020 ~ (damages*(1/(1+(0.05))^t_since_today))),
    #               weighted_damages7 = case_when(year <= 2020 ~ (damages*((1+(0.07))^t_since_k)),
    #                                              year > 2020 ~ (damages*(1/(1+(0.07))^t_since_today))))
    
    #  damages_i_t4 <- subset(damages_i_t4, !is.na(coef_id))
    
    # damages_i_t4 <- subset(damages_i_t4, ISO3 != i)
    
    # scale by population
    damages_i_t4$damages_pop <- damages_i_t4$damages * damages_i_t4$SP.POP.TOTL
    damages_i_t4 <- damages_i_t4 %>% 
      dplyr::mutate(weighted_damages1_scld = weighted_damages1 *SP.POP.TOTL,
                    weighted_damages2_scld = weighted_damages2 *SP.POP.TOTL,
                    weighted_damages3_scld = weighted_damages3 *SP.POP.TOTL,
                    weighted_damages5_scld = weighted_damages5 *SP.POP.TOTL,
                    weighted_damages7_scld = weighted_damages7 *SP.POP.TOTL,
                    weighted_damages_ramsey_scld = weighted_damages_ramsey *SP.POP.TOTL, 
                    gdp_noemms_scld = gdp_noemms *SP.POP.TOTL,
                    gdp_ssp370_scld = gdp_ssp370 *SP.POP.TOTL)
    
    damages_i_t4$emitter <- i
    
    #  sum(damages_i_t4$weighted_damages2_scld[damages_i_t4$year <= 2100 & damages_i_t4$weighted_damages2_scld > 0 ], na.rm = T)
    
    #    damages_summ <- damages_i_t4 %>% dplyr::group_by(emitter, year) %>% 
    #     dplyr::summarise(total_damages = sum(damages_pop, na.rm = T),
    #                     total_damages2 = sum(weighted_damages2_scld, na.rm = T),
    #                    total_damages5 = sum(weighted_damages5_scld, na.rm = T),
    #                   total_damages7 = sum(weighted_damages7_scld, na.rm = T),
    #                  .groups = "keep")
    
    # add to data frame
    mother_df <- rbind(mother_df, damages_i_t4)
    
    toc()
  }
  return(mother_df)
}


sum(damages_i_t4$weighted_damages2_scld, na.rm = T)
sum(damages_i_t4$weighted_damages3_scld, na.rm = T)



### now let us do the 5 lag model and generate the total damages


# done clamping. now let us bring in the calculation function so we can 
# calculate the scc with the clamping

ratio_raster <- master_raster
experiment_df <- fair_exps_1tco2_2300
year_k <- 1990
future_forecast <- future_forecast_ssp370_2300
gdp_temp_dataset <- gdp_temp_data
temp_dataset <- "ERA"

calculate_bidamages <- function(ratio_raster, experiment_df, list_of_exps, 
                                year_k, future_forecast, gdp_temp_dataset, 
                                temp_dataset){
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
  
  #  fair_exps <- subset(fair_exps, !is.na(experiment_iso))
  # now bring them together
  deltat_calced_df <- left_join(deltat_df,
                                fair_exps,
                                by = c("merge_id"))
  
  # now multuply the grid level warming ratio by median temp response from FaIR
  #deltat_calced_df$deltat <- deltat_calced_df$layer * deltat_calced_df$median_deltat
  deltat_calced_df$deltat_scld <- deltat_calced_df$layer * deltat_calced_df$median_deltat   
  deltat_calced_df$deltat_fullemms_scld <- deltat_calced_df$layer * deltat_calced_df$deltat_fullemms
  deltat_calced_df$deltat_preturb_scld <- deltat_calced_df$layer * deltat_calced_df$deltaT_preturb
  
  # now let us take the columns we need from the fair experiment dataframe 
  deltaT_df <- deltat_calced_df %>% dplyr::select(c("x",
                                                    "y",
                                                    "deltat_fullemms_scld",
                                                    "deltat_preturb_scld",
                                                    "deltat_scld",
                                                    "year",
                                                    "experiment_iso"))
  #  "exp_yr"))
  
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
  
  # start an empty dataframe 
  mother_df <- data.frame()
  i <- 2020
  i <- "USA"
  i <- "all0"
  # now loop over the experiments and calculate total damages owed by each of 
  # the countries
  for (i in list_of_exps){
    tic()
    
    # subset by keeping one experiment for each loop
    deltaT_df1 <- subset(deltaT_df, experiment_iso == i)
    
    #  deltaT_df1 <- deltaT_df
    # merge with coordinates and country ids
    joined_final_df <- left_join(deltaT_df1,
                                 base_coords,
                                 by = c("x", "y"))
    
    # keep non missing ISOs exoeriments
    joined_final_df <- subset(joined_final_df, !is.na(ISO3))
    # keep to a certain year
    #joined_final_df <- subset(joined_final_df, year < 2021)
    
    # now let us calculate the average deltaT by country-year
    #    joined_final_df <- joined_final_df %>% dplyr::group_by(ISO3, year) %>% 
    #     dplyr::summarise(deltat = mean(deltat, na.rm = T),
    #                     .groups = "keep")
    
    joined_final_df <- joined_final_df %>% dplyr::group_by(ISO3, year) %>% 
      dplyr::summarise(deltat_fullemms = mean(deltat_fullemms_scld, na.rm = T),
                       deltat_preturb = mean(deltat_preturb_scld, na.rm = T),
                       deltat = mean(deltat_scld, na.rm = T),
                       .groups = "keep")
    
    joined_final_df$deltat_preturb[joined_final_df$year < 2021] <- 0
    joined_final_df$deltat_fullemms[joined_final_df$year < 2021] <- 0
    
    # now let us get the country-year panel
    gdp_temp_data1 <- gdp_temp_dataset
    gdp_temp_data1$year <- as.numeric(as.character(gdp_temp_data1$year))
    
    #gdp_temp_data1 <- subset(gdp_temp_data1, year >= 1980)
    # join country-year temp change with the country-year panel
    
    gdp_temp_data1 <- left_join(joined_final_df,
                                gdp_temp_data1,
                                by = c("ISO3", "year"))
    
    # we need to create country level annual average for last 5 years of observed 
    # data 
    
    if (temp_dataset == "ERA"){
      gdp_temp_data1$era_mwtemp[gdp_temp_data1$year >2020] <- NA 
      #  if (i < 2020){
      gdp_temp_data1$era_mwtemp_preturb_pre2020 <- gdp_temp_data1$era_mwtemp - gdp_temp_data1$deltat
      gdp_temp_data1 <- gdp_temp_data1 %>% dplyr::group_by(ISO3) %>% 
        dplyr::mutate(avg_temp_2016_2020_preturb = mean(era_mwtemp_preturb_pre2020[year > 2015 & year <= 2020], na.rm = T))
    }
    if (temp_dataset == "CRU"){
      gdp_temp_data1$cru_mwtemp[gdp_temp_data1$year >2020] <- NA 
      #  if (i < 2020){
      gdp_temp_data1$cru_mwtemp_preturb_pre2020 <- gdp_temp_data1$cru_mwtemp - gdp_temp_data1$deltat
      gdp_temp_data1 <- gdp_temp_data1 %>% dplyr::group_by(ISO3) %>% 
        dplyr::mutate(avg_temp_2016_2020_preturb = mean(cru_mwtemp_preturb_pre2020[year > 2015 & year <= 2020], na.rm = T))
    }
    
    
    # plot(gdp_temp_data1$year[gdp_temp_data1$ISO3 == "USA"], gdp_temp_data1$deltat[gdp_temp_data1$ISO3== "SAU"], type = "l")
    #  plot(fair_exps_1GtCO2_updated$year[fair_exps_1GtCO2_updated$experiment_iso == 2020], fair_exps_1GtCO2_updated$median_deltat[fair_exps_1GtCO2_updated$experiment_iso == 2020])
    
    
    #  }
    
    #  gdp_temp_data1 <- gdp_temp_data1 %>% dplyr::group_by(ISO3) %>% 
    #   dplyr::mutate(avg_temp_2016_2020 = mean(era_mwtemp[year > 2015 & year <= 2020], na.rm = T))
    
    #  if (i >= 2020){
    #  gdp_temp_data1 <- gdp_temp_data1 %>%  dplyr::group_by(ISO3, year) %>% 
    #  dplyr::mutate(era_mwtemp_fullemms = case_when(!is.na(era_mwtemp) ~ era_mwtemp, 
    #                                            is.na(era_mwtemp) & year >2020 ~ avg_temp_2016_2020 + deltat_fullemms),
    #               era_mwtemp_preturb= case_when(!is.na(era_mwtemp) ~ era_mwtemp, 
    #                                             is.na(era_mwtemp) & year >2020 ~ avg_temp_2016_2020 + deltat_preturb))
    # }
    
    #  if (i >= 2020){
    #    gdp_temp_data1 <- gdp_temp_data1 %>%  dplyr::group_by(ISO3, year) %>% 
    #      dplyr::mutate(era_mwtemp_preturb= case_when(!is.na(era_mwtemp) ~ era_mwtemp, 
    #                                                  is.na(era_mwtemp) & year >2020 ~ avg_temp_2016_2020_preturb + deltat_preturb))
    #  }
    
    #  gdp_temp_data1$era_mwtemp_fullemms <- gdp_temp_data1$era_mwtemp_preturb + gdp_temp_data1$deltat 
    
    #  if (i < 2020) {
    #  gdp_temp_data1 <- gdp_temp_data1 %>%  dplyr::group_by(ISO3, year) %>% 
    #  dplyr::mutate(era_mwtemp_fullemms = case_when(!is.na(era_mwtemp) ~ era_mwtemp, 
    #                                                 is.na(era_mwtemp) & year >2020 ~ avg_temp_2016_2020 + deltat_fullemms),
    #                  era_mwtemp_preturb= case_when(!is.na(era_mwtemp) ~ era_mwtemp_preturb_pre2020, 
    #                                                 is.na(era_mwtemp) & year >2020 ~ avg_temp_2016_2020_preturb + deltat_preturb))
    #  }
    #  gdp_temp_data1$deltat <- gdp_temp_data1$era_mwtemp_ssp370 - gdp_temp_data1$era_mwtemp_noemms
    #  if (i < 2020) {
    
    if (temp_dataset == "ERA") {
      gdp_temp_data1 <- gdp_temp_data1 %>%  dplyr::group_by(ISO3, year) %>% 
        dplyr::mutate(
          era_mwtemp_preturb= case_when(!is.na(era_mwtemp) ~ era_mwtemp_preturb_pre2020, 
                                        is.na(era_mwtemp) & year >2020 ~ avg_temp_2016_2020_preturb + deltat_preturb))
    }
    if (temp_dataset == "CRU"){
      gdp_temp_data1 <- gdp_temp_data1 %>%  dplyr::group_by(ISO3, year) %>% 
        dplyr::mutate(
          cru_mwtemp_preturb= case_when(!is.na(cru_mwtemp) ~ cru_mwtemp_preturb_pre2020, 
                                        is.na(cru_mwtemp) & year >2020 ~ avg_temp_2016_2020_preturb + deltat_preturb))
    }
    #  }
    
    if (temp_dataset == "ERA"){
      gdp_temp_data1$era_mwtemp_fullemms <- gdp_temp_data1$era_mwtemp_preturb + gdp_temp_data1$deltat  
    }
    if (temp_dataset == "CRU"){
      gdp_temp_data1$cru_mwtemp_fullemms <- gdp_temp_data1$cru_mwtemp_preturb + gdp_temp_data1$deltat  
    }
    
    #  gdp_temp_data1$deltat <- gdp_temp_data1$era_mwtemp_ssp370 - gdp_temp_data1$era_mwtemp_noemms
    #  gdp_temp_data1$adjusted_temp <- gdp_temp_data1$era_mwtemp - gdp_temp_data1$deltat
    
    # dev.off()
    #  par(mfrow = c(1,1))
    
    #  hist(gdp_temp_data1$era_mwtemp, add =T)
    #  hist(gdp_temp_data1$adjusted_temp, col = "red", add = T)
    # calculate reposnse from added temperature
    # gdp_temp_data1$response_tempnew <- calc_delta_g(gdp_temp_data1,
    #                                                gdp_temp_data1$era_mwtemp,
    #                                               bhm_era_reg,
    #                                              gdp_temp_data1$deltat,
    #                                             coef(bhm_era_reg)[1],
    #                                            coef(bhm_era_reg)[2])
    
    
    bhm_model <- run_bhm_model_reg_lag5("pooled")
    
    gdp_temp_data1$temp <- coef(bhm_model)[1]
    gdp_temp_data1$temp2 <- coef(bhm_model)[2]
    gdp_temp_data1$temp_l1 <- coef(bhm_model)[3]
    gdp_temp_data1$temp2_l1 <- coef(bhm_model)[4]
    gdp_temp_data1$temp_l2 <- coef(bhm_model)[5]
    gdp_temp_data1$temp2_l2 <- coef(bhm_model)[6]
    gdp_temp_data1$temp_l3 <- coef(bhm_model)[7]
    gdp_temp_data1$temp2_l3 <- coef(bhm_model)[8]
    gdp_temp_data1$temp_l4 <- coef(bhm_model)[9]
    gdp_temp_data1$temp2_l4 <- coef(bhm_model)[10]
    gdp_temp_data1$temp_l5 <- coef(bhm_model)[11]
    gdp_temp_data1$temp2_l5 <- coef(bhm_model)[12]
    
    gdp_temp_data1$era_mwtemp_fullemms[gdp_temp_data1$era_mwtemp_fullemms > 30 & !is.na(gdp_temp_data1$era_mwtemp_fullemms)] <- 30
    gdp_temp_data1$era_mwtemp_preturb[gdp_temp_data1$era_mwtemp_preturb > 30 & !is.na(gdp_temp_data1$era_mwtemp_preturb)] <- 30
    
    #plot(gdp_temp_data1$year, gdp_temp_data1$era_mwtemp_fullemms)
    #if (experiment == "ssp370" | experiment == "ssp370" | experiment == "ssp370" | experiment == "ssp370"){
    #    gdp_temp_data1$era_mwtemp_fullemms[gdp_temp_data1$era_mwtemp_fullemms > 30] <- 30
    #  }
    
    if (temp_dataset == "ERA"){
      gdp_temp_data1$resp_temp_fullemms <- (gdp_temp_data1$era_mwtemp_fullemms * (gdp_temp_data1$temp + gdp_temp_data1$temp_l1 +
                                                                                    gdp_temp_data1$temp_l2 + gdp_temp_data1$temp_l3 +
                                                                                    gdp_temp_data1$temp_l4 + gdp_temp_data1$temp_l5)) + 
        ((gdp_temp_data1$era_mwtemp_fullemms^2) * (gdp_temp_data1$temp2 + gdp_temp_data1$temp2_l1 + gdp_temp_data1$temp2_l2 +
                                                     gdp_temp_data1$temp2_l3 + gdp_temp_data1$temp2_l4 + gdp_temp_data1$temp2_l5))
      
      gdp_temp_data1$resp_temp_preturb <- (gdp_temp_data1$era_mwtemp_preturb * (gdp_temp_data1$temp + gdp_temp_data1$temp_l1 +
                                                                                  gdp_temp_data1$temp_l2 + gdp_temp_data1$temp_l3 +
                                                                                  gdp_temp_data1$temp_l4 + gdp_temp_data1$temp_l5)) + 
        ((gdp_temp_data1$era_mwtemp_preturb^2) * (gdp_temp_data1$temp2 + gdp_temp_data1$temp2_l1 + gdp_temp_data1$temp2_l2 +
                                                    gdp_temp_data1$temp2_l3 + gdp_temp_data1$temp2_l4 + gdp_temp_data1$temp2_l5))
      
      gdp_temp_data1$delta_g_era <- gdp_temp_data1$resp_temp_preturb - gdp_temp_data1$resp_temp_fullemms
      
      #  maxtemp_2020 <- max(gdp_temp_data1$era_mwtemp[gdp_temp_data1$year < 2021], na.rm = T)
      # gdp_temp_data1$max_delta_g_era <- gdp_temp_data1$delta_g_era[gdp_temp_data1$year < 2021 & gdp_temp_data1$era_mwtemp == maxtemp_2020 & !is.na(gdp_temp_data1$delta_g_era)]
      #max_g <- gdp_temp_data1$delta_g_era[gdp_temp_data1$year < 2021 & gdp_temp_data1$era_mwtemp == maxtemp_2020 & !is.na(gdp_temp_data1$delta_g_era)]
      #gdp_temp_data1$delta_g_era[gdp_temp_data1$year > 2020 & gdp_temp_data1$delta_g_era < max_g] <- max_g 
      
    }
    
    if (temp_dataset == "CRU"){
      gdp_temp_data1$resp_temp_fullemms <- (gdp_temp_data1$cru_mwtemp_fullemms * gdp_temp_data1$temp) + 
        ((gdp_temp_data1$cru_mwtemp_fullemms^2) * gdp_temp_data1$temp2)
      
      gdp_temp_data1$resp_temp_preturb <- (gdp_temp_data1$cru_mwtemp_preturb * gdp_temp_data1$temp) + 
        ((gdp_temp_data1$cru_mwtemp_preturb^2) * gdp_temp_data1$temp2)
      
      gdp_temp_data1$delta_g_cru <- gdp_temp_data1$resp_temp_preturb - gdp_temp_data1$resp_temp_fullemms
      
    }
    
    #summary(gdp_temp_data1$resp_temp_preturb)
    #summary(gdp_temp_data1$resp_temp_fullemms)
    #hist(gdp_temp_data1$delta_g_era)
    
    #gdp_temp_data1_vra <- subset(gdp_temp_data1, ISO3 == "BRA")
    #plot(gdp_temp_data1_vra$year, gdp_temp_data1_vra$era_mwtemp_fullemms - gdp_temp_data1_vra$era_mwtemp_preturb, type = "l")
    #lines(gdp_temp_data1_vra$year, gdp_temp_data1_vra$era_mwtemp_preturb, type = "l", col = "red")
    
    #gdp_temp_data1$temp,
    #gdp_temp_data1$temp2)
    
    # now let us calculate deltaG
    #gdp_temp_data1$delta_g_era <- gdp_temp_data1$response_tempactual_era - gdp_temp_data1$response_tempnew
    
    # test1 <- gdp_temp_data1 %>% dplyr::select(c("response_tempactual_era", "response_tempnew","delta_g_era", "ISO3", "year"))
    # let us bring in the population data at the country-year level
    gdp_temp_data1 <- left_join(gdp_temp_data1,
                                pop_wdi,
                                by = c("ISO3" = "iso3c",
                                       "year" = "year"))
    
    gdp_temp_data1 <- gdp_temp_data1 %>% 
      dplyr::mutate(SP.POP.TOTL = case_when(is.na(SP.POP.TOTL) ~ pop,
                                            TRUE ~ SP.POP.TOTL))
    
    identifier <- gdp_temp_data1 %>% 
      #  dplyr::group_by(ISO3, year) %>% 
      # dplyr::summarise(diff_lgdp = mean(diff_lgdp, na.rm = T),
      #                 .group = "keep") %>% 
      ungroup(.) %>%  
      dplyr::select(c("ISO3", "diff_lgdp", "year", "NY.GDP.PCAP.KD")) %>% 
      subset(.,year == year_k) %>% 
      dplyr::mutate(ssp_data = case_when(is.na(diff_lgdp) ~ "no",
                                         TRUE ~ "yes")) %>% 
      subset(., ssp_data == "yes") %>% 
      dplyr::select(c("ISO3"))
    
    gdp_temp_data1 <- left_join(identifier, gdp_temp_data1)
    
    # unlist...
    if (temp_dataset == "ERA"){
      gdp_temp_data1$delta_g_era <- unlist(gdp_temp_data1$delta_g_era)
      # now let us calculate adjusted growht rate by adding deltaG to observed growth
      gdp_temp_data1$adj_growth <- (gdp_temp_data1$delta_g_era + gdp_temp_data1$diff_lgdp)
      
    }
    if (temp_dataset == "CRU"){
      gdp_temp_data1$delta_g_cru <- unlist(gdp_temp_data1$delta_g_cru)
      # now let us calculate adjusted growht rate by adding deltaG to observed growth
      gdp_temp_data1$adj_growth <- (gdp_temp_data1$delta_g_cru + gdp_temp_data1$diff_lgdp)
      
    }
    
    
    # let us add 1 to growth variables so we can calculate cumulative growth
    gdp_temp_data1$diff_lgdp <- gdp_temp_data1$diff_lgdp + 1
    gdp_temp_data1$adj_growth <- gdp_temp_data1$adj_growth + 1
    #year_k <- 1990  
    # now let us set GDP at year k as initial gdp number to calculate impact of 
    # added temoerature
    # gdp_temp_data1 <- subset(gdp_temp_data1, !is.na(coef_id))
    #year_k <- 1990
    
    gdp_temp_data1 <- gdp_temp_data1 %>% dplyr::group_by(ISO3) %>% 
      dplyr::mutate(gdp_year = NY.GDP.PCAP.KD[year == year_k])
    
    gdp_temp_data1 <- subset(gdp_temp_data1, year >= year_k)
    
    damages_i_t4 <- gdp_temp_data1 %>% dplyr::group_by(ISO3) %>% 
      dplyr::mutate(cum_adj_growthz = cumprod(adj_growth),
                    cum_growth_real = cumprod(diff_lgdp))
    
    # finally comput edamages...
    #  damages_i_t4 <- damages_i_t4 %>% 
    #    dplyr::mutate(damages = (gdp_year * cum_adj_growthz) - (gdp_year * cum_growth_real))
    
    damages_i_t4 <- damages_i_t4 %>% 
      dplyr::mutate(gdp_noemms = (gdp_year * cum_adj_growthz),
                    gdp_ssp370 = (gdp_year * cum_growth_real),
                    damages = gdp_noemms - gdp_ssp370)
    
    
    #damages_i_t4$gdp <- (damages_i_t4$gdp_year * damages_i_t4$cum_growth_real)
    #experiment_year <- unique(damages_i_t4$exp_yr)
    damages_i_t4 <- damages_i_t4 %>% dplyr::group_by(year) %>% 
      dplyr::mutate(average_gdp = case_when(year <= 2020 ~ mean(NY.GDP.PCAP.KD, na.rm = T),
                                            year > 2020 ~ mean(gdp_ssp370, na.rm = T)))
    
    damages_i_t4$average_gdp_2020 = mean(damages_i_t4$NY.GDP.PCAP.KD[damages_i_t4$year == 2020], na.rm = T)
    
    # calculate discounted damages for past and for future (note different 
    #- processes for discounting past damages and future damages)
    damages_i_t4 <- damages_i_t4 %>% 
      dplyr::mutate(t_since_k = year - year_k,
                    t_since_today = year - 2020,
                    weighted_damages1 = case_when(year <= 2020 ~ (damages*((1+(0.01))^t_since_k)),
                                                  year > 2020 ~ (damages*(1/(1+(0.01))^t_since_today))),
                    weighted_damages2 = case_when(year <= 2020 ~ (damages*((1+(0.02))^t_since_k)),
                                                  year > 2020 ~ (damages*(1/(1+(0.02))^t_since_today))),
                    weighted_damages3 = case_when(year <= 2020 ~ (damages*((1+(0.03))^t_since_k)),
                                                  year > 2020 ~ (damages*(1/(1+(0.03))^t_since_today))),
                    weighted_damages5 = case_when(year <= 2020 ~ (damages*((1+(0.05))^t_since_k)),
                                                  year > 2020 ~ (damages*(1/(1+(0.05))^t_since_today))),
                    weighted_damages7 = case_when(year <= 2020 ~ (damages*((1+(0.07))^t_since_k)),
                                                  year > 2020 ~ (damages*(1/(1+(0.07))^t_since_today))),
                    weighted_damages_ramsey = case_when(year <= 2020 ~ (damages*((1+(0.02))^t_since_k)),
                                                        year > 2020 ~ damages*((1/(1.002^(t_since_today)))*((average_gdp/average_gdp_2020)^(-1.24)))),
                    ramsey_discount = ((1/(1.002^(t_since_today)))*((average_gdp/average_gdp_2020)^(-1.24))))
  
    
    #  damages_i_t4 <- damages_i_t4 %>% 
    #  dplyr::mutate(t_since_k = year - year_k,
    #        t_since_today = year - experiment_year,
    #         weighted_damages2 = case_when(year <= 2020 ~ (damages*((1+(0.02))^t_since_k)),
    #                                        year > 2020 ~ (damages*(1/(1+(0.02))^t_since_today))),
    #           weighted_damages3 = case_when(year <= 2020 ~ (damages*((1+(0.03))^t_since_k)),
    #                                          year > 2020 ~ (damages*(1/(1+(0.03))^t_since_today))),
    #             weighted_damages5 = case_when(year <= 2020 ~ (damages*((1+(0.05))^t_since_k)),
    #                                            year > 2020 ~ (damages*(1/(1+(0.05))^t_since_today))),
    #               weighted_damages7 = case_when(year <= 2020 ~ (damages*((1+(0.07))^t_since_k)),
    #                                              year > 2020 ~ (damages*(1/(1+(0.07))^t_since_today))))
    
    #  damages_i_t4 <- subset(damages_i_t4, !is.na(coef_id))
    
    # damages_i_t4 <- subset(damages_i_t4, ISO3 != i)
    
    # scale by population
    damages_i_t4$damages_pop <- damages_i_t4$damages * damages_i_t4$SP.POP.TOTL
    damages_i_t4 <- damages_i_t4 %>% 
      dplyr::mutate(weighted_damages1_scld = weighted_damages1 *SP.POP.TOTL,
                    weighted_damages2_scld = weighted_damages2 *SP.POP.TOTL,
                    weighted_damages3_scld = weighted_damages3 *SP.POP.TOTL,
                    weighted_damages5_scld = weighted_damages5 *SP.POP.TOTL,
                    weighted_damages7_scld = weighted_damages7 *SP.POP.TOTL,
                    weighted_damages_ramsey_scld = weighted_damages_ramsey *SP.POP.TOTL,
                    gdp_noemms_scld = gdp_noemms *SP.POP.TOTL,
                    gdp_ssp370_scld = gdp_ssp370 *SP.POP.TOTL)
    
    damages_i_t4$emitter <- i
    
    #  sum(damages_i_t4$weighted_damages2_scld[damages_i_t4$year <= 2100 & damages_i_t4$weighted_damages2_scld > 0 ], na.rm = T)
    
    #    damages_summ <- damages_i_t4 %>% dplyr::group_by(emitter, year) %>% 
    #     dplyr::summarise(total_damages = sum(damages_pop, na.rm = T),
    #                     total_damages2 = sum(weighted_damages2_scld, na.rm = T),
    #                    total_damages5 = sum(weighted_damages5_scld, na.rm = T),
    #                   total_damages7 = sum(weighted_damages7_scld, na.rm = T),
    #                  .groups = "keep")
    
    # add to data frame
    mother_df <- rbind(mother_df, damages_i_t4)
    
    toc()
  }
  return(mother_df)
}

# end of script


sum(damages_i_t4$weighted_damages2_scld, na.rm = T)



dataframe_scc <- data.frame(scenario = c("Growth at 2100", "1% Growth", "2& Growth", "Growth at 2100 + clamping",
                                         "Growth at 2100 + 5lag BHM"),
                            dr1 = c(NA, NA, NA, NA, NA),
                            dr2 = c(NA, NA, NA, NA, NA),
                            dr3 = c(NA, NA, NA, NA, NA),
                            dr_ramsey = c(NA, NA, NA, NA, NA))

# Ok, so now we have all the scenarios, let us run each 

future_ssp <- list(future_forecast_ssp370_2300, 
                   future_forecast_ssp370_2300_1pct,
                   future_forecast_ssp370_2300_2pct)

scenario_list <- list("Growth at 2100",
                      "1% Growth",
                      "2& Growth")

#for (j in 1:length(future_ssp)){
#  for (i in unique(dataframe_scc$scenario)) {
j <- 1
for (j in 1:length(future_ssp)){
    tic()
    
    ssp <- future_ssp[[j]]
    #ssp <- future_forecast_ssp370_2300_adj
    #ssp <- future_forecast_ssp370_2300
    #gdptemp_dta <- generate_gdptemp_panel_5lags("pooled", ssp, 1990, "ERA")
    gdptemp_dta <- generate_gdptemp_panel("pooled", ssp, 1990, "ERA")
    
  #future_ssp <- future_forecast_ssp370_2300
    
  damages_df <- calculate_bidamages(master_raster,
                                    fair_exps_1tco2_2300,
                                    2020,
                                    1990,
                                    ssp,
                                    gdptemp_dta,
                                    "ERA")
  assign(paste0("damages_", j), damages_df)
  #damages_df$weighted_damages_ramsey_scld <- damages_df$weighted_damages_ramsey * damages_df$SP.POP.TOTL
  #i <- "Growth at 2100"
  #i <- "1% Growth"
  #i <- "2& Growth"
  #i <- "Growth at 2100 + 5lag BHM"
  #i <- "Growth at 2100 + clamping"
  i <- scenario_list[[j]]
  
  #dataframe_scc$dr1[dataframe_scc$scenario == i] <- sum(damages_df$weighted_damages1_scld, na.rm = T)
  #dataframe_scc$dr2[dataframe_scc$scenario == i] <- sum(damages_df$weighted_damages2_scld, na.rm = T)
  #dataframe_scc$dr3[dataframe_scc$scenario == i] <- sum(damages_df$weighted_damages3_scld, na.rm = T)
  dataframe_scc$dr_ramsey[dataframe_scc$scenario == i] <- sum(damages_df$weighted_damages_ramsey_scld, na.rm = T)
  toc()
}

damages_1a <- subset(damages_1, ISO3 == 'USA')
damages_2 <- subset(damages_2, ISO3 == 'USA')
damages_3 <- subset(damages_3, ISO3 == 'USA')


plot(damages_1a$year[damages_1a$year >2020], damages_1a$ramsey_discount[damages_1a$year >2020], type = "l", col = "black", ylab = "SDF", 
     xlab = "Year") #, lwd = 5)
lines(damages_2$year[damages_2$year >2020], damages_2$ramsey_discount[damages_2$year >2020], col = "red")
lines(damages_3$year[damages_3$year >2020], damages_3$ramsey_discount[damages_3$year >2020], col = "blue")

segments(x0 = 2200, x1= 2225, y0 = 0.8, y1 = 0.8, col = "black")
segments(x0 = 2200, x1= 2225, y0 = 0.7, y1 = 0.7, col = "red")
segments(x0 = 2200, x1= 2225, y0 = 0.6, y1 = 0.6, col = "blue")

text(2230, 0.8, "Growth at 2100 rate", adj = 0)
text(2230, 0.7, "1% Growth", adj = 0)
text(2230, 0.6, "2% Growth", adj = 0)

plot(damages_1a$year[damages_1a$year >2020], damages_1a$average_gdp[damages_1a$year >2020], type = "l", col = "black", ylab = "SDF", 
     xlab = "Year") #, lwd = 5)
lines(damages_2$year[damages_2$year >2020], damages_2$average_gdp[damages_2$year >2020], col = "red")
lines(damages_3$year[damages_3$year >2020], damages_3$average_gdp[damages_3$year >2020], col = "blue")

hist(damages_1$diff_lgdp[damages_1$year == 2101])
median(damages_1$diff_lgdp[damages_1$year == 2101], na.rm = T)


plot(damages_1a$ramsey_discount, damages_3$ramsey_discount)

damages_df$ramsey <- damages_df$damages / damages_df$weighted_damages_ramsey_scld

plot(damages_df$year, damages_df$ramsey)
