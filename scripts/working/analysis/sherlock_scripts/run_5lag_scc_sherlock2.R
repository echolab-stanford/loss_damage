##############################################################################
# Mustafa Zahid, March 1st, 2024
# This is a script to be run through sherlock. This script calculates the 
# damages per 1tco2 of emissions in a given year under a sample of the 
# various paramters (FaIR run, BHM coef, CGM model used for pattern scaling)
#############################################################################

############################################################################# install and read needed libraries 
#install.packages('readr', repos='http://cran.us.r-project.org')
#install.packages('doParallel', repos='http://cran.us.r-project.org')
#install.packages('foreach', repos='http://cran.us.r-project.org')
#install.packages('exactextractr', repos='http://cran.us.r-project.org')
#install.packages('sf', repos='http://cran.us.r-project.org')
#install.packages('raster', repos='http://cran.us.r-project.org')

library("dplyr")
library("readr")
library("doParallel")
library("foreach")
library("exactextractr")
library("sf")
library("raster")

############################################################################# set path and read data 
path <- "/scratch/users/mhzahid/sherlock_files_060223/"
# bhm model 
load(paste0(path, "bhm_era_reg.RData"))
load(paste0(path, "bhm_era_reg_5lag.Rdata"))
# cgm rasters 
load(paste0(path, "list_r_rasters_20230822.RData"))
# fair dataframes 
fair_exps_1tco2_disagg <- readRDS(paste0(path, "fair_exps_1tco2_2100_k90_20230523.rds"))
fair_exps_1tco2_disagg <- subset(fair_exps_1tco2_disagg, year <2101)
# world shapefiles and population information 
pop_wdi <- readRDS(paste0(path, "pop_wdi.rds"))  
world <- read_sf(paste0(path, "world_sf.shp"))  
pop <- raster(paste0(path, "pop_raster.tiff"))
# gdp - temp panels
gdp_temp_data_k90 <- readRDS(paste0(path, "gdp_temp_data_k90_norm.rds"))
gdp_temp_data_k90_2300 <- readRDS(paste0(path, "gdp_temp_data_k90_2300.rds"))
gdp_temp_data_k90_5lag <- readRDS(paste0(path, "gdp_temp_data_5lags_2100_20240311.rds"))
#gdp_temp_data_k90_5lag <- subset(gdp_temp_data_k90_5lag, year < 2101)
# future gdp projections per SSP 370 
future_forecast_ssp370_2300 <- readRDS(paste0(path, "future_forecast_ssp370_2300.rds"))
future_forecast_ssp370 <- readRDS(paste0(path, "future_forecast_ssp370.rds"))
# now we need to reed the bootstrapped coeffecients from bhm
pooledbs <- as.data.frame(readRDS(paste0(path, "pooledregression_boostraps_era.rds")))
laggedbs <- as.data.frame(readRDS(paste0(path, "laggedregression_bootstraps_era_20240311a.rds")))
pooledbs$coef_id <- 1:nrow(pooledbs)
laggedbs$coef_id <- 1:nrow(laggedbs)
# read the years we want to loop over 
list_of_exps_1990_2020 <- 1990:2020

############################################################################# read the function to be used for calculating teh damages
calculate_damages_pulse <- function(ratio_raster, experiment_df, list_of_exps, 
                                    year_k, future_forecast, gdp_temp_dataset, 
                                    temp_dataset, bhm_model, bootstrapped, 
                                    clamping, growth_past_2100, settlement_year,
                                    coefdf){
  
  #read raster data for warming ratio 
  deltat_df <- exactextractr::exact_extract(ratio_raster, 
                                            st_as_sf(world),
                                            fun = "weighted_mean",
                                            weights = pop,
                                            default_weight = 0,
                                            append_cols = c("ISO3"))
  
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
                                by = c("merge_id"), 
                                multiple = "all", 
                                relationship = "many-to-many")
  
  
  # now multuply the grid level warming ratio by median temp response from FaIR
  deltat_calced_df$deltat_scld <- deltat_calced_df$weighted_mean * deltat_calced_df$median_deltat   
  deltat_calced_df$deltat_fullemms_scld <- deltat_calced_df$weighted_mean * deltat_calced_df$deltat_fullemms
  deltat_calced_df$deltat_preturb_scld <- deltat_calced_df$weighted_mean * deltat_calced_df$deltaT_preturb
  
  # calculating model growth response after adjusting for the delta T by creating 
  # a function that takes in the temperature variable, as well as the model used
  calc_delta_g <- function(dataset, temp_var, model, deltaT, coef1, coef2) {
    response_tempnew <- ((temp_var - deltaT)*(coef1)) +
      (((temp_var - deltaT)^2)*(coef2)) 
    response_tempnew
  }
  
  # start an empty dataframe 
  mother_df <- data.frame()
  #unique(fair_exps_1tco2_2300$experiment_iso)
  #i <- "USA"
  #i <- "all0"
  # now loop over the experiments and calculate total damages owed by each of 
  # the countries
  
  for (i in list_of_exps){
    
    # subset by keeping one experiment for each loop
    
    deltaT_df1 <- deltat_calced_df[ which(deltat_calced_df$experiment_iso==i), ]
    
    # keep non missing ISOs exoeriments
    deltaT_df1 <- subset(deltaT_df1, !is.na(ISO3))
    
    deltaT_df1 <- deltaT_df1 %>% dplyr::group_by(ISO3, year) %>% 
      dplyr::summarise(deltat_fullemms = mean(deltat_fullemms_scld, na.rm = T),
                       deltat_preturb = mean(deltat_preturb_scld, na.rm = T),
                       deltat = mean(deltat_scld, na.rm = T),
                       .groups = "keep")
    
    deltaT_df1$deltat_preturb[deltaT_df1$year < 2021] <- 0
    deltaT_df1$deltat_fullemms[deltaT_df1$year < 2021] <- 0
    
    # now let us get the country-year panel
    gdp_temp_data1 <- gdp_temp_dataset
    
    gdp_temp_data1$year <- as.numeric(as.character(gdp_temp_data1$year))
    
    
    # join country-year temp change with the country-year panel
    
    gdp_temp_data1 <- left_join(deltaT_df1,
                                gdp_temp_data1,
                                by = c("ISO3", "year"))
    
    
    # we need to create country level annual average for last 5 years of observed 
    # data 
    if (temp_dataset == "ERA"){
      gdp_temp_data1$era_mwtemp[gdp_temp_data1$year >2020] <- NA 
      #  if (i < 2020){
      gdp_temp_data1$era_mwtemp_preturb_pre2020 <- gdp_temp_data1$era_mwtemp - gdp_temp_data1$deltat
      gdp_temp_data1 <- gdp_temp_data1 %>% dplyr::group_by(ISO3) %>% 
        dplyr::mutate(avg_temp_2010_2020_preturb = mean(era_mwtemp_preturb_pre2020[year > 2009 & year <= 2020], na.rm = T))
      
    }
    if (temp_dataset == "CRU"){
      gdp_temp_data1$cru_mwtemp[gdp_temp_data1$year >2020] <- NA 
      #  if (i < 2020){
      gdp_temp_data1$cru_mwtemp_preturb_pre2020 <- gdp_temp_data1$cru_mwtemp - gdp_temp_data1$deltat
      gdp_temp_data1 <- gdp_temp_data1 %>% dplyr::group_by(ISO3) %>% 
        dplyr::mutate(avg_temp_2010_2020_preturb = mean(cru_mwtemp_preturb_pre2020[year > 2009 & year <= 2020], na.rm = T))
    }
    
    
    #remotes::install_version("dplyr", "1.0.10")
    
    if (temp_dataset == "ERA") {
      
      gdp_temp_data1 <- gdp_temp_data1 %>%  dplyr::group_by(ISO3, year) %>% 
        dplyr::mutate(
          era_mwtemp_preturb= case_when(!is.na(era_mwtemp) ~ era_mwtemp_preturb_pre2020, 
                                        is.na(era_mwtemp) & year >2020 ~ avg_temp_2010_2020_preturb + deltat_preturb))
    }
    
    if (temp_dataset == "CRU"){
      gdp_temp_data1 <- gdp_temp_data1 %>%  dplyr::group_by(ISO3, year) %>% 
        dplyr::mutate(
          cru_mwtemp_preturb= case_when(!is.na(cru_mwtemp) ~ cru_mwtemp_preturb_pre2020, 
                                        is.na(cru_mwtemp) & year >2020 ~ avg_temp_2010_2020_preturb + deltat_preturb))
    }
    
    
    if (temp_dataset == "ERA"){
      gdp_temp_data1$era_mwtemp_fullemms <- gdp_temp_data1$era_mwtemp_preturb + gdp_temp_data1$deltat  
    }
    if (temp_dataset == "CRU"){
      gdp_temp_data1$cru_mwtemp_fullemms <- gdp_temp_data1$cru_mwtemp_preturb + gdp_temp_data1$deltat  
    }
    
    if (bootstrapped == T){
      gdp_temp_data1$temp <- coefdf$temp_l5  
      gdp_temp_data1$temp2 <- coefdf$temp2_l5 
    }
    if (bootstrapped == F){
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
    }
    
    gdp_temp_data1$era_mwtemp_fullemms[gdp_temp_data1$era_mwtemp_fullemms > 30 & !is.na(gdp_temp_data1$era_mwtemp_fullemms)] <- 30
    gdp_temp_data1$era_mwtemp_preturb[gdp_temp_data1$era_mwtemp_preturb > 30 & !is.na(gdp_temp_data1$era_mwtemp_preturb)] <- 30
    
    
    if (temp_dataset == "ERA"){
      gdp_temp_data1$resp_temp_fullemms <- (gdp_temp_data1$era_mwtemp_fullemms * gdp_temp_data1$temp) + 
        ((gdp_temp_data1$era_mwtemp_fullemms^2) * gdp_temp_data1$temp2)
      
      gdp_temp_data1$resp_temp_preturb <- (gdp_temp_data1$era_mwtemp_preturb * gdp_temp_data1$temp) + 
        ((gdp_temp_data1$era_mwtemp_preturb^2) * gdp_temp_data1$temp2)
      
      gdp_temp_data1$delta_g_era <- gdp_temp_data1$resp_temp_preturb - gdp_temp_data1$resp_temp_fullemms
      
    }
    if (growth_past_2100 == 0){
      gdp_temp_data1$delta_g_era[gdp_temp_data1$year > 2100] <- 0
    }
    
    if (temp_dataset == "CRU"){
      gdp_temp_data1$resp_temp_fullemms <- (gdp_temp_data1$cru_mwtemp_fullemms * gdp_temp_data1$temp) + 
        ((gdp_temp_data1$cru_mwtemp_fullemms^2) * gdp_temp_data1$temp2)
      
      gdp_temp_data1$resp_temp_preturb <- (gdp_temp_data1$cru_mwtemp_preturb * gdp_temp_data1$temp) + 
        ((gdp_temp_data1$cru_mwtemp_preturb^2) * gdp_temp_data1$temp2)
      
      gdp_temp_data1$delta_g_cru <- gdp_temp_data1$resp_temp_preturb - gdp_temp_data1$resp_temp_fullemms
      
    }
    
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
      dplyr::select(c("ISO3", "diff_lgdp_for_damages", "year", "NY.GDP.PCAP.KD")) %>% 
      subset(.,year == year_k) %>% 
      dplyr::mutate(ssp_data = case_when(is.na(diff_lgdp_for_damages) ~ "no",
                                         TRUE ~ "yes")) %>% 
      subset(., ssp_data == "yes") %>% 
      dplyr::select(c("ISO3"))
    
    gdp_temp_data1 <- left_join(identifier, gdp_temp_data1)
    
    # Now let us clamp teh growth rates on both sides 
    # turn this off if you do not want clamping
    if (clamping == "clamp_growth"){
      
      gdp_temp_data1 <- left_join(gdp_temp_data1,
                                  minmax_data,
                                  by = c("Income group" = "Income.group"))
      
      gdp_temp_data1 <- gdp_temp_data1 %>% 
        dplyr::mutate(diff_lgdp_for_damages = case_when(diff_lgdp_for_damages > max_dg ~ max_dg,
                                                        diff_lgdp_for_damages < min_dg ~ min_dg,
                                                        TRUE ~ diff_lgdp_for_damages))
    }
    
    # unlist...
    if (temp_dataset == "ERA"){
      gdp_temp_data1$delta_g_era <- unlist(gdp_temp_data1$delta_g_era)
      # now let us calculate adjusted growht rate by adding deltaG to observed growth
      gdp_temp_data1$adj_growth <- (gdp_temp_data1$delta_g_era + gdp_temp_data1$diff_lgdp_for_damages)
      
    }
    if (temp_dataset == "CRU"){
      gdp_temp_data1$delta_g_cru <- unlist(gdp_temp_data1$delta_g_cru)
      # now let us calculate adjusted growht rate by adding deltaG to observed growth
      gdp_temp_data1$adj_growth <- (gdp_temp_data1$delta_g_cru + gdp_temp_data1$diff_lgdp)
      
    }
    
    
    # let us add 1 to growth variables so we can calculate cumulative growth
    gdp_temp_data1$diff_lgdp_for_damages <- gdp_temp_data1$diff_lgdp_for_damages + 1
    gdp_temp_data1$adj_growth <- gdp_temp_data1$adj_growth + 1
    #year_k <- 1990  
    # now let us set GDP at year k as initial gdp number to calculate impact of 
    # added temoerature
    # gdp_temp_data1 <- subset(gdp_temp_data1, !is.na(coef_id))
    #year_k <- 1990
    
    gdp_temp_data1 <- gdp_temp_data1 %>% dplyr::group_by(ISO3) %>% 
      dplyr::mutate(gdp_year = NY.GDP.PCAP.KD_for_damages[year == year_k])
    
    gdp_temp_data1 <- subset(gdp_temp_data1, year >= year_k)
    
    damages_i_t4 <- gdp_temp_data1 %>% dplyr::group_by(ISO3) %>% 
      dplyr::mutate(cum_adj_growthz = cumprod(adj_growth),
                    cum_growth_real = cumprod(diff_lgdp_for_damages))
    
    # finally compute edamages...
    damages_i_t4 <- damages_i_t4 %>% 
      dplyr::mutate(gdp_noemms = (gdp_year * cum_adj_growthz),
                    gdp_ssp370 = (gdp_year * cum_growth_real),
                    damages = gdp_noemms - gdp_ssp370)
    
    damages_i_t4 <- ungroup(damages_i_t4)
    
    damages_i_t4 <- damages_i_t4 %>% 
      dplyr::mutate(average_gdp_2020 = mean(gdp_ssp370[year == 2020], na.rm = T))
    
    damages_i_t4 <- damages_i_t4 %>% 
      dplyr::group_by(year) %>% 
      dplyr::mutate(average_gdp = mean(gdp_ssp370, na.rm = T))
    
    
    # calculate discounted damages for past and for future (note different 
    #- processes for discounting past damages and future damages)
    damages_i_t4 <- damages_i_t4 %>% 
      dplyr::mutate(t_since_k = settlement_year - year,
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
    
    
    
    # add to data frame
    mother_df <- rbind(mother_df, damages_i_t4)
    
  }
  return(mother_df)
}

############################################################################# now loop over the years and assign multiple cpus
# Get number of cores set in bash script
num_cores = as.integer(Sys.getenv("SLURM_CPUS_PER_TASK")) - 1
# set up parallel backend
registerDoParallel(num_cores)

# parallelize the loop using foreach
foreach(i=1:1000) %dopar% {
  
  #sample fair run
  num_loop_i <- sample(unique(fair_exps_1tco2_disagg$num_loop), 1)
  fair_exps_1gtc_disagg_i <- subset(fair_exps_1tco2_disagg, 
                                    num_loop == num_loop_i)
  # sample cgm raster 
  raster_i <- list_r_rasters[[sample(1:30, 1)]]
  #sample bhm coeffeceint
  laggedbs_i <- subset(laggedbs, coef_id == sample(unique(laggedbs$coef_id), 1))
  # run the function
  damages_i <- calculate_damages_pulse(raster_i,
                                       fair_exps_1gtc_disagg_i,
                                       list_of_exps_1990_2020,
                                       1990,
                                       future_forecast_ssp370,
                                       gdp_temp_data_k90_5lag,
                                       "ERA",
                                       bhm_era_reg,
                                       T,
                                       "NO",
                                       "NO" ,
                                       2020,
                                       laggedbs_i)
  damages_i$sim_id <- i
  damages_i$coef_id <- laggedbs_i$coef_id
  
  damages_i <- damdamages_iages %>% 
    dplyr::select(c("sim_id",
                    "emitter", 
                    "ISO3",
                    "year",
                    "weighted_damages2_scld", 
                    "weighted_damages_ramsey_scld",
                    "coef_id",
                    "temp", 
                    "temp2"))
  
  write_rds(damages_i, 
            paste0(path, "data_20240311_6/scc_5lag_2100_", i, ".rds"))
  
  return(NULL)
}

stopImplicitCluster()

#end of script 