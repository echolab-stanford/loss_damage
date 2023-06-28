list_of_exps <- c(2020)
numloop <- 1
num_rast <- 1
finished_rasters <- list()


list_of_exps <- c(2020)
list_of_estimates <- list()
list_of_estimates <- data.frame()


fair_exps_1gtc_disagg_i <- subset(fair_exps_1gtc_disagg, 
                                  num_loop == 44)

i <- 1

gdp_temp_data <-  subset(gdp_temp_data, ISO3 != "ERI" &
                           ISO3 != "OMN" | 
                     ISO3 != "SYR" & ISO3 != "TKM" & 
                     ISO3 != "VEN" & ISO3 != "YEM" &
                     ISO3 != "ARE" & ISO3 != "AGO" & 
                     ISO3 != "AFG" & ISO3 != "ATA" & 
                     ISO3 != "ESH" & ISO3 != "GRL" &
                     ISO3 != "GUF" & ISO3 != "MMR" &
                     ISO3 != "PRK" & ISO3 != "SJM" &
                     ISO3 != "TWN")

length(unique(gdp_temp_data$ISO3))    

for (i in 4001:10000) {
  tic()
  
  fair_exps_1gtc_disagg_i <- subset(fair_exps_1gtc_disagg, 
                                    num_loop == sample(unique(fair_exps_1gtc_disagg$num_loop), 1))
  
 # fair_exps_1gtc_disagg_i <- subset(fair_exps_1gtc_disagg, 
  #                                  num_loop == 1)
  
  
  gdp_temp_data_sbstd <- subset(gdp_temp_data, coef_id == sample(1:1000, 1))
  
  #gdp_temp_data_sbstd <- subset(gdp_temp_data, coef_id == 1)
  

  total_damages_1gtC <- calculate_bidamages(list_rasters[[sample(1:29, 1)]],
                                            fair_exps_1gtc_disagg_i, 
                                            list_of_exps, 
                                            1990,
                                            future_forecast_df,
                                            gdp_temp_data_sbstd)
  

  #list_of_estimates <- rbind(list_of_estimates,
   #                          total_damages_1gtC)
  #hist(list_of_estimates$total_damages2)
  
  
  write_rds(total_damages_1gtC, 
            paste0("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/net_scc_base/net_scc_est_", i, ".rds"))
  
  #list_of_estimates[[i]] <- total_damages_1gtC
  
  toc()
}

for (i in 1:10000) {
  tic()
  
  fair_exps_1gtc_disagg_i <- subset(fair_exps_1gtc_disagg, 
                                    num_loop == sample(unique(fair_exps_1gtc_disagg$num_loop), 1))
  
  # fair_exps_1gtc_disagg_i <- subset(fair_exps_1gtc_disagg, 
  #                                  num_loop == 1)
  
  
  gdp_temp_data_sbstd <- subset(gdp_temp_data, coef_id == sample(1:1000, 1))
  
  #gdp_temp_data_sbstd <- subset(gdp_temp_data, coef_id == 1)
  
  
  total_damages_1gtC <- calculate_bidamages(list_rasters[[sample(1:29, 1)]],
                                            fair_exps_1gtc_disagg_i, 
                                            list_of_exps, 
                                            1990,
                                            future_forecast_df,
                                            gdp_temp_data_sbstd)
  
  
  #list_of_estimates <- rbind(list_of_estimates,
  #                          total_damages_1gtC)
  #hist(list_of_estimates$total_damages2)
  
  
  write_rds(total_damages_1gtC, 
            paste0("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/net_scc_ssp1/net_scc_est_", i, ".rds"))
  
  #list_of_estimates[[i]] <- total_damages_1gtC
  
  toc()
}

for (i in 5001:10000) {
  tic()
  
  fair_exps_1gtc_disagg_i <- subset(fair_exps_1gtc_disagg, 
                                    num_loop == sample(unique(fair_exps_1gtc_disagg$num_loop), 1))
  
  # fair_exps_1gtc_disagg_i <- subset(fair_exps_1gtc_disagg, 
  #                                  num_loop == 1)
  
  
  gdp_temp_data_sbstd <- subset(gdp_temp_data, coef_id == sample(1:1000, 1))
  
  #gdp_temp_data_sbstd <- subset(gdp_temp_data, coef_id == 1)
  
  
  total_damages_1gtC <- calculate_bidamages(list_rasters[[sample(1:29, 1)]],
                                            fair_exps_1gtc_disagg_i, 
                                            list_of_exps, 
                                            1990,
                                            future_forecast_df,
                                            gdp_temp_data_sbstd)
  
  
  #list_of_estimates <- rbind(list_of_estimates,
  #                          total_damages_1gtC)
  #hist(list_of_estimates$total_damages2)
  
  
  write_rds(total_damages_1gtC, 
            paste0("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/net_scc_ssp2/net_scc_est_", i, ".rds"))
  
  #list_of_estimates[[i]] <- total_damages_1gtC
  
  toc()
}



list_of_exps <- c(2020)

for (i in 7002:10000) {
  tic()
  
  fair_exps_1gtc_disagg_i <- subset(fair_exps_1gtc_disagg, 
                                    num_loop == sample(unique(fair_exps_1gtc_disagg$num_loop), 1))
  
  # fair_exps_1gtc_disagg_i <- subset(fair_exps_1gtc_disagg, 
  #                                  num_loop == 1)
  
  
  gdp_temp_data_sbstd <- subset(gdp_temp_data, coef_id == sample(1:1000, 1))
  
  #gdp_temp_data_sbstd <- subset(gdp_temp_data, coef_id == 1)
  
  
  total_damages_1gtC <- calculate_bidamages(list_rasters[[sample(1:29, 1)]],
                                            fair_exps_1gtc_disagg_i, 
                                            list_of_exps, 
                                            1990,
                                            future_forecast_df,
                                            gdp_temp_data_sbstd)
  
  
  #list_of_estimates <- rbind(list_of_estimates,
  #                          total_damages_1gtC)
  #hist(list_of_estimates$total_damages2)
  
  
  write_rds(total_damages_1gtC, 
            paste0("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/net_scc_lag5/net_scc_est_", i, ".rds"))
  
  #list_of_estimates[[i]] <- total_damages_1gtC
  
  toc()
}

sum(total_damages_1gtC$total_damages2) / 1000000000

totals <- do.call(rbind, list_of_estimates)
median(totals$total_damages2) / 1000000000
write_rds(totals, "~/BurkeLab Dropbox/Projects/loss_damage/data/processed/net_scc_base/net_scc_est_1_4000.rds")
totals$total_damages2 <-totals$total_damages2 / 1000000000
median(totals$total_damages2)

for (numloop in unique(fair_exps_1gtc_disagg$num_loop)){
  tic()
  fair_exps_1gtc_disagg_i <- subset(fair_exps_1gtc_disagg, num_loop == numloop)
  for (num_rast in 1:29){
    total_damages_1gtC <- calculate_bidamages(list_rasters[[num_rast]],
                                              fair_exps_1gtc_disagg_i, 
                                              list_of_exps, 
                                              1980,
                                              future_forecast_df)
    
    #raster_name <- substr(name(raster), 1,10)
    
    run <- paste0(num_rast, '_', numloop)
    
    #finished_rasters[[run]] <- total_damages_1gtC
    
    write_rds(total_damages_1gtC, 
              paste0("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/estimates_uncertainty/total_damages_cgm", 
                     num_rast, "_fair", numloop, ".rds"))
    toc()
    print(paste0("finished with ", run))
  }
}