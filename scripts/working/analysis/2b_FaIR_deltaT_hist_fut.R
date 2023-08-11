##############################################################################
# Mustafa Zahid, June 16th, 2022
# This R script reads the FAIR temperature response time series data and 
# and outputs a mdeian or range of estimates for temperature response to 
# emissions according to FaIR.
# Input(s): raw FaIR temperature response data from desired experiments
# Output(s): processed median FaIR termperature response to be used with the 
# calculated CGM warming ratio in the previous step
# Outputs using this script: This script is used to produce the fair responses 
# used to calculate imapcts of 1gtco2/1tco2 on temperature. It is also used 
# for carbon capture, SPP GDP impacts, and green gdp plot. 
# Last updated: June 2023
##############################################################################

# we need to calculate the difference in temperature response between an 
# experiment where we run with full emissions, and where we shut off emissions 
# from a country
# the following function processes the temperature response from the FaIR models
# the output from this. The arguments it takes is year k 
process_exp_data_hist_fut <- function(date, experiment, year_k, aggregating){
  # set path and get list of files from directory
  
  path <- paste0("FaIR/", date)
  #fair_exp <- experiment
  # now let us generate the year k identifier to call the files
  year_k_exp <- paste0("k", substr(year_k, 3,4))
  # generate list of files 
  
  all_data = list.files(path=path,
                        pattern = paste0("fair_temp_resp_", year_k_exp, "_", experiment), 
                        full.names = TRUE,
                        recursive = TRUE,
                        include.dirs = FALSE)
  
  # now read files into one list 
  tic()
  listofdfs <- mclapply(all_data, read_csv, mc.cores = 7)
  toc()    

  # ... and append all dataframes
  fair_exps <- do.call(rbind, listofdfs)
  
  # Let us clean the dataframe so that we can use it to aggregate
  fair_exps$num_loop <- as.numeric(gsub("([0-9]+).*$", "\\1", fair_exps$loop))
  fair_exps <- fair_exps[-c(1:2),]
  fair_exps$year <- as.numeric(fair_exps$Scenario) + year_k
  fair_exps <- fair_exps %>% 
    dplyr::mutate(loop = case_when(num_loop <= 9 ~ paste0("00", loop),
                                   (num_loop >9 & num_loop < 100) ~ paste0("0", loop),
                                   TRUE ~ loop))
    
  if (experiment == "hist_fut_yriso" | experiment == "hist_fut_060123_yriso") {
    fair_exps$exp_yr <- substr(fair_exps$loop, 17,20) 
    fair_exps$experiment_iso <- substr(fair_exps$loop, 10,12) 
  }
  
  #  head(fair_exps)
  
  if (experiment == "1tCO2_hist_2300" | experiment == "1GtCO2_hist_2300" | 
      experiment == "cc_hist" |
      experiment == "1GtCO2_hist_2100" |
      experiment == "1tCO2_hist_2100"  | experiment == "10tCO2_hist_2300"  | 
      experiment == "100tCO2_hist_2300" | experiment == "1000tCO2_hist_2300" | 
      experiment == "1MtCO2_hist_2300"  | experiment == "10GtCO2_hist_2300" | 
      experiment == "100GtCO2_hist_2300" )
       {
    fair_exps$experiment_iso <- substr(fair_exps$loop, 5,8) 
  }
#  if (experiment == "1GtC_hist_fut_ssp245" | experiment == "1GtC_hist_fut_ssp119" ) {
#    fair_exps$experiment_iso <- substr(fair_exps$loop, 5,8) 
#  }
  if (experiment == "hist_bi_v2022" |
      experiment == "hist_bitt_v2022" | experiment == "hist_biconsump_v2022" |
      experiment == "hist_biprod_v2022" | experiment == "hist_biusa_v2022" 
  ) {
    fair_exps$experiment_iso <- substr(fair_exps$loop, 10,12) 
  }
#    if (experiment == "ssp370" | experiment == "ssp245" | experiment == "ssp119" | experiment == "ssp126"){
  #    fair_exps$experiment_iso <- substr(fair_exps$loop, 10,13)
  #  }
  
  # cast response as numeric 
  fair_exps$Test <- as.numeric(fair_exps$Test)
  
  #select the needde variables
  #  if (experiment == "hist_fut_yriso" | experiment == "hist_fut_060123_yriso"){ 

#    fair_exps <- fair_exps %>% dplyr::select(c("num_loop", 
#                                               "experiment_iso",
#                                               "exp_yr",
#                                               "year", "Test"))}
#  
#  if (experiment != "hist_fut_yriso" & experiment != "hist_fut_060123_yriso"){
#    fair_exps <- fair_exps %>% dplyr::select(c("num_loop", 
#                                               "experiment_iso",
#                                               "year", "Test"))}
#  
  
  #  fair_exps <- fair_exps %>% dplyr::select(c("num_loop", 
  #                                            "experiment_iso",
  #                                           "year", "Test"))
  
  
  if (experiment == "1tCO2_hist_2300" | experiment == "1GtCO2_hist_2300" | 
      experiment == "cc_hist" |
      experiment == "1GtCO2_hist_2100" |
      experiment == "1tCO2_hist_2100"  | experiment == "10tCO2_hist_2300"  | 
      experiment == "100tCO2_hist_2300" | experiment == "1000tCO2_hist_2300" | 
      experiment == "1MtCO2_hist_2300"  | experiment == "10GtCO2_hist_2300" | 
      experiment == "100GtCO2_hist_2300" ){
    fair_exp_all <- subset(fair_exps, experiment_iso == "loop")
  } 
  if (experiment == "1GtC_hist_fut_ssp245" | experiment == "1GtC_hist_fut_ssp119" ) {
    fair_exp_all <- subset(fair_exps, experiment_iso == "loop")
  }
  if (experiment == "hist_bi_v2022" |
      experiment == "hist_bitt_v2022" | experiment == "hist_biconsump_v2022" |
      experiment == "hist_biprod_v2022" | experiment == "hist_biusa_v2022" 
  )
  {
    fair_exp_all <- subset(fair_exps, experiment_iso == "all") 
  }
  
  if (experiment == "carbon_debt" | experiment == "carbon_debt_op" | 
      experiment == "carbon_debt_indent" | experiment == "carbon_debt_indentcelebs") { 
    fair_exp_all <- subset(fair_exps, experiment_iso == "all")
  }
  if (experiment == "ssp370" | experiment == "ssp245" | experiment == "ssp119" | 
      experiment == "ssp126"){
    fair_exp_all <- subset(fair_exps, experiment_iso == "all1")
  }
  
  fair_exp_all <- fair_exp_all %>% dplyr::select(c("num_loop", "experiment_iso",
                                                   "year", "Test"))
  colnames(fair_exp_all)[2] <- "all"
  
  # now let us join the data from all the experiments and the data from running 
  # the FaIR with all emissions
  
  fair_exps <- left_join(fair_exps,
                         fair_exp_all,
                         by = c("num_loop", "year"))
  
  if (experiment == "1tCO2_hist_2300" | experiment == "1GtCO2_hist_2300" | 
      experiment == "cc_hist" |
      experiment == "1GtCO2_hist_2100" |
      experiment == "1tCO2_hist_2100"  | experiment == "10tCO2_hist_2300"  | 
      experiment == "100tCO2_hist_2300" | experiment == "1000tCO2_hist_2300" | 
      experiment == "1MtCO2_hist_2300"  | experiment == "10GtCO2_hist_2300" | 
      experiment == "100GtCO2_hist_2300" ){
    # we subtract FaIR ran with all emissions from the left hand side
    fair_exps <- subset(fair_exps, experiment_iso != "loop")
  }
  if (experiment == "1GtC_hist_fut_ssp245" | experiment == "1GtC_hist_fut_ssp119" ) {
    fair_exps <- subset(fair_exps, experiment_iso != "loop")
  }
  if (experiment == "hist_bi_v2022" |
      experiment == "hist_bitt_v2022" | experiment == "hist_biconsump_v2022" |
      experiment == "hist_biprod_v2022" | experiment == "hist_biusa_v2022" 
  ){
    fair_exps <- subset(fair_exps, experiment_iso != "all")
  }
#  if (experiment == "carbon_debt" | experiment == "carbon_debt_op" | 
#      experiment == "carbon_debt_indent" | experiment == "carbon_debt_indentcelebs") {
##    # we subtract FaIR ran with all emissions from the left hand side
#    fair_exps <- subset(fair_exps, experiment_iso != "all")
#  }
#  if (experiment == "ssp370" | experiment == "ssp245" | experiment == "ssp119" | experiment == "ssp126"){
#    fair_exps <- subset(fair_exps, experiment_iso != "all1")
#  }
  
  # now we can calculate difference (deltaT)
  fair_exps$deltaT <- fair_exps$Test.y - fair_exps$Test.x
  
  #fair_exps$deltaT <- fair_exps$Test.x
  
  fair_exps_2020 <- subset(fair_exps, experiment_iso == "2020")
    
  if ( aggregating == T){
    # now we need to aggregate this to the country-year level and calculate the 
    # median
#    if (experiment == "hist_fut_yriso" | experiment == "hist_fut_060123_yriso"){
#      fair_exps <- fair_exps %>% 
#        dplyr::group_by(year, experiment_iso, exp_yr) %>% 
#        dplyr::summarise(median_deltat = median(deltaT, na.rm = T),
#                         median_deltat_fullemms = median(Test.y, na.rm = T),
#                         median_deltat_preturb = median(Test.x, na.rm = T),
#                         .groups = "keep")
#    }
    if (experiment != "hist_fut_yriso" & experiment != "hist_fut_060123_yriso")
      fair_exps <- fair_exps %>% 
        dplyr::group_by(year, experiment_iso) %>% 
        dplyr::summarise(median_deltat_preturb = median(Test.x, na.rm = T),
                         median_deltat_fullemms = median(Test.y, na.rm = T),
                         median_deltat = median(deltaT, na.rm = T),
                         .groups = "keep")
    }
    
  #plot(fair_exps$year, fair_exps$median_deltat_preturb)
  
  if (experiment == "1tCO2_hist_2300" | experiment == "1GtCO2_hist_2300" | 
      experiment == "cc_hist" |
      experiment == "1GtCO2_hist_2100" |
      experiment == "1tCO2_hist_2100"  | experiment == "10tCO2_hist_2300"  | 
      experiment == "100tCO2_hist_2300" | experiment == "1000tCO2_hist_2300" | 
      experiment == "1MtCO2_hist_2300"  | experiment == "10GtCO2_hist_2300" | 
      experiment == "100GtCO2_hist_2300" ){
    for (i in year_k:2100){
      fair_exps1 <- subset(fair_exps, experiment_iso == i)
      fair_exps1$deltat_fullemms <- fair_exps1$median_deltat_fullemms - fair_exps1$median_deltat_fullemms[fair_exps1$year == 2020]
      fair_exps$deltat_fullemms[fair_exps$experiment_iso == i] <- fair_exps1$deltat_fullemms
      
      fair_exps1$deltaT_preturb <- fair_exps1$median_deltat_preturb - fair_exps1$median_deltat_preturb[fair_exps1$year == 2020]
      fair_exps$deltaT_preturb[fair_exps$experiment_iso == i] <- fair_exps1$deltaT_preturb
    }
  }
  
  #unique(fair_exps$exp_yr)
#  i <- 2019
#  j <- "CHN"
#  if (experiment == "hist_fut_yriso" | experiment == "hist_fut_060123_yriso"){
#    for (i in year_k:2020){
#      for (j in unique(fair_exps$experiment_iso)){
#        
#      fair_exps1 <- subset(fair_exps, experiment_iso == j & exp_yr == i)
#      fair_exps1$deltat_fullemms <- fair_exps1$median_deltat_fullemms - fair_exps1$median_deltat_fullemms[fair_exps1$year == 2020 & fair_exps1$experiment_iso == j]
#      fair_exps$deltat_fullemms[fair_exps$experiment_iso == j & fair_exps$exp_yr == i] <- fair_exps1$deltat_fullemms
#      
#      fair_exps1$deltaT_preturb <- fair_exps1$median_deltat_preturb - fair_exps1$median_deltat_preturb[fair_exps1$year == 2020 & fair_exps1$experiment_iso == j]
#      fair_exps$deltaT_preturb[fair_exps$experiment_iso == j & fair_exps$exp_yr == i] <- fair_exps1$deltaT_preturb
#    }
#    }
#  }

 
 
  #plot(fair_exps1$year, fair_exps1$median_deltat, type = "l")    
  
  
  if (experiment == "ssp370" | experiment == "ssp245" | experiment == "ssp119" | 
      experiment == "ssp126" ){ #experiment == "hist_fut_bilateral" | #experiment == "hist_bi_v2022" |
      #experiment == "hist_bitt_v2022"){
    fair_exps$deltaT_fullemms <- 0
    fair_exps$deltaT_preturb <- 0
    fair_exps$deltaT_fullemms <- fair_exps$median_deltat_fullemms - fair_exps$median_deltat_fullemms[fair_exps$year == 2020]
    fair_exps$deltaT_preturb <- fair_exps$median_deltat_preturb - fair_exps$median_deltat_preturb[fair_exps$year == 2020]
  }
  
  plot(fair_exps$year[fair_exps$year <= 2100], fair_exps$median_deltat[fair_exps$year <= 2100])
  # clear out this standout country that produces negative values (St. Kitts & Nevis)
  fair_exps <- subset(fair_exps, experiment_iso != "KNA")
  
  # return the dataset with median deltaT from FaIR
  return(fair_exps)
}


# end of script

