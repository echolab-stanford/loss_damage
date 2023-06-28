##############################################################################
# Mustafa Zahid, June 16th, 2022
# This R script reads the FAIR temperature response time series data and 
# and outputs a mdeian or range of estimates for temperature response to 
# emissions according to FaIR.
# Input(s): raw FaIR temperature response data from desired experiments
# Output(s): processed median FaIR termperature response to be used with the 
# calculated CGM warming ratio in the previous step
##############################################################################

# the following function processes the temperature response from the FaIR models
# the output from this. The arguments it takes is year k 
process_exp_data_futssp <- function(experiment, year_k){
  # set path and get list of files from directory
  path <- "~/BurkeLab Dropbox/Projects/loss_damage/FaIR"
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
  
  if (experiment == "1GtC_hist_fut") {
    fair_exps$experiment_iso <- substr(fair_exps$loop, 5,8) 
  }
  if (experiment == "1GtC_hist_fut_ssp245" | experiment == "1GtC_hist_fut_ssp119" ) {
    fair_exps$experiment_iso <- substr(fair_exps$loop, 5,8) 
  }
  if (experiment == "hist_fut" | experiment == "hist_fut_ssp119" | 
      experiment == "hist_fut_ssp126" | experiment == "hist_fut_ssp245" |
      experiment == "hist_fut_noemms_ssp126" | experiment == "hist_fut_noemms_ssp245" |
      experiment == "hist_fut_noemms_ssp119" | experiment == "futssp370_noemms" | 
      experiment == "futssp370_emms" | experiment == "futssp126_noemms" | 
      experiment == "futssp126_emms" | experiment == "futssp245_noemms" | 
      experiment == "futssp245_emms" | experiment == "futssp119_noemms" | 
      experiment == "futssp119_emms" 
  ) {
    fair_exps$experiment_iso <- substr(fair_exps$loop, 10,12) 
  }
  if (experiment == "carbon_debt") {
    fair_exps$experiment_iso <- substring(fair_exps$loop, 10) 
  }
  
  # cast response as numeric 
  fair_exps$Test <- as.numeric(fair_exps$Test)
  
  #select the needde variables
  fair_exps <- fair_exps %>% dplyr::select(c("num_loop", 
                                             "experiment_iso",
                                             "year", "Test"))
  # now let us join the data from all the experiments and the data from running 
  # the FaIR with all emissions
  
  # now we need to aggregate this to the country-year level and calculate the 
  # median
  fair_exps <- fair_exps %>% 
    dplyr::group_by(year, experiment_iso) %>% 
    dplyr::summarise(median_deltat = median(Test, na.rm = T),
                     .groups = "keep")
  
  # clear out this standout country that produces negative values (St. Kitts & Nevis)
  fair_exps <- subset(fair_exps, experiment_iso != "KNA")
  
  # return the dataset with median deltaT from FaIR
  return(fair_exps)
}

# end of script