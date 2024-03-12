##############################################################################
# Mustafa Zahid, June 30th, 2023
# This script is used to prepare the data needed to plot figure 2e. 
##############################################################################
remove(list=ls())
gc()
sf::sf_use_s2(FALSE)
setwd("~/GitHub/loss_damage")

run_date <- "20240311"
# read in the needed libraries 
source("scripts/working/analysis/0_read_libs.R")

setwd(dropbox_path)
#############################################################################
#############################################################################
# read data 
# set path and get list of files from directory
path <- paste0(output_path, "_3")

################################################################################ ssp 2100 growth rates w/damages into 2300 (fig2eiii)
# generate list of files 
all_data = list.files(path=path,
                      pattern = "scc_", 
                      full.names = TRUE,
                      recursive = TRUE,
                      include.dirs = FALSE)
listofdfs_rams <- list()
listofdfs_2dr <- list()
for (i in 1:1000){
  tic()
  data_i <- readRDS(all_data[i])
  #data_i$loop_id <- i
  data_i <- data_i %>% 
    dplyr::select(c("emitter", 
                    "year",
                    #"loop_id",
                    "sim_id",
                    "weighted_damages2_scld", 
                    "weighted_damages_ramsey_scld",
                    "coef_id",
                    "temp",
                    "temp2"
                    ))
  data_ramsey_i <- data_i %>% 
    dplyr::group_by(emitter, sim_id) %>% 
    dplyr::summarise(total_damages_ramsey = sum(weighted_damages_ramsey_scld, na.rm = T))
  
  data_dr2_i <- data_i %>% 
    dplyr::group_by(emitter, sim_id) %>% 
    dplyr::summarise(total_damages2 = sum(weighted_damages2_scld, na.rm = T))
  
  listofdfs_rams[[i]] <- data_ramsey_i
  listofdfs_2dr[[i]] <- data_dr2_i
  toc()
}

totals_2100g_ramsey <- do.call(rbind, listofdfs_rams)
totals_2100g_2dr <- do.call(rbind, listofdfs_2dr)

################################################################################ no growth post 2100 w/damages into 2300 (fig2eii)
# generate list of files 
path <- paste0(output_path, "_2")

all_data = list.files(path=path,
                      pattern = "scc_" ,
                      full.names = TRUE,
                      recursive = TRUE,
                      include.dirs = FALSE)


# now read files into one list 
listofdfs_rams <- list()
listofdfs_2dr <- list()
for (i in 1:1000){
  tic()
  data_i <- readRDS(all_data[i])
  #data_i$loop_id <- i
  data_i <- data_i %>% 
    dplyr::select(c("emitter", 
                    "year",
                    "sim_id",
                    "weighted_damages2_scld", 
                    "weighted_damages_ramsey_scld",
                    "coef_id",
                    "temp",
                    "temp2"))
  data_ramsey_i <- data_i %>% 
    dplyr::group_by(emitter, sim_id) %>% 
    dplyr::summarise(total_damages_ramsey = sum(weighted_damages_ramsey_scld, na.rm = T))
  
  data_dr2_i <- data_i %>% 
    dplyr::group_by(emitter, sim_id) %>% 
    dplyr::summarise(total_damages2 = sum(weighted_damages2_scld, na.rm = T))
  
  listofdfs_rams[[i]] <- data_ramsey_i
  listofdfs_2dr[[i]] <- data_dr2_i
  
  toc()
}
totals_nog_2dr <- do.call(rbind, listofdfs_2dr)
totals_nog_ramsey <- do.call(rbind, listofdfs_rams)


################################################################################ no impacts post 2100
path <- paste0(output_path, "_5b")

# generate list of files 
all_data = list.files(path=path,
                      pattern = "scc_" ,
                      full.names = TRUE,
                      recursive = TRUE,
                      include.dirs = FALSE)

# now read files into one list 
listofdfs <- list()
for (i in 1:1000){
  tic()
  data_i <- readRDS(all_data[i])
  data_i <- data_i %>% 
    dplyr::select(c("emitter", 
                    "year",
                    "ISO3",
                    "sim_id",
                    "weighted_damages2_scld", 
                    "weighted_damages_ramsey_scld",
                    "coef_id",
                    "temp",
                    "temp2"))
  
  data_ramsey_i <- data_i %>% 
    dplyr::group_by(emitter, sim_id) %>% 
    dplyr::summarise(total_damages_ramsey = sum(weighted_damages_ramsey_scld, na.rm = T))
  
  data_dr2_i <- data_i %>% 
    dplyr::group_by(emitter, sim_id) %>% 
    dplyr::summarise(total_damages2 = sum(weighted_damages2_scld, na.rm = T))
  
  listofdfs_rams[[i]] <- data_ramsey_i
  listofdfs_2dr[[i]] <- data_dr2_i

  toc()
}

# ... and append all dataframes
totals_noi_2dr <- do.call(rbind, listofdfs_2dr)
totals_noi_ramsey <- do.call(rbind, listofdfs_rams)


#############################################################################
#############################################################################
# prep data 

totals_2100g_2dr$scenario <- "ssp_2100_2dr"
totals_2100g_ramsey$scenario <- "ssp_2100_ramsey"

totals_nog_2dr$scenario <- "nogrowth_2dr"
totals_nog_ramsey$scenario <- "nogrowth_ramsey"

totals_noi_2dr$scenario <- "noimpacts_2dr"
totals_noi_ramsey$scenario <- "noimpacts_ramsey"

# put all datasets into a list 
listofdfs <- list(totals_2100g_2dr,
                  totals_2100g_ramsey,
                  totals_nog_2dr,
                  totals_nog_ramsey,
                  totals_noi_2dr,
                  totals_noi_ramsey)

# write that list into directory 
save(listofdfs, file = paste0(fig_prepped_dta, run_date, "/listof_scc_est_dfs_fig2e.RData"))

# end of script 
