##############################################################################
# Mustafa Zahid, June 30th, 2023
# This script is used to prepare the data needed to plot figure S13. 
##############################################################################
remove(list=ls())
gc()
sf::sf_use_s2(FALSE)
setwd("~/GitHub/loss_damage")

# read in the needed libraries 
source("scripts/working/analysis/0_read_libs.R")

setwd(dropbox_path)
#############################################################################
#############################################################################
# read data 
# set path and get list of files from directory
path <- "sherlock_files_060223/desktop_placeholder_062623/"

################################################################################ ssp 2100 growth rates
# generate list of files 
all_data = list.files(path=path,
                      pattern = "scc_2100growth", 
                      full.names = TRUE,
                      recursive = TRUE,
                      include.dirs = FALSE)

# now read files into one list 
tic()
listofdfs <- mclapply(all_data, readRDS, mc.cores = 7)
toc()    

# ok now we need to add loop id so we can summarize
for (i in 1:length(listofdfs)){
  tic()
  x <- listofdfs[[i]]
  x$loop_id <- i
  listofdfs[[i]] <- x
  toc()
}

# ... and append all dataframes
total_damages_uncertainty_2100g <- do.call(rbind, listofdfs)
rm(listofdfs)


################################################################################ no growth post 2100
# generate list of files 
all_data = list.files(path=path,
                      pattern = "scc_nogrowth" ,
                      full.names = TRUE,
                      recursive = TRUE,
                      include.dirs = FALSE)

# now read files into one list 
tic()
listofdfs <- mclapply(all_data, readRDS, mc.cores = 7)
toc()    

# ok now we need to add loop id so we can summarize
for (i in 1:length(listofdfs)){
  tic()
  x <- listofdfs[[i]]
  x$loop_id <- i
  listofdfs[[i]] <- x
  toc()
}

# ... and append all dataframes
total_damages_uncertainty_nog <- do.call(rbind, listofdfs)
rm(listofdfs)

#############################################################################
#############################################################################
# prep data 

# ok now let us aggregate. We want 4 numbers. These numbers will then be 
# plotted 
totals_2100g_2dr <- total_damages_uncertainty_2100g %>% 
  dplyr::group_by(emitter, loop_id) %>% 
  dplyr::summarise(total_damages2 = sum(weighted_damages2_scld, na.rm = T))

# now we need daamages at ramsey params
totals_2100g_ramsey <- total_damages_uncertainty_2100g %>% 
  dplyr::group_by(emitter, loop_id) %>% 
  dplyr::summarise(total_damages_ramsey = sum(weighted_damages_ramsey_scld, na.rm = T))

# now lwt us do the no growth post 2100 scenario
totals_nog_2dr <- total_damages_uncertainty_nog %>% 
  dplyr::group_by(emitter, loop_id) %>% 
  dplyr::summarise(total_damages2 = sum(weighted_damages2_scld, na.rm = T))

# now we need daamages at ramsey params
totals_nog_ramsey <- total_damages_uncertainty_nog %>% 
  dplyr::group_by(emitter, loop_id) %>% 
  dplyr::summarise(total_damages_ramsey = sum(weighted_damages_ramsey_scld, na.rm = T))


totals_2100g_2dr$scenario <- "ssp_2100_2dr"
totals_2100g_ramsey$scenario <- "ssp_2100_ramsey"
totals_nog_2dr$scenario <- "nogrowth_2dr"
totals_nog_ramsey$scenario <- "nogrowth_ramsey"

# put all datasets into a list 
listofdfs <- list(totals_2100g_2dr,
                  totals_2100g_ramsey,
                  totals_nog_2dr,
                  totals_nog_ramsey)

# write that list into directory 
save(listofdfs, file = paste0(fig_prepped_dta, "/20230629/listof_scc_est_dfs_figs13.RData"))

# end of script 
