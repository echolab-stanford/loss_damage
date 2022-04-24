##############################################################################
# Mustafa Zahid, March 23rd, 2022
# This script runs the written scripts that produce the datasets and the 
# figures needed for this project 
##############################################################################
  remove(list=ls())
  
  #read in the needed libraries 
  source("~/BurkeLab Dropbox/Projects/loss_damage/scripts/0_read_libs.R")

  #this libraries have not been read for some reason from the script above
  library("maptools")
  library("WDI")
  library("wbstats")
  library("scales")
  library("countrycode")
  library("ggalluvial")
  library("MetBrewer")
  library("RColorBrewer")

##############################################################################
######################### PART I: Calculate Deltas ###########################
##############################################################################
  #calculate deltaTs 
  source("~/BurkeLab Dropbox/Projects/loss_damage/scripts/1_deltaT.R")
  
  # specify the needed ensembles and forcings and levels of aggregation 
  list_of_experiments <- list("MIROC6",
                              "cmip6")
  list_of_forcings <- list("historical",
                           "hist-nat")
  list_of_dims <- list("across_all",
                       "by_all")
  
  #calculate the deltas across ensembles/realizations
  calc_deltas(list_of_experiments,
              list_of_forcings,
              165,
              "across_all")
  
  #calculate the deltas by ensemble/realizations
  calc_deltas_realization(list_of_experiments,
                          list_of_forcings,
                          165,
                          "by_all")
  
  # read in the calculated datasets 
  for (i in list_of_experiments) {
    for (j in list_of_forcings){
      for (k in list_of_dims) {
        all <- readRDS(paste0("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/",j,
                              "_", i, "_", k, "_", "wavgs.rds"))
        assign(paste0(j, "_", i, "_", k), all)
      }
    }
  }
  
  
##############################################################################
######################## PART II: Calculate DeltaTs ##########################
##############################################################################
# now calculate the deltaTs
  #across_ensembles
  all_across <- calc_deltaT(historical_cmip6_across_all,
                            `hist-nat_cmip6_across_all`,
                            "not_realization")
  
  #by ebsemble/realization
  all_by_ensmeble <- calc_deltaT(historical_cmip6_by_all,
                                 `hist-nat_cmip6_by_all`,
                                 "realization")
  
  #by realization for MIROC6
  all_by_realization <- calc_deltaT(historical_MIROC6_by_all,
                                    `hist-nat_MIROC6_by_all`,
                                    "realization")

##############################################################################
###################### PART III: Visualizze DeltaTs ##########################
##############################################################################
#visualize deltaT
# now let us plot and save the figures 
  list_of_deltaTs <- list("deltaT_1960",
                          "deltaT_1980",
                          "deltaT_1990")
  
  for (i in  c(1960,1980,1990)) {
    plot <- plot_density(all_across, i, "- All Climate Model Ensemble Pooled")
    assign(paste0("density_i_t_", i), plot)
  }

  #operate the function to plot year trend for deltaT
  for (i in c(1960,1980,1990)) {
    plot <- plot_trend_year(all_by_ensmeble, i, "- All Climate Model Ensemble Pooled")
    assign(paste0("trend_t_", i), plot)
  }

  #operate the function to plot year trend for deltaT
  for (i in c(1960,1980,1990)) {
    plot <- plot_trend_year(all_by_realization, i, "- All Climate Model Ensemble Pooled")
    assign(paste0("trend_t_", i), plot)
  }

##############################################################################
###################### PART IV: Calculate DeltaGs* ###########################
##############################################################################
# DeltaGs are the degrowth decrement calculated from the BHM model for what the 
# gdp would have been had thier not been anthropeginic warming vs actual growth 
# read in the code for calculating the growth decrement
  source("~/BurkeLab Dropbox/Projects/loss_damage/scripts/2_deltaG.R")
  
  # read in the income data so that we can use the population data
  new_wdi_cache <- WDIcache() 
  WDIsearch("gdp.*ppp* *capita.*US\\$", cache = new_wdi_cache)
  wdi_dat <- WDI(indicator = c("NY.GDP.PCAP.PP.KD", "NY.GDP.PCAP.KD",
                               "NY.GDP.MKTP.CD", "SP.DYN.LE00.IN", 
                               "SP.DYN.IMRT.IN", "SP.POP.TOTL"), 
                 start = 1960, 
                 end = 2021,
                 extra = TRUE) 
  pop <- wdi_dat %>% subset(year == 2005) %>% 
    dplyr::select(c("iso3c", "SP.POP.TOTL")) 
  
  
  #read in the gdp/temp dataset to calculate the degrouwth at the country-year 
  # level, and recover dG. 
  gdp_temp_data <- readRDS("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/temp_gdp_world_panel.rds")
  
  # calculate the growth decrement
  calculate_degrowth(all_across,
                     "main")
  
  # calculate the growth decrement by ensemble
  calculate_degrowth_byvar(all_by_ensmeble,
                     "ensembles",
                     realization)
  
  # calculate the growth decrement by realization
  calculate_degrowth_byvar(all_by_realization,
                           "miroc6",
                           realization)
  
  # calculate the growth decrement by uncertainty by bootstrapping BHM
  calculate_degrowth_w_uncertainty(all_across,
                                   1000,
                                   "bhm_iter")
  
##############################################################################
################ PART V: Calculate Total Damages (bilateral) #################
##############################################################################
# Before calculating the total damages that are owed to country i by 
# emitter j, we need to incorporate the proportional contribution to warming by 
# each of the countries. This is (phi) in the paper. Proportional contribution 
# has already been calculated in Python and now we will read in the result 
# with all the iterations and take the median 
# read in the r script for visualizing density plots by varying the sources of 
# the uncertainty
  source("~/BurkeLab Dropbox/Projects/loss_damage/scripts/3_FAIR_phi.R")
  source("~/Desktop/loss_damages/scripts/4_total_damages.R")
  
  #calculate median phi for each country year
  median_phi_1980 <- read_FAIR_median_phi(1980)
  median_phi_1990 <- read_FAIR_median_phi(1990)
  
  # read in the growth decrement data 
  for (i in list_of_deltaTs) {
    for (j in c("cru", "era")) {
      year <- substr(i, 8,11)
      damages <- readRDS(paste0("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/damages_main_", j,
                                "_", year, ".rds"))
      assign(paste0("damages_main_",j, "_", year), damages)
    }
  }
  
  
  # now calculate the total damages owed by each country to another
  #start year = 1980
  damages_across_all_cru_k_1980 <- calc_damages_all_ensembles(damages_main_cru_1980,
                                                           median_phi_1980,
                                                           dg_cru_deltaT_1980,
                                                           "gdp_cru_1980",
                                                           gdp_cru_1980,
                                                           "dg_cru_deltaT_1980")
  damages_across_all_era_k_1980 <- calc_damages_all_ensembles(damages_main_era_1980,
                                                              median_phi_1980,
                                                              dg_cru_deltaT_1980,
                                                              "gdp_era_1980",
                                                              gdp_era_1980,
                                                              "dg_era_deltaT_1980")
  #start year = 1990
  damages_across_all_cru_k_1990 <- calc_damages_all_ensembles(damages_main_cru_1990,
                                                           median_phi_1990,
                                                           dg_cru_deltaT_1990,
                                                           "gdp_cru_1990",
                                                           gdp_cru_1990,
                                                           "dg_cru_deltaT_1990")
  damages_across_all_era_k_1990 <- calc_damages_all_ensembles(damages_main_era_1990,
                                                           median_phi_1990,
                                                           dg_era_deltaT_1990,
                                                           "gdp_era_1990",
                                                           gdp_era_1990,
                                                           "dg_era_deltaT_1990")
  

##############################################################################
################ PART V: Calculate Total Damages Sensitivity #################
##############################################################################
  #here we focus on a specific bilateral flow and calculate the distribution of 
  # total damages owed to country i by country j
  source("~/Desktop/loss_damages/scripts/5_damages_uncertainty.R")
  
  #calculate phi by each FAIR loop 
  phi_loops_1990 <- read_FAIR_phi(1990) 
  phi_loops_1980 <- read_FAIR_phi(1980)
  phi_loops_1980 <- phi_loops_1980 %>% 
    dplyr::mutate(loop = as.numeric(gsub("_1980", "", loop)))
  phi_loops_1990 <- phi_loops_1990 %>% 
    dplyr::mutate(loop = as.numeric(gsub("_1990", "", loop)))
  
  # before proceeding with calculating uncertainty, let us botstrapp BHM estimates 
  #calculate the growth decrement by uncertainty by bootstrapping BHM
  calculate_degrowth_w_uncertainty(all_across,
                                   1000,
                                   "bhm_iter")
  
  # Now loop over choice of countries to calculate total damages by sources of 
  # uncertainty
  for (isos in c("IDN", "IND", "BRA"))  {
    #### a) sensitivity under varying climate inputs to the FAIR model  #####
    # let us call the fair phi estimates under the different climate  model inputs
    # now let us calculate the bilateral damages for a given two countries for k = 
    # 1980 and k = 1990
    list_damages <- list()
    for (i in 1:900){
      damages_1990 <- as.data.frame(calc_damages_all_fair_loops(
        "USA",
        isos,
        damages_cru_1990,
        "damages_main_cru_1990",
        phi_loops_1990,
        dg_cru_deltaT_1990,
        "gdp_cru_1990",
        gdp_cru_1990,
        "dg_cru_deltaT_1990",
        1990,
        i))
      list_damages[[i]] <- damages_1990
      print(paste0(round(((900-i)/900), 2)*100, "%"))
    }
    #calculate total damages owed by USA to India
    damages_1990_loop_usa_isos <- calc_byloops("USA",
                                               isos,
                                               900,
                                               list_damages)
    
    # now let us do it for k = 1980 
    list_damages <- list()
    for (i in 1:869){
      damages_1980 <- as.data.frame(calc_damages_all_fair_loops(
        "USA",
        isos,
        damages_cru_1980,
        "damages_main_cru_1980",
        phi_loops_1980,
        dg_cru_deltaT_1980,
        "gdp_cru_1980",
        gdp_cru_1980,
        "dg_cru_deltaT_1980",
        1980,
        i))
      list_damages[[i]] <- damages_1980
      print(paste0(round(((869-i)/869), 2)*100, "%"))
    }
    #calculate total damages owed by USA to India
    damages_1980_loop_isos <- calc_byloops("USA",
                                           isos,
                                           869,
                                           list_damages)
    
    
    #### b) sensitivity under varying ensemble models  #####
    # let us calculate the total damages owed by the CGM ensemble 
    damages_1990_all_ensembles_isos <- calc_damages_by_ensembles("USA",
                                                                  isos,
                                                                  damages_cru_1990,
                                                                  "damages_ensemble_cru_1990",
                                                                  median_phi_1990,
                                                                  dg_cru_deltaT_1990,
                                                                  "gdp_cru_1990",
                                                                  gdp_cru_1990,
                                                                  "dg_cru_deltaT_1990")
    
    damages_1980_all_ensembles_isos <- calc_damages_by_ensembles("USA",
                                                                  isos,
                                                                  damages_cru_1980,
                                                                  "damages_ensemble_cru_1980",
                                                                  median_phi_1980,
                                                                  dg_cru_deltaT_1980,
                                                                  "gdp_cru_1980",
                                                                  gdp_cru_1980,
                                                                  "dg_cru_deltaT_1980")
    
    #### c) sensitivity from BHM initial conditions of MIROC6 runs  #####
    # let us calculate the total damages owed by the CGM ensemble 
    damages_1980_all_realizations_isos <- calc_damages_by_ensembles("USA",
                                                                     isos,
                                                                     damages_cru_1980,
                                                                     "damages_miroc6_cru_1980",
                                                                     median_phi_1980,
                                                                     dg_cru_deltaT_1980,
                                                                     "gdp_cru_1980",
                                                                     gdp_cru_1980,
                                                                     "dg_cru_deltaT_1980")
    
    damages_1990_all_realizations_isos <- calc_damages_by_ensembles("USA",
                                                                     isos,
                                                                     damages_cru_1990,
                                                                     "damages_miroc6_cru_1990",
                                                                     median_phi_1990,
                                                                     dg_cru_deltaT_1990,
                                                                     "gdp_cru_1990",
                                                                     gdp_cru_1990,
                                                                     "dg_cru_deltaT_1990")
    
    
    
    #### d) sensitivity from BHM model by bootstrapping coeffecient models  #####
    # let us calculate the total damages owed by the CGM ensemble 
    calc_damages_allbhm_k90_isos <- calc_damages_all_bhm_iterations("USA",
                                                                    isos, 
                                                                    "damages_bhm_iter_cru_1990",
                                                                    median_phi_1990,
                                                                    dg_cru_deltaT_1990,
                                                                    "gdp_cru_1990",
                                                                    gdp_cru_1990,
                                                                    "dg_cru_deltaT_1990")
    
    calc_damages_allbhm_k80_isos <- calc_damages_all_bhm_iterations("USA",
                                                                    isos, 
                                                                    "damages_bhm_iter_cru_1980",
                                                                    median_phi_1980,
                                                                    dg_cru_deltaT_1980,
                                                                    "gdp_cru_1980",
                                                                    gdp_cru_1980,
                                                                    "dg_cru_deltaT_1980")
    
    assign(paste0("damages_1990_loop_", isos), damages_1990_loop_usa_isos)
    assign(paste0("damages_1980_loop_", isos), damages_1980_loop_isos)
    assign(paste0("damages_1990_all_ensembles_", isos), damages_1990_all_ensembles_isos)
    assign(paste0("damages_1980_all_ensembles_", isos),damages_1980_all_ensembles_isos)
    assign(paste0("damages_1980_all_realizations_", isos),damages_1980_all_realizations_isos)
    assign(paste0("damages_1990_all_realizations_", isos),damages_1990_all_realizations_isos)
    assign(paste0("calc_damages_allbhm_k90_", isos),calc_damages_allbhm_k90_isos)
    assign(paste0("calc_damages_allbhm_k80_", isos),calc_damages_allbhm_k80_isos)
  }
  
  ##############################################################################
  ################ PART VI: Visualize Total Damages Sensitivity ################
  ##############################################################################
  
  BRA_1980 <- visualize_uncertainty(damages_1980_all_ensembles_BRA,
                                    damages_1980_all_realizations_BRA,
                                    calc_damages_allbhm_k80_BRA,
                                    damages_1980_loop_BRA,
                                    "BRA")
  BRA_1990 <- visualize_uncertainty(damages_1990_all_ensembles_BRA,
                                    damages_1990_all_realizations_BRA,
                                    calc_damages_allbhm_k90_BRA,
                                    damages_1990_loop_BRA,
                                    "BRA")
  
  IND_1980 <- visualize_uncertainty(damages_1980_all_ensembles_IND,
                                    damages_1980_all_realizations_IND,
                                    calc_damages_allbhm_k80_IND,
                                    damages_1980_loop_IND,
                                    "IND")
  IND_1990 <- visualize_uncertainty(damages_1990_all_ensembles_IND,
                                    damages_1990_all_realizations_IND,
                                    calc_damages_allbhm_k90_IND,
                                    damages_1990_loop_IND,
                                    "IND")
  
  IDN_1980 <- visualize_uncertainty(damages_1980_all_ensembles_IDN,
                                    damages_1980_all_realizations_IDN,
                                    calc_damages_allbhm_k80_IDN,
                                    damages_1980_loop_IDN,
                                    "IDN")
  IDN_1990 <- visualize_uncertainty(damages_1990_all_ensembles_IDN,
                                    damages_1990_all_realizations_IDN,
                                    calc_damages_allbhm_k90_IDN,
                                    damages_1990_loop_IDN,
                                    "IDN")
  
  fig_BRA_80 <- ggplot(BRA_1980, aes(x = total_damages, y = uncertainty)) + 
    geom_point(shape = "|", size = 12, color = "deepskyblue4") + 
    theme_classic() + geom_vline(xintercept = 0, color = "black") + 
    geom_vline(xintercept = median(BRA_1980$total_damages), color = "lightpink") +
    ggtitle("Start Year = 1980") + 
    theme_minimal() +
    scale_x_continuous(limits = c(min(IND_1980$total_damages), max(IND_1980$total_damages)), 
                       labels = label_number(suffix = " B", scale = 1e-9)) + 
    ylab("") + xlab("") + scale_y_discrete(limits=rev) +
    theme(axis.text.y = element_text(family = "Times New Roman", face = "plain", colour = "black", size = 11),
          axis.text.x = element_text(family = "Times New Roman", face = "plain", colour = "black", size = 10),
          axis.title = element_text(family = "Times New Roman", size = 12))
  #labs(caption = "red line is median total damages")  
  
  fig_BRA_90  <- ggplot(BRA_1990, aes(x = total_damages, y = uncertainty)) + 
    geom_point(shape = "|", size = 12, color = "deepskyblue4") + 
    theme_classic() + geom_vline(xintercept = 0, color = "black") + 
    geom_vline(xintercept = median(BRA_1990$total_damages), color = "lightpink") +
    ggtitle("Start Year = 1990") + 
    scale_x_continuous(limits = c(min(IND_1990$total_damages), max(IND_1990$total_damages)), 
                       labels = label_number(suffix = " B", scale = 1e-9)) + 
    theme_minimal() + 
    ylab("") + xlab("") + scale_y_discrete(limits=rev) +
    theme(axis.text.y = element_text(family = "Times New Roman", face = "plain", colour = "black", size = 11),
          axis.text.x = element_text(family = "Times New Roman", face = "plain", colour = "black", size = 10),
          axis.title = element_text(family = "Times New Roman", size = 12))
  #labs(caption = "red line is median total damages", family = "Times", size = 11)  
  
  bra <- ggpubr::ggarrange(fig_BRA_80,
                           fig_BRA_90,
                           ncol = 2,
                           nrow = 1)
  
  bra <- ggpubr::annotate_figure(bra, 
                                 left = ggpubr::text_grob("Brazil", 
                                                          family = "Times New Roman", 
                                                          rot = 90, 
                                                          size = 15, 
                                                          face = "bold"))
  
  fig_IND_80 <- ggplot(IND_1980, aes(x = total_damages, y = uncertainty)) + 
    geom_point(shape = "|", size = 12, color = "deepskyblue4") + 
    theme_classic() + geom_vline(xintercept = 0, color = "black") + 
    geom_vline(xintercept = median(IND_1980$total_damages), color = "lightpink") +
    #ggtitle("") + 
    theme_minimal() +
    scale_x_continuous(limits = c(min(IND_1980$total_damages), max(IND_1980$total_damages)), 
                       labels = label_number(suffix = " B", scale = 1e-9)) + 
    ylab("") + xlab("") + scale_y_discrete(limits=rev) +
    theme(axis.text.y = element_text(family = "Times New Roman", face = "plain", colour = "black", size = 11),
          axis.text.x = element_text(family = "Times New Roman", face = "plain", colour = "black", size = 10),
          axis.title = element_text(family = "Times New Roman", size = 12))
  #labs(caption = "red line is median total damages")  
  
  
  fig_IND_90  <- ggplot(IND_1990, aes(x = total_damages, y = uncertainty)) + 
    geom_point(shape = "|", size = 12, color = "deepskyblue4") + 
    theme_classic() + geom_vline(xintercept = 0, color = "black") + 
    geom_vline(xintercept = median(IND_1990$total_damages), color = "lightpink") +
    #ggtitle("k = 1990") + 
    scale_x_continuous(limits = c(min(IND_1990$total_damages), max(IND_1990$total_damages)), 
                       labels = label_number(suffix = " B", scale = 1e-9)) + 
    theme_minimal() + 
    ylab("") + xlab("") + scale_y_discrete(limits=rev) +
    theme(axis.text.y = element_text(family = "Times New Roman", face = "plain", colour = "black", size = 11),
          axis.text.x = element_text(family = "Times New Roman", face = "plain", colour = "black", size = 10),
          axis.title = element_text(family = "Times New Roman", size = 12))
  #labs(caption = "red line is median total damages", family = "Times", size = 11)  
  
  ind <- ggpubr::ggarrange(fig_IND_80,
                           fig_IND_90,
                           ncol = 2, 
                           nrow = 1)
  ind <- ggpubr::annotate_figure(ind, 
                                 left = ggpubr::text_grob("India", 
                                                          family = "Times New Roman", 
                                                          rot = 90, 
                                                          size = 15, 
                                                          face = "bold"))
  
  fig_IDN_80 <- ggplot(IDN_1980, aes(x = total_damages, y = uncertainty)) + 
    geom_point(shape = "|", size = 12, color = "deepskyblue4") + 
    theme_classic() + geom_vline(xintercept = 0, color = "black") + 
    geom_vline(xintercept = median(IDN_1980$total_damages), color = "lightpink") +
    #ggtitle("") + 
    theme_minimal() +
    scale_x_continuous(limits = c(min(IND_1980$total_damages), max(IND_1980$total_damages)), 
                       labels = label_number(suffix = " B", scale = 1e-9)) + 
    ylab("") + xlab("") + scale_y_discrete(limits=rev) +
    theme(axis.text.y = element_text(family = "Times New Roman", face = "plain", colour = "black", size = 11),
          axis.text.x = element_text(family = "Times New Roman", face = "plain", colour = "black", size = 10),
          axis.title = element_text(family = "Times New Roman", size = 12))
  #labs(caption = "red line is median total damages")  

  fig_IDN_90  <- ggplot(IDN_1990, aes(x = total_damages, y = uncertainty)) + 
    geom_point(shape = "|", size = 12, color = "deepskyblue4") + 
    theme_classic() + geom_vline(xintercept = 0, color = "black") + 
    geom_vline(xintercept = median(IDN_1990$total_damages), color = "lightpink") +
    #ggtitle("k = 1990") + 
    scale_x_continuous(limits = c(min(IND_1990$total_damages), max(IND_1990$total_damages)), 
                       labels = label_number(suffix = " B", scale = 1e-9)) + 
    theme_minimal() + 
    ylab("") + xlab("") + scale_y_discrete(limits=rev) +
    theme(axis.text.y = element_text(family = "Times New Roman", face = "plain", colour = "black", size = 11),
          axis.text.x = element_text(family = "Times New Roman", face = "plain", colour = "black", size = 10),
          axis.title = element_text(family = "Times New Roman", size = 12)) + 
    labs(caption = "red line is median total damages", family = "Times", size = 11)  
  
  idn <- ggpubr::ggarrange(fig_IDN_80,
                           fig_IDN_90,
                           ncol = 2, 
                           nrow = 1)
  
  idn <- ggpubr::annotate_figure(idn, 
                                 left = ggpubr::text_grob("Indonesia", 
                                                          family = "Times New Roman", 
                                                          rot = 90, 
                                                          size = 15, 
                                                          face = "bold"))
  
  all <- ggpubr::ggarrange(bra, 
                           ind, 
                           idn,
                           ncol = 1,
                           nrow = 3)
  
  all <- ggpubr::annotate_figure(all, bottom = ggpubr::text_grob("Damages in US $ (in Billions)", 
                                                                 family = "Times New Roman", 
                                                                 face = "bold",
                                                                 size = 15))
  
  
  
##############################################################################
################# PART V: Visualize Bilateral Flow of L&D  ###################
##############################################################################
  source("~/Desktop/loss_damages/scripts/6_visualize_flow_sankey.R")

  # visualize sankey flow diagrams
  for (i in c("ERA5", 
              "CRU")) {
    tic()
    for (j in c("2", "3", "5")) {
      if (i == "ERA5" & j == "2") {
        
        sankey <- visualize_bidamages(1990,
                                      i,
                                      damages_across_all_era_k_1990,
                                      weighted_damages2,
                                      j) 
      }
      else if (i == "ERA5" & j == "3") {
          sankey <- visualize_bidamages(1990,
                                        i,
                                        damages_across_all_era_k_1990,
                                        weighted_damages3,
                                        j) 
        }
      else if (i == "ERA5" & j == "5") {
          sankey <- visualize_bidamages(1990,
                                        i,
                                        damages_across_all_era_k_1990,
                                        weighted_damages5,
                                        j) 
        }
      else if (i == "CRU" & j == "2") {
          sankey <- visualize_bidamages(1990,
                                        i,
                                        damages_across_all_cru_k_1990,
                                        weighted_damages2,
                                        j) 
        }
      else if (i == "CRU" & j == "3") {
          sankey <- visualize_bidamages(1990,
                                        i,
                                        damages_across_all_cru_k_1990,
                                        weighted_damages3,
                                        j) 
        }
      else if (i == "CRU" & j == "5") {
        sankey <- visualize_bidamages(1990,
                                      i,
                                      damages_across_all_cru_k_1990,
                                      weighted_damages5,
                                      j) 
      }
      assign(paste0("damages_flow_",j,"disc_k90_", i), sankey)
    }
    toc()
  }
  

  
# visualize sankey flow diagrams
  for (i in c("ERA5", 
              "CRU")) {
    for (j in c("2", "3", "5")) {
      if (i == "ERA5" & j == "2") {
        sankey <- visualize_bidamages(1980,
                                      i,
                                      damages_across_all_era_k_1980,
                                      weighted_damages2,
                                      j) 
      }
      else if (i == "ERA5" & j == "3") {
        sankey <- visualize_bidamages(1980,
                                      i,
                                      damages_across_all_era_k_1980,
                                      weighted_damages3,
                                      j) 
      }
      else if (i == "ERA5" & j == "5") {
        sankey <- visualize_bidamages(1980,
                                      i,
                                      damages_across_all_era_k_1980,
                                      weighted_damages5,
                                      j) 
      }
      else if (i == "CRU" & j == "2") {
        sankey <- visualize_bidamages(1980,
                                      i,
                                      damages_across_all_cru_k_1980,
                                      weighted_damages2,
                                      j) 
      }
      else if (i == "CRU" & j == "3") {
        sankey <- visualize_bidamages(1980,
                                      i,
                                      damages_across_all_cru_k_1980,
                                      weighted_damages3,
                                      j) 
      }
      else if (i == "CRU" & j == "5") {
        sankey <- visualize_bidamages(1980,
                                      i,
                                      damages_across_all_cru_k_1980,
                                      weighted_damages5,
                                      j) 
      }
      assign(paste0("damages_flow_",j,"disc_k80_", i), sankey)
      }
  }
  

#end of script
