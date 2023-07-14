##############################################################################
# Mustafa Zahid, March 28th, 2022
# This script is to read all the needed libraries
# Last updated: June 2023
##############################################################################
#remove(list=ls())

#Let us download the needed packages 
# Data cleaning
packs <- c("tidyr", 
           "tidyverse", 
           "dplyr", 
           "plyr",
           "statar",
           "reshape",
           "data.table",
           "zoo",
           "parallel",
           "tictoc",
           "lubridate",
           "sp", 
           "readxl",
           "sf", 
           "raster", 
           "terra", 
           "rgdal", 
           "rgeos", 
           "tmap", 
           "spData",
           "chron",
           "ncdf4",
           "maptools",
           "scales",
           "maptools",
           "hrbrthemes",
           "ggthemes",
           "scales",
           "MetBrewer",
           "WDI",
           "wbstats",
           "maptools",
           "scales",
           "countrycode",
           "ggalluvial",
           "MetBrewer",
           "RColorBrewer",
           "shadowtext",
           "doParallel",
           "foreach",
           "gt",
           "gtable"
           )


#for (i in 1:length(packs)){
#  install.packages(packs[[i]])
#  print(i)
#}



# calling the packages
lapply(packs, 
       library, 
       character.only = TRUE)


# run any functions that could be needed in the future
gt_theme_538 <- function(data,...) {
  data %>%
    opt_all_caps()  %>%
    opt_table_font(
      font = list(
        google_font("Chivo"),
        default_fonts()
      )
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "bottom", color = "transparent", weight = px(2)
      ),
      locations = cells_body(
        columns = TRUE,
        # This is a relatively sneaky way of changing the bottom border
        # Regardless of data size
        rows = nrow(data$`_data`)
      )
    )  %>% 
    tab_options(
      column_labels.background.color = "white",
      table.border.top.width = px(3),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "black",
      data_row.padding = px(3),
      source_notes.font.size = 12,
      table.font.size = 16,
      heading.align = "left",
      ...
    ) 
}



# create a function to add a unit letter next to the total amount displayed 
addUnits <- function(n) {
  labels <- ifelse(n < -1e12, paste0(round(n/-1e12), 'T'), # negative trillions (benefits)
                   ifelse(n < 1000, n,  # less than thousands
                          ifelse(n < 1e6, paste0(round(n/1e3), 'k'),  # in thousands
                                 ifelse(n < 1e9, paste0(round(n/1e6), 'M'),  # in millions
                                        ifelse(n < 1e12, paste0(round(n/1e9), 'B'), # in billions
                                               ifelse(n < 1e15, paste0(round(n/1e12), 'T'),# in trillions
                                                      'too big!'
                                               ))))))
  return(labels)
}



# set paths 
dropbox_path <- "~/BurkeLab Dropbox/Projects/loss_damage/"
# get user login 
user <- as.character(Sys.info()["user"])

# create a file in the processed data directory 
dir.create(file.path(paste0("figures/", run_date)))
dir.create(file.path(paste0(dropbox_path, "data/output/", run_date)))
#dir.create(file.path(paste0(dropbox_path, "data/figures/", run_date)))
dir.create(file.path(paste0(getwd(), "/data/figures/", run_date)))
dir.create(file.path(paste0(dropbox_path, "/data/processed/", run_date)))

output_path <- paste0(dropbox_path, "data/output/")
processed_path <- paste0(dropbox_path, "data/processed/")
raw_path <- paste0(dropbox_path, "data/raw/")
fig_prepped_dta <- paste0(getwd(), "/data/figures/")

# end of script
