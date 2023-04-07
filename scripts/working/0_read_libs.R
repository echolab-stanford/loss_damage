##############################################################################
# Mustafa Zahid, March 28th, 2022
# This script is to read all the needed libraries
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
           "WDI",
           "scales",
           "countrycode",
           "ggalluvial",
           "MetBrewer",
           "RColorBrewer"
           )
# calling the packages
lapply(packs, 
       library, 
       character.only = TRUE)

# call specific packages from github
devtools::install_github("vincentarelbundock/WDI")

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


library("readxl")

