##############################################################################
# Mustafa Zahid, March 28th, 2022
# This script is to read all the needed libraries
##############################################################################
remove(list=ls())

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
           "wbstats")
# calling the packages
lapply(packs, 
       library, 
       character.only = TRUE)
