##############################################################################
# Mustafa Zahid, May 27th, 2022 
# This script calculates the grid level ration of warming 
# relative to global average area-weighted warming.
# input: CGM rasters 1850-2014, World polygon
# output:grid level warming eatio
##############################################################################

calculate_grid_warming_ratio <- function(cgm_model) {
##############################################################################
########## PART I: Extract 1995-2014 values from the cgm model ###############
##############################################################################
  # before extracting values from the rasters, let us set the empty lists which 
  # we will fill with rasters 
  list_of_hist_rasters <- list()
  list_of_nat_rasters <- list()

  # now let us loop over the historical and natural runs and run the calculations
  for (i in 145:165) {
    tic()
    j <- i - 144
    rasters_hist <- readAll(raster::brick(paste0("~/BurkeLab Dropbox/Projects/loss_damage/data/raw/cmip6/tas_Amon_", 
                                                 "historical","_", cgm_model,
                                                 "_ncecat_ann_mean.nc"),
                                          varname = "tas",
                                          level = i))
    rasters_hist <- rotate(rasters_hist)
    list_of_hist_rasters[[j]] <- rasters_hist
    
    rasters_nat <- readAll(raster::brick(paste0("~/BurkeLab Dropbox/Projects/loss_damage/data/raw/cmip6/tas_Amon_", 
                                                "hist-nat","_", cgm_model,
                                                "_ncecat_ann_mean.nc"),
                                         varname = "tas",
                                         level = i))
    rasters_nat <- rotate(rasters_nat)
    list_of_nat_rasters[[j]] <- rasters_nat
    toc()
  }

  # now let us stack the historical rasters and natural rasters separatley
  raster_hist_stack <- stack(list_of_hist_rasters)
  raster_nat_stack <- stack(list_of_nat_rasters)

  # now let us aggregate teh temperature for both historical and natural runs
  raster_hist_stack_mean <- calc(raster_hist_stack, mean)
  raster_nat_stack_mean <- calc(raster_nat_stack, mean)

  # Now let us subtract the natural average from the historical eq'n (2)
  raster_deltat <- raster_hist_stack_mean - raster_nat_stack_mean
  
  
################################################################################
######## PART II: Calculating global average area-weighted temperature #########
################################################################################

  #extract area from each grid
  area <- area(raster_deltat)
  areasf <- as.data.frame(area)
  
  # now let us extract area and temperature differential so that we can 
  # calculate global average
  example <- as.data.frame(as.matrix(rasterToPoints(area)))
  example1 <- as.data.frame(as.matrix(rasterToPoints(raster_deltat)))
  
  # chnange layer naming  
  colnames(example)[3] <- "area"
  
  # merge grid level area dataset and grid level deltat dataset
  example_merged <- left_join(example1, 
                              example,
                              by = c("x", "y"))

  # finally, calculate area-weighted global avergage (eq'n(3))
  area_weghted_deltat <- weighted.mean(example_merged$layer,
                                       w = example_merged$area)

  # now calculate the grid level ratio by dividing deltat raster by global 
  # average
  raster_deltat_calced <- raster_deltat / area_weghted_deltat
  
  #return the raster 
  #return(raster_deltat_calced)
  
  # finally, let us save the raster 
  writeRaster(raster_deltat_calced, 
              paste0("~/BurkeLab Dropbox/Projects/loss_damage/data/processed/raster_deltat_ratio_", 
                     cgm_model, ".tiff"),
              overwrite = T)
}
#end of script

