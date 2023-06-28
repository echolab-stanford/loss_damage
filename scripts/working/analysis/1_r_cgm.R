##############################################################################
# Mustafa Zahid, Aug 10th, 2022 
# This script calculates the grid level ration of warming 
# relative to global average area-weighted warming.
# input: CGM historical rasters 1850-2014, ssp370 future projections,
# World polygon
# output:grid level warming ratio per each model
# Last updated: January 2023
##############################################################################


# so now the goal is to get a grid level warming ratio 
calculate_grid_warming_ratio <- function(run1, run2, cgm_model) {
  ##############################################################################
  ############### PART I: Extract values from the cgm model ####################
  ##############################################################################
  # read the ssp3 raster values
  raster_ssp3 <- readAll(raster::brick(paste0("data/raw/historical_ssp370/tas_Amon_", run1, 
                                              "_", cgm_model, "_ann_mean_2pt5degree.nc"),
                          varname = "tas"))
  # maintain the last decade as the average to be subtracted from
  raster_eoc <- raster_ssp3[[75:86]]
  # now rotate so that we have coordinates that are -180,180,-90,90
  raster_eoc <- rotate(raster_eoc)
  
  # now let us do the historical runs of the models
  raster_hist <- readAll(raster::brick(paste0("data/raw/historical_ssp370/tas_Amon_", run2, 
                                              "_",cgm_model, "_ann_mean_2pt5degree.nc"),
                                       varname = "tas"))
  
  # keep the first 50 years (1850-1900) as the baseline
  raster_boc <- raster_hist[[1:51]]
  # now rotate so that we have coordinates that are -180,180,-90,90
  raster_boc <- rotate(raster_boc)
  
  # now we can aggregate both set of rasters 
  raster_hist_mean <- calc(raster_boc, mean)
  raster_ssp3_mean <- calc(raster_eoc, mean)
  
  # Now let us subtract the historical (1850-1900) average from the ssp3
  # (2090-2100) average
  raster_deltat <- raster_ssp3_mean - raster_hist_mean
  #plot(raster_deltat)
  
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
  return(raster_deltat_calced)
  
  # finally, let us save the raster (we can write the rasters out to have a -
  # permanent version, otherwise just leave it commented out)
  #writeRaster(raster_deltat_calced, 
   #           paste0("data/processed/raster_deltat_ratio_", 
    #                 cgm_model, ".tiff"),
     #         overwrite = T)
}
#end of script

