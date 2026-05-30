library(tidyverse)
library(sf)
library(data.table)
library(terra)
library(tigris)
library(doParallel)
library(foreach)

options(scipen =  99999)


#-------------------------------------------------------------------------------------------
#   Purpose: Calculate daily Heat Index (HI) for 800m PRISM climate rasters
#-------------------------------------------------------------------------------------------

# Function to calculate daily heat index
fxn.HI <- function(year_sel){
  print(pattern_year)
  
  # create date pattern in PRISM files (ex. *20080101_bil.bil, *20080102_bil.bil, *20080103_bil.bil,)
  pattern_year =  paste0(year_sel, "_bil.bil$")
  
  # select files for specified dates (pattern_year) in the list of TMAX and VPDMAX filenames
  files.list.tmax_sel <- files.list.tmax[str_detect(files.list.tmax, paste(pattern_year, collapse = "|"))]
  tmax.names_sel <- substring(files.list.tmax_sel, 80, 87)
  
  
  files.list.vpdmax_sel <- files.list.vpdmax[str_detect(files.list.vpdmax, paste(pattern_year, collapse = "|"))]
  vpdmax.names_sel <- substr(files.list.vpdmax_sel, 84, 91)
  
  
  # Read in selected tempmax and VPDmax raster files (ie. all days in a given year-month)
  rast.tmax <- rast(files.list.tmax_sel)
  names(rast.tmax) <- tmax.names_sel
  
  rast.vpdmax <- rast(lapply(files.list.vpdmax_sel, rast))
  names(rast.vpdmax) <- vpdmax.names_sel
  
  #------------------------------ HI calculation - Daily max HI --------------------------------#
  # HI calculation - Daily HI
  # source: https://www.wpc.ncep.noaa.gov/html/heatindex_equation.shtml
  
  # calculate minimum relative humidity
  rast.RHmin <- (610.94 * exp((17.625 * rast.tmax)/(243.04+rast.tmax)) - rast.vpdmax * 100) / (610.94 *exp((17.625*rast.tmax)/(243.04 + rast.tmax)) )
  rast.RHmin <- rast.RHmin * 100
  
  # calculate maximum heat index
  rast.tmax.F <- rast.tmax * 9/5 + 32
  rast.HImax <-  0.5*(rast.tmax.F + 61 + (1.2*(rast.tmax.F - 68)) + 0.094*rast.RHmin )
  
  # adjust maximum heat index
  rast.HImax.2.F <- ifel( rast.HImax >= 80,
                          -42.379 + 2.04901523*rast.tmax.F + 10.14333127*rast.RHmin - .22475541*rast.tmax.F*rast.RHmin - .00683783*rast.tmax.F*rast.tmax.F - .05481717*rast.RHmin*rast.RHmin + .00122874*rast.tmax.F*rast.tmax.F*rast.RHmin + .00085282*rast.tmax.F*rast.RHmin*rast.RHmin - .00000199*rast.tmax.F*rast.tmax.F*rast.RHmin*rast.RHmin, # rast.HImax
                          ifel(rast.HImax >= 80 & rast.HImax < 112 & rast.RHmin < 13,
                               rast.HImax - (((13 - rast.RHmin )/4) * sqrt((17-abs(rast.tmax.F-0.95))/17)),
                               ifel(rast.HImax >= 80 & rast.HImax < 87 & rast.RHmin > 85,
                                    rast.HImax + ((rast.RHmin - 85) / 10) * ((87-rast.tmax.F)/5),
                                    rast.HImax
                               )
                          )
  )
  
  # Name each layer (vpdmax and tmax file names are the same)
  names(rast.HImax.2.F) <- vpdmax.names_sel
  
  # save daily HI raster for each year 
  file_sel.name <- unique( substr(year_sel, 1, 6) )
  folder = "ADD PATH HERE"
  file <- paste0(folder, "/01_data/01_HI_month/PRISM_HI_", file_sel.name, ".tif")
  writeRaster(rast.HImax.2.F, file, overwrite = TRUE)
  
  return(NULL)
  
}


#### folders should have 365*17+5
folder = "ADD PRISM DATA PATH HERE"
# list of all filenames in the TMAX folder
folder.tmax <- paste0(folder, "/00_PRISM/PRISM_tmax")
files.list.tmax <- list.files(folder.tmax, pattern = ".bil$", full.names = T) # n = 6210
tmax.names <- list.files(folder.tmax, pattern = ".bil$")


# list of all filenames in the VPDMAX folder
folder.vpdmax <- paste0(folder, "/00_PRISM/PRISM_vpdmax")
files.list.vpdmax <- list.files(folder.vpdmax, pattern = ".bil$", full.names = T)  # n = 6210
vpdmax.names <- list.files(folder.vpdmax, pattern = ".bil$")



# run WBGT calculation
## List of all year-month combinations between 2008-2023 (ex. 200801)
ls.year <- do.call(paste0, expand.grid(str_pad(2008:2023, width = 2, side = "left", pad = 0),
                                       str_pad(2, width = 2, side = "left", pad = 0)))

ls.year <- ls.year[order(ls.year)]
n_cores <- 20
cluster <- makeCluster(n_cores)
registerDoParallel(cluster)

## Parallel structure: For each year-month combination, run fxn.HI for all days in each month
ls.HI <- ls()
ls.HI <- foreach(i = ls.year,
                   .packages = c("terra", "stringr", "lubridate")) %dopar%{
                     print(i)
                    
                     # Get list of all days in a given month (ex. 20080101, 20080102, etc)
                     if (as.numeric(substr(i, 5,6)) == 2 & as.numeric(substr(i, 1,4)) %in% c(2008, 2012, 2016, 2020) ){
                       ls.months_days =  str_replace_all( as.character(seq.Date(ymd(paste0(i, "01")), ymd(paste0(i, "29")), by = "day")), "-", "" )
                     } else if (as.numeric(substr(i, 5,6)) == 2 & !(as.numeric(substr(i, 1,4)) %in% c(2008, 2012, 2016, 2020, 2024) ) ){
                       ls.months_days =  str_replace_all( as.character(seq.Date(ymd(paste0(i, "01")), ymd(paste0(i, "28")), by = "day")), "-", "" )
                     } else if(as.numeric(substr(i, 5,6)) %in% c(4,6,9,11)){
                       ls.months_days =  str_replace_all( as.character(seq.Date(ymd(paste0(i, "01")), ymd(paste0(i, "30")), by = "day")), "-", "" )
                     } else {
                       ls.months_days =  str_replace_all( as.character(seq.Date(ymd(paste0(i, "01")), ymd(paste0(i, "31")), by = "day")), "-", "" )
                     }
                     
                     # Calculate HI
                     fxn.HI(ls.months_days) 
                   }

stopCluster(cl = cluster)


