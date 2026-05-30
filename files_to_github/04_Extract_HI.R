library(tidyverse)
library(data.table)
library(collapse)
library(sf)
library(tigris)
library(qs)
library(terra)
library(exactextractr)
library(parallel)

options(scipen = 99999)

#-------------------------------------------------------------------------------------------
#   Purpose: Calculate daily crop-area-weighted average HI in each county by year-month
#-------------------------------------------------------------------------------------------

# Function to extract HI and calculate county daily crop-area-weighted avg HI
fxn.extract.summarise.HI<- function(year_mon_state){
  
  print(paste0("started - ", year_mon_state) )
  
  year.sel = substr(year_mon_state, 1, 4)
  mon.sel = substr(year_mon_state, 5, 6)
  yearmon.sel = paste0(year.sel, mon.sel)
  state = substr(year_mon_state, 8,9)
  
    #--------------------------------------------------------------------------------------------------
    #                          Extract daily HI to each crop vector in given year-month-state
    #                             1. label vector as "crop" vs "non-crop"
    #                             2. Extract HI values to each vector
    #--------------------------------------------------------------------------------------------------
    # Read in CSB crop year-state shp (.qs extraction)
    x <- "ADD PATH HERE" 
    file = paste0(x, "/01_data/00_CSB_cleaned/", "00_CSB_crop_HH_class_", year.sel, "_", state, ".qs")
    sf.crop <- qread(file)  %>%
      mutate(crop_id = 1:nrow(.)) %>%
      # label vector as "crop" (1) or "non-crop" (0)
      mutate(crop_class = case_when(
        crop_class == 0 ~ 0,
        crop_class %in% c(1, 2) ~ 1
      ))
    
    sf.crop <- st_transform(sf.crop , "EPSG:5070") ## change projection
    
    
    # Read in HI data
    folder <- "ADD PATH HERE"
    file.list <- list.files(paste0(folder, "/01_data/01_HI_month", full.names = T))
    files <- file.list[str_detect(file.list, paste0(yearmon.sel, ".tif") )]
    rast.HI <- project(rast(files), "EPSG:5070")
    
    # Extract average daily HI in each crop vector, bind to crop sf, pivot to long format and add date columns
    sf.county.HI <- exact_extract(rast.HI, sf.crop, 'mean',
                                  progress = FALSE) %>%
      bind_cols( sf.crop )  %>%
      rename(geometry = Shape) %>%
      pivot_longer(names_to = "yearmonday",
                   values_to = "HI_crop_mean",
                   cols = 1:(ncol(.)-13)) %>%
      fmutate(yearmonday = substr(yearmonday, 6, nchar(yearmonday)),
              year = as.numeric(substr(yearmonday, 1,4)),
              month = substr(yearmonday, 5,6),
              month.abb = month.abb[as.numeric(month)])  %>%
      relocate(geometry, .after = last_col() )
    
    print(paste0("extracted - ", year_mon_state) )
    
    
    #--------------------------------------------------------------------------------------------------
    #                   Calculate county daily crop-area-weighted avg HI for each crop class
    #--------------------------------------------------------------------------------------------------
    df.HI <- sf.county.HI %>%
      st_drop_geometry() %>%
      rename(CNTYNAME = NAME)  %>%
      fgroup_by(GEOID, STATEFIPS, STATEABB, CNTYFIPS, CNTYNAME, Year, month, month.abb, yearmonday, crop_class)  %>%
      fmutate(total_county_crop_area = fsum(Shp_area_m2 , na.rm = TRUE) , 
              weight = Shp_area_m2 / total_county_crop_area
      ) %>%
      ungroup() %>%
      fgroup_by(GEOID, STATEFIPS, STATEABB, CNTYFIPS, CNTYNAME, Year, month, month.abb, yearmonday, crop_class, total_county_crop_area) %>%
      fsummarise(mean.HI.wtd = fsum(HI_crop_mean * weight, na.rm = TRUE),
                 mean.HI.unwtd = fmean(HI_crop_mean, na.rm = TRUE)) %>%
      ungroup()
    #--------------------------------------------------------------------------------------------------
    
    ## save results
    folder = "ADD PATH HERE"
    file = paste0(folder, "/01_data/02_CSB_HI_daily_county/02_CSB_HI_summary_", year_mon_state ,".csv")
    fwrite(df.HI, file)
    
    print(paste0("saved - ", year_mon_state) )
    
    return(NULL)
  }


# Create list of year-state combinations
chr.list <- do.call(paste0, expand.grid(2008:2023, str_pad(1:12, width = 2, pad = "0", side = "left" )
                                        , "_", state.abb[!state.abb %in% c( "AK", 'HI', "PR", "AS", "MP", "GU", "VI")]))
chr.list <- sort(chr.list)
# year_mon_state = "200801_AZ"


# run function in parallel
n_cores <- 15
mclapply(chr.list ,
         fxn.extract.summarise.HI,
         mc.cores = n_cores)
closeAllConnections()





