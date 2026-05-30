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
#   Purpose: Calculate daily crop-area weighted wildfire smoke PM2.5 (from Childs et al)
#     for each county by year-month
#-------------------------------------------------------------------------------------------



fxn.extract.HI.smoke<- function(year_mon_state){
  print(paste0("started - ", year_mon_state))
  
  year.sel = substr(year_mon_state, 1, 4)
  mon.sel = substr(year_mon_state, 5, 6)
  yearmon.sel = paste0(year.sel, mon.sel)
  state = substr(year_mon_state, 8,9)
  
    
  #--------------------------------------------------------------------------------------------------
  #                          Extract smoke PM2.5 to each crop vector
  #                             1. Label vector as "crop" vs "non-crop"
  #                             2. Intersect CSB crop and WF grid geometries
  #                             3. Match WF smoke PM2.5 predictions on smoke days to grid cell-days
  #                             4. Join to crop-WF grid intersected shp
  #--------------------------------------------------------------------------------------------------
      # Read in crop data
      x <- "ADD PATH HERE"
      file = paste0(x, "/01_data/00_CSB_cleaned/", "00_CSB_crop_HH_class_", year.sel, "_", state, ".qs")
      sf.crop <- qread(file)  %>%
        mutate(crop_id = 1:nrow(.)) %>%
        # label vector as "crop" (1) or "non-crop" (0)
        mutate(crop_class = case_when(
          crop_class == 0 ~ 0,
          crop_class %in% c(1, 2) ~ 1
          ))
      
      sf.crop <- st_transform(sf.crop , "EPSG:5070") #align projection
      
      ## 1. Intersect crop class sf with WF grid sf
      sf.crop.grid <- st_intersection(sf.crop %>% st_make_valid(), grid_10km) %>%
        select(-c(COORDX, COORDY)) 
      
      print(paste0("intersect - ", year_mon_state))
      
      qsave(sf.crop.grid, file.crop)
    

      ## 2. Match WF smoke PM2.5 predictions on smoke days to WF grid cell-days
      ### Get sequence of days for each month-year combination
      mon.sel <- as.numeric(mon.sel)
      if (mon.sel == 2 & year.sel %in% c(2008, 2012, 2016, 2020, 2024) ){
        dates = seq.Date(ymd(paste0(yearmon.sel, "01")), ymd(paste0(yearmon.sel, "29")), by = "day")
      } else if (mon.sel == 2 & !year.sel %in% c(2008, 2012, 2016, 2020, 2024)  ){
        dates = seq.Date(ymd(paste0(yearmon.sel, "01")), ymd(paste0(yearmon.sel, "28")), by = "day")
      } else if(mon.sel %in% c(4,6,9,11)){
        dates = seq.Date(ymd(paste0(yearmon.sel, "01")), ymd(paste0(yearmon.sel, "30")), by = "day")
      } else {
        dates = seq.Date(ymd(paste0(yearmon.sel, "01")), ymd(paste0(yearmon.sel, "31")), by = "day")
      }
      
      ### Get full combination of grid cell-days
      out = expand.grid(grid_ID = unique( sf.crop.grid[["grid_ID"]]), date = dates) 
      
      ### Filter predictions for year-month and create grid_ID
      preds.1 <- preds %>%
        fmutate(date_filter = paste0(year(date), str_pad(month(date), width = 2, pad = "0", side = "left" ) )) %>%
        filter(date_filter == yearmon.sel) %>%
        rename(grid_ID = grid_id_10km) %>%
        filter(grid_ID %in% unique(out$grid_ID))
      
      ### Join WF grid cell-days wtih smokePM predictions
      df.out <- join( x = out, y = preds.1,
                      on = c("grid_ID", "date"),
                      how = "left") %>%
        fmutate(smokePM_pred = replace_na(smokePM_pred, 0) ,
                date_filter = paste0(year(date), str_pad(month(date), width = 2, pad = "0", side = "left" ) )) %>%
        fmutate(Year = as.character(year(date)),
                month = str_pad(month(date), pad = "0", side = "left", width =2 ))
      
      ## 3. Join WF smoke predictions with crop-WF grid
      df.county.smoke <- join(x = sf.crop.grid %>% st_drop_geometry(), y = df.out,
                              on = "grid_ID", 
                              how = "left",
                              multiple = TRUE) %>%
        fmutate(yearmonday = gsub("-", "", as.character(date)) )
      
      
      print(paste0("extracted smoke - ", year_mon_state))
    
    #--------------------------------------------------------------------------------------------------
    #                    Extract daily HI to each crop vector in given year-month-state
    #--------------------------------------------------------------------------------------------------
    # Read in HI data
    file.list <- list.files( "/gpfs/gibbs/project/carrion/jp3343/01_WBGT_month", full.names = T)
    files <- file.list[str_detect(file.list, paste0(yearmon.sel, ".tif") )]
    rast.HI <- project(rast(files), "EPSG:5070")
    
    # Extract average daily HI in each crop vector, bind to crop sf, pivot to long format and add date columns
    df.county.HI <- exact_extract(rast.HI,  sf.crop.grid, 'mean',
                                  progress = FALSE) %>%
      bind_cols(  sf.crop.grid %>% st_drop_geometry()  )  %>%
      pivot_longer(names_to = "yearmonday",
                   values_to = "HI_crop_mean",
                   cols = 1:(ncol(.)-13)) %>%
      fmutate(yearmonday = substr(yearmonday, 6, nchar(yearmonday)),
              year = as.numeric(substr(yearmonday, 1,4)),
              month = substr(yearmonday, 5,6),
              month.abb = month.abb[as.numeric(month)])  
    
    print(paste0("HI extracted - ", year_mon_state))
    
    
    #--------------------------------------------------------------------------------------------------
    #           Calculate daily county crop-area-weighted (and unweighted) HI and WF smoke PM2.5
    #                 - crop-area-weighted avg HI
    #                 - crop-area-weighted avg PM2.5
    #--------------------------------------------------------------------------------------------------
    
    # 1. Join HI df and smoke df
    df.HI.smoke <- join(x = df.county.HI, y = df.county.smoke,
                        on = c("GEOID", "STATEFIPS", "NAME", "STATEABB",
                               "CNTYFIPS", "Shp_area_m2", "CSBACRES", "Year", "crop_value", "crop_cat",
                               "crop_class", "crop_id", "grid_ID", "yearmonday", "month"), 
                        how = "full") %>%
      transmute(GEOID, STATEFIPS, STATEABB, 
                CNTYFIPS, CNTYNAME = NAME, Year, month, month.abb, yearmonday, 
                Shp_area_m2, crop_class, crop_id, grid_ID, HI_crop_mean, smokePM_pred )
    
    # 2. Calculate daily county and crop-class crop-area-weighted and unweighted averages for each county
    df.county.HI.smoke <-  df.HI.smoke  %>%
      st_drop_geometry() %>%
      fgroup_by(GEOID, STATEFIPS, STATEABB, CNTYFIPS, CNTYNAME, Year, month, month.abb, yearmonday, crop_class)  %>%
      fmutate(total_county_crop_area = fsum(Shp_area_m2 , na.rm = TRUE) , 
              weight = Shp_area_m2 / total_county_crop_area
      ) %>%
      ungroup() %>%
      fgroup_by(GEOID, STATEFIPS, STATEABB, CNTYFIPS, CNTYNAME, Year, month, month.abb, yearmonday, crop_class, total_county_crop_area) %>%
      fsummarise(mean.HI.wtd = fsum(HI_crop_mean * weight, na.rm = TRUE),
                 mean.HI.unwtd = fmean(HI_crop_mean, na.rm = TRUE),
                 smokePM_pred.wtd = fsum(smokePM_pred  * weight, na.rm = TRUE ),
                 smokePM_pred.unwtd = fmean(smokePM_pred, na.rm = TRUE)) %>%
      ungroup()
    #--------------------------------------------------------------------------------------------------
    
    
    # Save results
    folder = "ADD PATH HERE"
    file = paste0(folder, "/01_data/03_CSB_HI_smoke_county/03_CSB_HI_SMOKE_", year_mon_state ,".csv")
    fwrite(  df.county.HI.smoke , file)
    
    print(paste0("saved - ", year_mon_state) )
    
    return(NULL)
  }
  


# Extract WF smoke PM2.5
## Load 10 km grid
folder = "ADD PATH HERE"
file.shp <- paste0(folder, "/01_data/00_WF_smoke/10km_grid_wgs84.shp")
grid_10km = st_read(file.shp, quiet = TRUE) %>%
  st_transform("EPSG:5070") %>%
  rename(grid_ID = ID)

## Load WF smoke PM2.5 predictions
file <- paste0(folder, "/01_data/00_WF_smoke/smokePM2pt5_predictions_daily_10km_20060101-20231231.rds")
preds =  readRDS(file) 



# Create list of year-state combinations
chr.list <- do.call(paste0, expand.grid(2008:2023, str_pad(1:12, width = 2, pad = "0", side = "left" )
                                        , "_", state.abb[!state.abb %in% c( "AK", 'HI', "PR", "AS", "MP", "GU", "VI")]) %>%
                      arrange(Var1, Var2))

# run function in parallel
n_cores <- 20
mclapply(chr.list,  
         function(item) {
           tryCatch({
             fxn.extract.HI.smoke(item)
           }, error = function(e) {
             message(paste("Error processing item:", item, "-", e$message))
             print(paste0("ERROR - ", item))
             return(NULL) # Return NA or NULL on error
           })
         },
         mc.cores =  n_cores )
closeAllConnections()
