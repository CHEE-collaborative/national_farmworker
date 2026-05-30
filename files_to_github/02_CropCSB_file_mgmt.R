library(tidyverse)
library(data.table)
library(collapse)
library(sf)
library(tigris)
library(qs)
library(parallel)

#-------------------------------------------------------------------------------------------
# Purpose: Data management - clean and organize USDA Cropland CSB polygon shapefiles into year and state shapefiles
#          with land area categorized as "crop" vs "non-crop"
#-------------------------------------------------------------------------------------------


## Function to clean USDA Cropland CSB shp to state and year files and assign crop vs non-crop value
fxn.clean.csb <- function(st_year) {
  
  st = substr(st_year, 1,2) ## state FIPS
  year.csb = substr(st_year, 4,5) ## end year of CSB file
  
  print(paste0("started - ", st_year))
  
  # 1. Read CSB data - query rows only for a given state
  folder <- "ADD FOLDER PATH HERE"
  x <- paste0(folder, "01_data/001_Cropland_CROS_CSB")
  ## Select geodatabase (gdb) file name within CSB folder
  file <- list.files(list.files(x, full.names = T, pattern = paste0(year.csb, "_rev23")), 
                     full.names = T, pattern = "gdb" )
  ## Read in rows only in given state (st)
  layername = st_layers(file)$name
  statefips = st
  query.sel = paste0("SELECT * FROM ", layername ," WHERE STATEFIPS = '", statefips ,"'")
  sf.csb <- st_read(file, query =  query.sel)
  
  # 2. Filter all US counties to only those in given state - this is standard county information (FIPS, names, etc)
  df.state.sel <- df.counties %>%
    filter(STATEFP == statefips)
  state.abb.1 <- unique(df.state.sel$STATEABB) ## get state abbreviation 
  
  
  # 3. Clean CSB file
  sf.csb.clean <- sf.csb %>%
    select(-c(INSIDE_X, INSIDE_Y, Shape_Length, ASD, CSBID, CSBYEARS)) %>%
    ## Create GEOID
    fmutate(GEOID = paste0(STATEFIPS, CNTYFIPS)) %>% 
    ## Join to County information
    left_join(., df.state.sel, by = c("STATEFIPS" = "STATEFP", "GEOID")) %>%
    select(GEOID, STATEFIPS,  NAME, STATEABB, CNTYFIPS, NAME, Shape_Area, CSBACRES, starts_with("CDL")) %>%
    ## Pivot data to long format
    pivot_longer(names_to = "Year",
                 values_to = "crop_value",
                 cols = 8:15) %>%
    rename(Shp_area_m2 = Shape_Area) %>%
    ## clean year
    fmutate(Year = substr(Year, 4,8))  %>%
    ## join to crop values dataframe
    left_join(., df.crop, by = c("crop_value"))
  
  # select unique years in CSB file
  ls.years <- funique(sf.csb.clean$Year)
  
  ## apply save function to save CSB crop shp for each state and year
  lapply(ls.years, fxn.save, df = sf.csb.clean, x = x, state = state.abb.1)
}


## Function to save CSB crop shp for each state and year
fxn.save <- function(i, df, x, state ) {
  print(paste0('save - ', i, "_", state) )
  
  # Filter state sf for given year
  sf.save <- df %>%
    filter(Year == i)
  
  # Save State-Year-CSB crop shp as a .qs file in folder
  ## .qs filetype was used to optimize data compression. 
  ## Unfortunately, no longer available on CRAN
  ## Need to download .tar.gz file and install manually or use different file type
  
  x = "ADD PATH HERE"
  file = paste0(x, "/01_data/00_CSB_cleaned/", "00_CSB_crop_HH_class_", i, "_", state, ".qs")
  print(file)
  qsave(x = sf.save, file)
}


## df of all counties in the US - used in function
df.counties <- counties(cb = TRUE, progress_bar = FALSE, year = 2021) %>%
  st_drop_geometry() %>%
  filter(!STUSPS %in% c("AK", 'HI', "PR", "AS", "MP", "GU", "VI")) %>%
  transmute(STATEFP, NAME, STATEABB = STUSPS, GEOID)

## Create dataframe of all states with end year of CSB file
fips.state <- unique(df.counties$STATEFP)
csb.years = c(15,20,24)
ls.st_year = do.call(paste0, expand.grid(fips.state, "_", csb.years))

### test values
# i = "06_20"
# fxn.clean.csb(i)

## dataframe of crop classification
non_hand_harvested <- c( "Corn",  "Cotton", "Rice",  "Sorghum",
                         "Soybeans", "Sunflower", "Peanuts", "Tobacco", "Sweet Corn",  
                         "Pop or Orn Corn",  "Barley", "Durum Wheat", "Spring Wheat", 
                         "Winter Wheat", "Other Small Grains",  "Dbl Crop WinWht/Soybeans", 
                         "Rye", "Oats",  "Millet", "Speltz",  "Canola", "Flaxseed",  "Safflower", 
                         "Rape Seed",  "Mustard",  "Alfalfa",  "Other Hay/Non Alfalfa",  "Camelina", 
                         "Buckwheat", "Hops",  "Herbs",  "Clover/Wildflowers", "Sod/Grass Seed", 
                         "Dbl Crop WinWht/Corn",  "Dbl Crop Oats/Corn", "Dbl Crop Triticale/Corn",
                         "Dbl Crop Lettuce/Durum Wht",  "Dbl Crop Durum Wht/Sorghum", 
                         "Dbl Crop Barley/Sorghum", "Dbl Crop WinWht/Sorghum", "Dbl Crop Barley/Corn",  
                         "Dbl Crop WinWht/Cotton", "Dbl Crop Soybeans/Cotton",  "Dbl Crop Soybeans/Oats",
                         "Dbl Crop Corn/Soybeans", "Dbl Crop Barley/Soybeans", "Triticale",
                         "Switchgrass",  "Fallow/Idle Cropland", "Sugarbeets", "Dry Beans", "Potatoes", "Other Crops",
                         "Sugarcane", "Sweet Potatoes" )
other_areas <- c(   "Forest",  "Shrubland", "Background",
                    "Barren", "Clouds/No Data",  "Developed",  "Water",  "Wetlands", 
                    "Nonag/Undefined", "Aquaculture", "Open Water",  "Perennial Ice/Snow",  
                    "Developed/Open Space", "Developed/Low Intensity", "Developed/Med Intensity",
                    "Developed/High Intensity",  "Barren", "Deciduous Forest",  "Evergreen Forest",
                    "Mixed Forest",  "Shrubland",  "Grassland/Pasture",  "Woody Wetlands",
                    "Herbaceous Wetlands" , "Pasture/Grass" )
hand_harvested <- c("Mint",  "Misc Vegs & Fruits",  "Watermelons", 
                    "Onions", "Cucumbers", "Chick Peas", "Lentils", "Peas", "Tomatoes",
                    "Cranberries", "Hops", "Herbs", "Cherries", "Peaches", "Apples", "Grapes",
                    "Christmas Trees", "Other Tree Crops", "Citrus", "Pecans", "Almonds", 
                    "Walnuts", "Pears", "Pistachios", "Carrots", "Asparagus", "Garlic", 
                    "Cantaloupes", "Prunes", "Olives", "Oranges", "Honeydew Melons", 
                    "Broccoli", "Avocados", "Peppers", "Pomegranates", "Nectarines",
                    "Greens", "Plums", "Strawberries", "Squash", "Apricots", "Vetch",
                    "Lettuce", "Pumpkins", "Dbl Crop Lettuce/Durum Wht",
                    "Dbl Crop Lettuce/Cantaloupe", "Dbl Crop Lettuce/Cotton", 
                    "Dbl Crop Lettuce/Barley", "Blueberries", "Cabbage", "Cauliflower",
                    "Celery", "Radishes", "Turnips", "Eggplants", "Gourds")
folder = "ADD PATH HERE"
y <- paste0(folder, "01_data/crop_codes.csv")
df.crop <- fread(y) %>%
  mutate(crop_class = case_when(
    crop_cat %in% non_hand_harvested ~ 2,
    crop_cat %in% hand_harvested ~ 1,
    crop_cat %in% other_areas ~ 0
  ))

# Run in parallel
n_cores <- 15
mclapply(ls.st_year ,
         fxn.clean.csb ,
         mc.cores = n_cores)
closeAllConnections()

