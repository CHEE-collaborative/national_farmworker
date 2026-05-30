library(tidyverse)
library(data.table)
library(collapse)
library(sf)
library(tigris)
library(parallel)

options(scipen = 99999)


#-------------------------------------------------------------------------------------------
#        Purpose: Harmonize farmworkers, HI, HI-smoke files for analysis
#-------------------------------------------------------------------------------------------

folder <- "ADD PATH HERE"

#-------------------------Farmworkers---------------------------------------------------
file <- paste0(folder, "/01_data/01_farmworker_imputation_pred.csv")
df.farmworker <- fread(file) 

df.farmworker.clean <- df.farmworker %>%
  arrange(STATENAME, Year) %>%
  transmute(Year,
            STATEFIPS = STATEFP,
            STATEABB,
            STATENAME = str_to_title(STATENAME),
            CNTYFIPS = COUNTYFP,
            GEOID = as.numeric(STATEFP*1000 + COUNTYFP),
            variable, n_farmworkers, n_farmworker_imput  )  %>%
  filter(!STATENAME %in% c("Alaska", "Hawaii")) %>%
  mutate(region_2 = case_when(
    STATEABB %in% c("CT", "ME", "MA", "NH", "VT", "RI") ~ "New England",
    STATEABB %in% c("NY", "PA", "NJ") ~ "Middle Atlantic",
    STATEABB %in% c("OH", "IN", "IL", "MI", "WI") ~ "East North Central",
    STATEABB %in% c("DE", "MD", "DC", "VA", "WV", "NC", "SC", "GA", "FL") ~ "South Atlantic",
    STATEABB %in% c("KY", "TN", "AL", "MS") ~ "East South Central",
    STATEABB %in% c("TX", "LA", "AR", "OK") ~ "West South Central",
    STATEABB %in% c("ND", "MN", "SD", "IA", "NE", "KS", "MO") ~ "West North Central",
    STATEABB %in% c("MT", "WY", "ID", "NV", "UT", "CO", "AZ", "NM" ) ~ "Mountain",
    STATEABB %in% c("WA", "OR", "CA") ~ "Pacific"
  ),
  region_1 = case_when(
    region_2 %in% c("New England", "Middle Atlantic" ) ~ "Northeast",
    region_2 %in% c("East North Central", "West North Central") ~ "Midwest",
    region_2 %in% c("South Atlantic", "East South Central", "West South Central") ~ "South",
    region_2 %in% c("Mountain", "Pacific") ~ "West"
  )) 


file <- paste0(folder, "/01a_final data/01_farmworker_imputation_pred.rds")
saveRDS(df.farmworker.clean, file)




#-------------------------Heat Index------------------------------------------------
folder.HI = paste0(folder, "/01_data/02_CSB_HI_daily_county")
ls.files <- list.files(folder.HI, full.names = T)
df.HI <- bind_rows(lapply(ls.files, function(x) fread(x) %>% mutate(crop_class = as.character(crop_class))))

df.HI.f<- df.HI %>%
  mutate(region_2 = case_when(
    STATEABB %in% c("CT", "ME", "MA", "NH", "VT", "RI") ~ "New England",
    STATEABB %in% c("NY", "PA", "NJ") ~ "Middle Atlantic",
    STATEABB %in% c("OH", "IN", "IL", "MI", "WI") ~ "East North Central",
    STATEABB %in% c("DE", "MD", "DC", "VA", "WV", "NC", "SC", "GA", "FL") ~ "South Atlantic",
    STATEABB %in% c("KY", "TN", "AL", "MS") ~ "East South Central",
    STATEABB %in% c("TX", "LA", "AR", "OK") ~ "West South Central",
    STATEABB %in% c("ND", "MN", "SD", "IA", "NE", "KS", "MO") ~ "West North Central",
    STATEABB %in% c("MT", "WY", "ID", "NV", "UT", "CO", "AZ", "NM" ) ~ "Mountain",
    STATEABB %in% c("WA", "OR", "CA") ~ "Pacific"
  ),
  region_1 = case_when(
    region_2 %in% c("New England", "Middle Atlantic" ) ~ "Northeast",
    region_2 %in% c("East North Central", "West North Central") ~ "Midwest",
    region_2 %in% c("South Atlantic", "East South Central", "West South Central") ~ "South",
    region_2 %in% c("Mountain", "Pacific") ~ "West"
  )) 

file = paste0(folder, "/01a_final data/02_HI_2008-2023.rds")
saveRDS(df.HI.f, file)

  

#-------------------------HI and WF Smoke coexposure-----------------------------------
folder.HI.smoke = paste0(folder, "/01_data/03_CSB_HI_smoke_county")
ls.files <- list.files(folder.HI.smoke, full.names = T)
df.HI.smoke <- bind_rows(lapply(ls.files, function(x) fread(x) 
                                %>% mutate(crop_class = as.character(crop_class))))

df.HI.smoke.f<- df.HI.smoke %>% 
  mutate(region_2 = case_when(
    STATEABB %in% c("CT", "ME", "MA", "NH", "VT", "RI") ~ "New England",
    STATEABB %in% c("NY", "PA", "NJ") ~ "Middle Atlantic",
    STATEABB %in% c("OH", "IN", "IL", "MI", "WI") ~ "East North Central",
    STATEABB %in% c("DE", "MD", "DC", "VA", "WV", "NC", "SC", "GA", "FL") ~ "South Atlantic",
    STATEABB %in% c("KY", "TN", "AL", "MS") ~ "East South Central",
    STATEABB %in% c("TX", "LA", "AR", "OK") ~ "West South Central",
    STATEABB %in% c("ND", "MN", "SD", "IA", "NE", "KS", "MO") ~ "West North Central",
    STATEABB %in% c("MT", "WY", "ID", "NV", "UT", "CO", "AZ", "NM" ) ~ "Mountain",
    STATEABB %in% c("WA", "OR", "CA") ~ "Pacific"
  ),
  region_1 = case_when(
    region_2 %in% c("New England", "Middle Atlantic" ) ~ "Northeast",
    region_2 %in% c("East North Central", "West North Central") ~ "Midwest",
    region_2 %in% c("South Atlantic", "East South Central", "West South Central") ~ "South",
    region_2 %in% c("Mountain", "Pacific") ~ "West"
  )) 

file = paste0(folder, "/01a_final data/03_HI_smoke_2008-2023.rds")
saveRDS(df.HI.smoke.f, file)










