library(tidyverse)
library(data.table)
library(collapse)
library(sf)
library(tigris)
library(openxlsx)

options(scipen = 99999)


#-------------------------------------------------------------------------------------------------------
#         PURPOSE: Extract HI-WF smoke co-exposure data to use in Emerging Hotspot Analysis in ArcGIS
#-------------------------------------------------------------------------------------------------------

folder = "ADD PATH HERE"

# Read in farmworker file
file <- paste0(folder, "/01_data/01_farmworker_imputation_pred.csv")
df.farmworker <- fread(file) %>%
  mutate(variable = case_when(
    variable %in% c("Hired Labor", "Unpaid Labor") ~ "Hired+Unpaid Labor",
    variable == "Migrant Labor (incl hired)" ~ "Migrant Labor"
  )) %>%
  group_by(Year, STATEFIPS, STATEABB, STATENAME, CNTYFIPS, GEOID, region_2, region_1, variable) %>%
  summarise(across(c(n_farmworkers, n_farmworker_imput), ~sum(.)))


# Read in HI-WF smoke file
file = paste0(folder, "/01a_final data/03_HI_smoke_2008-2023.csv")
df.HI.smoke <- fread(file ) %>%
  mutate(region_2 = factor(region_2, levels = c(  "East North Central" , "Middle Atlantic",   "New England" ,
                                                  "West South Central", "East South Central" , "South Atlantic",
                                                  "Pacific", "Mountain", "West North Central" ))) %>%
  mutate(month.abb = fct_reorder(month.abb, month))


# Load counties sf (from tigris)
sf.counties <- counties(cb = TRUE, progress_bar = FALSE, year = 2021) %>%
  filter(!STUSPS %in% c("AK", 'HI', "PR", "AS", "MP", "GU", "VI")) %>%
  transmute(STATEFIPS = as.numeric( STATEFP), CNTYNAME = NAME, STATEABB = STUSPS, GEOID = as.numeric(GEOID)) %>%
  st_make_valid() %>%
  mutate(GEOID = paste0("G", GEOID)) %>%
  st_transform(5070)



# Calculate county FW-coexposure days (HI > thresholds (80F, 90F, 100F, 110F) + WF smoke)
df.hi.WF.nDays.ann <- df.HI.smoke %>%
  ## create flag for coexposure days
  mutate(coexpos = if_else(smokePM_pred.wtd > 0 , "coexpos_days", "no coexpos"),
         coexpos = if_else(is.na(smokePM_pred.wtd), "no coexpos", coexpos)) %>%
  select(region_2, region_1, GEOID, STATEFIPS, STATEABB, CNTYFIPS, CNTYNAME, Year, month, month.abb, yearmonday, crop_class, total_county_crop_area, mean.HI.wtd, smokePM_pred.wtd, coexpos) %>%
  ## Count annual days with HI above threshold for each county-crop class-coexpos status
  fgroup_by( Year, GEOID,  crop_class, coexpos) %>%
  fsummarise(n_days_80F = fsum(mean.HI.wtd >=80),
             n_days_90F = fsum(mean.HI.wtd >=90),
             n_days_100F = fsum(mean.HI.wtd >=100),
             n_days_110F = fsum(mean.HI.wtd >=110)
             )  %>%
  ## Long format
  pivot_longer(names_to = "thresholds",
               values_to = "n Days >= threshold HI",
               cols = -c( GEOID, Year, crop_class, coexpos)) %>%
  mutate(`thresholds` = factor(`thresholds`, 
                               levels = c("n_days_80F",
                                          "n_days_90F",
                                          "n_days_100F",
                                          "n_days_110F"))) %>%
  ## Add farmworker counts
  full_join(., df.farmworker %>% filter(variable == "Hired+Unpaid Labor")) %>%
  ## Calculate annual FW-coexposure days
  mutate(farmworker_days = n_farmworker_imput * `n Days >= threshold HI`) %>%
  select(-c(n_farmworkers, n_farmworker_imput, `n Days >= threshold HI`))  %>%
  ## filter for +crops, coexposure days only
  filter(crop_class == 1,
         coexpos == "coexpos_days") %>%
  ## Prepare df for ArcGIS
  pivot_wider(names_from = thresholds,
              values_from = farmworker_days,
              names_prefix = "fw_") %>%
  mutate(GEOID1 = GEOID,
         GEOID = paste0("G", GEOID)) %>%
  select(Year, GEOID, GEOID1, everything())



# Join to sf.counties to create shp file; Assume NA FW-coexpos days are 0 days of exposure
sf.counties.fwdays <- full_join(sf.counties, df.hi.WF.nDays.ann ) %>%
  mutate(across(14:17, ~if_else(is.na(.), 0, .)))

# Save FW-coexposure days df
file = paste0(folder, "01a_final data/04_datacube_counties_FW_coexpos_natl.shp")
st_write(sf.counties.fwdays, filename, append = FALSE)





