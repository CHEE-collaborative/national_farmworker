library(tidyverse)
library(data.table)
library(collapse)
library(sf)
library(tigris)
library(xts)
library(zoo)

#-------------------------------------------------------------------------------------------
# Purpose: Imput counts for hired, unpaid, and migrant farmworkers in each county
#          between 2002-2023. Counties with no farmworker counts will be NA
#-------------------------------------------------------------------------------------------

# Read in farmworker counts from Agriculture Census
folder <- "ADD FOLDER PATH HERE"
file <- paste0(folder, "/01_data/AgCensus_2002_2007_2012_2017_2022.csv")
df.farmworker <- fread(file) %>%
  select(Year, State, County, `State ANSI`, `County ANSI`, `Data Item`, Value) %>%
  rename(STATENAME = State,
         COUNTYNAME = County,
         STATEFP = `State ANSI`,
         COUNTYFP = `County ANSI`,
         variable = `Data Item`, ## variable = farmworker count type (hired, unpaid, migrant)
         n_farmworkers = Value) %>%
  mutate(variable = case_when(
    str_detect(variable, "HIRED") ~ "Hired Labor",
    str_detect(variable, "UNPAID") ~ "Unpaid Labor",
    str_detect(variable, "MIGRANT") ~ "Migrant Labor (incl hired)",
  )) %>%
  mutate(n_farmworkers = as.numeric(n_farmworkers) ) %>%
  filter(!STATENAME %in% c("ALASKA", "HAWAII"))



# Function to imput farmworker counts between 2002 and 2023 for each county and farmworker type
fxn.imput <- function(df.sel){
  
  print(df.sel)
  
  state.sel <- unique(df.sel[["STATENAME"]])
  county.sel <- unique(df.sel[["COUNTYNAME"]])
  variable.sel <- unique(df.sel[["variable"]])
  
  df <- df.farmworker %>%
    filter(STATENAME == state.sel,
           COUNTYNAME == county.sel,
           variable == variable.sel) %>%
    group_by(STATENAME, COUNTYNAME, COUNTYFP, variable) %>%
    arrange(Year, .by_group = TRUE)
  
  na.check <- unique(df[["n_farmworkers"]])
  
  if(!all(is.na(na.check))){
    ts.full <- data.frame(Year = 2002:2023)
    ts <- full_join(ts.full, df) %>%
      arrange(Year) %>%
      mutate(n_farmworker_imput = na.approx(n_farmworkers, rule = 2 ) ) %>%
      fill(STATENAME, COUNTYNAME, STATEFP, COUNTYFP,  variable ) %>%
      fill(STATENAME, COUNTYNAME, STATEFP, COUNTYFP,  variable, .direction = "up" ) 
    
  } 
  
}

# Create list of unique state-county-farmworker type combinations
ls.df <- df.farmworker %>% select(STATENAME, COUNTYNAME, COUNTYFP, variable) %>% 
  unique() %>%
  mutate(n_id = 1:nrow(.))
ls.df.1 <- split(ls.df, f = ls.df$n_id)

# Apply imputation function
df.pred <- bind_rows( lapply(ls.df.1, fxn.lm) )

# Fill in missing counties
## 1. Create full grid of unique county-years (2002-2023)-farmworker type combinations
df.counties <- counties(cb = TRUE, progress_bar = FALSE, year = 2021) %>%
  st_drop_geometry() %>%
  filter(!STUSPS %in% c("AK", 'HI', "PR", "AS", "MP", "GU", "VI")) %>%
  transmute(STATEFIPS = as.numeric( STATEFP), 
            STATENAME = STATE_NAME, 
            COUNTYNAME = NAME, 
            COUNTYFP, 
            STATEABB = STUSPS, 
            GEOID = as.numeric(GEOID)) 
df.counties.fw <- expand.grid(GEOID = df.counties$GEOID, 
                              variable = c( "Hired Labor", "Migrant Labor (incl hired)", "Unpaid Labor"),
                              Year = 2002:2023)
df.counties.fw <- full_join(df.counties, df.counties.fw) %>%
  transmute(STATEFP = STATEFIPS, STATENAME = toupper(STATENAME), 
            COUNTYFP = as.numeric(COUNTYFP),
            STATEABB, GEOID, variable, Year)

## 2. Join full county-year-farmworker type grid with imputations. Counties with no FW counts will be NA
df.pred.full <- join(df.counties.fw, df.pred, 
                     on = c("STATEFP", "STATENAME", "COUNTYFP", "Year", "variable"), 
                     how = "full")

# Save file
file <- paste0(folder, "/01_data/01_farmworker_imputation_pred.csv")
fwrite(ls.pred.full, file)




