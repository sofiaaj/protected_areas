library(tidyverse)
library(broom)
library(gridExtra)
library(tidyr)
library(kableExtra)
library(data.table)
library(parallel)
library(doParallel)
library(fixest)
setwd("~/Library/CloudStorage/GoogleDrive-sa9973@princeton.edu/My Drive/research/protected_areas")

# 1980, 1990, 2000 using decennial
# 2005 - 2021 using ACS 5-year estimates (140 census tract geographies)
# 
# Characteristics:
# Total population
# Population density
# Urban/Rural (not in ACS)
# Race and Hispanic or Latino
# Household type (married or not, % family households are married)
# Educational attainment for 25 years and over (highest earned, so lower cats don't include upper cats)
# Unemployment rate
# % in labor force
# Household income --> converted to categories using income
# Median household income
# Median household income by race
# Cumulative (making more than) household income brackets (eg. % houses making more than 10k)
# Owner vs renter occupied units
# Year-Round housing units
# Maybe calculate non-occupied housing units (occupancy and vacancy status)
# Median Year Structure Built
# Median house value
# Rent as percent of household income
# Median gross rent
# % living in poverty for population age 18 to 64
# 
# all characteristics are on 2010 geographies
# data pulled from social explorer
# report example: https://www.socialexplorer.com/reports/socialexplorer/en/report/867995ec-4eb6-11ef-a70a-a7ea2c7d2040

setwd("~/Library/CloudStorage/GoogleDrive-sa9973@princeton.edu/My Drive/research/protected_areas")
tract_vars = readxl::read_xlsx('data/manual/tract_master_codebook.xlsx')

get_tract_data = function(yr){
  print(yr)
  tc = fread(paste0('data/raw/historical_tract_characteristics/tract_characteristics_', yr, '.csv'))
  if(yr < 2007){
    curr_vars = tract_vars %>% filter(year == yr)
  }else{
    curr_vars = tract_vars %>% filter(year == 2007)
  }
  df = tc %>% select(curr_vars$var_old) %>% set_names(curr_vars$var_new) %>% mutate(year = yr)
  df = process_vars(df,yr)
  return(df)
}


process_vars = function(df,yr){
  df = df %>%
    mutate(white_hisp_pct = white_or_hispanic/total_pop,
           black_hisp_pct = black_or_hispanic/total_pop,
           white_pct = white_alone/total_pop,
           black_pct = black_alone/total_pop,
           asian_pct = asian_alone/total_pop,
           hispanic_pct = hispanic/total_pop,
           unemployed_pct = unemployed/population_in_labor_force,
           owner_pct = owner_occupied_housing_units/occupied_housing_units,
           occupied_pct = occupied_housing_units/year_round_housing_units)
  if(yr == 1980){
    df = df %>%
      mutate(less_than_hs = (elementary_school+some_hs)/persons_25_or_older,
             hs_or_ged = all_hs/persons_25_or_older,
             some_college = some_college/persons_25_or_older,
             # note: we assume college completion if have 4 or more years in college
             bs_or_higher = all_college/persons_25_or_older)
  } else {
    df = df %>%
      mutate(poverty_pct = pop_under_poverty/total_pop_with_poverty_status,
             less_than_hs = less_than_hs/persons_25_or_older,
             hs_or_ged = hs_or_ged/persons_25_or_older,
             some_college = some_college/persons_25_or_older)
    if(yr == 1990){
      df = df %>%
        mutate(bs_or_higher = (bachelors+more_than_bs)/persons_25_or_older)
      
    }else{
      df = df %>%
        mutate(bs_or_higher = (bachelors+masters+prof_degree+doctorate)/persons_25_or_older,
               # not a perfect comparison
               rent_income_percent_35_or_more = rent_income_percent_30_49+rent_income_percent_50_or_more)
    }
  }
  df = df %>%
    # defining rent burdened as 35 (or 30) percent or more of income spent on rent
  mutate(rent_burden_pct = rent_income_percent_35_or_more/renter_occupied_housing_units)
  df = df %>%
    select(-any_of(c("white_or_hispanic", "black_or_hispanic", "white_alone", "black_alone", "asian_alone", 
                    "hispanic", "unemployed", "population_in_labor_force", "owner_occupied_housing_units", 
                    "occupied_housing_units", "year_round_housing_units", "elementary_school", "some_hs", 
                    "all_hs", "some_college", "all_college", "bachelors", "more_than_bs", "masters", 
                    "prof_degree", "doctorate", "rent_income_percent_30_49", "rent_income_percent_50_or_more",
                    "rent_income_percent_35_or_more",
                    "pop_under_poverty", "total_pop_with_poverty_status", "renter_occupied_housing_units")))
  
  return(df)
}

# removing 2007 for now because it's on 2000 geographies
# 2018 and 2019 are on 2020 geographies so we ignore for now
tract_characteristics = map_dfr(c(1980,1990,2000,2008:2017), get_tract_data)

# income percentiles obtained using mcib procedure
percentiles = fread('data/intermediate/income_percentiles_1980_2017.csv') %>%
  filter(percentile %in% c('p33','p66')) %>%
  mutate(percentile = paste0('national_percentile_',percentile)) %>%
  spread(percentile,r_mcib)


tract_characteristics = merge(tract_characteristics, percentiles, by = "year")

#tract categories:
#Race/ethnicity: Majority non-Hispanic white, 
#Majority non-Hispanic Black, 
#Majority Hispanic, 
#Majority other
#Income: High-income (majority is at 75th percentile of the national income distribution or above), 
#Low- income (majority is below 25th percentile of national income distribution or below), 
#Mixed income (all other)
#Then also just do one where we use tracts to calculate percentiles

tract_characteristics = tract_characteristics %>%
  mutate(other_pct = 1-white_pct-black_pct-asian_pct-hispanic_pct) %>%
  mutate(tc_race_1 = case_when(
    is.na(white_pct) ~ 'NA',
    white_pct >= black_pct & white_pct >= hispanic_pct & white_pct >= asian_pct & white_pct >= other_pct ~ 'majority_white',
    black_pct >= white_pct & black_pct >= hispanic_pct & black_pct >= asian_pct & black_pct >= other_pct ~ 'majority_black',
    hispanic_pct >= white_pct & hispanic_pct >= black_pct & hispanic_pct >= asian_pct & hispanic_pct >= other_pct ~ 'majority_hispanic',
    year > 1980 & asian_pct >= white_pct & asian_pct >= black_pct & asian_pct >= hispanic_pct & asian_pct >= other_pct ~ 'majority_asian',
    TRUE ~ 'other'
  )) 

tract_characteristics = tract_characteristics %>%
  mutate(tc_race_2 = case_when(
    is.na(white_pct) ~ 'NA',
    white_pct >= 0.5 ~ 'majority_white',
    black_pct >= 0.5 ~ 'majority_black',
    hispanic_pct >= 0.5 ~ 'majority_hispanic',
    year > 1980 & asian_pct >= 0.5 ~ 'majority_asian',
    TRUE ~ 'other'
  )) 

tract_characteristics = tract_characteristics %>%
  mutate(tc_income_1 = case_when(
    is.na(median_household_income) ~ "NA",
    median_household_income <= national_percentile_p33 ~ "low_income",
    median_household_income <= national_percentile_p66 ~ "middle_income",
    median_household_income > national_percentile_p66 ~ "high_income"
  )) 

tract_characteristics = tract_characteristics %>%
  group_by(year) %>%
  mutate(tc_income_2 = ntile(median_household_income,n=3),
         tc_race_black = ntile(black_pct,n=3),
         tc_race_hispanic = ntile(hispanic_pct,n=3),
         tc_race_white = ntile(white_pct,n=3),
         tc_ownership = ntile(owner_pct,n=3),
         tc_home_value = ntile(median_house_value,n=3))


# rural-urban codes from the Economic Research Service
# see get_ruca_codes.R for code

tract_ruca = fread('data/intermediate/tract_to_ruca_codes_crosswalked.csv')

# use 1990 codes for 1980, 1990
# use 2000 codes for 2000
# use 2010 codes for 2008-2017
tract_ruca_1990 = tract_ruca %>% 
  filter(year == 1990) %>%
  select(-year)
tract_ruca_2000 = tract_ruca %>% 
  filter(year == 2000) %>%
  select(-year)
tract_ruca_2010 = tract_ruca %>%
  filter(year == 2010) %>%
  select(-year)

tract_characteristics_1980_1990 = tract_characteristics %>%
  filter(year < 2000) %>%
  mutate(geoid = as.numeric(geoid)) %>%
  merge(.,tract_ruca_1990,by='geoid',all.x=T) 

tract_characteristics_2000 = tract_characteristics %>%
  filter(year == 2000) %>%
  mutate(geoid = as.numeric(geoid)) %>%
  merge(.,tract_ruca_2000,by='geoid',all.x=T) 

tract_characteristics_2008_2017 = tract_characteristics %>%
  filter(year > 2000) %>%
  mutate(geoid = as.numeric(geoid)) %>%
  merge(.,tract_ruca_2010,by='geoid',all.x=T) 

tract_characteristics = bind_rows(tract_characteristics_1980_1990,
                                  tract_characteristics_2000,
                                  tract_characteristics_2008_2017) 

fwrite(tract_characteristics, 'data/derived/tract_characteristics_1980_2017.csv')

