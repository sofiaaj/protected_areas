library(data.table)
library(stringr)
library(tidyverse)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
setwd("~/Library/CloudStorage/GoogleDrive-sa9973@princeton.edu/My Drive/research/protected_areas")

process_state_file = function(file){
  state_name = str_extract(file,'[A-Z]{2}')
  df = fread(file) %>%
    mutate(state = state_name)
  return(df)
}

tract_characteristics = fread('data/derived/tract_characteristics_1980_2017.csv') %>%
  mutate(geoid = as.numeric(geoid)) %>%
  mutate(year = ifelse(year <= 2000, year, year + 2))


process_data = function(robust=F){
  
  if(robust){
    path = 'data/intermediate/processed_pad_tracts_by_state/robust'
  } else {
    path = 'data/intermediate/processed_pad_tracts_by_state/main'
  }
  df = map_dfr(list.files(path,full.names = TRUE),
               process_state_file)
  fwrite(df,ifelse(robust,
                   'data/derived/all_state_tracts_robust.csv',
                   'data/derived/all_state_tracts.csv'))
  
  df = df %>%
    filter(year != -99 & year >= 1980 & year <= 2019) %>%
    mutate(geoid = as.numeric(geoid))
  
  with_chars = df %>%
    filter(year %in% c(1980,1990,2000,2010:2019))
  
  with_chars = merge(with_chars, tract_characteristics, by = c("geoid","year"),all.x=T)
  
  pa_final = bind_rows(with_chars %>% mutate(has_covariates = 1),
                       df %>% filter(!year %in% c(1980,1990,2000,2010:2019)) %>% 
                         mutate(has_covariates = 0))
  
  
  pa_final = pa_final %>%
    mutate(ruca_code_1 = str_extract(ruca_code,'^[^:]+')) %>%
    mutate(across(where(is.character), ~ na_if(.,""))) %>%
    filter(!state %in% c('AK','HI','DC')) %>% 
    filter(nbhd.land.acres != 0) #%>%
    #mutate(perc.area = all_conserved_acreage/nbhd.land.acres) %>%
    #group_by(space_type,year) %>%
    #mutate(upper = quantile(all_agreements,0.975)) %>%
    #mutate(all_agreements = ifelse(all_agreements > upper, upper, all_agreements)) %>%
    #group_by(space_type,year) %>%
    #mutate(upper = quantile(perc.area,0.975)) %>%
    #mutate(perc.area = ifelse(perc.area > upper, upper, perc.area)) 
  
  fwrite(pa_final, ifelse(robust,
                         'data/derived/protected_areas_master_file_clean_robust.csv',
                         'data/derived/protected_areas_master_file_clean.csv'))
}

process_data()
process_data(robust = TRUE)




 

