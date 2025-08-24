library(igraph)
library(haven)
library(tidyverse)
library(Matrix)
library(chattr)
library(kableExtra)
library(sf)
library(data.table)
library(tidycensus)
library(data.table)
library(tidyverse)
#library(parallel)
data(fips_codes)
geo.precision = units::set_units(1, 'meters')



get_tracts = function(statefips){
  nbhds = tigris::tracts(state = statefips,
                         year = 2010) %>%
    rename_with(tolower) %>%
    st_transform(5070) %>%
    select(-countyfp,-statefp) %>%
    set_names(str_replace_all(colnames(.),'10','')) %>%
    select(statefp,countyfp,geoid,
           aland,awater,geometry ) %>%
    filter(aland > 0) %>%
    st_set_precision(geo.precision) %>%
    st_make_valid()
  return(nbhds)
}

get_num_agreement_df = function(nbhds,statefips,robust=FALSE){
  
  state_name = fips_codes %>% 
    filter(state_code == statefips) %>% pull(state) %>% unique()
  
  filepath = ifelse(robust,
                    paste0('data/temporary_processing_files/num_agreements_robust/',state_name,'.rds'),
                    paste0('data/temporary_processing_files/num_agreements/',state_name,'.rds'))
  
  full_df = readRDS(filepath) %>%
    st_make_valid()
  
  full_df = full_df %>% mutate(id = row_number())
  ints = st_intersects(nbhds,full_df)
  df_no_geom = full_df %>% st_drop_geometry()
  
  make_agree_df = function(i){
    gid = nbhds[i,]$geoid
    curr = df_no_geom %>% filter(id %in% ints[[i]]) %>% mutate(geoid = gid)
    return(curr)
  }
  
  results_df = map_dfr(1:nrow(ints),make_agree_df)
  results_df$agree = 1
  
  all_tracts = nbhds %>% 
    st_drop_geometry() %>% 
    select(geoid) %>%
    unique() %>%
    mutate(year_est = 0,
           agree = 0,
           space_type = 'LOCAL/PRIVATE')
  
  results_df = bind_rows(all_tracts,
                         all_tracts %>% mutate(space_type = 'FEDERAL/STATE'),
                         results_df)
  
  num_agreements = results_df %>%
    group_by(geoid,space_type,year_est) %>%
    summarise(count=sum(agree))
  
  return(num_agreements)
  
}

get_acres_df = function(nbhds,statefips,robust=FALSE){
  
  state_name = fips_codes %>% 
    filter(state_code == statefips) %>% 
    pull(state) %>% 
    unique()
  
  filepath = ifelse(robust,
                    paste0('data/temporary_processing_files/acres_robust/',state_name,'.rds'),
                    paste0('data/temporary_processing_files/acres/',state_name,'.rds'))
  
  
  acres_df = readRDS(filepath) %>%
    st_make_valid() %>%
    st_buffer(0)
  
  print('Calculating spatial overlap...')
  
  perc.by.nbhd = geox::get.spatial.overlap(nbhds,acres_df,
                                           'geoid','id',
                                           filter.threshold = 0.005)
  
  # add full list of tracts
  perc.by.nbhd <- perc.by.nbhd %>%
    full_join(tibble(nbhds)[c('geoid', 'aland')])
  
  # make 0s explicit (replace NAs where there was no conserved area in
  # tract/nbhd), and add nbhd acreage
  perc.by.nbhd = perc.by.nbhd %>%
    mutate(perc.area = if_else(is.na(perc.area),
                               0, perc.area),
           nbhd.land.acres = as.numeric(units::set_units(units::set_units(aland, 'm^2'),
                                                         'acres')))
  
  # add land type and year back and rearrange
  perc.by.nbhd = perc.by.nbhd %>%
    left_join(tibble(acres_df)[c('id', 'year_est', 'space_type')],
              by = 'id') %>%
    select(-id) %>%
    select(geoid, space_type, year_est, nbhd.land.acres, perc.area) %>%
    mutate(conserved.acreage = nbhd.land.acres * perc.area)
  
  state_fed_missing = perc.by.nbhd %>%
    filter(is.na(space_type)) %>%
    mutate(space_type = 'FEDERAL/STATE')
  
  perc.by.nbhd = perc.by.nbhd %>%
    mutate(space_type = ifelse(is.na(space_type),'LOCAL/PRIVATE',space_type)) %>%
    bind_rows(.,state_fed_missing) 
  
  perc.by.nbhd = perc.by.nbhd %>% 
    mutate(year_est = ifelse(is.na(year_est),0,year_est))
  
  # combined acres are split halfway between local/private and state/federal
  combined = perc.by.nbhd %>%
    filter(space_type == 'COMBINED') %>%
    mutate_at(c('perc.area','conserved.acreage'),~./2)
  
  combined = bind_rows(combined %>% mutate(space_type = 'LOCAL/PRIVATE'),
                       combined %>% mutate(space_type = 'FEDERAL/STATE'))
  
  perc.by.nbhd = perc.by.nbhd %>% 
    filter(space_type != 'COMBINED') %>%
    bind_rows(.,combined)
  
  perc.by.nbhd = perc.by.nbhd %>%
    group_by(geoid,space_type,year_est,nbhd.land.acres) %>%
    summarise(perc.area = sum(perc.area),
              conserved.acreage = sum(conserved.acreage))
  
  return(perc.by.nbhd)
}

extend_years_acres = function(perc.by.nbhd,max_year){
  
  perc.by.nbhd = perc.by.nbhd %>%
    ungroup() %>%
    complete(nesting(geoid,nbhd.land.acres),
             space_type,fill=list(perc.area=0,conserved.acreage=0,year_est=0))
  
  all_space = perc.by.nbhd %>%
    group_by(geoid,year_est,nbhd.land.acres) %>%
    summarise(conserved.acreage = sum(conserved.acreage),
              perc.area = sum(perc.area)) %>%
    mutate(space_type = 'ALL PROTECTED')
  
  dates = perc.by.nbhd %>%
    bind_rows(.,all_space) %>%
    droplevels() %>%
    mutate(year_est = ifelse(is.na(year_est) | year_est < 1970,1970,year_est)) %>%
    group_by(geoid,space_type,year_est) %>%
    summarise(conserved.acreage = sum(conserved.acreage),
              nbhd.land.acres = mean(nbhd.land.acres)) %>%
    ungroup() %>%
    complete(nesting(geoid,nbhd.land.acres), space_type, year_est = seq(1970, max_year, 1), 
             fill=list(conserved.acreage=0)) %>%
    group_by(geoid, space_type) %>%
    mutate(cumulative_acreage = cumsum(conserved.acreage)) %>%
    mutate(perc.area = cumulative_acreage / nbhd.land.acres)
  
  overall = perc.by.nbhd %>%
    bind_rows(.,all_space) %>%
    group_by(geoid,space_type,nbhd.land.acres) %>%
    summarise(perc.area = sum(perc.area),
              conserved.acreage = sum(conserved.acreage)) %>%
    mutate(year_est = -99)
  
  final_acres = bind_rows(dates,overall)
  
  return(final_acres)
}

extend_num_agreements = function(num_agreements,max_year){
  
  all_space = num_agreements %>%
    group_by(geoid,year_est) %>%
    summarise(count = sum(count)) %>%
    mutate(space_type = 'ALL PROTECTED')
  
  dates = num_agreements %>%
    bind_rows(.,all_space) %>%
    droplevels() %>%
    mutate(year_est = ifelse(is.na(year_est) | year_est < 1970,1970,year_est)) %>%
    group_by(geoid,space_type,year_est) %>%
    summarise(count = sum(count)) %>%
    ungroup() %>%
    complete(geoid, space_type, year_est = seq(1970, max_year, 1), 
             fill=list(count=0)) %>%
    group_by(geoid, space_type) %>%
    mutate(cumulative_agreements = cumsum(count))
  
  overall = num_agreements %>%
    bind_rows(.,all_space) %>%
    group_by(geoid,space_type) %>%
    summarise(count = sum(count)) %>%
    mutate(year_est = -99)
  
  final_agreement = bind_rows(dates,overall)
  return(final_agreement)
}

merge_clean = function(final_acres,final_agreement){
  pad_nbhd_final = merge(final_acres,final_agreement,
                         by=c('geoid','space_type','year_est')) %>%
    rename(new_conserved_acreage = conserved.acreage,
           all_conserved_acreage = cumulative_acreage,
           new_agreements = count,
           all_agreements = cumulative_agreements,
           year = year_est) %>%
    mutate(all_conserved_acreage = ifelse(year == -99, 
                                          new_conserved_acreage, all_conserved_acreage),
           all_agreements = ifelse(year == -99, 
                                   new_agreements, all_agreements))
  return(pad_nbhd_final)
}


main = function(statefips,robust=FALSE){
  nbhds = get_tracts(statefips)
  print('Getting num agreements...')
  agreement_df = get_num_agreement_df(nbhds,statefips,robust)
  print('Getting acre share dataset...')
  acres_df = get_acres_df(nbhds,statefips,robust)
  max_year = 2023
  print('Extending years...')
  final_acres = extend_years_acres(acres_df,max_year)
  final_agreement = extend_num_agreements(agreement_df,max_year)
  pad_tract = merge_clean(final_acres,final_agreement)
  print('Returning processed data...')
  return(pad_tract)
}



