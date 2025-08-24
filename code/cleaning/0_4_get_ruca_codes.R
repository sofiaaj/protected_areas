library(tidyverse)
library(data.table)

# change to set to folder where file is located
setwd("~/Library/CloudStorage/GoogleDrive-sa9973@princeton.edu/My Drive/research/protected_areas")



library(foreach)
library(doParallel)
library(tidycensus)

cl <- makeCluster(5)
registerDoParallel(cl)

theme2 = theme_bw () +
  theme(axis.text=element_text(size=8),
        plot.subtitle=element_text(size=8),
        plot.title=element_text(size=8),
        axis.title=element_text(size=9),
        legend.text=element_text(size=9),
        legend.title=element_blank(),
        strip.text = element_text(size=8),
        base_size = 4,
        text=element_text(family="Times New Roman"))

theme_set(theme2)

get_tracts = function(statefips,yr=1990){
  library(dplyr)
  library(sf)
  curr_cb = ifelse(yr == 1990,TRUE,FALSE)
  nbhds = tigris::tracts(state = statefips,
                         cb = curr_cb,
                         year = yr) %>%
    rename_with(tolower) %>%
    st_transform(5070) %>%
    mutate(area_m2 = st_area(geometry)) %>%
    st_drop_geometry()
  if(yr == 1990){
    nbhds = nbhds %>%
      mutate(geoid = as.character(as.numeric(paste0(statefp, countyfp, tractbase,tractsuf))))
  } else{
    nbhds = nbhds %>%
      mutate(geoid = as.character(as.numeric(ctidfp00)))
  }
  nbhds = nbhds %>%
    select(geoid,area_m2)
  return(nbhds)
}

data(fips_codes)
error_states = c('AK','HI','DC')
fips = fips_codes %>% 
  select(state, state_code) %>%
  unique() %>%
  filter(state_code < 60) %>%
  filter(!state %in% error_states) %>%
  pull(state_code)

tract_1990_sizes = foreach(i = fips) %dopar% {
  get_tracts(i)
}

tract_1990_sizes = bind_rows(tract_1990_sizes)
fwrite(tract_1990_sizes,'data/intermediate/tract_1990_area.csv')

tract_2000_sizes = foreach(i = fips,.combine = rbind) %dopar% {
  get_tracts(i,2000)
}

fwrite(tract_2000_sizes,'data/intermediate/tract_2000_area.csv')

## rural-urban codes
## obtained from: https://www.ers.usda.gov/data-products/rural-urban-commuting-area-codes/
## different codes for 1990, 2000, and 2010
## combining excel files into a csv

library(tidyverse)
ruca2010 = readxl::read_xlsx('data/raw/ers_rural_urban/ruca2010revised.xlsx')
ruca2010 = ruca2010 %>%
  select(4,5) %>%
  set_names(c('geoid','ruca_code')) %>%
  mutate(geoid = as.numeric(geoid),
         ruca_code = as.numeric(ruca_code)) %>%
  filter(!is.na(geoid)) %>%
  mutate(year = 2010) 

ruca2000 = readxl::read_xls('data/raw/ers_rural_urban/ruca2000.xls')
ruca2000 = ruca2000 %>%
  select(4,5) %>%
  set_names(c('geoid','ruca_code')) %>%
  mutate(geoid = as.numeric(geoid),
         ruca_code = as.numeric(ruca_code)) %>%
  filter(!is.na(geoid)) %>%
  mutate(year = 2000) 

ruca1990 = readxl::read_xls('data/raw/ers_rural_urban/ruca1990.xls')
ruca1990 = ruca1990 %>%
  select(1,2) %>%
  set_names(c('geoid','ruca_code')) %>%
  mutate(geoid = str_remove_all(geoid,'\\.')) %>%
  mutate(geoid = as.numeric(geoid),
         ruca_code = as.numeric(ruca_code)) %>%
  filter(!is.na(geoid)) %>%
  mutate(year = 1990) 

tract_ruca = bind_rows(ruca2010,ruca2000,ruca1990)
fwrite(tract_ruca,'data/intermediate/tract_to_ruca_codes.csv')


## recall we use consistent 2010 geographies, so 1990 and 2000 ERS tracts
## must be crosswalked to 2010 tract codes
## crosswalks obtained from: https://www.nhgis.org/geographic-crosswalks
# crosswalking between 1990 and 2010

tract_ruca_1990 = tract_ruca %>%
  filter(year == 1990) %>%
  mutate(geoid = as.numeric(geoid))

cw_90_10 = fread('data/raw/ers_rural_urban/us_tract_1990_2010_crosswalk.csv') %>%
  select(tr1990gj,tr2010gj,parea) %>%
  mutate(geoid_1990 = paste0(substr(tr1990gj,2,3),substr(tr1990gj,5,7),substr(tr1990gj,9,100)),
         geoid_1990 = str_pad(geoid_1990,11,'0',side='right')) %>%
  mutate(geoid_2010 = paste0(substr(tr2010gj,2,3),substr(tr2010gj,5,7),substr(tr2010gj,9,100)),
         geoid_2010 = str_pad(geoid_2010,11,'0',side='right')) %>%
  select(geoid_1990,geoid_2010,parea) %>%
  mutate_at(vars(geoid_1990,geoid_2010),as.numeric)

# micro is called "large town" in 1990
tract_ruca_1990 = merge(tract_ruca_1990,cw_90_10,by.x='geoid',by.y='geoid_1990') %>%
  mutate(ruca_code = case_when(
    # code list available in excel files (in raw_data/ruca_codes folder)
    ruca_code < 2 ~ 'Metro: Core',
    ruca_code < 3 ~ 'Metro: High commute',
    ruca_code < 4 ~ 'Metro: Low commute',
    ruca_code < 5 ~ 'Micro: Core',
    ruca_code < 6 ~ 'Micro: High commute',
    ruca_code < 7 ~ 'Micro: Low commute',
    ruca_code < 8 ~ 'Small town: Core',
    ruca_code < 9 ~ 'Small town: High commute',
    ruca_code < 10 ~ 'Small town: Low commute',
    ruca_code < 11 ~ 'Rural',
    TRUE ~ 'Missing'
  )) 


tract_cw_descriptives = tract_ruca_1990 %>%
  group_by(geoid) %>%
  mutate(count_1990 = n()) %>%
  group_by(geoid_2010) %>%
  mutate(count_2010 = n()) %>%
  group_by(geoid_2010) %>%
  mutate(num_codes = n_distinct(ruca_code))

# single match
case1 = tract_cw_descriptives %>%
  filter(count_2010 == 1) %>%
  select(geoid_2010,ruca_code) %>%
  unique()

# multiple matches

mult_matches_unique_code = tract_cw_descriptives %>%
  filter(count_2010 > 1 & num_codes == 1) %>%
  select(geoid_2010,ruca_code) %>%
  unique()

mult_matches_unique_broad_code = tract_cw_descriptives %>%
  filter(count_2010 > 1 & num_codes > 1) %>%
  mutate(ruca_code_broad = str_split(ruca_code,':',simplify = T)[,1]) %>%
  group_by(geoid_2010) %>%
  filter(n_distinct(ruca_code_broad) == 1) %>%
  select(geoid_2010,ruca_code_broad) %>%
  unique()

# duplicates we must deal with

geo_sizes = fread('data/intermediate/tract_1990_area.csv') 
mult_matches = tract_cw_descriptives %>%
  filter(count_2010 > 1 & num_codes > 1) %>%
  merge(.,geo_sizes,by='geoid',all.x=T) %>%
  mutate(area_m2 = ifelse(is.na(area_m2),0,area_m2)) %>%
  # multiple share of 1990 tract in 2010 tract by size
  mutate(parea = parea * area_m2) %>%
  group_by(geoid_2010,ruca_code) %>%
  # choose the largest share
  summarise(parea = sum(parea)) %>%
  group_by(geoid_2010) %>%
  slice_max(parea,n=1,with_ties=F) %>%
  select(-parea)

tract_ruca_1990 = bind_rows(case1,
                            mult_matches_unique_code,
                            mult_matches) %>%
  rename(geoid = geoid_2010) 

descriptives_1990 = data.frame(
  total_tracts_2010 = tract_ruca_1990 %>% 
    select(geoid) %>%
    unique() %>%
    nrow(.),
  case1 = nrow(case1),
  mult_matches_unique_code = nrow(mult_matches_unique_code),
  mult_matches_unique_broad_code = nrow(mult_matches_unique_broad_code),
  mult_matches_all = nrow(mult_matches),
  year = 1990
)


# repeat process for 2000
tract_ruca_2000 = tract_ruca %>% 
  filter(year == 2000) %>%
  mutate(geoid = as.numeric(geoid))

cw_00_10 = fread('data/raw/ers_rural_urban/us_tract_2000_2010_crosswalk.csv') %>%
  select(tr2000gj,tr2010gj,parea) %>%
  mutate(geoid_2000 = paste0(substr(tr2000gj,2,3),substr(tr2000gj,5,7),substr(tr2000gj,9,100)),
         geoid_2000 = str_pad(geoid_2000,11,'0',side='right')) %>%
  mutate(geoid_2010 = paste0(substr(tr2010gj,2,3),substr(tr2010gj,5,7),substr(tr2010gj,9,100)),
         geoid_2010 = str_pad(geoid_2010,11,'0',side='right')) %>%
  select(geoid_2000,geoid_2010,parea) %>%
  mutate_at(vars(geoid_2000,geoid_2010),as.numeric)

tract_ruca_2000 = merge(tract_ruca_2000,cw_00_10,by.x='geoid',by.y='geoid_2000') %>%
  mutate(ruca_code = case_when(
    ruca_code < 2 ~ 'Metro: Core',
    ruca_code < 3 ~ 'Metro: High commute',
    ruca_code < 4 ~ 'Metro: Low commute',
    ruca_code < 5 ~ 'Micro: Core',
    ruca_code < 6 ~ 'Micro: High commute',
    ruca_code < 7 ~ 'Micro: Low commute',
    ruca_code < 8 ~ 'Small town: Core',
    ruca_code < 9 ~ 'Small town: High commute',
    ruca_code < 10 ~ 'Small town: Low commute',
    ruca_code < 11 ~ 'Rural',
    TRUE ~ 'Missing'
  )) 


tract_cw_descriptives = tract_ruca_2000 %>%
  group_by(geoid) %>%
  mutate(count_2000 = n()) %>%
  group_by(geoid_2010) %>%
  mutate(count_2010 = n()) %>%
  group_by(geoid_2010) %>%
  mutate(num_codes = n_distinct(ruca_code))

# single match
case1 = tract_cw_descriptives %>%
  filter(count_2010 == 1) %>%
  select(geoid_2010,ruca_code) %>%
  unique()

# multiple matches
mult_matches_unique_code = tract_cw_descriptives %>%
  filter(count_2010 > 1 & num_codes == 1) %>%
  select(geoid_2010,ruca_code) %>%
  unique()

mult_matches_unique_broad_code = tract_cw_descriptives %>%
  filter(count_2010 > 1 & num_codes > 1) %>%
  mutate(ruca_code_broad = str_split(ruca_code,':',simplify = T)[,1]) %>%
  group_by(geoid_2010) %>%
  filter(n_distinct(ruca_code_broad) == 1) %>%
  select(geoid_2010,ruca_code_broad) %>%
  unique()

# duplicates we must deal with

geo_sizes = fread('data/intermediate/tract_2000_area.csv') 
mult_matches = tract_cw_descriptives %>%
  filter(count_2010 > 1 & num_codes > 1) %>%
  merge(.,geo_sizes,by='geoid',all.x=T) %>%
  mutate(area_m2 = ifelse(is.na(area_m2),0,area_m2)) %>%
  # multiple share of 2000 tract in 2010 tract by size
  mutate(parea = parea * area_m2) %>%
  group_by(geoid_2010,ruca_code) %>%
  # choose the largest share
  summarise(parea = sum(parea)) %>%
  group_by(geoid_2010) %>%
  slice_max(parea,n=1,with_ties=F) %>%
  select(-parea)

tract_ruca_2000 = bind_rows(case1,
                            mult_matches_unique_code,
                            mult_matches) %>%
  rename(geoid = geoid_2010) 

descriptives_2000 = data.frame(
  total_tracts_2010 = tract_ruca_2000 %>% 
    select(geoid) %>%
    unique() %>%
    nrow(.),
  case1 = nrow(case1),
  mult_matches_unique_code = nrow(mult_matches_unique_code),
  mult_matches_unique_broad_code = nrow(mult_matches_unique_broad_code),
  mult_matches_all = nrow(mult_matches),
  year = 2000
)

descriptives = bind_rows(descriptives_1990,descriptives_2000)

descriptives = descriptives %>%
  mutate(mult_matches_all = mult_matches_all - mult_matches_unique_broad_code) %>%
  rename(mult_matches_other = mult_matches_all) %>%
  mutate_at(2:5,~./total_tracts_2010) %>%
  rename(case2 = mult_matches_unique_code,
         case3a = mult_matches_unique_broad_code,
         case3b = mult_matches_other) %>%
  select(-total_tracts_2010) %>%
  gather(value,share,-year)


library(cowplot)

p = descriptives %>%
  mutate(value = factor(value,levels = c('case1','case2','case3a','case3b'))) %>%
  mutate(value = recode(value,
                        case1 = 'Single match',
                        case2 = 'Multiple matches\nunique code',
                        case3a = 'Multiple matches\nunique broad code',
                        case3b = 'Multiple matches\nother')) %>%
  ggplot(aes(x = value,y = share)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~year) +
  labs(x='',
       y='Share of tracts') +
  geom_text(aes(label = scales::percent(share,accuracy=1),
                y = share + 0.04),
            size = 1.5) +
  theme_bw(base_size = 4)

img_plt <- ggdraw() +  
  draw_image('output/manual/ruca_code_processing_diagram.png')

p = plot_grid(img_plt,
          p, 
          ncol = 1)


ggsave('output/figures/process/ruca_code_processing.png',p,
       width = 5,height = 3)

# our data uses 2010 geographies so we don't need to crosswalk 2010
tract_ruca_2010 = tract_ruca %>%
  filter(year == 2010) %>%
  mutate(ruca_code = case_when(
    ruca_code < 2 ~ 'Metro: Core',
    ruca_code < 3 ~ 'Metro: High commute',
    ruca_code < 4 ~ 'Metro: Low commute',
    ruca_code < 5 ~ 'Micro: Core',
    ruca_code < 6 ~ 'Micro: High commute',
    ruca_code < 7 ~ 'Micro: Low commute',
    ruca_code < 8 ~ 'Small town: Core',
    ruca_code < 9 ~ 'Small town: High commute',
    ruca_code < 10 ~ 'Small town: Low commute',
    ruca_code < 11 ~ 'Rural',
    TRUE ~ 'Missing'
  )) %>%
  mutate(geoid = as.numeric(geoid))

tract_ruca = bind_rows(tract_ruca_1990 %>% mutate(year = 1990),
                       tract_ruca_2000 %>% mutate(year = 2000),
                       tract_ruca_2010)

fwrite(tract_ruca, 'data/intermediate/tract_to_ruca_codes_crosswalked.csv')
