library(tidyverse)
library(broom)
library(gridExtra)
library(tidyr)
library(kableExtra)
library(data.table)
library(patchwork)
library(fixest)
library(sf)
library(foreach)

theme = theme_bw () + theme(axis.text=element_text(size=8),
                           plot.subtitle=element_text(size=9),
                           axis.title=element_text(size=9),
                           legend.text=element_text(size=9),
                           legend.title=element_blank(),
                           strip.text = element_text(size=8))
theme_set(theme)

ROBUST = FALSE
setwd("~/Library/CloudStorage/GoogleDrive-sa9973@princeton.edu/My Drive/research/protected_areas")

#from SOCDS
county_permits = fread('data/raw/construction_permits/res_construction_by_county.csv')
county_permits = county_permits %>%
  mutate(STATE = STUSAB) %>%
  select(GEOID,STATE,contains('permit')) %>%
  gather(permit_type,count,-GEOID,-STATE) %>%
  mutate(year = as.numeric(str_extract(permit_type, "\\d{4}")),
         permit_type = str_remove(permit_type, "_\\d{4}")) %>%
  filter(year <= 2019) %>%
  mutate(count = ifelse(is.na(count), 0, count)) %>%
  spread(permit_type, count) %>%
  rename(geoid = GEOID) %>%
  mutate(geoid = str_pad(as.character(geoid), 5, pad = "0")) %>%
  rename_with(tolower) 

if(ROBUST){
  pa_all = fread("data/derived/protected_areas_master_file_clean_robust.csv")
  FIG_ROOT_DIR = "output/figures/construction_permits/robust/"
  TAB_ROOT_DIR = "output/tables/construction_permits/robust/"
}else{
  pa_all = fread("data/derived/protected_areas_master_file_clean.csv")
  FIG_ROOT_DIR = "output/figures/construction_permits/main/"
  TAB_ROOT_DIR = "output/tables/construction_permits/main/"
}

if(!ROBUST){
  
p = county_permits %>%
  group_by(year,state) %>%
  summarise(all_permits = sum(all_permits)) %>%
  summarise(all_permits = mean(all_permits)/1000) %>%
  ggplot(aes(x=year,y=all_permits)) +
  geom_line() +
  labs(x = 'Year', y = 'Permits (in thousands)',
       subtitle = 'Avg. Residential Construction Permits by State')

# not in paper
#ggsave(paste0(FIG_ROOT_DIR,'permits_over_time.png'),
#       height = 3,width = 5)

#https://dataverse.harvard.edu/file.xhtml?fileId=10775159&version=1.0
county_pops = fread('data/raw/construction_permits/historical_county_populations.csv')
county_pops = county_pops %>%
  select(-cty) %>%
  gather(year_dec,population,-cty_fips) %>%
  mutate(year_dec = as.numeric(str_extract(year_dec, "\\d{4}"))) %>%
  mutate(cty_fips = str_pad(as.character(cty_fips), 5, pad = "0")) 

county_shape = tigris::counties(year=2010)

county_shape = county_shape %>%
  select(GEOID10,NAMELSAD10,geometry) %>%
  rename(geoid = GEOID10) %>%
  st_as_sf() %>%
  st_transform(5070)

map_plot = county_permits %>%
  mutate(year_dec = round(year/10)*10) %>%
  merge(., county_pops, by.x = c('geoid','year_dec'), by.y = c('cty_fips','year_dec')) %>%
  filter(!state %in% c('AK','HI')) %>%
  filter(year >= 2015) %>%
  group_by(state,geoid,year) %>%
  summarise(permits_per_capita = all_permits / population) %>%
  summarise(permits_per_1000 = mean(permits_per_capita, na.rm = TRUE)*1000) %>%
  merge(., county_shape, by = 'geoid') 

p = ggplot() +
  geom_sf(data=map_plot,
          aes(geometry=geometry,fill = permits_per_1000), color = NA) +
  theme_void() +
  scale_fill_viridis_c(option = 'C',direction=-1,name = 'Permits per 1000 residents') +
  labs(subtitle='Residential Construction Permits, 2015-2019') +
  theme(legend.position = 'bottom')

#not in paper
#ggsave(paste0(FIG_ROOT_DIR,'permits_map.png'),p,width=8,height=6)  

}


county_pa = pa_all %>%
  mutate(geoid = str_pad(as.character(geoid), 11, pad = "0")) %>%
  mutate(county = substr(geoid, 1, 5)) %>%
  group_by(county,state,year,space_type) %>%
  summarise(total_agreements = sum(all_agreements, na.rm = TRUE),
            share_protected_area = sum(all_conserved_acreage)/sum(nbhd.land.acres)) %>%
  rename(geoid = county)

county_pa = merge(county_pa, county_permits %>% select(-state), by = c('geoid','year'))

county_pa = county_pa %>%
  mutate(share_protected_area = share_protected_area * 100) 

mod1.1 = feols(all_permits ~ share_protected_area,
               data=county_pa %>% filter(space_type == 'LOCAL/PRIVATE'),
               cluster = 'geoid')
mod1.2 = feols(all_permits ~ share_protected_area | year^state,
               data=county_pa %>% filter(space_type == 'LOCAL/PRIVATE'),
               cluster = 'geoid')
mod1.3 = feols(all_permits ~ share_protected_area | geoid + year^state,
               data=county_pa %>% filter(space_type == 'LOCAL/PRIVATE'),
               cluster = 'geoid')

mod2.1 = feols(all_permits ~ share_protected_area,
               data=county_pa %>% filter(space_type == 'FEDERAL/STATE'),
               cluster = 'geoid')
mod2.2 = feols(all_permits ~ share_protected_area | year^state,
               data=county_pa %>% filter(space_type == 'FEDERAL/STATE'),
               cluster = 'geoid')
mod2.3 = feols(all_permits ~ share_protected_area | geoid + year^state,
               data=county_pa %>% filter(space_type == 'FEDERAL/STATE'),
               cluster = 'geoid')

curr_dict = c("share_protected_area"    = "% Acres Protected")

esttex(mod1.1,mod1.2,mod1.3,drop = c("Constant"),dict = curr_dict,
       style.tex = style.tex(main="aer",signif.code=c("**"=0.01, "*"=0.05))) %>%
  writeLines(.,paste0(TAB_ROOT_DIR,'permit_county_mod_loc_priv.tex'))

esttex(mod2.1,mod2.2,mod2.3,drop = c("Constant"),dict = curr_dict,
       style.tex = style.tex(main="aer",signif.code=c("**"=0.01, "*"=0.05))) %>%
  writeLines(.,paste0(TAB_ROOT_DIR,'permit_county_mod_fed_st.tex'))

clean_table = readLines(paste0(TAB_ROOT_DIR,'clean_permits_table_template.tex'))
clean_table = paste(clean_table, collapse = "\n")
tab1 = readLines(paste0(TAB_ROOT_DIR,'permit_county_mod_loc_priv.tex'))
tab2 = readLines(paste0(TAB_ROOT_DIR,'permit_county_mod_fed_st.tex'))

start_line = which(str_detect(tab1, 'Acres Protected'))
end_line = which(str_detect(tab1, 'Within R'))
tab1 = tab1[start_line:end_line]
tab1  = paste(tab1, collapse = '\n')
out <- sub("TK_TABLE_1", tab1, clean_table, fixed = TRUE)

start_line = which(str_detect(tab2, 'Acres Protected'))
end_line = which(str_detect(tab2, 'Within R'))
tab2 = tab2[start_line:end_line]
tab2  = paste(tab2, collapse = '\n')
out <- sub("TK_TABLE_2", tab2, out, fixed = TRUE)

writeLines(out, paste0(TAB_ROOT_DIR,"permits_results.tex"))

