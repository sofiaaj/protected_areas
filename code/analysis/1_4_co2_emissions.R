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

theme = theme_bw () +theme(axis.text=element_text(size=8),
                           plot.subtitle=element_text(size=9),
                           axis.title=element_text(size=9),
                           legend.text=element_text(size=9),
                           legend.title=element_blank(),
                           strip.text = element_text(size=8))
theme_set(theme)

robust = F
setwd("~/Library/CloudStorage/GoogleDrive-sa9973@princeton.edu/My Drive/research/protected_areas")

# data in grams, converting to metric tons
emissions = fread('data/raw/co2_emissions/emissions_by_tract.csv') %>%
  mutate_at(vars(contains("emissions")), ~ ./1e6) %>%
  rename_with(tolower)

emissions_std = emissions %>%
  select(contains('total_emissions')) %>%
  scale() %>%
  as.data.frame() %>%
  rename_with(~ paste0(.,'_zscore'))

emissions = cbind(emissions,emissions_std)

emissions_county = emissions %>%
  mutate(geoid = as.character(geoid),
         geoid = str_pad(geoid, 11, pad = "0"),
         county = substr(geoid, 1, 5))  %>%
  group_by(county) %>%
  summarise(total_emissions = mean(total_emissions),
            transportation_emissions = mean(transportation_total_emissions)) %>%
  rename(geoid = county) %>%
  mutate(emissions_quintile = ntile(total_emissions,5))


if(robust){
  pa_all = fread("data/derived/protected_areas_master_file_clean_robust.csv")
  FIG_ROOT_DIR = "output/figures/co2_emissions/robust/"
  TAB_ROOT_DIR = "output/tables/co2_emissions/robust/"
}else{
  pa_all = fread("data/derived/protected_areas_master_file_clean.csv")
  FIG_ROOT_DIR = "output/figures/co2_emissions/main/"
  TAB_ROOT_DIR = "output/tables/co2_emissions/main/"
}


pa = pa_all %>%
  filter(year == 2019)

pa = merge(pa,emissions %>% select(-county,-name,-state),by='geoid')

pa_county = pa %>%
  filter(space_type == 'LOCAL/PRIVATE') %>%
  mutate(geoid = as.character(geoid),
         geoid = str_pad(geoid, 11, pad = "0"),
         county = substr(geoid, 1, 5)) %>%
  group_by(county) %>%
  summarise(avg_agreements = mean(all_agreements)) %>%
  rename(geoid = county)  %>%
  mutate(avg_agreements_quintile = ntile(avg_agreements, 5)) 


county_shape = tigris::counties(year=2010)
county_shape = county_shape %>%
  select(GEOID10,geometry) %>%
  rename(geoid = GEOID10) %>%
  st_as_sf() %>%
  st_transform(5070)

county_data = merge(pa_county,emissions_county,by='geoid')

map_data = merge(county_data,county_shape,by='geoid') %>%
  st_as_sf() %>%
  st_transform(5070)

map1 = ggplot() +
  geom_sf(data=map_data,
          color=NA,
          aes(fill = avg_agreements_quintile)) +
  scale_fill_viridis_c(option = 'C',direction=-1,name = 'Protection Quintile') +
  theme_void() +
  theme(legend.position='bottom')

map2 = ggplot() +
  geom_sf(data=map_data,
          color=NA,
          aes(fill = emissions_quintile)) +
  scale_fill_viridis_c(option = 'C',direction=-1,name = 'Emissions Quintile') +
  theme_void() +
  theme(legend.position='bottom')

# p = map1 + map2
# ggsave(paste0(FIG_ROOT_DIR,'emissions_map.png'),p,width=12,height=6)


# metric tons per household
# 
# tco2e per household
# 1 tco2e = 1 metric ton (1,000 kg) of co2
# 
# Driving a gasoline car for 5,000 miles.
# 
# 25 mpg vehicle (https://www.bts.gov/content/average-fuel-efficiency-us-light-duty-vehicles)
# 
# Around 3250 miles: https://calculator.carbonfootprint.com/calculator.aspx?tab=4
# 
# 1 kg beef -> 60 kg co2 -> 0.06

pa = pa %>%
  mutate(perc.area = perc.area*100) %>%
  filter(ruca_code != 'Rural' & ruca_code != '' & ruca_code != 'Missing')

mod_share_lp = feols(total_emissions ~ perc.area | state^ruca_code + tc_income_1 + tc_race_1,
                  se="iid",data=pa %>% filter(space_type == 'LOCAL/PRIVATE'))
mod_agree_lp = feols(total_emissions ~ all_agreements | state^ruca_code + tc_income_1 + tc_race_1,
                  se="iid",data=pa %>% filter(space_type == 'LOCAL/PRIVATE'))
mod_share_fs = feols(total_emissions ~ perc.area | state^ruca_code + tc_income_1 + tc_race_1,
                     se="iid",data=pa %>% filter(space_type == 'FEDERAL/STATE'))
mod_agree_fs = feols(total_emissions ~ all_agreements | state^ruca_code + tc_income_1 + tc_race_1,
                     se="iid",data=pa %>% filter(space_type == 'FEDERAL/STATE'))

esttex(mod_share_lp,mod_share_fs,mod_agree_lp,mod_agree_fs,
       style.tex = style.tex(main="aer",signif.code=c("**"=0.01, "*"=0.05))) %>%
  writeLines(paste0(TAB_ROOT_DIR,'emissions_baseline_descriptive.tex'))

run_mod = function(df){
  mod = feols(curr_emissions ~ perc.area | state^ruca_code + tc_race_1 + tc_income_1,
              se="iid",
              data=df)
  return(tidy(mod))
}

get_all_mods = function(df){
  mods = bind_rows(
    run_mod(df %>% mutate(curr_emissions = total_emissions_zscore)) %>% mutate(emissions = 'total'),
    run_mod(df %>% mutate(curr_emissions = transportation_total_emissions_zscore)) %>% mutate(emissions = 'transportation'),
    run_mod(df %>% mutate(curr_emissions = services_total_emissions_zscore)) %>% mutate(emissions = 'services'),
    run_mod(df %>% mutate(curr_emissions = housing_total_emissions_zscore)) %>% mutate(emissions = 'housing'),
    run_mod(df %>% mutate(curr_emissions = goods_total_emissions_zscore)) %>% mutate(emissions = 'goods'),
    run_mod(df %>% mutate(curr_emissions = food_total_emissions_zscore)) %>% mutate(emissions = 'food'))
  return(mods)
}



lp_mods = get_all_mods(pa %>% filter(space_type == 'LOCAL/PRIVATE')) %>%
  mutate(space_type = 'LOCAL/PRIVATE')

fs_mods = get_all_mods(pa %>% filter(space_type == 'FEDERAL/STATE')) %>%
  mutate(space_type = 'FEDERAL/STATE')

all_mods = bind_rows(lp_mods,fs_mods)

p = all_mods %>%
  mutate(conf.low = estimate - 1.96 * std.error,
         conf.high = estimate + 1.96 * std.error,
         space_type = factor(space_type,levels=c('LOCAL/PRIVATE','FEDERAL/STATE')),
         emissions = factor(emissions,levels=c('total','transportation','services','housing','goods','food'))) %>%
  ggplot(aes(x=estimate,
             y=emissions,
             shape=space_type)) +
  scale_y_discrete(limits=rev) +
  geom_errorbar(aes(xmin=conf.low,xmax=conf.high,linetype=space_type),width=0,position=position_dodge(width=0.5)) +
  geom_point(position=position_dodge(width=0.5)) +
  geom_vline(xintercept=0,linetype='dashed',color='red') +
  scale_shape_manual(values=c(16,17)) +
  labs(x='Coefficient Estimate',
       y='Emission Category') +
  theme(legend.position='bottom') 

# not in paper
# ggsave(paste0(FIG_ROOT_DIR,'emissions_vs_protection.png'),p,width=6,height=4)

base_year = 1980

new_protection_base = pa_all %>%
  filter(year %in% c(2019,base_year)) %>%
  filter(ruca_code != 'Rural' & ruca_code != '' & ruca_code != 'Missing') %>%
  group_by(geoid) %>%
  filter(n_distinct(year) == 2) %>%
  group_by(geoid,year) %>%
  mutate(perc.protected.all = perc.area[space_type == 'ALL PROTECTED']) 

new_protection = new_protection_base %>%
  arrange(geoid) %>%
  group_by(geoid,space_type) %>%
  mutate(agreements_2019 = all_agreements[year == 2019],
         acres_2019 = perc.area[year == 2019]) %>%
  filter(year == base_year) %>%
  mutate(treat_agree = ifelse(agreements_2019-all_agreements > 0,1,0),
         treat_acres = ifelse(acres_2019-perc.area > 0,1,0))

new_protection = merge(new_protection,emissions %>% select(-county,-name,-state),by='geoid')

mod_agree_lp = feols(total_emissions ~ treat + log(population_density+1) + 
                       perc.protected.all + less_than_hs 
                 | state^ruca_code + tc_income_1 + tc_race_1, 
                 se="hetero",
                 data=new_protection %>% filter(space_type == 'LOCAL/PRIVATE') %>% mutate(treat = treat_agree))

mod_agree_fs = feols(total_emissions ~ treat + log(population_density+1) + 
                       perc.protected.all + less_than_hs 
               | state^ruca_code + tc_income_1 + tc_race_1, 
               se="hetero",
               data=new_protection %>% filter(space_type == 'FEDERAL/STATE') %>% mutate(treat = treat_agree))

mod_acres_lp = feols(total_emissions ~ treat + log(population_density+1) + perc.protected.all + less_than_hs 
                | state^ruca_code + tc_income_1 + tc_race_1, 
                se="hetero",
                data=new_protection %>% filter(space_type == 'LOCAL/PRIVATE') %>% mutate(treat = treat_acres))

mod_acres_fs = feols(total_emissions ~ treat + log(population_density+1) + perc.protected.all + less_than_hs 
                | state^ruca_code + tc_income_1 + tc_race_1, 
                se="hetero",
                data=new_protection %>% filter(space_type == 'FEDERAL/STATE') %>% mutate(treat = treat_acres))

esttex(mod_acres_lp,mod_acres_fs,mod_agree_lp,mod_agree_fs,
       style.tex = style.tex(main="aer",signif.code=c("**"=0.01, "*"=0.05))) %>%
  writeLines(paste0(TAB_ROOT_DIR,'emissions_soo.tex'))


clean_table = readLines(paste0(TAB_ROOT_DIR,'clean_emissions_table_template.tex'))
clean_table = paste(clean_table, collapse = "\n")

tab1 = readLines(paste0(TAB_ROOT_DIR,'emissions_baseline_descriptive.tex'))
tab2 = readLines(paste0(TAB_ROOT_DIR,'emissions_soo.tex'))

start_line = which(str_detect(tab1, 'perc.'))
end_line = which(str_detect(tab1, 'Within R'))
tab1 = tab1[start_line:end_line]
tab1 = str_replace(tab1,'perc.area','\\\\% Acres Protected')
tab1 = str_replace(tab1,'all\\\\\\_agreements','All Agreements')
tab1  = paste(tab1, collapse = '\n')
out <- sub("TK_TABLE_1", tab1, clean_table, fixed = TRUE)

start_line = which(str_detect(tab2, 'treat'))
end_line = which(str_detect(tab2, 'Within R'))
tab2 = tab2[start_line:end_line]
tab2 = str_replace(tab2,'treat','New Protection')
tab2 = str_replace(tab2,'log\\(population\\\\\\_density\\+1\\)','Log of Population Density (+1)')
tab2 = str_replace(tab2,'perc.protected.all','\\\\% Acres Protected 1980')
tab2 = str_replace(tab2,'less\\\\\\_than\\\\\\_hs','\\\\% Pop Less than HS 1980')
tab2  = paste(tab2, collapse = '\n')
out <- sub("TK_TABLE_2", tab2, out, fixed = TRUE)

writeLines(out, paste0(TAB_ROOT_DIR,"emissions_results.tex"))

