library(tidyverse)
library(broom)
library(gridExtra)
library(tidyr)
library(MatchIt)
library(fixest)
library(kableExtra)
library(data.table)
library(parallel)
library(doParallel)
library(sf)
library(maps)
library(RColorBrewer)
library(tigris)
library(patchwork)


setwd("~/Library/CloudStorage/GoogleDrive-sa9973@princeton.edu/My Drive/research/protected_areas")
theme2 = theme_bw () +
  theme(axis.text=element_text(size=8),
        plot.subtitle=element_text(size=10),
        plot.title=element_text(size=10),
        axis.title=element_text(size=9),
        legend.text=element_text(size=9),
        legend.title=element_blank(),
        strip.text = element_text(size=8),
        text=element_text(family="Times New Roman"))

robust = F

if(robust){
  full_df = fread("data/derived/protected_areas_master_file_clean_robust.csv")
  FIG_ROOT_DIR = "output/figures/historical_trends_protected_areas/robust/"
  TAB_ROOT_DIR = "output/tables/historical_trends_protected_areas/robust/"
}else{
  full_df = fread("data/derived/protected_areas_master_file_clean.csv")
  FIG_ROOT_DIR = "output/figures/historical_trends_protected_areas/main/"
  TAB_ROOT_DIR = "output/tables/historical_trends_protected_areas/main/"
}



theme_set(theme2)
paired_colors <- brewer.pal(n = 9, name = "Greens")
selected_colors <- paired_colors[c(6, 9)]

acre_growth = full_df %>%
  filter(space_type != 'ALL PROTECTED') %>%
  group_by(space_type,year) %>%
  summarise(protected = sum(all_conserved_acreage, na.rm = TRUE)/1e6) %>%
  mutate(baseline = protected[year==1980]) %>%
  mutate(acres_growth = (protected - baseline)/baseline) %>%
  mutate(label = paste0(round(protected,1),'M'))

acres_1980 = acre_growth %>%
  filter(year == 1980) %>%
  ungroup() %>%
  mutate(fs_protected = protected[space_type == 'FEDERAL/STATE']) %>%
  filter(space_type == 'LOCAL/PRIVATE') %>%
  mutate(label = paste0('Acres in 1980:\nLP: ',
                        round(protected,1), 'M\nFS: ',
                        round(fs_protected,1), 'M'))

p1 = ggplot(acre_growth, aes(x = year, y = acres_growth)) +
  geom_line(aes(color = space_type, fill=space_type)) +
  labs(x = 'Year', y = 'Growth in Protected Acres',
       subtitle = 'Growth in Protected Acres in the US') +
  scale_color_manual(values=selected_colors) +
  scale_fill_manual(values=selected_colors) +
  scale_y_continuous(labels = scales::percent,
                     expand = expansion(mult = c(0.05, 0.1))) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  geom_text(data = filter(acre_growth, year %in% c(2019)), aes(label = label),
            hjust=0,size=3.5,family = 'serif') +
  ggrepel::geom_label_repel(data = acres_1980, aes(label = label),
                            hjust=0, 
                            family = 'serif',
                            size=3.5, fill='white', color='black') +
  theme(legend.position='none')

county_growth = full_df %>%
  mutate(geoid = str_pad(as.character(geoid), 11, pad = "0")) %>%
  mutate(county = substr(geoid, 1, 5)) %>%
  filter(space_type != 'ALL PROTECTED') %>%
  mutate(new_private_conservation = ifelse(new_conserved_acreage > 0, 1, 0)) %>%
  mutate(year_dec = floor(year/10)*10) %>%
  group_by(county,year_dec,space_type) %>%
  summarise(new_private_conservation = pmin(1,sum(new_private_conservation, na.rm = TRUE))) %>%
  ungroup() %>%
  group_by(year_dec,space_type) %>%
  summarise(new_private_conservation = mean(new_private_conservation, na.rm = TRUE)) %>%
  mutate(year_dec = paste0(as.character(year_dec),'-',as.character(year_dec+9))) 

p2 = county_growth %>%
  ggplot(aes(x = year_dec, y = new_private_conservation,fill=space_type,color=space_type)) +
  geom_col(position = 'dodge') +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values=selected_colors) +
  scale_color_manual(values=selected_colors) +
  labs(x = '', y = 'Share of Counties',
       subtitle = 'Share of Counties with Growth in Protected Areas') +
  theme(legend.position='bottom')


p = p1 / p2

if(!robust){
ggsave(paste0(FIG_ROOT_DIR,"protected_areas_growth.png"),p,
       width=6,height=6)
}

df = full_df %>% 
  filter(ruca_code != 'Rural' & ruca_code != '' & ruca_code != 'Missing') %>%
  filter(year %in% c(1980,1990,2000,2010,2019)) %>%
  filter(!is.na(total_pop) & !is.na(white_pct) & !is.na(median_household_income)) %>%
  filter(nbhd.land.acres != 0 & total_pop != 0) %>%
  group_by(geoid) %>%
  filter(n() == 15) %>%
  mutate(perc.area = perc.area*100)

p = df %>%
  group_by(year,space_type) %>%
  summarise(`Share of Tract Protected` = mean(perc.area),
            `Number of agreements` = mean(all_agreements)) %>%
  filter(space_type != 'ALL PROTECTED') %>%
  gather(variable,value,-year,-space_type) %>%
  mutate(value = round(value,3)) %>%
  ggplot(aes(x = as.character(year), y = value,fill=space_type)) +
  geom_col(position=position_dodge(width=0.8)) +
  geom_text(aes(label = value), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5,
            size=3,
            family='serif') +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) + 
  scale_fill_manual(values=selected_colors) +
  facet_wrap(~variable,scales='free_y',nrow=2) +
  labs(x = 'Year',
       y = '')

if(!robust){
ggsave(paste0(FIG_ROOT_DIR,"protected_areas_overtime.png"),p,
       width=6,height=5)
}


paired_colors <- brewer.pal(n = 8, name = "Set2")
selected_colors <- paired_colors[c(1,2,3)]

df = df %>%
  mutate(race_income = paste(tc_race_1,tc_income_1,sep="_")) %>%
  filter(tc_race_1 != 'majority_asian' & tc_race_1 != 'other')

df$race_income = relevel(as.factor(df$race_income), ref = "majority_white_high_income")

pred_df = df %>% 
  ungroup() %>%
  select(race_income,tc_race_1,tc_income_1) %>% 
  unique() %>%
  mutate(ruca_code = "Metro: Core",
         state = "CA")


run_mod = function(yr,df,type,predict=F){
  print(yr)
  curr_yr = df %>% filter(year == yr & space_type == type) 
  if(predict){
    mod1 = feols(outcome ~ race_income + state*ruca_code,
                 data=curr_yr,
                 vcov="HC1")
   preds = cbind(pred_df,predict(mod1,newdata=pred_df,se.fit=T)) 
   preds$space_type = type
   preds$year = yr
   return(preds)
  }else{
    mod1 = feols(outcome ~ race_income | state^ruca_code,
                 data=curr_yr,
                 vcov="HC1")
  return(tidy(mod1) %>% mutate(year = yr))
  }
}

df = df %>% mutate(outcome=perc.area)
mods0 = map_dfr(as.character(c(1980,1990,2000,2010,2019)),run_mod,df,"ALL PROTECTED")
mods1 = map_dfr(as.character(c(1980,1990,2000,2010,2019)),run_mod,df,"LOCAL/PRIVATE")
mods2 = map_dfr(as.character(c(1980,1990,2000,2010,2019)),run_mod,df,"FEDERAL/STATE")


theme_set(theme2)
make_plot = function(mods0,mods1,mods2){
p = bind_rows(mods1 %>% mutate(space_type="LOCAL/PRIVATE"),
              mods2 %>% mutate(space_type="FEDERAL/STATE")) %>%
  filter(grepl('race',term)) %>%
  mutate(term = str_replace(term,'race_income','')) %>%
  mutate(income = ifelse(grepl('high',term),'High Income',
                         ifelse(grepl('middle',term),'Middle Income','Low Income')),
         race = ifelse(grepl("black",term),"Majority Black",
                       ifelse(grepl("hispanic",term),"Majority Hispanic",
                              "Majority White"))) %>%
  mutate(lwr = estimate - std.error*1.96,
         upr = estimate + std.error*1.96,
         year = as.numeric(year)) %>%
  ggplot(aes(x=year, y=estimate,color=income,fill=income,shape=income)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=lwr, 
                    ymax=upr), 
                width=.1) +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_fill_manual(values=selected_colors) +
  scale_color_manual(values=selected_colors) +
  facet_grid(race~space_type,scales="free_y") +
  theme(legend.position = "bottom")
return(p)
}


p1 = make_plot(mods0,mods1,mods2) +
  labs(x="Year",
       y="Difference in share of tract protected (pp)") 
ggsave(paste0(FIG_ROOT_DIR,"differences_protection_area_overtime.png"),
       p1,
       width=6,
       height=6)


df = df %>% mutate(outcome=all_agreements)
mods0 = map_dfr(as.character(c(1980,1990,2000,2010,2019)),run_mod,df,"ALL PROTECTED")
mods1 = map_dfr(as.character(c(1980,1990,2000,2010,2019)),run_mod,df,"LOCAL/PRIVATE")
mods2 = map_dfr(as.character(c(1980,1990,2000,2010,2019)),run_mod,df,"FEDERAL/STATE")

p2 = make_plot(mods0,mods1,mods2) +
  labs(x="Year",
       y="Difference in number of protection agreements")

if(!robust){
ggsave(paste0(FIG_ROOT_DIR,"differences_protection_agreements_overtime.png"),
       p2,
       width=6,
       height=6)
}
