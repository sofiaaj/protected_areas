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
library(patchwork)
library(ggpubr)
library(maps)
library(RColorBrewer)
library(tigris)
library(fixest)

setwd("~/Library/CloudStorage/GoogleDrive-sa9973@princeton.edu/My Drive/research/protected_areas")

theme1 = theme_void() +
  theme(axis.text=element_text(size=8),
        plot.subtitle=element_text(size=10),
        axis.title=element_text(size=8),
        legend.text=element_text(size=9),
        strip.text = element_text(size=10),
        text=element_text(family="serif"))

theme2 = theme_bw () +
  theme(axis.text=element_text(size=8),
        plot.subtitle=element_text(size=8),
        plot.title=element_text(size=8),
        axis.title=element_text(size=9),
        legend.text=element_text(size=9),
        legend.title=element_blank(),
        strip.text = element_text(size=8),
        text=element_text(family="Times New Roman"))

robust = FALSE

if(robust){
  df = fread("data/derived/protected_areas_master_file_clean_robust.csv")
  FIG_ROOT_DIR = "output/figures/contemporary_trends_protected_areas/robust/"
  TAB_ROOT_DIR = "output/tables/contemporary_trends_protected_areas/robust/"
}else{
  df = fread("data/derived/protected_areas_master_file_clean.csv")
  FIG_ROOT_DIR = "output/figures/contemporary_trends_protected_areas/main/"
  TAB_ROOT_DIR = "output/tables/contemporary_trends_protected_areas/main/"
}

df = df %>% 
  filter(ruca_code != 'Rural' & ruca_code != '' & ruca_code != 'Missing') %>%
  filter(year == 2019) %>%
  filter(!is.na(total_pop) & nbhd.land.acres != 0 & total_pop != 0 & !is.na(median_household_income)) %>%
  filter(ruca_code != 'Missing') %>%
  mutate(perc.area = perc.area * 100) 

# figure1 if not robust
if(!robust){
us_states <- states(cb = TRUE) %>% 
  filter(STUSPS != "HI" & STUSPS != "AK") %>%
  rename(state=STUSPS)# Exclude Hawaii and Alaska for a continental US map

states = df %>% 
  group_by(state,space_type) %>%
  summarise(average_num_agreements = mean(all_agreements),
            average_share_protected = mean(perc.area))

merged_data <- us_states %>% 
  merge(.,states,by = "state")

theme_set(theme1)
p1 = merged_data %>%
  filter(space_type != 'ALL PROTECTED') %>%
  ggplot() +
  geom_sf(aes(fill = average_share_protected)) +
  scale_fill_continuous(name = "Avg. share of tract acres protected (%)", 
                        low = "white", 
                        high = "green4") +
  theme(axis.text = element_blank(), axis.title = element_blank(), 
        axis.ticks = element_blank(), panel.grid = element_blank()) +
  facet_grid(~space_type) +
  theme(legend.position = "bottom")

p2 = merged_data %>%
  filter(space_type != 'ALL PROTECTED') %>%
  ggplot() +
  geom_sf(aes(fill = average_num_agreements)) +
  scale_fill_continuous(name = "Number of agreements", 
                        low = "white", 
                        high = "green4") +
  #theme_minimal() +
  labs(subtitle = "Avg. Number of Protection Agreements in Tract") +
  theme(axis.text = element_blank(), axis.title = element_blank(), 
        axis.ticks = element_blank(), panel.grid = element_blank()) +
  facet_wrap(~space_type)

p = grid.arrange(p1,p2,nrow=2)
p = p1

ggsave(paste0(FIG_ROOT_DIR,"protected_areas_by_state_and_type.png"),
       p,
       height=5,width=7)
}

# figure 2
theme_set(theme2)
paired_colors <- brewer.pal(n = 9, name = "Greens")
selected_colors <- paired_colors[c(6, 9)]
p = df %>%
  group_by(ruca_code,space_type) %>%
  summarise(average_num_agreements = mean(all_agreements),
            average_share_protected = mean(perc.area)) %>%
  filter(space_type != "ALL PROTECTED" & ruca_code != "Missing") %>%
  gather(measure,value,-ruca_code,-space_type) %>%
  mutate(label = as.character(round(value,1)),
         label = ifelse(measure == 'average_share_protected',paste0(label,'%'),label)) %>%
  mutate(measure = ifelse(measure == "average_num_agreements","Average Number of Agreements","Average Share of Tract Is Protected (%)")) %>%
  mutate(measure = factor(measure,levels=c("Average Share of Tract Is Protected (%)","Average Number of Agreements"))) %>%
  ggplot(aes(x=ruca_code,y=value,fill=space_type)) +
  geom_bar(stat="identity",position="dodge") +
  facet_wrap(~measure,scales="free_x") +
  scale_fill_manual(values=selected_colors) +
  coord_flip() +
  theme(axis.title=element_blank()) +
  geom_text(aes(label=label),position=position_dodge(width=0.9),hjust=-0.2,size=2.5) +
  theme(legend.position="bottom") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)))

ggsave(paste0(FIG_ROOT_DIR,"protected_area_by_ruca_code.png"),p,
       width=7,height=4)

### START NEIGHBORHOOD ANALYSIS ####

# figures 3 and 4
df$tc_income_1 = relevel(as.factor(df$tc_income_1), ref = "high_income")
loc_priv = df %>% filter(space_type == "LOCAL/PRIVATE")
fed_st = df %>% filter(space_type == "FEDERAL/STATE")

modA1 = feols(perc.area ~ tc_income_1 | state^ruca_code,
              vcov='HC1',
              data=loc_priv)

modA2 = feols(perc.area ~ tc_income_1 | state^ruca_code,
              vcov='HC1',
              data=fed_st)

modA = bind_rows(tidy(modA1) %>% 
                   mutate(space_type="LOCAL/PRIVATE"),
                 tidy(modA2) %>% 
                   mutate(space_type="FEDERAL/STATE")) %>%
  mutate(outcome="perc.area")

modB1 = feols(all_agreements ~ tc_income_1 | state^ruca_code,
              vcov='HC1',
              data=loc_priv)

modB2 = feols(all_agreements ~ tc_income_1 | state^ruca_code,
              vcov='HC1',
              data=fed_st)

modB = bind_rows(tidy(modB1) %>% mutate(space_type="LOCAL/PRIVATE"),
                 tidy(modB2) %>% mutate(space_type="FEDERAL/STATE")) %>%
  mutate(outcome="all agreements")

make_plot = function(mod){
  p = mod %>%
    filter(grepl('tc_income_1',term)) %>%
    mutate(term = str_to_title(str_replace(str_replace(term,'tc_income_1',''),"_"," "))) %>%
    ggplot(aes(y=term,x=estimate)) +
    geom_point() +
    geom_errorbar(aes(xmin=estimate - std.error*1.96,
                      xmax=estimate + std.error*1.96),
                  width=0) +
    facet_wrap(~space_type,nrow=2) +
    geom_vline(xintercept=0, linetype="dashed")
  return(p)
}

p1 = make_plot(modA) + 
  labs(title="",
       x="Difference in share of tract that is protected",
       y="")
p2 = make_plot(modB) + 
  labs(x="Difference in number of agreements in tract",
       title="",
       y="")
p = grid.arrange(p1,p2,
                 nrow=1)

# not in paper
#ggsave(paste0(FIG_ROOT_DIR,"income_differences_protection.png"),
#       p,
#       width=8,
#       height=4)



df$tc_race_1 = relevel(as.factor(df$tc_race_1), 
                       ref = "majority_white")

curr_df = df %>%
  filter(tc_race_1 != "other" & tc_race_1 != 'majority_asian')

loc_priv = curr_df %>% filter(space_type == "LOCAL/PRIVATE")
fed_st = curr_df %>% filter(space_type == "FEDERAL/STATE")

modA1 = feols(perc.area ~ tc_race_1 | state^ruca_code,
              vcov="HC1",
              data=loc_priv)

modA2 = feols(perc.area ~ tc_race_1 | state^ruca_code,
           vcov="HC1",
           data=fed_st)

modA = bind_rows(tidy(modA1) %>% mutate(space_type="LOCAL/PRIVATE"),
                 tidy(modA2) %>% mutate(space_type="FEDERAL/STATE")) %>%
  mutate(outcome="perc.area")

modB1 = feols(all_agreements ~ tc_race_1 | state^ruca_code,
              vcov="HC1",
              data=loc_priv)
modB2 = feols(all_agreements ~ tc_race_1 | state^ruca_code,
              vcov="HC1",
              data=fed_st)

modB = bind_rows(tidy(modB1) %>% mutate(space_type="LOCAL/PRIVATE"),
                 tidy(modB2) %>% mutate(space_type="FEDERAL/STATE")) %>%
  mutate(outcome="all agreements")

make_plot = function(mod){
  p = mod %>%
    filter(grepl('tc_race_1',term)) %>%
    mutate(term = str_to_title(str_replace(str_replace(term,'tc_race_1',''),"_"," "))) %>%
    ggplot(aes(y=term,x=estimate)) +
    geom_point() +
    geom_errorbar(aes(xmin=estimate - std.error*1.96,
                      xmax=estimate + std.error*1.96),
                  width=0) +
    facet_wrap(~space_type,nrow=2) +
    geom_vline(xintercept=0, linetype="dashed")
  return(p)
}

p1 = make_plot(modA) + 
  labs(title="",
       x="Difference in share of tract that is protected",
       y="")
p2 = make_plot(modB) + 
  labs(x="Difference in number of agreements in tract",
       title="",
       y="")

p = grid.arrange(p1,p2,
                 nrow=1)

# not in paper
#ggsave(paste0(FIG_ROOT_DIR,"racial_differences_protection.png"),
#       p,
#       width=8,
#       height=4)



## race income combined
 
df = df %>%
  mutate(race_income = paste(tc_race_1,tc_income_1,sep="_"))

df$race_income = relevel(as.factor(df$race_income), ref = "majority_white_high_income")

curr_df = df %>%
  filter(tc_race_1 != 'majority_asian' & tc_race_1 != 'other')

loc_priv = curr_df %>% filter(space_type == "LOCAL/PRIVATE")
fed_st = curr_df %>% filter(space_type == "FEDERAL/STATE")

modA1 = feols(perc.area ~ race_income | state^ruca_code,
              vcov="HC1",
              data=loc_priv)
modA2 = feols(perc.area ~ race_income | state^ruca_code,
              vcov="HC1",
              data=fed_st)

modA = bind_rows(tidy(modA1) %>% mutate(space_type="LOCAL/PRIVATE"),
                 tidy(modA2) %>% mutate(space_type="FEDERAL/STATE")) %>%
  mutate(outcome="perc.area")

modB1 = feols(all_agreements ~ race_income | state^ruca_code,
              vcov="HC1",
              data=loc_priv)
modB2 = feols(all_agreements ~ race_income | state^ruca_code,
              vcov="HC1",
              data=fed_st)

modB = bind_rows(tidy(modB1) %>% mutate(space_type="LOCAL/PRIVATE"),
                 tidy(modB2) %>% mutate(space_type="FEDERAL/STATE")) %>%
  mutate(outcome="all agreements")


level_order = c(
  'Majority Hispanic, Low Income',
  'Majority Hispanic, Middle Income',
  'Majority Black, Middle Income',
  'Majority White, Low Income',
  'Majority Black, Low Income',
  'Majority White, Middle Income',
  'Majority Hispanic, High Income',
  'Majority Black, High Income'
)

make_plot = function(mod){
  p = mod %>%
    filter(grepl('race_income',term)) %>%
    mutate(race = str_extract(term,'majority_(black|white|hispanic)'),
           income = str_extract(term,'(low|middle|high)_income'),
           term = paste(race,income,sep=", "),
           term = str_to_title(str_replace_all(term,'_',' '))) %>%
    ggplot(aes(y=factor(term,level_order),x=estimate,shape=space_type,color=space_type)) +
    geom_point(position=position_dodge(width=0.4)) +
    geom_errorbar(aes(xmin=estimate - std.error*1.96,
                      xmax=estimate + std.error*1.96),
                  width=0,
                  position=position_dodge(width=0.4)) +
    geom_vline(xintercept=0, linetype="dashed") +
    scale_color_manual(values=selected_colors) +
    theme(legend.position='bottom')
  return(p)
}

p1 = make_plot(modA) + 
  labs(title="",
       x="Difference in share of tract that is protected (pp)",
       y="")
p2 = make_plot(modB) + 
  labs(x="Difference in number of agreements in tract",
       title="",
       y="")

p = p1 + p2 +   plot_layout(guides = 'collect', axes = "collect", ) & theme(legend.position = 'bottom')

ggsave(paste0(FIG_ROOT_DIR,"racial_income_differences_protection.png"),
       p,
       width=8,
       height=4)



## models with covariates
run_mods = function(curr_df){
  mod1 = feols(outcome ~ tc_race_1,
               data=curr_df,
               vcov="HC1")
  mod2 = feols(outcome ~ tc_race_1 | state^ruca_code,
               data=curr_df,
               vcov="HC1")
  mod3 = feols(outcome ~ tc_race_1*tc_income_1 | state^ruca_code,
               data=curr_df,
               vcov="HC1")
  mod4 = feols(outcome ~ tc_race_1*tc_income_1 + tc_ownership | state^ruca_code,
               data=curr_df,
               vcov="HC1")
  mod5 = feols(outcome ~ tc_race_1*tc_income_1 + tc_ownership + tc_home_value | state^ruca_code,
               data=curr_df,
               vcov="HC1")
  return(list(mod1,mod2,mod3,mod4,mod5))

}

df$tc_ownership = relevel(as.factor(df$tc_ownership), ref = "3")
df$tc_home_value = relevel(as.factor(df$tc_home_value), ref = "3")


loc_priv = df %>% 
  filter(space_type == "LOCAL/PRIVATE" & !is.na(median_house_value)) %>%
  droplevels() 

mods1 = run_mods(loc_priv %>% rename(outcome = all_agreements))
mods2 = run_mods(loc_priv %>% rename(outcome = perc.area))
setFixest_dict(c(tc_race_1majority_asian="Majority Asian",
                 tc_race_1majority_black="Majority Black",
                 tc_race_1majority_hispanic="Majority Hispanic",
                 tc_race_1other="Other Race Category",
                 tc_income_1middle_income="Middle Income",
                 tc_income_1low_income="Low Income",
                 tc_ownership1="Low Ownership Rate",
                 tc_ownership2="Medium Ownership Rate",
                 tc_home_value1="Low Home Values",
                 tc_home_value2="Medium Home Values",
                 state="State",
                 ruca_code="Rural/Urban Code"))


#etable(mods1,signif.code = c("**"=0.01, "*"=0.05),
#       digits="r3",depvar=F,tex=T,drop="Constant",
#       replace=T,
#       file=paste0(TAB_ROOT_DIR,"loc_priv_agreements.tex"))

#etable(mods2,signif.code = c("**"=0.01, "*"=0.05),
#       digits="r3",depvar=F,tex=T,drop="constant",
#       replace=T,file=paste0(TAB_ROOT_DIR,"loc_priv_share_protected.tex"))

fed_st = df %>% 
  filter(space_type == "FEDERAL/STATE" & !is.na(median_house_value)) %>%
  droplevels()

mods1 = run_mods(fed_st %>% rename(outcome = all_agreements))
mods2 = run_mods(fed_st %>% rename(outcome = perc.area))

# not in paper
#etable(mods1,signif.code = c("**"=0.01, "*"=0.05),
#       digits="r3",depvar=F,tex=T,drop="Constant",
#       replace=T,
#       file=paste0(TAB_ROOT_DIR,"fed_st_agreements.tex"))

#etable(mods2,signif.code = c("**"=0.01, "*"=0.05),
#       digits="r3",depvar=F,tex=T,drop="constant",
#       replace=T,
#       file=paste0(TAB_ROOT_DIR,"fed_st_share_protected.tex"))


## version with race and income categories

run_mods = function(curr_df){
  mod1 = feols(outcome ~ race_income,
               data=curr_df,
               vcov="HC1")
  mod2 = feols(outcome ~ race_income | state^ruca_code,
               data=curr_df,
               vcov="HC1")
  mod3 = feols(outcome ~  race_income + tc_ownership | state^ruca_code,
               data=curr_df,
               vcov="HC1")
  mod4 = feols(outcome ~ race_income + tc_ownership + tc_home_value | state^ruca_code,
               data=curr_df,
               vcov="HC1")
  return(list(mod1,mod2,mod3,mod4))
  
}

df$tc_ownership = relevel(as.factor(df$tc_ownership), ref = "3")
df$tc_home_value = relevel(as.factor(df$tc_home_value), ref = "3")
df = df %>%
  mutate(race_income = paste(tc_race_1,tc_income_1,sep="_"))

df$race_income = relevel(as.factor(df$race_income), ref = "majority_white_high_income")

loc_priv = df %>% 
  filter(space_type == "LOCAL/PRIVATE" & !is.na(median_house_value)) %>%
  droplevels()

mods1A = run_mods(loc_priv %>% rename(outcome = all_agreements))
mods2A = run_mods(loc_priv %>% rename(outcome = perc.area))

setFixest_dict(c(race_incomemajority_asian_high_income="Majority Asian, High Income",
                 race_incomemajority_asian_low_income="Majority Asian, Middle Income",
                 race_incomemajority_asian_middle_income="Majority Asian, Low Income",
                 race_incomemajority_black_high_income="Majority Black, High Income",
                 race_incomemajority_black_middle_income="Majority Black, Middle Income",
                 race_incomemajority_black_low_income="Majority Black, Low Income",
                 race_incomemajority_hispanic_high_income="Majority Hispanic, High Income",
                 race_incomemajority_hispanic_middle_income="Majority Hispanic, Middle Income",
                 race_incomemajority_hispanic_low_income="Majority Hispanic, Low Income",
                 race_incomemajority_white_middle_income="Majority White, Middle Income",
                 race_incomemajority_white_low_income="Majority White, Low Income",
                 race_incomeother_high_income="Majority Other, High Income",
                 race_incomeother_middle_income="Majority Other, Middle Income",
                 race_incomeother_low_income="Majority Other, Low Income",
                 tc_ownership1="Low Ownership Rate",
                 tc_ownership2="Medium Ownership Rate",
                 tc_home_value1="Low Home Values",
                 tc_home_value2="Medium Home Values",
                 state="State",
                 ruca_code="Rural/Urban Code"))


quick_clean = function(mod,kind){
  return(tidy(mod) %>%
           filter(grepl('race_income',term)) %>%
           mutate(term = str_replace(term,"race_income","")) %>%
           mutate(term = str_replace(term,"majority_",""),
                  race = str_split_fixed(term,"_",n=2)[,1],
                  income = str_split_fixed(term,"_",n=2)[,2],
                  income = str_replace(income,"_"," ")) %>%
           mutate(race = paste0("Majority ",str_to_title(race)),
                  income = str_to_title(income)) %>%
           select(term,estimate,std.error,race,income) %>%
           mutate(kind = kind))
}

mod_to_fig = function(mods2,combined=F){
  fig_df = bind_rows(quick_clean(mods2[[1]],"Baseline"),
                     quick_clean(mods2[[2]],"+ State-RUCA effects"),
                     quick_clean(mods2[[3]],"+ Ownership Rate Controls"),
                     quick_clean(mods2[[4]],"+ Home Value Controls"))
  
  fig_df$income = factor(fig_df$income,levels=c("Low Income","Middle Income","High Income"))
  fig_df$race = factor(fig_df$race,levels=c("Majority White","Majority Black","Majority Hispanic","Majority Asian","Majority Other"))
  if(combined){
    return(fig_df)
  }
  p = fig_df %>%
    filter(race != "Majority Other" & race != "Majority Asian") %>%
    ggplot(aes(x=estimate,y=kind)) +
    geom_point() +
    geom_errorbarh(aes(xmin=estimate-1.96*std.error,xmax=estimate+1.96*std.error),height=0) +
    geom_vline(xintercept=0,linetype='dashed') +
    facet_grid(race~income) +
    labs(x = 'Estimated Difference in Share of Tract That Is Protected',
         y = '')
  return(p)
}

#etable(mods1A,signif.code = c("**"=0.01, "*"=0.05),
#       digits="r3",depvar=F,tex=T,drop="Constant",
#       replace=T,
#       file=paste0(TAB_ROOT_DIR,"loc_priv_agreements_race_income_cats.tex"))

if(!robust){
etable(mods2A,signif.code = c("**"=0.01, "*"=0.05),
       digits="r3",depvar=F,tex=T,drop="Constant",
       replace=T,
       file=paste0(TAB_ROOT_DIR,"loc_priv_share_protected_race_income_cats.tex"))
}

p = mod_to_fig(mods2A)

# not in paper
#ggsave(paste0(FIG_ROOT_DIR,"loc_priv_share_protected_race_income_cats.png"),
#       p,
#       width=7,
#       height=5)

fed_st = df %>% 
  filter(space_type == "FEDERAL/STATE" & !is.na(median_house_value)) %>%
  droplevels()

mods1B = run_mods(fed_st %>% rename(outcome = all_agreements))
mods2B = run_mods(fed_st %>% rename(outcome = perc.area))

# not in paper
#etable(mods1B,signif.code = c("**"=0.01, "*"=0.05),
#       digits="r3",depvar=F,tex=T,drop="Constant",
#       replace=T,
#       file=paste0(TAB_ROOT_DIR,"fed_st_agreements_race_income_cats.tex"))

if(!robust){
etable(mods2B,signif.code = c("**"=0.01, "*"=0.05),
       digits="r3",depvar=F,tex=T,drop="constant",
       replace=T,
       file=paste0(TAB_ROOT_DIR,"fed_st_share_protected_race_income_cats.tex"))
}

p = mod_to_fig(mods2B)

# not in paper
#ggsave(paste0(FIG_ROOT_DIR,"fed_st_share_protected_race_income_cats.png"),
#       p,
#       width=8,
#       height=6)

mods2A = mod_to_fig(mods2A,combined=T)
mods2B = mod_to_fig(mods2B,combined=T)

fig_df = bind_rows(mods2A %>% mutate(space_type = 'LOCAL/PRIVATE'),
                   mods2B %>% mutate(space_type = 'FEDERAL/STATE')) 
p = fig_df %>%
  filter(race != "Majority Other" & race != "Majority Asian") %>%
  ggplot(aes(x=estimate,y=kind,shape=space_type,color=space_type)) +
  geom_point(position=position_dodge(width=0.5)) +
  geom_errorbarh(aes(xmin=estimate-1.96*std.error,xmax=estimate+1.96*std.error),
                 position=position_dodge(width=0.5),
                 height=0) +
  geom_vline(xintercept=0,linetype='dashed') +
  scale_color_manual(values=selected_colors) +
  facet_grid(race~income) +
  labs(x = 'Estimated Difference in Share of Tract That Is Protected (pp)',
       y = '') +
  theme(legend.position = 'bottom')

ggsave(paste0(FIG_ROOT_DIR,"share_protected_race_income_cats_combined.png"),
       p,
       width=8,
       height=5)

mods1A = mod_to_fig(mods1A,combined=T)
mods1B = mod_to_fig(mods1B,combined=T)

fig_df = bind_rows(mods1A %>% mutate(space_type = 'LOCAL/PRIVATE'),
                   mods1B %>% mutate(space_type = 'FEDERAL/STATE')) 
p = fig_df %>%
  filter(race != "Majority Other" & race != "Majority Asian") %>%
  ggplot(aes(x=estimate,y=kind,shape=space_type,color=space_type)) +
  geom_point(position=position_dodge(width=0.5)) +
  geom_errorbarh(aes(xmin=estimate-1.96*std.error,xmax=estimate+1.96*std.error),
                 position=position_dodge(width=0.5),
                 height=0) +
  geom_vline(xintercept=0,linetype='dashed') +
  scale_color_manual(values=selected_colors) +
  facet_grid(race~income) +
  labs(x = 'Estimated Difference in Number of Protected Agreements',
       y = '') +
  theme(legend.position = 'bottom')

if(!robust){
ggsave(paste0(FIG_ROOT_DIR,"num_agreements_race_income_cats_combined.png"),
       p,
       width=8,
       height=5)
}
