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
library(ggpubr)
library(RColorBrewer)
library(tigris)
library(MatchIt)

setwd("~/Library/CloudStorage/GoogleDrive-sa9973@princeton.edu/My Drive/research/protected_areas")

theme = theme_bw () +
  theme(
    axis.text = element_text(size = 8),
    plot.subtitle = element_text(size = 10),
    plot.title = element_text(size = 10),
    axis.title = element_text(size = 9),
    legend.text = element_text(size = 9),
    legend.title = element_blank(),
    strip.text = element_text(size = 8),
    text = element_text(family = "Times New Roman")
  )
theme_set(theme)

robust = F

if(robust){
  full_df = fread("data/derived/protected_areas_master_file_clean_robust.csv")
  FIG_ROOT_DIR = "output/figures/virginia_case_study/robust/"
  TAB_ROOT_DIR = "output/tables/virginia_case_study/robust/"
}else{
  full_df = fread("data/derived/protected_areas_master_file_clean.csv")
  FIG_ROOT_DIR = "output/figures/virginia_case_study/main/"
  TAB_ROOT_DIR = "output/tables/virginia_case_study/main/"
}

full = full_df %>%
  group_by(year, state) %>%
  filter(space_type != "ALL PROTECTED") %>%
  group_by(geoid, space_type) %>%
  mutate(
    ruca_code_1 = case_when(
      year < 1990 ~ ruca_code_1[year == 1980],
      year < 2000 ~ ruca_code_1[year == 1990],
      year < 2010 ~ ruca_code_1[year == 2000],
      TRUE ~ ruca_code_1
    )
  )

tax_credit_st = c('AR','CA','CO','CT','DE','GA','IA','MD','MA','MS','NM','NY','NC','SC')

# states in census region
rel_states = c('DE','FL','GA','MD','NC','SC','VA','WV','AL','KY','MS','TN','AR','LA','OK','TX')
rel_states = rel_states[!rel_states %in% tax_credit_st]

full_subset = full %>%
  filter(ruca_code_1 != 'Rural' &
           ruca_code_1 != '' & ruca_code_1 != 'Missing') %>%
  filter(state %in% rel_states)

va_sum = full_subset %>%
  filter(state == 'VA') %>%
  group_by(year, space_type) %>%
  summarise(
    perc.area = mean(perc.area, na.rm = T),
    all_agreements = mean(all_agreements, na.rm = T)
  ) %>%
  filter(year >= 1993 & year <= 2006)

share_tract = va_sum %>%
  ggplot(aes(x = year, y = perc.area)) +
  geom_line() +
  facet_wrap( ~ space_type) +
  labs(x = "Year",
       y = "Share Tract Protected")

agreements = va_sum %>%
  ggplot(aes(x = year, y = all_agreements)) +
  geom_line() +
  facet_wrap( ~ space_type) +
  labs(x = "Year",
       y = "Number of Agreements")

p = grid.arrange(
  share_tract,
  agreements,
  ncol = 1,
  top = text_grob("Trends in Protected Areas in Virginia")
)

ggsave(paste0(FIG_ROOT_DIR, "virginia_trends.png"),
       p,
       width = 9,
       height = 4)

full_subset = full_subset %>%
  filter(year >= 1994 & year <= 2006) %>%
  group_by(geoid, space_type) %>%
  filter(n() == 13)

pre_treatment_chars = full_subset %>%
  filter(space_type == "LOCAL/PRIVATE") %>%
  filter(year >= 1990 & year <= 2000) %>%
  group_by(geoid) %>%
  mutate(perc.area = perc.area[year == 1994]) %>%
  filter(year == 2000 & !is.na(median_household_income) & !is.na(white_pct)) %>%
  mutate(treat = ifelse(state == "VA", 1, 0))

RATIO = 3

out <-
  matchit(
    treat ~ median_household_income + white_pct + perc.area,
    data = pre_treatment_chars,
    replace = T,
    exact = ~  ruca_code,
    distance = "mahalanobis",
    method = "nearest",
    ratio = RATIO
  )


match_data = match.data(out) %>%
  select(geoid, weights) %>%
  unique()

analysis_df_1 = full_subset %>%
  ungroup() %>%
  filter(geoid %in% match.data(out)$geoid) %>%
  filter(space_type == "LOCAL/PRIVATE") %>%
  mutate(treat = ifelse(state == 'VA', 1, 0)) %>%
  group_by(geoid, space_type) %>%
  mutate(pre_treat_income_type = tc_income_1[year == 2000],
         pre_treat_race_type = tc_race_1[year == 2000]) %>%
  filter(year <= 2006 & year >= 1994)

analysis_df_1 = merge(analysis_df_1, match_data, by = "geoid")

# simple version, no matching
analysis_df_2 = full_subset %>%
  ungroup() %>%
  filter(space_type == "LOCAL/PRIVATE") %>%
  mutate(treat = ifelse(state == 'VA', 1, 0)) %>%
  group_by(geoid, space_type) %>%
  mutate(pre_treat_income_type = tc_income_1[year == 2000],
         pre_treat_race_type = tc_race_1[year == 2000]) %>%
  filter(year <= 2006 & year >= 1994) %>%
  mutate(weights = 1)

run_main_model = function(df) {
  
  p_df = df %>%
    mutate(treat = ifelse(treat == 1, "Virginia", "Control Tracts")) %>%
    group_by(treat, year, space_type) %>%
    summarise(perc.area = weighted.mean(perc.area, weights, na.rm = T))
  
  p1 = p_df %>%
    ggplot(aes(
      x = year,
      y = perc.area,
      shape = as.factor(treat),
      linetype = as.factor(treat)
    )) +
    geom_line() +
    geom_point(size = 1) +
    geom_vline(xintercept = 1999, linetype = "dashed") +
    scale_x_continuous(breaks = seq(1993, 2006, 3)) +
    scale_y_continuous(expand = expansion(mult = 0.5)) +
    theme(legend.position = 'none') +
    guides(colour = guide_legend(nrow = 1)) +
    labs(
      title = "Raw Trends",
      x = "Year",
      y = "Share Tract Protected",
      color = ""
    )
  
  p2 =  p_df %>%
    group_by(treat) %>%
    mutate(baseline = perc.area[year == 1994]) %>%
    mutate(perc.area = (perc.area - baseline) / baseline) %>%
    ggplot(aes(
      x = year,
      y = perc.area,
      shape = as.factor(treat),
      linetype = as.factor(treat)
    )) +
    geom_line() +
    geom_point(size = 1) +
    geom_vline(xintercept = 1999, linetype = "dashed") +
    scale_x_continuous(breaks = seq(1993, 2006, 3)) +
    theme(legend.position = 'bottom') +
    guides(colour = guide_legend(nrow = 1)) +
    labs(
      title = "% Change in Share Tract Protected Since 1994",
      x = "Year",
      y = "% Change in Share Tract Protected",
      color = ""
    )
  
  mod = feols(
    perc.area ~ i(year, treat, ref = 1999) |
      geoid + year,
    cluster = ~ geoid,
    weights = ~ weights,
    data = df
  )
  
  p3 = tidy(mod) %>%
    mutate(year = str_extract(term, "\\d{4}")) %>%
    mutate(year = as.numeric(year)) %>%
    add_case(year = 1999,
             estimate = 0,
             std.error = 0) %>%
    ggplot(aes(x = year, y = estimate)) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(
      ymin = estimate - std.error * 1.96,
      ymax = estimate + std.error * 1.96
    ),
    width = 0) +
    geom_vline(xintercept = 1999, linetype = "dashed") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_x_continuous(breaks = seq(1990, 2010, 2)) +
    labs(
      title = "Event Study",
      x = "Year",
      y = "Coefficient Estimate",
      color = ""
    )
  
  p = grid.arrange(p1,p2, p3, nrow = 3,
                   heights = c(1,1.2,1))
  return(p)
}

main_model_1 = run_main_model(analysis_df_1)
main_model_2 = run_main_model(analysis_df_2)

ggsave(
  paste0(FIG_ROOT_DIR, "virginia_tax_effect.png"),
  main_model_1,
  width = 5,
  height = 6
)

ggsave(
  paste0(FIG_ROOT_DIR, "virginia_tax_effect_alt_control_strategy.png"),
  main_model_2,
  width = 5,
  height = 6
)

## Treatment heterogeneity

out <-
  matchit(
    treat ~ median_household_income + white_pct + perc.area,
    data = pre_treatment_chars %>%
      filter(treat == 0 | tc_income_1 == 'low_income'),
    replace = T,
    exact = ~  ruca_code,
    ties = F,
    distance = "mahalanobis",
    method = "nearest",
    ratio = RATIO
  )

match_data = match.data(out) %>%
  select(geoid, weights) %>%
  unique()

analysis_df_low_1 = analysis_df_2 %>%
  filter(pre_treat_income_type == 'low_income' | treat == 0) %>%
  select(-weights) %>%
  merge(., match_data, by = 'geoid')

analysis_df_low_2 = analysis_df_2 %>%
  filter(pre_treat_income_type == 'low_income') %>%
  mutate(weights = 1)

##

out <-
  matchit(
    treat ~ median_household_income + white_pct + perc.area,
    data = pre_treatment_chars %>%
      filter(treat == 0 |
               tc_income_1 == 'middle_income'),
    replace = T,
    ties = F,
    exact = ~  ruca_code,
    distance = "mahalanobis",
    method = "nearest",
    ratio = RATIO
  )

match_data = match.data(out) %>%
  select(geoid, weights) %>%
  unique()

analysis_df_mid_1 = analysis_df_2 %>%
  filter(pre_treat_income_type == 'middle_income' | treat == 0) %>%
  select(-weights) %>%
  merge(., match_data, by = 'geoid')

analysis_df_mid_2 = analysis_df_2 %>%
  filter(pre_treat_income_type == 'middle_income') %>%
  mutate(weights = 1)

##

out <-
  matchit(
    treat ~ median_household_income + white_pct + perc.area,
    data = pre_treatment_chars %>%
      filter(treat == 0 | tc_income_1 == 'high_income'),
    replace = T,
    ties = F,
    exact = ~  ruca_code,
    distance = "mahalanobis",
    normalize = F,
    method = "nearest",
    ratio = RATIO
  )

match_data = match.data(out) %>%
  select(geoid, weights) %>%
  unique()

analysis_df_top_1 = analysis_df_2 %>%
  filter(pre_treat_income_type == 'high_income' | treat == 0) %>%
  select(-weights) %>%
  merge(., match_data, by = 'geoid')

analysis_df_top_2 = analysis_df_2 %>%
  filter(pre_treat_income_type == 'high_income') %>%
  mutate(weights = 1)

run_income_model = function(analysis_df_low,analysis_df_mid,analysis_df_top){

p_df = bind_rows(
  analysis_df_low %>% mutate(income_type = "Bottom Income Distribution"),
  analysis_df_mid %>% mutate(income_type = "Middle Income Distribution"),
  analysis_df_top %>% mutate(income_type = "Top Income Distribution")) %>%
  mutate(treat = ifelse(treat == 1, "Virginia", "Control Tracts")) %>%
  group_by(year, treat, income_type) %>%
  summarise(perc.area = weighted.mean(perc.area, weights, na.rm = T))


p1 = p_df %>%
  ggplot(aes(
    x = year,
    y = perc.area,
    linetype = as.factor(treat),
    shape = as.factor(treat)
  )) +
  geom_line() +
  geom_point(size = 1) +
  geom_vline(xintercept = 1999, linetype = "dashed") +
  facet_wrap( ~ income_type) +
  scale_x_continuous(breaks = seq(1993, 2006, 3)) +
  scale_y_continuous(expand = expansion(mult = 0.2)) +
  labs(
    title = "Raw Trends",
    x = "Year",
    y = "Share Tract Protected",
    color = ""
  ) +
  theme(legend.position = "none")

p2 = p_df %>%
  group_by(treat, income_type) %>%
  mutate(baseline = perc.area[year == 1994]) %>%
  mutate(perc.area = (perc.area - baseline)) %>%
  ggplot(aes(
    x = year,
    y = perc.area,
    linetype = as.factor(treat),
    shape = as.factor(treat)
  )) +
  geom_line() +
  geom_point(size = 1) +
  geom_vline(xintercept = 1999, linetype = "dashed") +
  facet_wrap( ~ income_type) +
  scale_x_continuous(breaks = seq(1993, 2007, 3)) +
  scale_y_continuous(expand = expansion(mult = 0.5)) +
  labs(
    title = "% Change in Share Tract Protected Since 1994",
    x = "Year",
    y = "% Change in Share Tract Protected",
    color = ""
  ) +
  theme(legend.position = "bottom")


mod_1 = feols(
  perc.area ~ i(year, treat, ref = 1999) |
    geoid + year,
  cluster = ~ geoid,
  weights = ~ weights,
  data = analysis_df_low
)

mod_2 = feols(
  perc.area ~ i(year, treat, ref = 1999) |
    geoid + year,
  cluster = ~ geoid,
  weights = ~ weights,
  data = analysis_df_mid
)

mod_3 = feols(
  perc.area ~ i(year, treat, ref = 1999) |
    geoid + year,
  cluster = ~ geoid,
  weights = ~ weights,
  data = analysis_df_top
)

p3 = bind_rows(
  tidy(mod_1) %>%
    add_case(
      term = '1999',
      estimate = 0,
      std.error = 0
    ) %>%
    mutate(income_type = "Bottom Income Distribution"),
  tidy(mod_2) %>%
    add_case(
      term = '1999',
      estimate = 0,
      std.error = 0
    ) %>%
    mutate(income_type = "Middle Income Distribution"),
  tidy(mod_3) %>% add_case(
    term = '1999',
    estimate = 0,
    std.error = 0
  ) %>%
    mutate(income_type = "Top Income Distribution")
) %>%
  mutate(year = str_extract(term, "\\d{4}")) %>%
  mutate(year = as.numeric(year)) %>%
  ggplot(aes(x = year, y = estimate)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(
    ymin = estimate - std.error * 1.96,
    ymax = estimate + std.error * 1.96
  ),
  width = 0) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap( ~ income_type) +
  geom_vline(xintercept = 1999, linetype = "dashed") +
  scale_x_continuous(breaks = seq(1993, 2006, 3)) +
  labs(
    title = "Event Study",
    x = "Year",
    y = "Coefficient Estimate",
    color = ""
  )

p = grid.arrange(p1, p2, p3, nrow = 3, heights = c(1, 1.2, 1))
return(p)
}

income_model_1 = run_income_model(analysis_df_low_1, analysis_df_mid_1, analysis_df_top_1)
income_model_2 = run_income_model(analysis_df_low_2, analysis_df_mid_2, analysis_df_top_2)

ggsave(
  paste0(FIG_ROOT_DIR, "virginia_tax_effect_income_heterogeneity.png"),
  income_model_1,
  width = 7,
  height = 7
)

ggsave(
  paste0(FIG_ROOT_DIR, "virginia_tax_effect_income_heterogeneity_alt_control_strategy.png"),
  income_model_2,
  width = 7,
  height = 7
)


# ####

out <-
  matchit(
    treat ~ median_household_income + white_pct + perc.area,
    data = pre_treatment_chars %>%
      filter(treat == 0 | tc_race_1 == 'majority_white'),
    replace = T,
    exact = ~  ruca_code,
    distance = "mahalanobis",
    normalize = F,
    method = "nearest",
    ratio = RATIO
  )

match_data = match.data(out) %>%
  select(geoid, weights) %>%
  unique()

analysis_df_white_1 = analysis_df_2 %>%
  filter(treat == 0 | pre_treat_race_type == 'majority_white') %>%
  select(-weights) %>%
  merge(., match_data, by = 'geoid')

analysis_df_white_2 = analysis_df_2 %>%
  filter(pre_treat_race_type == 'majority_white') %>%
  mutate(weights = 1)


out <-
  matchit(
    treat ~ median_household_income + white_pct + perc.area,
    data = pre_treatment_chars %>%
      filter(
        treat == 0 |
          tc_race_1 %in% c('majority_hispanic', 'majority_black')
      ),
    replace = T,
    exact = ~  ruca_code,
    distance = "mahalanobis",
    normalize = F,
    method = "nearest",
    ratio = RATIO
  )

match_data = match.data(out) %>%
  select(geoid, weights) %>%
  unique()

analysis_df_black_hisp_1 = analysis_df_2 %>%
  filter(treat == 0 |
           pre_treat_race_type %in% c('majority_hispanic', 'majority_black')) %>%
  select(-weights) %>%
  merge(., match_data, by = 'geoid')

analysis_df_black_hisp_2 = analysis_df_2 %>%
  filter(pre_treat_race_type %in% c('majority_hispanic', 'majority_black')) %>%
  mutate(weights = 1)


run_race_model = function(analysis_df_white, analysis_df_black_hisp) {

p_df = bind_rows(
  analysis_df_white %>% mutate(race_type = "Majority White Tracts"),
  analysis_df_black_hisp %>% mutate(race_type = "Majority Black or Hispanic Tracts")
) %>%
  mutate(treat = ifelse(treat == 1, "Virginia", "Control Tracts")) %>%
  group_by(year, treat, race_type) %>%
  summarise(perc.area = mean(perc.area, na.rm = T))

p1 = p_df %>%
  ggplot(aes(
    x = year,
    y = perc.area,
    color = as.factor(treat)
  )) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 1999, linetype = "dashed") +
  facet_wrap( ~ race_type) +
  scale_x_continuous(breaks = seq(1994, 2006, 2)) +
  labs(
    title = "Raw Trends",
    x = "Year",
    y = "Share Tract Protected",
    color = ""
  ) +
  theme(legend.position = "none")

p2 = p_df %>%
  group_by(treat, race_type) %>%
  mutate(baseline = perc.area[year == 1994]) %>%
  mutate(perc.area = (perc.area - baseline)) %>%
  ggplot(aes(
    x = year,
    y = perc.area,
    color = as.factor(treat)
  )) +
  geom_line() +
  geom_point(size = 1) +
  geom_vline(xintercept = 1999, linetype = "dashed") +
  facet_wrap( ~ race_type) +
  scale_x_continuous(breaks = seq(1993, 2007, 3)) +
  scale_y_continuous(expand = expansion(mult = 0.5)) +
  labs(
    title = "% Change in Share Tract Protected Since 1994",
    x = "Year",
    y = "% Change in Share Tract Protected",
    color = ""
  ) +
  theme(legend.position = "bottom")



#
mod_1 = feols(
  perc.area ~ i(year, treat, ref = 1999) |
    geoid + year,
  cluster = ~ geoid,
  weights = ~ weights,
  data = analysis_df_white
)

mod_2 = feols(
  perc.area ~ i(year, treat, ref = 1999) |
    geoid + year,
  cluster = ~ geoid,
  weights = ~ weights,
  data = analysis_df_black_hisp
)

p3 = bind_rows(
  tidy(mod_1) %>%
    add_case(
      term = '1999',
      estimate = 0,
      std.error = 0
    ) %>%
    mutate(race_type = "Majority White Tracts"),
  tidy(mod_2) %>% add_case(
    term = '1999',
    estimate = 0,
    std.error = 0
  ) %>%
    mutate(race_type = "Majority Black or Hispanic Tracts")
) %>%
  mutate(year = str_extract(term, "\\d{4}")) %>%
  mutate(year = as.numeric(year)) %>%
  ggplot(aes(x = year, y = estimate)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(
    ymin = estimate - std.error * 1.96,
    ymax = estimate + std.error * 1.96
  ),
  width = 0) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap( ~ race_type) +
  geom_vline(xintercept = 1999, linetype = "dashed") +
  scale_x_continuous(breaks = seq(1994, 2006, 2)) +
  coord_cartesian(ylim = c(-0.005, 0.005)) +
  labs(
    title = "Event Study",
    x = "Year",
    y = "Coefficient Estimate",
    color = ""
  )

p = grid.arrange(p1, p2, p3, nrow = 3,
                 heights = c(1, 1.2, 1))
}

race_model_1 = run_race_model(analysis_df_white_1, analysis_df_black_hisp_1)
race_model_2 = run_race_model(analysis_df_white_2, analysis_df_black_hisp_2)

ggsave(
  paste0(FIG_ROOT_DIR, "virginia_tax_effect_racial_heterogeneity.png"),
  race_model_1,
  width = 7,
  height = 7
)

ggsave(
  paste0(FIG_ROOT_DIR, "virginia_tax_effect_racial_heterogeneity_alt_control_strategy.png"),
  race_model_2,
  width = 7,
  height = 7
)







