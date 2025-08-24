library(tidyverse)
library(data.table)
library(haven)
library(lorenz)
setwd("~/Library/CloudStorage/GoogleDrive-sa9973@princeton.edu/My Drive/research/protected_areas")

# relevant function
# uses functions from lorenz package that are not exported 
# freqs = vector of frequencies corresponding to each bin
# bounds = lower bound for each bin
# mean = overall income mean
# use MCIB procedure to calculate national income distribution
mcib_percentiles_r_function = function(freqs,bounds,mean){
  N <- sum(freqs)
  agg <- mean * N
  rescaled_bounds <- bounds[2:length(bounds)]/sqrt(N)
  F_x <- (cumsum(freqs)/sum(freqs))[1:(length(freqs) - 1)]
  mcib_coords <- lorenz:::freq_and_bounds_to_mcib_coords(freqs[1:(length(freqs) - 
                                                                    1)], bounds)
  slopes_ints <- lorenz:::mcib_coords_to_slopes_ints(mcib_coords)
  slopes_ints_rescaled <- lorenz:::mcib_coords_to_slopes_ints(mcib_coords/sqrt(N))
  slopes_ints_rescaled <- lorenz:::x_adj(slopes_ints_rescaled, bounds/sqrt(N), 
                                         mcib_coords/sqrt(N))
  poly_coefs <- lorenz:::slopes_ints_rescaled_to_poly_coefs(slopes_ints_rescaled, 
                                                            rescaled_bounds, F_x)
  closed_bracket_means <- lorenz:::MCIB_closed_bracket_means(slopes_ints, 
                                                             bounds, freqs)
  top_bracket_mean <- (agg - sum(freqs[1:(length(freqs) - 1)] * 
                                   closed_bracket_means))/freqs[length(freqs)]
  pareto_parms <- lorenz:::top_bracket_mean_to_rescaled_pareto_parms(top_bracket_mean, 
                                                                     bounds, N)
  percentile = data.frame('r_mcib' = lorenz:::inverse_cdf_full(seq(0.01, 0.99, 0.01), c(0, F_x, 1), 
                                                               poly_coefs, pareto_parms, F_x) * sqrt(N)) %>%
    mutate(percentile = paste0("p", seq(1, 99, 1)))
  
  return(percentile)
}


get_year_income_percentile = function(year){
  # also from social explorer
  file = paste0('data/raw/income_bins/income_bins_', year, '.csv')
  curr_bins = fread(file)
  curr_bins = curr_bins[2,] %>%
    gather()
  average = curr_bins %>%
    filter(key %like% 'Average') %>%
    pull(value) %>%
    as.numeric()
  curr_bins = curr_bins %>%
    filter(key %like% '\\$') %>%
    mutate(lower_bound = case_when(
      grepl("less than",key,ignore.case=T) ~ "$0",
      TRUE ~ str_extract(key, "\\$?\\d{1,3},?\\d{0,3}")
    )) %>%
    mutate(lower_bound = as.numeric(str_replace_all(lower_bound, "\\$|\\,", ""))) %>%
    rename(freq = value) %>%
    mutate_at(c('freq','lower_bound'),as.numeric)
  df = mcib_percentiles_r_function(curr_bins$freq, curr_bins$lower_bound, average) %>%
    mutate(year = year)
  return(df)
}

income_percentile = map_dfr(c(1980,1990,2000,2008:2017), get_year_income_percentile)
fwrite(income_percentile, 'data/intermediate/income_percentiles_1980_2017.csv')

