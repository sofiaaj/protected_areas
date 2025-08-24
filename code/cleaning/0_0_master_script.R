setwd("~/Library/CloudStorage/GoogleDrive-sa9973@princeton.edu/My Drive/research/protected_areas")

library(tidyverse)
library(tidycensus)
library(parallel)

data(fips_codes)
error_states = c('AK','HI','DC')
fips = fips_codes %>% 
  select(state, state_code) %>%
  unique() %>%
  filter(state_code < 60) %>%
  filter(!state %in% error_states) %>%
  pull(state_code)


process_state = function(sf, robust = FALSE) {
  statefips = sf
  
  state_name = fips_codes %>%
    filter(state_code == statefips) %>%
    pull(state) %>% unique()
  
  write(
    paste0('Processing... ', state_name,'\n'),
    file = paste0('code/code_updates/main_updates.txt'),
    append = TRUE
  )
  
  if(!robust){
    source('code/cleaning/0_1_clean_pad_main.R')
  }else{
    source('code/cleaning/0_1_clean_pad_robust.R')
  }
  
  write(
    paste0('Getting num agreeements for... ', state_name, '\n'),
    file = paste0('code/code_updates/main_updates.txt'),
    append = TRUE
  )
  
  num_agreement = main(statefips, 'num_agreements')
  saveRDS(num_agreement[[1]], num_agreement[[2]])
  
  write(
    paste0('Getting acres for... ', state_name,'\n'),
    file = paste0('code/code_updates/main_updates.txt'),
    append = TRUE
  )
  
  acres = main(statefips, 'acres')
  saveRDS(acres[[1]], acres[[2]])
  
  source('pad_to_tract.R')
  
  write( 
    paste0('PAD to tract for... ', state_name),
    file = paste0('code/code_updates/main_updates.txt'),
    append = TRUE
  )
  
  if(!robust){
    df = main(statefips)
    fwrite(df,
           paste0('data/processed_pad_tracts_by_state/main/', state_name, '.csv'))
  }else{
    df = main(statefips,robust = TRUE)
    fwrite(df,
           paste0('data/processed_pad_tracts_by_state/robust/', state_name, '.csv'))
    
  }
  
}

state_manager = function(f,robust=FALSE){
  
  write( 
    paste0('Process PAD for... ', f,'\n'),
    file = paste0('code/code_updates/main_updates.txt'),
    append = TRUE
  )
  
  check = try(process_state(f,robust))
  
  if(class(check) == 'try-error'){
    write( 
      paste0('Error for... ', f, 'Message: ', check,'\n'),
      file = paste0('code/code_updates/main_updates.txt'),
      append = TRUE
    )
  }
}



library(foreach)
library(doParallel)

cl <- makeCluster(5)
registerDoParallel(cl)

foreach(f = fips) %do% {
  library(tidyverse)
  state_manager(f)
}

# robust version
foreach(f = fips) %do% {
  library(tidyverse)
  state_manager(f,robust = TRUE)
}

stopCluster(cl)
registerDoSEQ()

