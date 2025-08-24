library(tidyverse)
library(broom)
library(gridExtra)
library(tidyr)
library(kableExtra)
library(data.table)
library(parallel)
library(doParallel)
library(fixest)
library(sf)

setwd("~/Library/CloudStorage/GoogleDrive-sa9973@princeton.edu/My Drive/research/protected_areas")

theme = theme_bw () +theme(axis.text=element_text(size=8),
                           plot.subtitle=element_text(size=9),
                           axis.title=element_text(size=9),
                           legend.text=element_text(size=9),
                           legend.title=element_blank(),
                           strip.text = element_text(size=8))
theme_set(theme)

make_fig = function(robust = FALSE){

if(robust){
  source('code/cleaning/0_1_clean_pad_robust.R')
} else {
  source('code/cleaning/0_1_clean_pad_main.R')
}

square1 <- st_polygon(list(rbind(c(0, 0), c(0, 10), c(10, 10), c(10, 0), c(0, 0))))

square2 <- st_polygon(list(rbind(c(5, 5), c(5, 15), c(15, 15), c(15, 5), c(5, 5))))

square3 <- st_polygon(list(rbind(c(8, 8), c(8, -5), c(18, -5), c(18, 8), c(8, 8))))

square4 <- st_polygon(list(rbind(c(12,0), c(12, -4), c(30, -4), c(30, 0), c(12, 0))))

square5 <- st_polygon(list(rbind(c(20,-3), c(28, -3), c(28, 15), c(20, 15), c(20,-3))))

square6 <- st_polygon(list(rbind(c(35,20), c(45, 20), c(45, 5), c(35, 5), c(35, 20))))

square7 <- st_polygon(list(rbind(c(40,10), c(50, 10), c(50, 0), c(40, 0), c(40, 10))))

square8 <- st_polygon(list(rbind(c(55,25), c(65, 25), c(65, 20), c(55, 20), c(55,25))))

# Create an sf dataframe
squares <- st_sf(geometry = st_sfc(square1, square2, square3, square4, square5, 
                                   square6,square7,square8))

covariates = data.frame(space_type = c('LOCAL/PRIVATE',
                                       'FEDERAL/STATE',
                                       'LOCAL/PRIVATE',
                                       'FEDERAL/STATE',
                                       'FEDERAL/STATE',
                                       'LOCAL/PRIVATE',
                                       'FEDERAL/STATE',
                                       'FEDERAL/STATE'),
                        year_est = c(2003,2004,0,2008,2009,2001,2001,2010))

squares = cbind(squares,covariates)
squares_plot = squares %>%
  mutate(year_est = ifelse(year_est == 0,'Missing date',year_est))
# Plot the squares
p1 = ggplot() +
  geom_sf(data = squares_plot, aes(fill=space_type),
          alpha=0.4,
          lwd=1) +
  geom_sf_text(data = squares_plot, 
               aes(label = year_est), 
               size = 3, 
               color = "black", 
               fontface = "bold") +
  scale_fill_manual(values = c('FEDERAL/STATE' = 'darkseagreen2', 
                               'LOCAL/PRIVATE' = 'lightcoral')) +
  coord_sf() + 
  theme(axis.text.x = element_blank(),axis.text.y = element_blank(),
        legend.position='none') +
  labs(subtitle = 'Pre-processed protected areas')


st_crs(squares) <- 5070

if(robust){
  squares = squares %>%
    filter(year_est > 0)
}

federal = fast_st_difference(squares,'FEDERAL/STATE',acres=F)
local = fast_st_difference(squares,'LOCAL/PRIVATE',acres=F)

p2 = ggplot() +
  geom_sf(data = bind_rows(federal,local), aes(fill=space_type),
          alpha=0.4,
          lwd=1) +
  geom_sf_text(data =  bind_rows(federal,local), 
               aes(label = year_est), 
               size = 3, 
               color = "black", 
               fontface = "bold") +
  scale_fill_manual(values = c('FEDERAL/STATE' = 'darkseagreen2', 'LOCAL/PRIVATE' = 'lightcoral')) +
  coord_sf() +
  theme(axis.text.x = element_blank(),axis.text.y = element_blank(),
        legend.position='none')


sq_clean = remove_overlaps(squares)
sq_clean = sq_clean %>%
  mutate(year_est = ifelse(year_est == 0,'Pre-1980',year_est))

p3 = ggplot() +
  geom_sf(data = sq_clean, aes(fill=space_type),
          alpha = 0.3,
          lwd=1) +
  geom_sf_text(data = sq_clean, 
               aes(label = year_est), 
               size = 3, 
               color = "black", 
               fontface = "bold") +
  coord_sf() +
  scale_fill_manual(values = c('FEDERAL/STATE' = 'darkseagreen2', 
                               'LOCAL/PRIVATE' = 'lightcoral',
                               'COMBINED'='mediumpurple1')) +
  theme(axis.text.x = element_blank(),axis.text.y = element_blank(),
        legend.position='bottom') +
  labs(subtitle = 'Post-processed protected areas')

p = ggpubr::ggarrange(p1,
                      p3,
                      ncol=1,heights=c(1,1.23))

filename = ifelse(robust,
                  'output/figures/process/protected_area_processing_robust.png',
                  'output/figures/process/protected_area_processing.png')
ggsave(p,
       filename = filename,
       height=8,width=6)
}

make_fig()
make_fig(robust=TRUE)
