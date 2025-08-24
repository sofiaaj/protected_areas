library(tidyverse)
library(gridExtra)
library(tidyr)
library(sf)
library(ggpubr)
library(maps)
library(tigris)
library(ggrepel)
library(rgeos)
library(data.table)
library(ggmap)
library(kableExtra)
setwd("~/Library/CloudStorage/GoogleDrive-sa9973@princeton.edu/My Drive/research/protected_areas")
geo.precision = units::set_units(1, 'meters')
api_key = readLines('data/manual/google_api_key.txt')
register_google(key = api_key)

setwd('~/Library/CloudStorage/GoogleDrive-sa9973@princeton.edu/My Drive/research/protected_areas/')
des_tp_to_agency = fread('data/manual/des_tp_to_agency.csv')
des_tp_to_name = fread('data/manual/des_tp_to_name.csv')

des_tp_to_agency %>%
  merge(.,
        des_tp_to_name,
        by='des_tp') %>%
  group_by(agency_des_tp) %>%
  summarise(des_tp = paste0(unique(des_name),collapse=', ')) %>%
  fwrite(.,'output/tables/process/des_tp_classification.csv')

# make maps of different tracts
# start with loop to find good candidate tracts

# full_df = fread("data/processed_states_2010_tracts/protected_areas_master_file_clean_v2.csv")
# candidates = full_df %>%
#   filter(year == 2019) %>%
#   select(geoid,perc.area,space_type,state,ruca_code) %>%
#   filter(space_type != 'ALL PROTECTED') %>%
#   spread(space_type,perc.area) %>%
#   filter(`FEDERAL/STATE` > 0 & `LOCAL/PRIVATE` > 0) %>%
#   group_by(ruca_code) %>%
#   mutate(mean_fs = mean(`FEDERAL/STATE`),
#          mean_lp = mean(`LOCAL/PRIVATE`)) %>%
#   mutate(diff_fs = abs(`FEDERAL/STATE` - mean_fs),
#          diff_lp = abs(`LOCAL/PRIVATE` - mean_lp)) %>%
#   arrange((diff_fs)) %>%
#   group_by(ruca_code) %>%
#   mutate(rank_fs = row_number()) %>%
#   arrange((diff_lp)) %>%
#   group_by(ruca_code) %>%
#   mutate(rank_lp = row_number()) %>%
#   filter(rank_fs <= 20 | rank_lp <= 20) %>%
#   mutate(type = ifelse(rank_fs <= 20,'fs','lp'))
# 
# 
# states = unique(candidates$state)
# data(fips_codes)
# for(st in states){
#   fips = unique(fips_codes[which(fips_codes$state == st),'state_code'])
#   pad = readRDS(paste0('data/intermediate_pad_nced/',st,'_pad.rds'))
#   state_data = get_tracts(fips) %>%
#     mutate(geoid = as.numeric(geoid))
#   tracts = candidates %>% mutate(geoid = as.numeric(geoid)) %>% filter(state == st) %>% pull(geoid)
#   for(curr_geoid in tracts){
#     tract_ruca = candidates %>% filter(geoid == curr_geoid) %>% pull(ruca_code)
#     tract_ruca =  gsub("[^a-zA-Z]", "", tract_ruca)
#     type = candidates %>% filter(geoid == curr_geoid) %>% pull(type)
#     curr_tract = state_data %>%
#       filter(geoid == curr_geoid) %>%
#       st_transform(4326)
#     lonlat = gCentroid(as(curr_tract, "Spatial"),byid=TRUE) %>% as.tibble()
#     map <- ggmap::get_map(location = c(lon=lonlat$x,
#                                        lat=lonlat$y),
#                           zoom=12)
#     map <- ggmap_bbox(map)
#     curr_tract = curr_tract %>% st_transform(3857)
#     protected = pad %>% st_transform(3857) %>% filter(st_geometry_type(geometry) == 'MULTIPOLYGON')
#     intersection = st_intersection(curr_tract,protected)
#     
#     m = ggmap(map) + 
#       coord_sf(crs = st_crs(3857)) + 
#       geom_sf(data=curr_tract,color="black",inherit.aes = F,lwd=0.5,fill=alpha("white",0.3)) +
#       geom_sf(data=intersection,lwd=1,aes(fill=space_type),alpha=0.3,inherit.aes = F)
#     
#     ggsave(paste0('data/intermediate_pad_nced_figs/',type,'_',tract_ruca,'_',st,'_',curr_geoid,'.png'),plot = m,width=8,height=8)
#     
#   }
# }

get_tracts = function(statefips){
  nbhds = tigris::tracts(state = statefips,
                         year = 2010) %>%
    rename_with(tolower) %>%
    st_transform(5070) %>%
    #erase_water(area_threshold = 0.95) %>%
    select(-countyfp,-statefp) %>%
    set_names(str_replace_all(colnames(.),'10','')) %>%
    select(statefp,countyfp,geoid,
           aland,awater,geometry ) %>%
    filter(aland > 0) %>%
    st_set_precision(geo.precision) %>%
    st_make_valid()
  return(nbhds)
}

ggmap_bbox = function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))
  
  # Coonvert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}



build_map = function(st,curr_geoid,zm,ruca_type){
  fips = unique(fips_codes[which(fips_codes$state == st),'state_code'])
  state_name = unique(fips_codes[which(fips_codes$state == st),'state_name'])
  pad = readRDS(paste0('data/intermediate/intermediate_pad_nced/',st,'_pad.rds'))
  state_data = get_tracts(fips) %>% mutate(geoid = as.numeric(geoid))
  curr_tract = state_data %>%
    filter(geoid == curr_geoid) %>%
    st_transform(4326)
  lonlat = gCentroid(as(curr_tract, "Spatial"),byid=TRUE) %>% as.tibble()
  
  map <- ggmap::get_map(location = c(lon=lonlat$x,
                                     lat=lonlat$y),
                        zoom=zm)
  map <- ggmap_bbox(map)
  curr_tract = curr_tract %>% st_transform(3857)
  protected = pad %>% st_transform(3857) %>% filter(st_geometry_type(geometry) == 'MULTIPOLYGON')
  intersection = st_intersection(curr_tract,protected) %>%
    filter(loc_own != 'UNK' & !grepl('Agricultural Conservation Easement Program',unit_nm,ignore.case=T)) %>%
    filter(loc_own != '' & !is.na(loc_own)) %>%
    mutate(centroid = st_centroid(geometry),
           label = LETTERS[row_number()]) %>%
    mutate(loc_own = ifelse(loc_own == 'PVT','Private',loc_own)) %>%
    mutate(label = paste(unit_nm,loc_own,sep="\n")) %>%
    mutate(label = str_to_title(label)) %>%
    arrange(-st_area(geometry)) %>%
    mutate(row_number_max = row_number()) %>%
    arrange(st_area(geometry)) %>%
    mutate(row_number_min = row_number()) %>%
    group_by(unit_nm) %>%
    filter(row_number() == 1)
  intersection$lon = st_coordinates(intersection$centroid)[,1]
  intersection$lat = st_coordinates(intersection$centroid)[,2]
  plot_title = paste0(state_name,'; ',ruca_type)
  map = ggmap(map) + 
    coord_sf(crs = st_crs(3857)) + 
    geom_sf(data=curr_tract,color="black",inherit.aes = F,lwd=1,fill=alpha("white",0.3)) +
    geom_sf(data=intersection,lwd=0.5,aes(fill=space_type,color=space_type),alpha=0.3,inherit.aes = F) +
    geom_label_repel(data=subset(intersection,row_number_max <= 5 | row_number_min <= 5),
                     aes(x = lon,
                         y = lat,
                         label = label),
                     box.padding = 1,
                     max.overlaps=Inf,
                     size=4) +
    theme(
      axis.title.x = element_blank(),  # Remove x-axis title
      axis.title.y = element_blank(),  # Remove y-axis title
      axis.text.x = element_blank(),   # Remove x-axis text
      axis.text.y = element_blank(),   # Remove y-axis text
      axis.ticks = element_blank(),
      legend.position = "none"
    ) +
    labs(title = plot_title)
  return(map)
}


# from those candidates we choose the following tracts
tract_df = data.frame(geoid = c(25017374800,42089300101,33007951100,34011010500,51099040500,56039967800,56025001603),
                      state = c('MA','PA','NH','NJ','VA','WY','WY'),
                      ruca_type = c('Core Metro Tract','High Commute Metro Tract','High Commute Small Town Tract','Low Commute Micro Tract','Low Commute Small Town Tract','Core Micro Tract','Core Metro Tract'),
                      zoom = c(14,12,12,12,12,12,12))


maps = list()
for(i in 1:nrow(tract_df)){
  curr_map = build_map(tract_df$state[i],tract_df$geoid[i],tract_df$zoom[i],tract_df$ruca_type[i])
  filename = paste0('output/figures/process/map_fig_',i,'.png')
  ggsave(filename,plot = curr_map,width=8,height=8)
}


### classification decisions

pad = map_dfr(list.files('data/intermediate/intermediate_pad_nced',pattern='pad.rds',full.names = T),readRDS)
pad = pad %>%
  mutate(space_type_choice = case_when(
    own_type %in% c('LOC','NGO','PVT') ~ 'own_type',
    own_type %in% c('FED','STAT') ~ 'own_type',
    mang_type %in% c('LOC','NGO','PVT') ~ 'mang_type',
    mang_type %in% c('FED','STAT') ~ 'mang_type',
    agency_des_tp %in% c('state','federal') ~ 'des_type',
    agency_des_tp %in% c('local','private') ~ 'des_type',
    TRUE ~ 'not_classified'
  ))
pad_choices = pad %>%
  st_drop_geometry() %>%
  group_by(space_type_choice) %>%
  summarise(share=n()) %>%
  mutate(share = share/sum(share)) %>%
  mutate(space_type_choice = case_when(
    space_type_choice == 'own_type' ~ 'Ownership Type',
    space_type_choice == 'mang_type' ~ 'Management Type',
    space_type_choice == 'des_type' ~ 'Designation Type',
    TRUE ~ 'Not Classified'
  )) %>%
  mutate(data_type = 'PAD')

nced = map_dfr(list.files('data/intermediate/intermediate_pad_nced',pattern='nced.rds',full.names = T),readRDS)
nced = nced %>%
  mutate(space_type_choice = case_when(
    owntype %in% c('LOC','NGO','PVT') ~ 'owntype',
    owntype %in% c('FED','STAT') ~ 'owntype',
    eholdtype %in% c('LOC','NGO','PVT') ~ 'eholdtype',
    eholdtype %in% c('FED','STAT') ~ 'eholdtype',
    grepl('national park',esmthldr,ignore.case=T) ~ 'eholdtype',
    grepl('^[A-Z][a-z]+,\\s[A-Z][a-z]+',sitename) ~ 'sitename',
    TRUE ~ 'not_classified'
  ))
nced_choices = nced %>%
  st_drop_geometry() %>%
  group_by(space_type_choice) %>%
  summarise(share=n()) %>%
  mutate(share = share/sum(share)) %>%
  mutate(space_type_choice = case_when(
    space_type_choice == 'owntype' ~ 'Ownership Type',
    space_type_choice == 'eholdtype' ~ 'Management Type',
    space_type_choice == 'sitename' ~ 'Site Name',
    TRUE ~ 'Not Classified'
  )) %>%
  mutate(data_type = 'NCED')

# sitename example:
# Mitchell, David Scott in Virginia or Sharpl, Bayard in Delaware
#nced %>%
  #st_drop_geometry() %>%
  #filter(space_type_choice == 'sitename')

bind_rows(pad_choices,nced_choices) %>%
  arrange(data_type,-share) %>%
  mutate(share = scales::percent(share)) %>%
  kable(format = "latex",
        booktabs = T,
        col.names = c('Classification Strategy', '% Records', 'Data Source')) %>%
  save_kable('output/tables/process/classification_strategy.tex')

## heterogeneity within broad RUCA codes

df = fread("data/derived/protected_areas_master_file_clean.csv")
df = df %>% 
  filter(ruca_code != 'Rural' & ruca_code != '' & ruca_code != 'Missing') %>%
  filter(year == 2019) %>%
  filter(!is.na(total_pop) & nbhd.land.acres != 0 & total_pop != 0 & !is.na(median_household_income)) %>%
  filter(ruca_code != 'Missing') 


st_crs(pad)
pad = pad %>%
  mutate(size = st_area(geometry))
pad_clean = pad %>%
  st_drop_geometry() %>%
  filter(space_type %in% c('FEDERAL/STATE','LOCAL/PRIVATE'))
des_tp_cw = fread('data/manual/des_tp_categories.csv') %>%
  mutate(des_name = case_when(
    des_name %in% c('not designated', 'unknown', 'other') ~ 'unknown or not designated',
    des_name %in% c('unknown easement', 'other easement') ~ 'other easement',
    des_name %in% c('state other or unknown','federal other or unknown') ~ 'state/federal other or unknown',
    TRUE ~ des_name
  ))
         
pad_clean = merge(pad_clean,des_tp_cw,by='des_tp',all.x=T)
sizes = pad_clean %>%
  group_by(des_name) %>%
  summarise(avg_size = round(mean(size/1000),2)) %>%
  arrange(-avg_size) %>%
  mutate(order = ifelse(des_name == 'other',100,row_number())) %>%
  arrange(order) %>%
  select(-order)

set.seed(10)
top_loc_ds = pad_clean %>%
  group_by(des_name,loc_ds) %>%
  summarise(count = n()) %>%
  group_by(des_name) %>%
  slice_max(count,n=5)

pad_clean %>%
  merge(.,top_loc_ds,by=c('des_name','loc_ds')) %>%
  group_by(des_name,count) %>%
  sample_n(min(10,n())) %>%
  select(des_name,loc_ds,count,unit_nm,state_nm) %>%
  arrange(des_name,-count) %>%
  fwrite(.,'output/tables/process/designation_types_examples.csv')

# edited by hand
examples_final = readxl::read_excel('output/tables/process/designation_types_examples_clean.xlsx')
examples_final = examples_final %>% filter(keep == 1) 

sub_designations = examples_final %>%
  filter(!is.na(loc_ds)) %>%
  select(des_name,loc_ds) %>%
  unique() %>%
  group_by(des_name) %>%
  summarize(loc_ds = paste(loc_ds,collapse = '; '))

examples = examples_final %>%
  filter(!is.na(unit_nm)) %>%
  mutate(unit_nm = ifelse(is.na(state_nm),unit_nm,paste(unit_nm,state_nm,sep=", "))) %>%
  select(des_name,unit_nm) %>%
  unique() %>%
  group_by(des_name) %>%
  summarize(unit_nm = paste(unit_nm,collapse = '; '))

merge(sizes %>% 
        rename(avg_size_m2 = avg_size) %>% 
        mutate(avg_size_m2 = round(as.numeric(avg_size_m2),1)),
      sub_designations,by='des_name') %>%
  merge(.,examples,by='des_name') %>%
  kable(format = "latex",
        booktabs = T,
        align = "l",
        longtable = T,
        col.names = c('Designation Type', 'Average Size', 'Top Sub-Categories', 'Examples')) %>%
  column_spec(1, width = "2cm") %>%
  column_spec(2, width = "1cm") %>%
  column_spec(3, width = "5cm") %>%
  column_spec(4, width = "8cm") %>%
  kable_styling(full_width = F, fixed_thead = T, latex_options = c("HOLD_position", "repeat_header")) %>%
  save_kable('output/tables/process/designation_type_examples.tex')

size = pad_clean %>%
  group_by(space_type) %>%
  summarize(avg_size = round(mean(size/1000),2)) %>%
  spread(space_type,avg_size) %>%
  mutate(variable_name = 'Average size (m^2)') %>%
  mutate_all(as.character)

top_des_tp = pad_clean %>%
  group_by(space_type,des_name) %>%
  summarize(count = sum(area)) %>%
  group_by(space_type) %>%
  mutate(share = count/sum(count)) %>%
  group_by(space_type) %>%
  slice_max(share,n=10) %>%
  mutate(share = round(share*100,2)) %>%
  select(-count) %>%
  arrange(desc(share)) %>%
  group_by(space_type) %>%
  mutate(rank = row_number()) %>%
  mutate(share = paste0(share,'%')) %>%
  mutate(des_name = paste0(des_name,': ',' (',share,')'))  %>%
  select(-share) %>%
  spread(space_type,des_name)  %>%
  select(-rank) %>%
  mutate(variable_name = 'Top designation types (% of total acres)') %>%
  mutate_all(as.character)

access_type = pad_clean %>%
  group_by(space_type,pub_access) %>%
  summarize(count = sum(area)) %>%
  group_by(space_type) %>%
  mutate(share = count/sum(count),
         order = case_when(
           pub_access == 'OA' ~ 1,
           pub_access == 'RA' ~ 2,
           pub_access == 'XA' ~ 3,
           pub_access == 'UK' ~ 4),
         pub_access = case_when(
           pub_access == 'OA' ~ 'Public can visit',
           pub_access == 'RA' ~ 'Access is restricted (e.g. requires permission)',
           pub_access == 'XA' ~ 'Closed to public',
           pub_access == 'UK' ~ 'Access is unknown')) %>%
  select(-count) %>%
  mutate(share = round(share*100,2),
         share = paste0(share,'%')) %>%
  spread(space_type,share) %>%
  arrange(order) %>%
  select(-order) %>%
  rename(variable_name = pub_access) %>%
  mutate_all(as.character) 

space_subtypes = pad_clean %>%
  mutate(space_type_1 = case_when(
    space_type_choice == 'own_type' ~ own_type,
    space_type_choice == 'mang_type' ~ mang_type,
    space_type_choice == 'des_type' ~ agency_des_tp)) %>%
  mutate(space_type_1 = case_when(
    space_type_1 == 'federal' ~ 'FED',
    space_type_1 == 'state' ~ 'STAT',
    space_type_1 == 'local' ~ 'LOC',
    space_type_1 == 'private' ~ 'PVT',
    TRUE ~ space_type_1)) %>%
  mutate(space_type_1 = case_when(
    space_type_1 == 'FED' ~ 'Federal Government',
    space_type_1 == 'STAT' ~ 'State Government',
    space_type_1 == 'LOC' ~ 'Local Government',
    space_type_1 == 'PVT' ~ 'Private Entity or NGO',
    space_type_1 == 'NGO' ~ 'Private Entity or NGO')) %>%
  group_by(space_type,space_type_1) %>%
  summarize(count = sum(area)) %>%
  group_by(space_type) %>%
  mutate(share = count/sum(count)) %>%
  arrange(-share) %>%
  group_by(space_type) %>%
  mutate(rank = row_number()) %>%
  mutate(space_type_1 = paste0(space_type_1,': ',' (',round(share*100,2),'%)')) %>%
  arrange(space_type,-share) %>%
  select(space_type,space_type_1,rank) %>%
  spread(space_type,space_type_1) %>%
  select(-rank) %>%
  mutate(variable_name = 'Subtypes distribution (% of total acres)') %>%
  mutate_all(as.character)

bind_rows(size,top_des_tp,access_type,space_subtypes) %>%
  select(3,1,2) %>%
  kable(format = "latex",
        booktabs = T) %>%
  column_spec(1, width = "3cm") %>%
  column_spec(2, width = "5cm") %>%
  column_spec(3, width = "5cm") %>%
  collapse_rows(column = 1,valign="top",latex_hline="none") %>%
  save_kable('output/tables/process/space_type_descriptives.tex')
