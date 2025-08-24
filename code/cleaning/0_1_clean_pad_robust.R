library(igraph)
library(haven)
library(tidyverse)
library(Matrix)
library(chattr)
library(kableExtra)
library(sf)
library(R.utils)
geo.precision = units::set_units(1, 'meters')
library(data.table)
library(tidycensus)
library(data.table)
library(tidyverse)
library(parallel)

## modifies clean_pad functions to make different assumptions!
## see paper

setwd("~/Library/CloudStorage/GoogleDrive-sa9973@princeton.edu/My Drive/research/protected_areas")
data(fips_codes)
pad.dir = 'data/PADUS3_0Geodatabase'
pad.dir = list.files('data/PADUS3_0Geodatabase',
                     pattern='gdb$',
                     full.names=T)
pad.cols <- Hmisc::Cs(featclass,mang_type,
                      loc_mang,loc_ds,loc_own,
                      unit_nm,esmthldr,eholdtyp,
                      own_type,own_name,
                      state_nm,date_est,
                      gis_acres,pub_access, 
                      gap_sts,des_tp,geometry)
nced.dir = 'data/NCED.gdb'
nced.cols = Hmisc::Cs(sitename,state,
                      esmthldr,eholdtype,owntype,
                      gapcat,year_est,
                      pubaccess,purpose,
                      geometry,id_nced)

regex_remove = 'landfill|irrigation|sanitation|flood|water agency|water storage|water district|water department|middleschool|highschool|elementaryschool|school|elementary|college|university|academy|magnet|academic|education|transportation|fire district'

read_pad = function(statefips,state_name,type='acres'){
  
  wktf <- tigris::states() %>%
    rename_with(tolower) %>%
    filter(statefp  == statefips) %>%
    st_transform(5070) %>%
    st_bbox() %>%
    st_as_sfc() %>%
    st_as_text()
  
  lyrs <- pad.dir %>% st_layers()
  
  if (type == 'acres') {
    pad <- map(lyrs$name[c(11, 13)],
               ~ st_read(
                 pad.dir,
                 layer = .x,
                 wkt_filter = wktf,
                 quiet = T
               )) %>%
      set_names(lyrs$name[c(11, 13)]) %>%
      map(tibble) %>%
      map( ~ rename_with(., tolower)) %>%
      map( ~ rename(., geometry = shape)) %>%
      map_dfr( ~ select(tibble(.x), all_of(pad.cols))) 
  } else {
    curr = lyrs$name[11]
    pad <- st_read(pad.dir,
                   layer = curr,
                   wkt_filter = wktf,
                   quiet = T)
    
    pad <- pad %>%
      tibble() %>%
      rename_with(tolower) %>%
      rename(geometry = shape) %>%
      select(all_of(pad.cols))
  }
  
  pad = pad %>%
    st_sf() %>%
    st_transform(5070) %>%
    filter(state_nm == state_name)
  
  des_tp_to_agency = fread('data/manual/des_tp_to_agency.csv')
  
  pad = pad %>% 
    merge(.,des_tp_to_agency,by='des_tp') %>%
    filter(mang_type != 'TRIB') %>%
    filter(!(grepl(regex_remove,unit_nm,ignore.case=T))) %>%
    filter(des_tp != 'MIL') %>%
    mutate(space_type = case_when(
      own_type %in% c('LOC','NGO','PVT') ~ 'LOCAL/PRIVATE',
      own_type %in% c('FED','STAT') ~ 'FEDERAL/STATE',
      mang_type %in% c('LOC','NGO','PVT') ~ 'LOCAL/PRIVATE',
      mang_type %in% c('FED','STAT') ~ 'FEDERAL/STATE',
      agency_des_tp %in% c('state','federal') ~ 'FEDERAL/STATE',
      agency_des_tp %in% c('local','private') ~ 'LOCAL/PRIVATE',
      TRUE ~ own_type
    ))
  
  return(pad)
}

read_nced = function(statefips,state_name){
  nced = st_read(nced.dir, quiet=T) %>%
    tibble() %>%
    filter(state == state_name) %>%
    rename_with(tolower) %>%
    rename(geometry = shape) %>%
    mutate(id_nced = row_number())%>%
    st_sf() %>%
    st_transform(5070)
  simple_features = nced %>% filter(st_geometry_type(geometry) == 'MULTIPOLYGON')
  rare_features = nced %>% filter(st_geometry_type(geometry) != 'MULTIPOLYGON')
  rare_features = simplify_multisurface(rare_features,statefips) %>% 
    st_sf() %>%
    st_transform(5070) 
  nced = bind_rows(simple_features,rare_features) %>%
    st_simplify() %>%
    st_make_valid() #%>%
    #select(all_of(nced.cols))
  nced = nced %>%
    filter(!(grepl(regex_remove,sitename,ignore.case=T))) %>%
    mutate(space_type = case_when(
      owntype %in% c('LOC','NGO','PVT') ~ 'LOCAL/PRIVATE',
      owntype %in% c('FED','STAT') ~ 'FEDERAL/STATE',
      eholdtype %in% c('LOC','NGO','PVT') ~ 'LOCAL/PRIVATE',
      eholdtype %in% c('FED','STAT') ~ 'FEDERAL/STATE',
      grepl('national park',esmthldr,ignore.case=T) ~ 'FEDERAL/STATE',
      grepl('^[A-Z][a-z]+,\\s[A-Z][a-z]+',sitename) ~ 'LOCAL/PRIVATE',
      TRUE ~ 'UNK')) 
  return(nced)
}

simplify_multisurface = function(rare_geos,statefips){
  new_folder_path = paste0('data/intermediate/shp_junk/state_',statefips)
  if (!dir.exists(new_folder_path)) {
    # Create the folder if it does not exist
    dir.create(new_folder_path)
  } 
  simplified = data.frame()
  for(i in 1:nrow(rare_geos)){
    curr_shp = rare_geos[i,]
    filename = paste0(new_folder_path,'/shp_file_1.shp')
    filename2 = paste0(new_folder_path,'/shp_file_2.shp')
    st_write(curr_shp,filename,quiet=T)
    gdalUtilities::ogr2ogr(filename,
                           filename2, 
                           explodecollections = T, 
                           nlt = 'CONVERT_TO_LINEAR',
                           overwrite=T)
    
    curr_shp = st_read(filename2,quiet=T)
    simplified = rbind(simplified,curr_shp)
    to_rm = list.files(new_folder_path,full.names=T)
    unlink(to_rm, force = TRUE)
  }
  return(as.data.frame(simplified))
}

fast_st_difference = function(full_df,st,acres=TRUE){
  print(paste0('st_difference for... ',st))
  if(acres){
    curr_st = full_df %>% 
      st_buffer(0) %>%
      filter(space_type == st) %>%
      arrange(year_est) %>%
      mutate(row_number = row_number())
  }else{
    curr_st = full_df %>% 
      filter(space_type == st) %>%
      arrange(year_est,-st_area(geometry)) %>%
      mutate(row_number = row_number())
  }
  
  check = st_intersects(curr_st)
  non_intersect_df = curr_st[sapply(check, length)==1,]
  intersect_df = curr_st[!sapply(check, length)==1,]
  
  i = rep(1:length(check), lengths(check))
  j = factor(unlist(check))
  
  tab = sparseMatrix(
    i = i,
    j = as.integer(j),
    x = TRUE,
    dimnames = list(NULL, levels(j))
  )
  
  connects = tcrossprod(tab, boolArith = TRUE)
  group = clusters(graph_from_adjacency_matrix(as(connects, "lsCMatrix")))$membership
  res = tapply(check, group, function(x)
    sort(unique(unlist(x))))
  res_intersect = res[(sapply(res, length) > 1)]
  print(paste0('Number of clusters... ',length(res_intersect)))
  
  get_difference = function(i) {
    print(i)
    cluster = res_intersect[[i]]
    curr = intersect_df %>%
      filter(row_number %in% cluster)
    print(nrow(curr))
    write(
      paste0('starting intersection number... ', i, ' for space type...', st, 'with rows....',nrow(curr),'\n'),
      file = paste0('code/code_updates/other_updates.txt'),
      append = TRUE
    )
    curr = curr %>%
      st_simplify() %>%
      st_set_precision(geo.precision) %>%
      st_make_valid() 
    
    if(acres){
      curr = curr %>%
      group_by(year_est) %>%
      summarise(., do_union = T)
    }
    
    diff_function = function(curr_df,buffer=F,buffer_value=10^-2){
      diff = curr_df %>%
        st_collection_extract() %>%
        st_simplify() %>%
        st_set_precision(geo.precision) %>%
        st_make_valid() %>%
        arrange(year_est)
      if(buffer){
        print('Needing to buffer...')
        print(buffer_value)
        diff = diff %>%
          st_buffer(buffer_value) %>%
          st_collection_extract() %>%
          st_simplify() %>%
          st_set_precision(geo.precision) %>%
          st_make_valid()
      }
      diff = diff %>%
        st_difference() 
      return(diff)
    }
    
    curr_diff = try(diff_function(curr))
    if(!is.data.frame(curr_diff)){
      # we only buffer if we have to!
      curr_diff = try(diff_function(curr,T))
      if(!is.data.frame(curr_diff)){
        curr_diff = try(diff_function(curr,T,10^-1))
      }
      if(!is.data.frame(curr_diff)){
        curr_diff = try(diff_function(curr,T,10))
      }
      if(!is.data.frame(curr_diff)){
        curr_diff = try(diff_function(curr,T,10^2))
      }
      if(!is.data.frame(curr_diff)){
        print('Returning without success...')
        curr_diff = curr
      }
    }
    
    write(
      paste0('finished intersection number... ', i, ' for space type...', st,'\n'),
      file = paste0('code/code_updates/other_updates.txt'),
      append = TRUE
    )
    return(curr_diff)
  }
  
  if(length(res_intersect) > 0){
    # apply st_difference to each cluster
    results = lapply(1:length(res_intersect), get_difference)
    #results = mclapply(1:length(res_intersect), get_difference, mc.cores = 10,mc.preschedule = F)
    results_merge = bind_rows(results)
    curr_st = bind_rows(results_merge,
                        non_intersect_df)
  }else{
    curr_st = bind_rows(intersect_df,
                        non_intersect_df)
  }
  curr_st$space_type = st
  return(curr_st)
}


get_intersections = function(i,check,local,federal) {
  write(
    paste0('getting intersection number...', i,'\n'),
    file = paste0('code/code_updates/other_updates.txt'),
    append = TRUE
  )
  t1 = local %>% filter(row_number == i) %>% 
    st_simplify() %>% st_collection_extract()
  t2 = federal %>% filter(row_number %in% check[[i]]) %>% st_simplify()
  t1 = t1 %>% st_make_valid() %>% st_buffer(0.0000001)
  t2 = t2 %>% st_make_valid() %>% st_buffer(0.0000001)
  intersect = st_intersection(t1, t2) %>% st_make_valid()
  intersect = intersect %>%
    mutate(
      space_type_final = case_when(
        year_est > year_est.1 ~ space_type.1,
        year_est < year_est.1 ~ space_type,
        TRUE ~ 'COMBINED'
      ),
      year_est = ifelse(year_est>=year_est.1,year_est.1,year_est)
    ) %>%
    mutate(row_number_local = row_number,
           row_number_federal = row_number.1) %>%
    select(space_type_final, year_est, 
           row_number_local, row_number_federal, geometry) %>%
    rename(space_type = space_type_final)
  intersect = intersect %>% st_make_valid() %>% st_buffer(0.0000001)
  return(intersect)
}


remove_intersection = function(i,int,local,federal,type){
  print(i)
  if(type == 'local'){
    df = local %>% filter(row_number == i)
    int = int %>% filter(row_number_local == i)
  } else {
    df = federal %>% filter(row_number == i)
    int = int %>% filter(row_number_federal == i)
  }
  diff_function = function(df,buffer=F){
    if(!buffer){
      final = df %>%
        st_set_precision(geo.precision) %>%
        st_make_valid() %>%
        st_buffer(0) %>%
        arrange(priority) %>%
        st_difference()
      
    }else{
      final = df %>%
        st_set_precision(geo.precision) %>%
        st_buffer(1) %>%
        st_make_valid() %>%
        arrange(priority) %>%
        st_difference()
      
    }
    return(final)
  }
  merge_df = bind_rows(int %>% mutate(priority=1),
                       df %>% mutate(priority=2))
  final = try(diff_function(merge_df,buffer=F))
  if(!is.data.frame(final)){
    print('Needing to buffer...')
    final = diff_function(merge_df,T)
  }
  return(final)
}


remove_overlaps = function(full_df){
  
  federal = fast_st_difference(full_df,'FEDERAL/STATE')
  local = fast_st_difference(full_df,'LOCAL/PRIVATE')
  write(
    paste0('finished getting local and federal st_difference...','\n'),
    file = paste0('code/code_updates/main_updates.txt'),
    append = TRUE
  )
  #local = local %>% st_collection_extract() %>% st_simplify() %>% arrange(year_est) %>% mutate(row_number = row_number()) 
  local = local %>% st_simplify() %>% arrange(year_est) %>% mutate(row_number = row_number()) 
  #federal = federal %>% st_collection_extract() %>% st_simplify() %>% arrange(year_est) %>% mutate(row_number = row_number())
  federal = federal %>% st_simplify() %>% arrange(year_est) %>% mutate(row_number = row_number())
  check = st_intersects(local, federal)
  if(length(check[[1]]) > 0){
  local_no_intersection = local[sapply(check, length)==0,]
  federal_no_intersection = federal %>% filter(!row_number %in% unlist(check))
  index_intersection = 1:length(check)
  index_intersection = index_intersection[sapply(check, length)>0]
  write(
    paste0('total intersections...',length(index_intersection),'\n'),
    file = paste0('code/code_updates/other_updates.txt'),
    append = TRUE
  )
  intersections = mclapply(index_intersection,get_intersections,check,local,federal,mc.cores=10,mc.preschedule = FALSE)
  intersections = bind_rows(intersections)
  print('Local clean...')
  local_clean = bind_rows(lapply(unique(intersections$row_number_local),
                                 remove_intersection,
                                 intersections,
                                 local,
                                 federal,
                                 'local')) 
  print('Federal clean...')
  federal_clean = bind_rows(lapply(unique(intersections$row_number_federal),
                                   remove_intersection,
                                   intersections,
                                   local,
                                   federal,
                                   'federal'))
  
  results_merge = bind_rows(intersections,local_clean,federal_clean,federal_no_intersection,local_no_intersection)
  } else {
    results_merge = bind_rows(federal,local)
  }
  pa_clean = results_merge %>%
    st_make_valid() %>%
    group_by(year_est, space_type) %>%
    summarise(., do_union = T) %>%
    ungroup() %>%
    st_set_precision(geo.precision) %>%
    st_make_valid() %>%
    mutate(id = 1:nrow(.)
           ,.before = everything())
  
  return(pa_clean)
}


main = function(statefips,type){
  
  state_name = fips_codes %>% 
    filter(state_code == statefips) %>% pull(state) %>% unique()

  pad = read_pad(statefips,state_name,type)
  nced = read_nced(statefips,state_name)
  
  # we use this to understand how classification decisions are made
  # since this is robust version, we don't do it here
  # saveRDS(pad %>% mutate(area = st_area(geometry)), 
  # paste0('data/intermediate/intermediate_pad_nced/',state_name,'_pad.rds'))
  # saveRDS(nced %>% mutate(area = st_area(geometry)), 
  # paste0('data/intermediate/intermediate_pad_nced/',state_name,'_nced.rds'))
  
  print('Read PAD and NCED data...')
  
  full_df = bind_rows(pad %>% 
                        filter(space_type %in% c('FEDERAL/STATE','LOCAL/PRIVATE')) %>%
                        mutate(year_est = as.numeric(date_est)) %>%
                        select(space_type,year_est,geometry),
                      nced %>%
                        filter(space_type %in% c('FEDERAL/STATE','LOCAL/PRIVATE')) %>%
                        select(space_type,year_est,geometry)) %>%
    st_sf() %>%
    st_transform(5070) %>%
    st_set_precision(geo.precision) %>%
    filter(!(is.na(year_est) | year_est <= 0))
    
  
  simple_features = full_df %>% filter(st_geometry_type(geometry) == 'MULTIPOLYGON')
  rare_features = full_df %>% filter(st_geometry_type(geometry) != 'MULTIPOLYGON')
  
  rare_features = simplify_multisurface(rare_features,statefips) %>% 
    st_sf() %>%
    st_transform(5070) 
  
  full_df = rbind(simple_features,rare_features) %>%
    st_simplify() %>%
    st_make_valid()
  
  if(type == 'num_agreements'){
    
    print('Removing agreement overlaps...')
    num_agreements = full_df
    
    num_agreements = num_agreements %>% 
      mutate(original_area = st_area(geometry),
             unique_id = row_number()) %>%
      filter(as.numeric(original_area) > 100)
    
    num_agreements_lp = fast_st_difference(num_agreements,'LOCAL/PRIVATE',acres=F) %>%
      group_by(unique_id) %>%
      summarise(.,do_union=T) %>%
      mutate(new_area = st_area(geometry)) %>%
      st_drop_geometry() 
    
    num_agreements_sp = fast_st_difference(num_agreements,'FEDERAL/STATE',acres=F) %>%
      group_by(unique_id) %>%
      summarise(.,do_union=T) %>%
      mutate(new_area = st_area(geometry)) %>%
      st_drop_geometry()
    
    num_agreements = merge(num_agreements,bind_rows(num_agreements_lp,num_agreements_sp),by='unique_id') %>%
      mutate(unique_share = new_area/original_area) %>%
      filter(as.numeric(unique_share) >= 0.5) %>%
      filter(as.numeric(new_area) > 100)
    
    filename = paste0('data/temporary_processing_files/num_agreements_robust/',
                      state_name,
                      '.rds')
    
    return(list(num_agreements,filename))
    
  }else{
    
    print('Removing acre overlaps...')
    pa_clean = remove_overlaps(full_df)
    filename = paste0('data/temporary_processing_files/acres_robust/',
                      state_name,
                      '.rds')
    return(list(pa_clean,filename))
    
  }
}

