match_complete_date <- function(each_municip_df, date_ref_df){
  # join two tables
  joined_df <- left_join(date_ref_df, each_municip_df, by = 'date')
  
  # drop the column of municip, and make NA into 0
  clean_df <- select(joined_df, c('date', 'n'))
  clean_df$n <- ifelse(is.na(clean_df$n), 0, clean_df$n)
  return(clean_df)
}





make_weekly <- function(daily_df){
  daily_df$iso_week <- isoweek(daily_df$date)
  daily_df$iso_year <- isoyear(daily_df$date)
  daily_df$yr_wk <- paste(daily_df$iso_year, daily_df$iso_week, sep = '-')
  return(daily_df)
}




sum_weekly <- function(daily_wkinfo_df){
  # necessary to follow the order of year, week
  ordered_yr_wk <- unique(select(daily_wkinfo_df, c(iso_week, iso_year, yr_wk)))
  # take sum
  weekly_df <- group_by(daily_wkinfo_df, yr_wk) %>% summarise(weekly_sum = sum(n))
  # match the order
  weekly_matched_df <- left_join(ordered_yr_wk, weekly_df, by = 'yr_wk')
  return(weekly_matched_df)
}



weekly_sum_county <- function(municip_code_vec, all_weekly_municip_list){
  # select subsets 
  county_list <- all_weekly_municip_list[municip_code_vec]
  
  # merge into one df and take weekly sum
  county_df <- do.call(rbind, county_list)
  weekly_county <- group_by(county_df, yr_wk) %>% summarise(weekly_sum = sum(weekly_sum))
  
  # match back in order 
  ordered_yr_wk <- unique(select(county_df, c(iso_week, iso_year, yr_wk)))
  weekly_matched_county_df <- left_join(ordered_yr_wk, weekly_county, by = 'yr_wk')
  return(weekly_matched_county_df)
}




weekly_sum_norge <- function(all_weekly_county_list){
  # merge into one df and take weekly sum
  norge_df <- do.call(rbind, all_weekly_county_list)
  weekly_norge <- group_by(norge_df, yr_wk) %>% summarise(weekly_sum = sum(weekly_sum))
  
  # match back in order 
  ordered_yr_wk <- unique(select(norge_df, c(iso_week, iso_year, yr_wk)))
  weekly_matched_norge_df <- left_join(ordered_yr_wk, weekly_norge, by = 'yr_wk')
  return(weekly_matched_norge_df)
}



