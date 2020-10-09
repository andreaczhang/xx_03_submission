# install dev packages 
# install.packages(c("fhidata","fhi","fhiplot"), 
#                  repos = c("https://folkehelseinstituttet.github.io/drat", "https://cran.rstudio.com"))


# examine data 

# data_individual$value %>% unique  # only 1 person 
# data_individual$location_code %>% unique %>% length  # 356
# data_individual$location_code %>% table # each has roughly 19300 +-




# ------ 1. examine date, and create date index ------- #
# data_individual$date %>% unique %>% length  # 4003 
# should be 4018 , 3 leap years and 8 regular years (366*3 + 365*8)
# find dates that are being gapped (0 cases)

all_dates <- unique(data_individual$date)
all_dates_chr <- as.character(all_dates)
all_dates_broken <- strsplit(all_dates_chr, '-') 

# take year, month, day from each list, and form year-month-day 
all_years <- map_chr(all_dates_broken, pluck(1))
all_months <- map_chr(all_dates_broken, pluck(2))
all_days <- map_chr(all_dates_broken, pluck(3))

# put in df 
dates_df <- data.frame(yr = all_years, 
                       mn = all_months, 
                       dy = all_days)



count_month_day <- group_by(dates_df, yr, mn) %>% summarise(n = n())
# filter(count_month_day, mn == '02')  # correct: 2012,2016, 2020 have 29 days
# filter(count_month_day, mn == '04') # 2015-04 only has 15 records 


# dates_df %>% filter(yr == '2015' & mn == '04')  # 2015-04-01 to 2015-04-15 are missing 
# data_individual %>% filter(date == '2015-04-01')


# reference complete date index 
dates_2015_apr <- data.frame(yr = '2015', 
                             mn = '04', 
                             dy = as.character(1:15))

# fill back in to complete the date index
# order by year, month and day (in numeric)
dates_df_complete <- rbind(dates_df, dates_2015_apr) %>% arrange(yr, mn, as.numeric(dy))
dates_string_new <- apply(dates_df_complete, 1, function(x){paste(x,  collapse = '-')})
dates_string_new_asdf <- data.frame(date = as.Date(dates_string_new))



# -------- 2. aggregate the counts per day per municip ------- #

counts_day_municipal <- group_by(data_individual, date, location_code) %>% summarise(n = n())

# check 0301 
# m0301 <- filter(counts_day_municipal, location_code == 'municip0301')

# create one df for each municip
all_locnames <- unique(data_individual$location_code)

data_municip_list <- list()
for(i in 1:length(all_locnames)){
  data_municip_list[[i]] <- filter(counts_day_municipal, location_code == all_locnames[i])
  cat('municip number', i, 'separated\n')
}
names(data_municip_list) <- all_locnames





# ------ 3. match the reference date, replace NA to 0 ----- #

# m0301 <- left_join(dates_string_new_asdf, data_municip_list$municip0301, by = 'date')


# try <- match_complete_date(each_municip_df = data_municip_list$municip0301, 
#                            date_ref_df = dates_string_new_asdf)

daily_municip_list <- map(data_municip_list, function(x){
  match_complete_date(each_municip_df = x, 
                      date_ref_df = dates_string_new_asdf)
})


# saveRDS(daily_municip_list, file = paste0(clean_path, '/daily_municip_list.RData'))



# ------- 4. make into isoweek, and take a weekly sum ------ # 

# attach weekly index 
# eg <- daily_municip_list$municip0301 
# make_weekly(eg)

daily_municip_wkinfo <- map(daily_municip_list, make_weekly)



# take weekly sum 
weekly_municip <- map(daily_municip_wkinfo, sum_weekly)

# saveRDS(weekly_municip, file = paste0(clean_path, '/weekly_municip_list.RData')





# ------- 5. aggregate into county + country ------- #
# examine the codes for county and country 

# all equal 
# all.equal(names(weekly_municip), norway_locations_b2020$municip_code )


# subsetting the appropriate counties 

locref <- norway_locations_b2020
# group_by(locref, county_name) %>% summarise(n = n()) %>% print(n = 11)
county_names <- unique(locref$county_name)  # 11 counties 


municip_code_by_county <- map(county_names, function(x){
  locref$municip_code[locref$county_name == x]
})
names(municip_code_by_county) <- county_names




# aggregate the data into county 
weekly_county <- list()
for(i in 1:11){
  weekly_county[[i]] <- weekly_sum_county(municip_code_vec = municip_code_by_county[[i]], 
                                          all_weekly_municip_list = weekly_municip)
  cat('county aggregation', i, 'complete\n')
}
names(weekly_county) <- county_names


# saveRDS(weekly_county, file = paste0(clean_path, '/weekly_county_list.RData')
# the outlier in the first, last and 2015-04 is because the missing and incomplete week


# weekly_county$Oslo %>% head
# 
# all_11counties <- map_df(weekly_county, function(x){x$weekly_sum_county})
# all_11counties %>% matplot(type = 'l')




# ------- 6. aggregate into country level ------- # 

norge <- do.call(rbind, weekly_county)
norge <- weekly_sum_norge(weekly_county)


# saveRDS(norge, file = paste0(clean_path, '/weekly_norge.RData')




