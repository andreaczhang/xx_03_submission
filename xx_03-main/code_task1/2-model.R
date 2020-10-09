# start with country level data
# clean_path <- paste0(folder_path, '/xx_03-main/data_clean/')


norge <- readRDS(file = paste0(clean_path, '/weekly_norge.RData'))
county <- readRDS(file = paste0(clean_path, '/weekly_county_list.RData'))
munip <- readRDS(file = paste0(clean_path, '/weekly_municip_list.RData'))


# put in one list 
all_weekly_data <- c(list(norge = norge), county, munip)


# ----- workflow -------- #
workflow <- function(weeklydf){
  
  # 1. remove obvious outliers 
  wkdf2 <- rm_outlier(weekly_df = weeklydf)

  # 2. construct month data, and select relevant columns
  pseudo_month <- c(rep(c(1, 3, 5, 7, 9, 10), each = 5), 
                    rep(c(4, 6, 8, 11, 12), each = 4), 
                    rep(2, 3)) %>% sort %>% as.factor
  
  week_month_ref <- data.frame(iso_week = seq(1, 53), 
                               month = pseudo_month[1:53])
  
  wkdf_with_month <- left_join(wkdf2, week_month_ref, by = 'iso_week') %>% 
    select(c(iso_year, month, weekly_sum))

  
  # 3. split data 
  wkdata_tr <- filter(wkdf_with_month, iso_year %in% as.character(seq(2010, 2019)))
  wkdata_te <- filter(wkdf_with_month, iso_year == '2020')

  # 4. build model on training data
  cat('building model\n')
  model1 <- glm(weekly_sum ~ iso_year  + month, data = wkdata_tr, family = 'poisson')
  
  # get PI on training data and remove outliers
  tr_pi <- boot_pi_train(model = model1, df = wkdata_tr, n_rep = 500)
  cat('bootstrap for PI: training\n')
  
  # plot(wkdata_tr$weekly_sum, type = 'l')
  # lines(tr_pi$upper, col = 'red', lwd = 2)
  
  wkdata_tr_rmoutbreak <- mutate(wkdata_tr, upper = tr_pi$upper) %>% 
    filter(weekly_sum <= upper)

  # refit
  model1_updated <- update(model1, data = wkdata_tr_rmoutbreak)

  # ----- and use the production data ------ #
  wkdata_te_X <- select(wkdata_te, -weekly_sum)
  
  te_pi <- boot_pi_test(model = model1_updated, df = wkdata_tr, n_rep = 500, newdf = wkdata_te_X)
  cat('bootstrap for PI: production\n')
  
  # identify outbreak in test data
  wkdata_te_outbreak <- mutate(wkdata_te, upper = te_pi$upper) %>% 
    filter(weekly_sum > upper)
  cat('Process done\n')
  
  true_2020 <- filter(wkdf2, iso_year == '2020')
  
  return(list(true_2020 = true_2020, 
              te_pi = te_pi, 
              outbreak = wkdata_te_outbreak))
}



# plot(wkdata_te$weekly_sum, type = 'l')
# lines(te_pi$upper, col = 'red', lwd = 2)


######### a careful treatment here would require tryCatch() ####### 

result <- list()
for(k in 1:length(all_weekly_data)){
  
  dat <- all_weekly_data[[k]]
  cat('Computing for dataset', k, '\n')
  
  tic()
  result[[k]] <- workflow(weeklydf = dat)
  toc()
  
  cat('Dataset', k, 'complete\n')
  
}

names(result) <- names(all_weekly_data)
# saveRDS(result, file = paste0(clean_path, '/result_weekly.RData'))






