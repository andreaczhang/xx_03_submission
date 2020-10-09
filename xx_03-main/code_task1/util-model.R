
rm_outlier <- function(weekly_df){
  n <- weekly_df$weekly_sum
  lower_bound <- quantile(n, 0.25) - IQR(n)  
  ####### usually it's 1.5IQR, but here we know the systematic error, so remove by force
  
  weekly_df2 <- filter(weekly_df, weekly_sum >= lower_bound)
  return(weekly_df2) 
}



boot_pi_train <- function(model, df, n_rep){
  # create re-samples
  set.seed(1)
  nsamp <- nrow(df)
  res <- list()
  
  for(i in 1:n_rep){
    boot_sample <- df[sample(1:nsamp, size = nsamp, replace = T), ]
    mod_updated <- update(model, data = boot_sample)
    pred_updated <- predict(mod_updated, type = 'response', newdata = df)
    pred_poi <- rpois(length(pred_updated), lambda = pred_updated)
    res[[i]] <- pred_poi
    # cat('boot', i, 'done\n')
  }
  res_df <- do.call(cbind, res)
  
  # get se 
  se <- apply(res_df, 1, sd)
  
  mean_pred <- predict(model, newdata = df, type = 'response')
  upr <- mean_pred + 2*se
  lwr <- mean_pred - 2*se
  
  res <- data.frame(mean_pred = mean_pred, 
                    upper = upr, 
                    lower = lwr)
  
  return(res)
}



boot_pi_test <- function(model, df, n_rep, newdf){
  # create re-samples
  set.seed(1)
  nsamp <- nrow(df)
  res <- list()
  
  for(i in 1:n_rep){
    boot_sample <- df[sample(1:nsamp, size = nsamp, replace = T), ]
    mod_updated <- update(model, data = boot_sample)
    # use new df
    pred_updated <- predict(mod_updated, type = 'response', newdata = newdf)
    pred_poi <- rpois(length(pred_updated), lambda = pred_updated)
    res[[i]] <- pred_poi
    # cat('boot', i, 'done\n')
  }
  res_df <- do.call(cbind, res)
  
  se <- apply(res_df, 1, sd)
  # use newdf
  mean_pred <- predict(model, newdata = newdf, type = 'response')
  upr <- mean_pred + 2*se
  lwr <- mean_pred - 2*se
  
  res <- data.frame(mean_pred = mean_pred, 
                    upper = upr, 
                    lower = lwr)
  
  
  return(res)
}

