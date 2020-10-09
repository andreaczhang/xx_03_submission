# extract outbreak data and visualise, export png

result <- readRDS(file = paste0(clean_path, '/result_weekly.RData'))


county_names <- c('Oslo', 'Rogaland', 'Møre og Romsdal', 'Nordland', 
                  'Viken', 'Innlandet', 'Vestfold og Telemark', 'Agder', 
                  'Vestland', 'Trøndelag', 'Troms og Finnmark')

result_country <- result[['norge']]
result_county <- result[county_names]
result_munip <- result[!names(result) %in% c('norge', county_names)]



visualise_res <- function(location_name, result_list, is_municip){
  
  dat_res <- result_list[[location_name]]
  
  if(is_municip == T){
    x <- filter(norway_locations_b2020, municip_code == location_name)$municip_name
  }else{
    x <- location_name
  }
  
  # select relevant results for plotting
  res_toplot <- select(dat_res$true_2020, c(yr_wk, weekly_sum))
  res_toplot <- dat_res$true_2020
  res_toplot$lower <- dat_res$te_pi$lower
  res_toplot$upper <- dat_res$te_pi$upper
  
  # also identify outbreak
  outbreak <- filter(res_toplot, weekly_sum>upper)
  
  # border range
  lower_border <- min(res_toplot$lower) - 0.1*mean(res_toplot$weekly_sum)
  upper_border <- max(res_toplot$upper) + 0.1*mean(res_toplot$weekly_sum)
  
  p <-  ggplot(res_toplot, aes(x = iso_week, y = weekly_sum)) + 
    geom_ribbon(aes(ymin = lower, 
                    ymax = upper, fill = 'Prediction\n Interval'), alpha = 0.3) + 
    scale_fill_manual("",values="lightblue")+ 
    geom_line() + 
    geom_point(data = outbreak, aes(x = iso_week, y = weekly_sum), 
               colour = 'red', size = 3, shape = 17) + 
    geom_line() + 
    theme_bw() + 
    xlim(1, 52) + 
    ylim(max(lower_border, 0), 
         upper_border) + 
    labs(title=paste('Weekly case and outbreak prediction for disease X (year 2020)\n ', x),
         x ="Week ", y = "Counts") + 
    scale_x_continuous(breaks = seq(1, 52, by = 3)) + 
    theme(
      plot.title = element_text(size=14, hjust = 0.5),
      axis.title.x = element_text(size=14),
      axis.title.y = element_text(size=14)
    )
  
  return(p = p)
}







# ===== produce and save ======== #
# first do the municipalities 

munic_code <- norway_locations_b2020$municip_code

for(i in 1:length(munic_code)){
  figures_municip <- visualise_res(location_name = munic_code[i], 
                                       result_list = result_munip, 
                                       is_municip = T)
  png(file = paste0(save_path, 'municip/outbreaks_', munic_code[i],'.png'), 
      bg = "white", width = 600, height = 350)
  print(figures_municip)
  dev.off()
  
  cat('figure', i, 'complete\n')
}

# names(all_figures_municip) <- norway_locations_b2020$municip_code




# ------ figures for county and country ------- #
for(i in 1:11){
  figure_county <- visualise_res(location_name = county_names[i], 
                                 result_list = result_county, 
                                 is_municip = F)
  
  county_code <- filter(norway_locations_b2020, county_name == county_names[i])$county_code
  png(file = paste0(save_path, 'county/outbreaks_', county_code,'.png'), 
      bg = "white", width = 600, height = 350)
  print(figure_county)
  dev.off()
  
  cat('figure', i, 'complete\n')
}


# norway 
figure_norge <- visualise_res(location_name = 'norge', 
                               result_list = result, 
                               is_municip = F)

png(file = paste0(save_path, 'norge/outbreaks_norge.png'), 
    bg = "white", width = 600, height = 350)
print(figure_norge)
dev.off()



