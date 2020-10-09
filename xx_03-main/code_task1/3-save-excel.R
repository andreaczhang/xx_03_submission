# extract outbreak data and export excel 

result <- readRDS(file = paste0(clean_path, '/result_weekly.RData'))


county_names <- c('Oslo', 'Rogaland', 'Møre og Romsdal', 'Nordland', 
                  'Viken', 'Innlandet', 'Vestfold og Telemark', 'Agder', 
                  'Vestland', 'Trøndelag', 'Troms og Finnmark')

result_country <- result[['norge']]
result_county <- result[county_names]
result_munip <- result[!names(result) %in% c('norge', county_names)]



# ======== out break results ======== #

report_outbreak <- function(location_name, result_list){
  
  relevant_tb <- result_list[[location_name]]
  
  overall_results <- data.frame(yr_wk = relevant_tb$true_2020$yr_wk, 
                                true_cases = relevant_tb$true_2020$weekly_sum, 
                                predicted_cases = relevant_tb$te_pi$mean_pred, 
                                upper = relevant_tb$te_pi$upper, 
                                lower = relevant_tb$te_pi$lower)
  
  outbreakres <- filter(overall_results, true_cases > upper)
  return(outbreakres)
}


# tryres <- report_outbreak(location_name = 'Oslo', result_list = result)
# x <- filter(norway_locations_b2020, municip_code == location_name)$municip_name



munic_code <- norway_locations_b2020$municip_code

for(i in 1:length(munic_code)){
  report_municip <- report_outbreak(location_name = munic_code[i], 
                                       result_list = result_munip)
  
  writexl::write_xlsx(report_municip, paste0(save_path, 'municip/outbreaks_', munic_code[i],'.xlsx'))
  cat('report', i, 'complete\n')
}


# now save the county and country 

for(i in 1:11){
  report_county <- report_outbreak(location_name = county_names[i], 
                                    result_list = result_county)
  
  county_code <- filter(norway_locations_b2020, county_name == county_names[i])$county_code
  writexl::write_xlsx(report_county, paste0(save_path, 'county/outbreaks_', county_code,'.xlsx'))
  cat('report', i, 'complete\n')
}


# save for norge
report_norge <- report_outbreak(location_name = 'norge', 
                                 result_list = result)

writexl::write_xlsx(report_norge, paste0(save_path, 'norge/outbreaks_norge.xlsx'))




