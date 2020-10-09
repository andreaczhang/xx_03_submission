# creative task: reporting with ggplot 

norge <- readRDS(file = paste0(clean_path, '/weekly_norge.RData'))
county <- readRDS(file = paste0(clean_path, '/weekly_county_list.RData'))
munip <- readRDS(file = paste0(clean_path, '/weekly_municip_list.RData'))


all_counties <- map(county, function(x){filter(x, iso_year == '2020' & iso_week >40) %>% 
    select(c(iso_week, weekly_sum))})

# make into df
all_counties_df <- do.call(rbind, all_counties)
rowname_simplified_county <- strsplit(rownames(all_counties_df), '[.]') %>% 
  map_chr(pluck(1))
all_counties_df$county <- rowname_simplified_county








# ------ 1. show oslo ------- #
# result: Fig1_oslo_trend 


oslo_df <- filter(all_counties_df, county == 'Oslo')

p1 <- ggplot(oslo_df, aes(x = iso_week, y = weekly_sum)) + 
  geom_line(size = 1) + 
  geom_point() + 
  geom_vline(xintercept = 52, color = 'red', linetype = 'dashed') + 
  ylim(c(0, 60)) +
  scale_x_continuous(breaks = seq(27, 53, by = 2)) + 
  theme_bw()+
  labs(title= 'Case number trend for disease X (week 41-52) \n Oslo',
       x ="Week ", y = "Counts") + 
  theme(
    plot.title = element_text(size=14, hjust = 0.5),
    axis.title.x = element_text(size=14),
    axis.title.y = element_text(size=14)
  )

png(file = paste0(save_path, 'creative_assignment/Fig1_oslo_trend.png'), 
    bg = "white", width = 800, height = 450)
print(p1)
dev.off()



# ----- 2. show many counties -------- #
# result: Fig2_county_trend

p2 <- ggplot(all_counties_df, aes(x = iso_week, y = weekly_sum, color = county)) + 
  geom_line(size = 1) + 
  geom_vline(xintercept = 52, color = 'red', linetype = 'dashed') + 
  facet_wrap(~ county) + 
  scale_fill_brewer(palette = 'Spectral') + 
  labs(title= 'Case number trend for disease X (week 41-52) \n All 11 counties',
       x ="Week ", y = "Counts") + 
  theme_bw() + 
  theme(
    plot.title = element_text(size=14, hjust = 0.5),
    axis.title.x = element_text(size=14),
    axis.title.y = element_text(size=14)
  )

png(file = paste0(save_path, 'creative_assignment/Fig2_county_trend.png'), 
    bg = "white", width = 800, height = 450)
print(p2)
dev.off()



# ----- 3. show all 11 counties ------ #
# result: Fig3_nation_trend

p3 <- ggplot(all_counties_df, aes(x = iso_week, y = weekly_sum, fill = county)) + 
  geom_bar(stat = 'identity') + 
  geom_vline(xintercept = 52.5, color = 'red', linetype = 'dashed') + 
  # facet_wrap(~ county) + 
  scale_fill_brewer(palette = 'Spectral') + 
  ylim(c(0, 14000))+
  labs(title= 'Case trend disease X (week 41-52) \n Norway vs counties',
       x ="Week ", y = "Counts") + 
  theme_bw() + 
  theme(
    plot.title = element_text(size=14, hjust = 0.5),
    axis.title.x = element_text(size=14),
    axis.title.y = element_text(size=14)
)

png(file = paste0(save_path, 'creative_assignment/Fig3_nation_trend.png'), 
    bg = "white", width = 800, height = 450)
print(p3)
dev.off()






# ======= municipality level ========= #
# show 4 week average

# all_munip <- map(munip, function(x){filter(x, iso_year == '2020' & iso_week >47) %>% 
#     select(c(iso_week, weekly_sum))})
# 
# all_munip_df <- do.call(rbind, all_munip)
# rowname_simplified_munip <- strsplit(rownames(all_munip_df), '[.]') %>% 
#   map_chr(pluck(1))
# all_munip_df$munip <- rowname_simplified_munip
# summary_munip <- all_munip_df %>% group_by(munip) %>% summarise(fourwk_mean = mean(weekly_sum))
# summary_munip$fourwk_mean %>% summary





