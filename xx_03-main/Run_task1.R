library(data.table)
library(fhi)
library(fhidata)
library(fhiplot)
library(magrittr)
library(dplyr)
library(lubridate)
library(purrr)
library(ggplot2)
library(tictoc)
# library(ciTools)


# You will need to change your working directory
# setwd("C:/Users/riwh/OneDrive - Folkehelseinstituttet/git/xx_03")

fileSources = file.path("code_task1", list.files("code_task1", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)


folder_path <- getwd()
data_individual <- readRDS(paste0(folder_path, '/xx_03-main/data_raw/individual_level_data.RDS'))
clean_path <- paste0(folder_path, '/xx_03-main/data_clean/')
save_path <- paste0(folder_path, '/xx_03-main/results_task1/')




# code goes here
# -------- 1. data cleaning -------- # 

source('./xx_03-main/code_task1/util-data.R')
source('./xx_03-main/code_task1/1-examinedata.R')



# ------- 2. model ------- #
#### MODEL: used the poisson model with trend and monthly effect 
# (idea from CreateFakeData.R, and constructed pseudo-month factor myself)

source('./xx_03-main/code_task1/util-model.R')
source('./xx_03-main/code_task1/2-model.R')



# ----- 3. reporting and visualise ------ #

source('./xx_03-main/code_task1/3-save-excel.R')
source('./xx_03-main/code_task1/4-save-plots.R')





# ------ 4. creative ------- # 














