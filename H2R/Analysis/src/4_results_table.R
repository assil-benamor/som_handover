rm(list = ls())

options(scipen = 999)

if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               hypegrammaR,
               rio,
               readxl,
               openxlsx)


source("src/functions/utils.R")
source("src/functions/results_table_functions_weigths_improved_purrr.R")

data <- read.csv("input/data/SOM1901_H2R_ClanData_December.csv",
                 stringsAsFactors = F,
                 na.strings = c("NA","#N/A",""," "))


koboToolPath = "input/tool/H2R_f2f_questionnaire_December 2021.xlsx"

questions <- read_xlsx(koboToolPath,
                       guess_max = 50000,
                       na = c("NA","#N/A",""," "),
                       sheet = 1) %>% filter(!is.na(name)) %>% 
  mutate(q.type=as.character(lapply(type, function(x) str_split(x, " ")[[1]][1])),
         list_name=as.character(lapply(type, function(x) str_split(x, " ")[[1]][2])),
         list_name=ifelse(str_starts(type, "select_"), list_name, NA))
  


choices <- read_xlsx(koboToolPath,
                       guess_max = 50000,
                       na = c("NA","#N/A",""," "),
                       sheet = 2)

                           



data <- data %>% select(-any_of(c("enum_code", "info_reg", "district_info", "info_settlement","base", "info_set_oth_near","market_settlement")))



results <- generate_results_table(data = data,
                              questions = questions,
                              choices = choices,
                              weights.column = NULL,
                              use_labels = T,
                              labels_column = "label::English (en)"
                              # "localisation_region_label"

)


export_table(results,"output/")




