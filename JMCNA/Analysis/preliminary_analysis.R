rm(list = ls())

if (!require("pacman")) install.packages("pacman")

p_load(
  rio,
  tidyverse,
  crayon,
  hypegrammaR,
  sjmisc,
  koboquest
)

source("src/load_data.R")
source("src/functions/results_table_functions_weigths_improved.R")

data %>% pull(district) %>% unique()

text <- filter(questions, str_detect(type, "(\\btext\\b)|(\\bnote\\b)"))$name
text <- c(text,data %>% select(ends_with("_other")) %>% colnames())


columns_to_exclude = c("today", "start", "end", "deviceid","resondent_status",
                       "consensus","settlements",
                       "respondent_source", "generated_table_list_label_16", 
                       "respondent_district","respondent_region","X_id", "_uuid", 
                       "submission_time", "index", "parent_table_name",
                       "parent_index", "tags", "notes","meta.instanceID","meta.audit",
                       "version","persons_having_difficulty_loop_count","many_shelters_count","reason_child_not_living_hh_count"
                       )                          


data_to_analyze <- data %>% 
  select(-one_of(text)) %>%
  select(-one_of(columns_to_exclude)) %>%
  select_if(~ !(all(is.na(.x))))

# data_to_analyze_tmp <- data_to_analyze %>% select(3,hh_size,strata,district,weights)


source("src/functions/results_table_functions_weigths_improved.R")


data_to_analyze2 <- data_to_analyze



data_to_analyze2 <- data_to_analyze %>% select(weights,
                                               population_group,
                                               district,
                                               strata,
                                               yes_no_refugee,
                                               disp_reasons_1:school_time)


var_lizt <- c("enrolled_girls_3_5", "enrolled_boys_3_5", "enrolled_girls_6_11", "enrolled_boys_6_11", "enrolled_girls_12_17", "enrolled_boys_12_17", "attend_school_girls_3_5", "attend_school_boys_3_5", "attend_school_girls_6_11", "attend_school_boys_6_11", "attend_school_girls_12_17", "attend_school_boys_12_17", "accessing_online_girls_3_5", "accessing_online_boys_3_5", "accessing_online_girls_6_11", "accessing_online_boys_6_11", "accessing_online_girls_12_17", "accessing_online_boys_12_17", "didnt_access_distance_learning_girls_3_5", "didnt_access_distance_learning_boys_3_5", "didnt_access_distance_learning_girls_6_11", "didnt_access_distance_learning_boys_6_11", "didnt_access_distance_learning_girls_12_17", "didnt_access_distance_learning_boys_12_17", "didnt_access_distance_learning_total", "tertiary_degree", "vocational_degree", "secondary_school", "middle_school", "primary_school", "quranic_school", "dont_know_educ_level", "dropped_out_girls_3_5", "dropped_out_boys_3_5", "dropped_out_girls_6_11", "dropped_out_boys_6_11", "dropped_out_girls_12_17", "dropped_out_boys_12_17")


data_to_analyze2 <- data_to_analyze2 %>% mutate_at(var_lizt,na_if,0)


{start_time <- Sys.time()
analysis <- table_maker(data_to_analyze2, 
                             questionnaire,
                             questions, 
                             choices,
                             "weights",
                             labels = T, 
                             language = "English", 
                             "SOM",
                             "population_group",
                             "district",
                             "strata"
)
end_time <- Sys.time()
end_time - start_time}


bk <- analysis

analysis[analysis==""] <- NA

analysis <- mutate_at(analysis,names(analysis)[2:ncol(analysis)],as.numeric)


for (i in 1:nrow(analysis)) {
  for (j in 2:ncol(analysis)) {
    if (analysis[i,1]!="Average" & !is.na(analysis[i,j])) {
      analysis[i,j] = (analysis[i,j]/100)
    }
  }
  
}

# saveRDS(analysis,"analysis_displacement.RDS")
# unique(data$district) %>% sort()
map(unique(data$district) %>% sort(),~analysis %>% select(starts_with(.x)) %>% colnames()) %>% unlist()

ord <- c(colnames(analysis) %>% head(4),map(unique(data$district) %>% sort(),~analysis %>% select(starts_with(.x)) %>% colnames()) %>% unlist())

analysis_ordered <- analysis %>% select(all_of(ord))

write.csv(analysis_ordered,"SOM_JMCNA_21_Results_tablettr.csv",row.names = F,na="")

# write.csv(analysis,'output/JMCNA_2020_Analysis_draft_V1_national_district_strata.csv',row.names = F,na = "")
# 
# source("src/functions/results_table_functions_weigths.R")
# 
# {start_time <- Sys.time()
# analysis2 <- table_maker(data_to_analyze_tmp , 
#                         questionnaire,
#                         questions, 
#                         choices,
#                         weighting_function,
#                         labels = T, 
#                         language = "English", 
#                         "SOM",
#                         "district",
#                         "strata"
# )
# end_time <- Sys.time()
# end_time - start_time}
# 
# rbind(analysis,analysis2) %>% View()
# 
# sleep_for_a_minute()
# 
# 
# 
# 
# 
