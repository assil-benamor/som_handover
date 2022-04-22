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

data <- data %>% select(-one_of(c("resondent_status", "respondent_district", "respondent_source","generated_table_list_label_16","version", "meta.audit", "meta.instanceID", "parent_table_name", "parent_index", "tags", "notes", "population_group")))
data <- data %>% select(-one_of(c("persons_having_difficulty_loop_count", "generated_table_list_label_241", "many_shelters_count", "reason_child_not_living_hh_count")))
data <- data %>% select(-one_of(c("categories_people_living_inthe_hhs", "age_gender_members_chronic_ilness", "persons_having_difficulty", "other_income_specify", "number_adults_working", "other_job_type_adult_specify", "number_children_working", "other_job_type_child_specify", "how_long_been_living_here", "when_leave_place_origin", "when_arrived_current_location", "resons_people_left_previouse_place", "children_enrolled_formal_schools", "children_attended_regular_schools", "children_accessing_online_classes", "children_didnt_access_distance_learning", "adults_completed_education", "children_dropped_out_formal_schools", "currently_having_enough_water", "preferred_means_info_f2f_who", "preferred_means_info_other_internet", "name_humanitrain_partner", "other_shelt")))
data <- data %>% select(region:X_id,strata)
data <- data %>% select(!ends_with("_other"))
data <- data %>% select(!ends_with("_note"))


sampling.frame <- load_samplingframe("input/sampling frame/sampling_frame_final.csv")

data <- data %>% filter(strata %in% sampling.frame$strata.names)

weighting_function <-map_to_weighting(sampling.frame = sampling.frame,
                                      data.stratum.column = "strata",
                                      sampling.frame.population.column = "population",
                                      sampling.frame.stratum.column = "strata.names",
                                      data = data)

data[["weights"]] <-  weighting_function(data)

data_to_export <- data


# data_to_export$region = left_join(data_to_export,read.csv("input/dashboard/district_state_mapping.csv",stringsAsFactors = F)) %>% pull(state)

# data_to_export %>% select(strata,region) %>% unique() %>% View()

# write.csv(data_to_export %>% select(strata,region) %>% unique(),"dist_state_dennis.csv",row.names = F)

data_questions_label = data.frame(var_name=colnames(data_to_export),
                                  stringsAsFactors = F)

questions <- questions %>% mutate(
  `label::English` = ifelse(type=="calculate",name,`label::English`)
)

data_questions_label <- left_join(data_questions_label,questions %>% select(name, questions = `label::English`), by = c("var_name"="name")) 


for (i in 2:nrow(data_questions_label)) {

  if (is.na(data_questions_label[i,2]) && grepl("\\.",data_questions_label[i,1])) {
    data_questions_label[i,2] =  data_questions_label[i-1,2]
  }
  
}


data_to_export_wide <- pivot_longer(data_to_export %>% mutate_all(as.character),cols=colnames(data_to_export)[5:(ncol(data_to_export)-3)], names_to = "Question", values_to = "Answer"  )

data_to_export_wide$question_id = gsub("\\.[^.]*$","",data_to_export_wide$Question)

data_to_export_wide <- left_join(data_to_export_wide,questions %>% select(question_type=type,question_id=name) %>% mutate(
  question_type = ifelse(question_type=="calculate","integer",gsub(" [^ ]*","",question_type))
))

data_to_export_wide <- left_join(data_to_export_wide,questions %>% select(list_name=type,question_id=name) %>% mutate(
  list_name = gsub(" or_other","",gsub("^[^ ]* ","",list_name))
))


data_to_export_wide <- left_join(data_to_export_wide,questions %>% select(question_id=name,question_label=`label::English`))


data_to_export_wide <- data_to_export_wide %>% mutate(
  Answer_2 = gsub("^[^.]*\\.","",Question)
)

data_to_export_wide <- data_to_export_wide %>% mutate(
  Answer_2 = ifelse(str_detect(Question,"\\."),Answer_2,Answer)
)

data_to_export_wide <- left_join(data_to_export_wide,choices %>% select(list_name,Answer_2=name,answer_label=`label::English`))


data_to_export_wide <- data_to_export_wide %>% mutate(
  answer_label = ifelse(Answer_2=="other","Other",answer_label)
)

# data_to_export_wide %>% 
#   # filter(question_type=="select_one") %>% 
#   filter(is.na(answer_label)) %>% 
#   filter(question_type=="select_one") %>% 
#   filter(!is.na(Answer)) %>% 
#   View()
# 
# data_to_export_wide %>% 
#   # filter(question_type=="select_one") %>% 
#   filter(is.na(answer_label)) %>% 
#   filter(question_type=="select_multiple") %>% 
#   filter(grepl("\\.",Question)) %>% 
#   filter(!is.na(Answer)) %>% 
#   View()

data_to_export_wide <- data_to_export_wide %>% mutate(
  answer_label = ifelse(is.na(answer_label),Answer,answer_label)
)

data_to_export_wide <- data_to_export_wide %>% filter(question_type!="select_multiple" | 
                                 (question_type=="select_multiple" & grepl("\\.",Question))) 


data_to_export_wide$Answer_2 <- NULL
data_to_export_wide$list_name <- NULL



sector_per_variable_df <- import("input/dashboard/sectors_per_variable_name21.csv")

data_to_export_wide <- left_join(data_to_export_wide,sector_per_variable_df,by = c("Question"="Variablename_in_dataset"))

data_to_export_wide[which(is.na(data_to_export_wide$Question_group)), "Question_group"] <- "Covid-19"

data_to_export_wide %>% filter(is.na(Question_group)) %>% pull(Question) %>% unique() %>% View()

write.csv(data_to_export_wide,"output/Tableau_input_Final_22.csv",row.names = F)




# data_to_export_wide <- data_to_export_wide %>% mutate(
#   `Answer (copy)` = Answer,
#   `answer new` = sub('.*\\.', '', Answer),
#   `answer_numeric` = ifelse(grepl("^[[:digit:]]+$", sub(".*?([0-9]+).*", "\\1", Answer,perl=TRUE), perl = T),sub(".*?([0-9]+).*", "\\1", Answer,perl=TRUE),"")
#   
# )
# 

