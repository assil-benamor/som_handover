rm(list = ls())

if (!require("pacman")) install.packages("pacman")
p_load(rio,
       tidyverse,
       koboquest,
       crayon,
       hypegrammaR,
       composr,
       clipr)

source("src/functions/data_merge_functions.R")
source("src/functions/data_merge_generator.R")

data <- read.csv("output/Indicators/aggregation_output_plus_lsg.csv",stringsAsFactors = F,na.strings = c(""," ","NA","#N/A"))

koboToolPath = "input/tool/REACH_SOM_DSA_Survey_Tool_for_data_merge.xlsx"

questions = import(koboToolPath,sheet="survey") %>% select(-1) %>% filter(!is.na(name))

choices = import(koboToolPath,sheet="choices")

questionnaire <- load_questionnaire(data = data,
                                    questions = questions,
                                    choices = choices,
                                    choices.label.column.to.use = "label::English (en)")

data$weights = 1
data$national <- "national"

data <- data %>% mutate_at(
c("water_access_distance_min", 
  "sanitation_access_distance_min", 
  "health_access_distance_min",
  "nutrition_access_distance_min",
  "education_access_distance_min", 
  "foodsecurity_access_distance_min"),
function(x){
  case_when(
    is.na(x) ~ x,
    x == "1530" ~ "15_30",
    x == "3160" ~ "31_60",
    TRUE ~ x
    
  )
}
)

data[data=="NC"] <- NA

data <- data %>% filter(localisation_district_label!="") 

indicators_to_include <- c("cccm_populationestimates_families", "cccm_populationestimates_individuals", "cccm_idps_arrived", "cccm_idps_departed", "cccm_idps_origin_first", "cccm_district_origin_first", "reason_displacement", "cccm_management", "cccm_committees", "decision_making_committees", "minorities_binary_3", "foodsecurity_access", "foodsecurity_access_distance_min", "foodsecurity_primary", "foodsecurity_coping_food", "foodsecurity_access_barriers", "food_sec_nc_index9", "foodsecurity_land_livestock", "foodsecurity_land_agriculture", "food_sec_index2", "nutrition_access_distance_min", "nutrition_distributions", "nutrition_services", "health_access_distance_min", "health_women_unskilledhealthpersonnel", "health_services", "health_facilities", "male_health_problems", "female_health_problems", "health_index2_modified", "health_index3_modified", "adeqaute_health_men", "adeqaute_health_women", "protection_childfriendlyspace", "protection_womenspace", "protection_restrictions_day", "protection_restrictions_night", "protection_incidents", "protection_index1", "protection_incidents_place", "insecure_areas", "protection_index2", "target_victims", "housing_property_incidences", "hlp_index1_modified", "rate_likelihood_eviction", "evictions_landowner", "hlp_nc_index2", "evictions_notice", "nfi_index1_modified", "nfi_nc_index3", "shelter_types", "shelter_fire", "shelter_flood", "shelter_publiclighting", "nfi_nc_index1", "nfi_items_available", "nfi_nc_index4", "minorities_binary_1", "water_access_distance_min", "water_sources_primary", "wash_nc_index3", "water_access_barriers", "sanitation_access_distance_min", "latrines_accessible_pwd", "sanitation_solidwastedisposal", "sanitation_access_impediments", "hygiene_handwashingfacilities", "wash_index1_modified", "minorities_binary_2", "education_access_distance_min", "education_facilities", "education_barriers_boys", "education_barriers_girls", "education_index1", "education_index2", "education_facilities_segregated", "education_facilities_fences", "number_schools_opened", "aap_informationsources", "aap_informationsources_pwd", "aap_access_barriers", "aap_feedbackmechanism", "minorities_binary_5", "covid_issue", "action_to_prevent")
indicators_to_include <- c(indicators_to_include,c("snfi_score", "wash_score", "protection_score", "fs_score", "health_score", "nutrition_score", "education_score", "hlp_score") )

# data_merge <- data %>% select(starts_with(indicators_to_include))

##################X
# data_merge_gen generates the differnet variables nedded for data merge depending on the 
# variables type:
# - Numerical : Calculate the average of the variable 
# - Select one/multiple : create a variable for each tuple (question,answer) with the %
#  and if the questions has more than 3 answers create top 4 answers variables with the labels and %
#  Note that this function does not run the analysis, it just returns the code to be used 
#  the code will be copied directely to the clipboard, so after you run it juste paste it into 
#  the script (eg: District level data and National level data)

################## X


l <- list(
  question =c("cccm_populationestimates_families", "cccm_populationestimates_individuals", "cccm_idps_arrived", "cccm_idps_departed", "cccm_idps_origin_first", "cccm_district_origin_first", "reason_displacement", "cccm_management", "cccm_committees", "decision_making_committees", "minorities_binary_3", "foodsecurity_access", "foodsecurity_access_distance_min", "foodsecurity_primary", "foodsecurity_coping_food", "foodsecurity_access_barriers", "food_sec_nc_index9", "foodsecurity_land_livestock", "foodsecurity_land_agriculture", "food_sec_index2", "nutrition_access_distance_min", "nutrition_distributions", "nutrition_services", "health_access_distance_min", "health_women_unskilledhealthpersonnel", "health_services", "health_facilities", "male_health_problems", "female_health_problems", "health_index2_modified", "health_index3_modified", "adeqaute_health_men", "adeqaute_health_women", "protection_childfriendlyspace", "protection_womenspace", "protection_restrictions_day", "protection_restrictions_night", "protection_incidents", "protection_index1", "protection_incidents_place", "insecure_areas", "protection_index2", "target_victims", "housing_property_incidences", "hlp_index1_modified", "rate_likelihood_eviction", "evictions_landowner", "hlp_nc_index2", "evictions_notice", "nfi_index1_modified", "nfi_nc_index3", "shelter_types", "shelter_fire", "shelter_flood", "shelter_publiclighting", "nfi_nc_index1", "nfi_items_available", "nfi_nc_index4", "minorities_binary_1", "water_access_distance_min", "water_sources_primary", "wash_nc_index3", "water_access_barriers", "sanitation_access_distance_min", "latrines_accessible_pwd", "sanitation_solidwastedisposal", "sanitation_access_impediments", "hygiene_handwashingfacilities", "wash_index1_modified", "minorities_binary_2", "education_access_distance_min", "education_facilities", "education_barriers_boys", "education_barriers_girls", "education_index1", "education_index2", "education_facilities_segregated", "education_facilities_fences", "number_schools_opened", "aap_informationsources", "aap_informationsources_pwd", "aap_access_barriers", "aap_feedbackmechanism", "minorities_binary_5", "covid_issue", "action_to_prevent"), 
  type = c("sum", "sum", "mean", "mean", "top3", "top3", "top3", "top3", "top3", "pct", "pct", "pct", "pct", "top3", "top3", "top3", "pct", "pct", "pct", "pct", "pct", "top3", "top3", "pct", "pct", "top3", "top3", "top3", "top3", "pct", "pct", "pct", "pct", "pct", "pct", "pct", "pct", "top3", "pct", "top3", "top3", "pct", "top3", "top3", "pct", "pct", "top3", "pct", "pct", "pct", "pct", "top3", "pct", "pct", "pct", "pct", "top3", "pct", "pct", "pct", "top3", "pct", "top3", "pct", "pct", "top3", "top3", "pct", "pct", "pct", "pct", "pct", "top3", "top3", "pct", "pct", "pct", "pct", "pct", "top3", "top3", "top3", "pct", "pct", "pct", "top3")
)


data <- data %>% mutate_at(c("wash_index1", "wash_index2", "wash_index3", "wash_index4", "wash_nc_index", "wash_nc_index2", "wash_nc_index3", "wash_nc_index4", "wash_nc_index5", "protection_index1", "protection_index2", "protection_nc_index1", "protection_nc_index2", "protection_nc_index3", "protection_nc_index4", "protection_nc_index5", "education_nc_index1", "education_nc_index2", "education_nc_index3", "education_nc_index4", "education_nc_index5", "education_nc_index6", "education_nc_index7", "education_nc_index8", "education_nc_index9", "education_index1", "education_index2", "health_index1", "health_index2", "health_index3", "health_index4", "health_nc_index1", "health_nc_index2", "health_nc_index3", "health_nc_index4", "health_nc_index5", "health_nc_index6", "health_nc_index7", "health_nc_index8", "health_nc_index9", "health_nc_index10", "health_nc_index11", "health_nc_index12", "health_nc_index13", "nutrition_nc_index1", "nutrition_nc_index2", "nutrition_nc_index3", "nfi_index1", "nfi_nc_index1", "nfi_nc_index2", "nfi_nc_index3", "nfi_nc_index4", "nfi_nc_index5", "nfi_nc_index6", "hlp_index1", "hlp_index2", "hlp_index3", "hlp_nc_index1", "hlp_nc_index2", "food_sec_index2", "food_sec_index3", "food_sec_nc_index1", "food_sec_nc_index2", "food_sec_nc_index3", "food_sec_nc_index4", "food_sec_nc_index5", "food_sec_nc_index6", "food_sec_nc_index7", "food_sec_nc_index8", "mean_nc_snfi", "mean_nc_wash", "mean_nc_hlt", "mean_nc_nutrition", "mean_nc_fs", "mean_nc_edu", "mean_nc_protection", "mean_nc_hlp", "snfi_score", "wash_score", "protection_score", "fs_score", "health_score", "nutrition_score", "education_score", "hlp_score", "protection_need", "education_need", "nutrition_need", "fs_need", "snfi_need", "wash_need", "health_need", "hlp_need", "number_of_needs", "site_duration_score")
                           ,as.character)



data <- data %>% mutate(
  number_schools_opened_modified = case_when(
    number_schools_opened == 0 ~ "0",
    number_schools_opened >= 1 ~ "1",
    TRUE ~ as.character(NA)
  ),
  
  wash_nc_index4_recoded_binary = case_when(
    wash_index4 %in% c("3","4") ~ "1",
    wash_index4 %in% c("1","2") ~ "0",
    TRUE ~ as.character(NA)
  ),
  
  minorities_binary_1 = ifelse(nfi_access_impediments.minorities == 1 , "1" , "0"),
  minorities_binary_2 = ifelse(sanitation_access_impediments.minorities == 1 , "1" , "0"),
  minorities_binary_3 = ifelse(support_access_impediments.minorities == 1 , "1" , "0"),
  minorities_binary_4 = ifelse(residents_no_food=="minority_headed_hh" , "1" , "0"),
  minorities_binary_5 = ifelse(unwilling_make_complaint_feedback == "minority_clan_member" , "1" , "0"),
  additional_minorities_index = as.character(additional_minorities_index),
  
  
  decision_making_committees_women_binary = as.character(decision_making_committees.women),
  
  
  #### Coding new indicators for the national factsheet
  
  hlp_index1_modified = case_when(
    hlp_index1 %in% c("3","4") ~ "1",
    hlp_index1 %in% c("1","2") ~ "0",
    TRUE ~ as.character(NA)
  ),
  
  nfi_index1_modified = case_when(
    nfi_index1 %in% c("3","4") ~ "1",
    nfi_index1 %in% c("1","2") ~ "0",
    TRUE ~ as.character(NA)
  ),
  
  wash_index1_modified = case_when(
    wash_index1 %in% c("3","4") ~ "1",
    wash_index1 %in% c("1","2") ~ "0",
    TRUE ~ as.character(NA)
  ),
  
  health_index2_modified = case_when(
    health_index2 %in% c("3","4") ~ "1",
    health_index2 %in% c("1","2") ~ "0",
    TRUE ~ as.character(NA)
  ),
  
  health_index3_modified = case_when(
    health_index3 %in% c("3","4") ~ "1",
    health_index3 %in% c("1","2") ~ "0",
    TRUE ~ as.character(NA)
  ),
  
  hygiene_handwashingfacilities_modified = case_when(
    hygiene_handwashingfacilities %in% c("all","many") ~ "1",
    hygiene_handwashingfacilities %in% c("none","some","few") ~ "0",
    TRUE ~ as.character(NA)
  ),
  
  cccm_idps_arrived_binary = case_when(
    cccm_idps_arrived >1 ~"1",
    TRUE ~ "0"
  ),
  
  foodsecurity_access_barriers_subset = case_when(
    is.na(foodsecurity_access_barriers.sec_issues) ~ "0",
    TRUE ~ "1"
  ),
  evictions_tenureagreement_subset = case_when(
    is.na(evictions_tenureagreement) ~ "0",
    TRUE ~ "1"
  ),
  water_access_barriers_subset = case_when(
    is.na(water_access_barriers.no_problem) ~ "0",
    TRUE ~ "1"
  ),
  nfi_items_available_subset = case_when(
    is.na(nfi_items_available.meds) ~ "0",
    TRUE ~ "1"
  ),
  decision_making_committees_women_subset = case_when(
    is.na(decision_making_committees.women) ~ "0",
    TRUE ~ "1"
  ),
  
  sanitation_access_impediments_subset = case_when(
    is.na(sanitation_access_impediments.women) ~ "0",
    TRUE ~ "1"
  ),
  

  number_schools_opened_modified_subset = case_when(
    is.na(number_schools_opened_modified) ~ "0",
    TRUE ~ "1"
  ),
  
  hygiene_access_impediments_subset = case_when(
    is.na(hygiene_access_impediments.lack_quant) ~ "0",
    TRUE ~ "1"
  ),
  
  
  
  
  snfi_score_binary  = case_when(
    snfi_score %in% c("3","4") ~ "3+",
    snfi_score %in% c("1","2") ~ "1_2",
    TRUE ~ as.character(NA)
  ),
  wash_score_binary  = case_when(
    wash_score %in% c("3","4") ~ "3+",
    wash_score %in% c("1","2") ~ "1_2",
    TRUE ~ as.character(NA)
  ),
  protection_score_binary  = case_when(
    protection_score %in% c("3","4") ~ "3+",
    protection_score %in% c("1","2") ~ "1_2",
    TRUE ~ as.character(NA)
  ),
  fs_score_binary  = case_when(
    fs_score %in% c("3","4") ~ "3+",
    fs_score %in% c("1","2") ~ "1_2",
    TRUE ~ as.character(NA)
  ),
  health_score_binary  = case_when(
    health_score %in% c("3","4") ~ "3+",
    health_score %in% c("1","2") ~ "1_2",
    TRUE ~ as.character(NA)
  ),
  nutrition_score_binary  = case_when(
    nutrition_score %in% c("3","4") ~ "3+",
    nutrition_score %in% c("1","2") ~ "1_2",
    TRUE ~ as.character(NA)
  ),
  education_score_binary  = case_when(
    education_score %in% c("3","4") ~ "3+",
    education_score %in% c("1","2") ~ "1_2",
    TRUE ~ as.character(NA)
  ),
  hlp_score_binary  = case_when(
    hlp_score %in% c("3","4") ~ "3+",
    hlp_score %in% c("1","2") ~ "1_2",
    TRUE ~ as.character(NA)
  )
  
)

  

# map_chr(c("snfi_score", "wash_score", "protection_score", "fs_score", "health_score", "nutrition_score", "education_score", "hlp_score"),
#        ~ sprintf('%s_binary  = case_when(
#     %s #in# c("3","4") ~ "3+",
#     %s #in# c("1","2") ~ "1_2",
#     TRUE ~ as.character(NA)
#   ),',.x,.x,.x)) %>% clipr::write_clip()


###### Adding one question 

# l <- list(
#   question =c("hygiene_access_impediments"),
#   type = c("top3")
# )
# 
# clipr::write_clip(data_merge_gen_v2(data= data,
#                                     questionnaire = questionnaire,
#                                     questions = questions,
#                                     liste_of_questions = c("hygiene_access_impediments"),
#                                     choices = choices,
#                                     group = "national",
#                                     df_name = "data",
#                                     df_output_name = "data_merge_national"))


######

  

clipr::write_clip(data_merge_gen_v2(data= data,
                                 questionnaire = questionnaire,
                                 questions = questions,
                                 liste_of_questions = indicators_to_include,
                                 choices = choices,
                                 group = "national",
                                 df_name = "data",
                                 df_output_name = "data_merge_national"))


somaliland_districts <- c("Baki", "Borama", "Lughaye", "Ceel Afweyn", "Ceerigaabo", "Laasqoray", "Caynabo", "Laas Caanood", "Xudun", "Burco", "Buuhoodle", "Owdweyne", "Sheikh", "Hargeysa")

Somaliland = T


if(Somaliland) {
  data <- data %>% filter(localisation_district_label %in% somaliland_districts)
} 

####### National level data  ####### 
data_merge_national <- data %>%
  group_by(national) %>%  summarize( 
    n_surveys = n(),
    hygiene_access_impediments_top_1_name = select_percents(hygiene_access_impediments, 1, ., questions, choices, 'label', group = !!get_group(.)),
    hygiene_access_impediments_top_1_pct = select_percents(hygiene_access_impediments, 1, ., questions, choices, 'percent', group = !!get_group(.)),
    hygiene_access_impediments_top_2_name = select_percents(hygiene_access_impediments, 2, ., questions, choices, 'label', group = !!get_group(.)),
    hygiene_access_impediments_top_2_pct = select_percents(hygiene_access_impediments, 2, ., questions, choices, 'percent', group = !!get_group(.)),
    hygiene_access_impediments_top_3_name = select_percents(hygiene_access_impediments, 3, ., questions, choices, 'label', group = !!get_group(.)),
    hygiene_access_impediments_top_3_pct = select_percents(hygiene_access_impediments, 3, ., questions, choices, 'percent', group = !!get_group(.)),
    hygiene_access_impediments_top_4_name = select_percents(hygiene_access_impediments, 4, ., questions, choices, 'label', group = !!get_group(.)),
    hygiene_access_impediments_top_4_pct = select_percents(hygiene_access_impediments, 4, ., questions, choices, 'percent', group = !!get_group(.)),
    cccm_idps_arrived_binary = percent_response(cccm_idps_arrived_binary, .,'1', group = !!get_group(.)),
    foodsecurity_access_barriers_subset = percent_response(foodsecurity_access_barriers_subset, .,'1', group = !!get_group(.)),
    evictions_tenureagreement_subset = percent_response(evictions_tenureagreement_subset, .,'1', group = !!get_group(.)),
    water_access_barriers_subset = percent_response(water_access_barriers_subset, .,'1', group = !!get_group(.)),
    nfi_items_available_subset = percent_response(nfi_items_available_subset, .,'1', group = !!get_group(.)),
    decision_making_committees_women_subset = percent_response(decision_making_committees_women_subset, .,'1', group = !!get_group(.)),
    sanitation_access_impediments_subset = percent_response(sanitation_access_impediments_subset, .,'1', group = !!get_group(.)),
    number_schools_opened_modified_subset = percent_response(number_schools_opened_modified_subset, .,'1', group = !!get_group(.)),
    hygiene_access_impediments_subset = percent_response(hygiene_access_impediments_subset, .,'1', group = !!get_group(.)),
    minorities_binary_1_0 = percent_response(minorities_binary_1, .,'0', group = !!get_group(.)),
    minorities_binary_1_1 = percent_response(minorities_binary_1, .,'1', group = !!get_group(.)),
    minorities_binary_2_0 = percent_response(minorities_binary_2, .,'0', group = !!get_group(.)),
    minorities_binary_2_1 = percent_response(minorities_binary_2, .,'1', group = !!get_group(.)),
    minorities_binary_3_0 = percent_response(minorities_binary_3, .,'0', group = !!get_group(.)),
    minorities_binary_3_1 = percent_response(minorities_binary_3, .,'1', group = !!get_group(.)),
    # minorities_binary_4_0 = percent_response(minorities_binary_4, .,'0', group = !!get_group(.)),
    # minorities_binary_4_1 = percent_response(minorities_binary_4, .,'1', group = !!get_group(.)),
    minorities_binary_5_0 = percent_response(minorities_binary_5, .,'0', group = !!get_group(.)),
    minorities_binary_5_1 = percent_response(minorities_binary_5, .,'1', group = !!get_group(.)),
    cccm_populationestimates_families = weighted_sum(x = cccm_populationestimates_families,df = .,group = !!get_group(.)),
    cccm_populationestimates_individuals = weighted_sum(x = cccm_populationestimates_individuals,df = .,group = !!get_group(.)),
    cccm_idps_arrived = weighted_mean(x = cccm_idps_arrived,df = .,digits = 2,group = !!get_group(.)),
    cccm_idps_departed = weighted_mean(x = cccm_idps_departed,df = .,digits = 2,group = !!get_group(.)),
    cccm_idps_origin_first_top_1_name = select_percents(cccm_idps_origin_first, 1, ., questions, choices, 'label', group = !!get_group(.)),
    cccm_idps_origin_first_top_1_pct = select_percents(cccm_idps_origin_first, 1, ., questions, choices, 'percent', group = !!get_group(.)),
    cccm_idps_origin_first_top_2_name = select_percents(cccm_idps_origin_first, 2, ., questions, choices, 'label', group = !!get_group(.)),
    cccm_idps_origin_first_top_2_pct = select_percents(cccm_idps_origin_first, 2, ., questions, choices, 'percent', group = !!get_group(.)),
    cccm_idps_origin_first_top_3_name = select_percents(cccm_idps_origin_first, 3, ., questions, choices, 'label', group = !!get_group(.)),
    cccm_idps_origin_first_top_3_pct = select_percents(cccm_idps_origin_first, 3, ., questions, choices, 'percent', group = !!get_group(.)),
    cccm_idps_origin_first_top_4_name = select_percents(cccm_idps_origin_first, 4, ., questions, choices, 'label', group = !!get_group(.)),
    cccm_idps_origin_first_top_4_pct = select_percents(cccm_idps_origin_first, 4, ., questions, choices, 'percent', group = !!get_group(.)),
    cccm_district_origin_first_top_1_name = select_percents(cccm_district_origin_first, 1, ., questions, choices, 'label', group = !!get_group(.)),
    cccm_district_origin_first_top_1_pct = select_percents(cccm_district_origin_first, 1, ., questions, choices, 'percent', group = !!get_group(.)),
    cccm_district_origin_first_top_2_name = select_percents(cccm_district_origin_first, 2, ., questions, choices, 'label', group = !!get_group(.)),
    cccm_district_origin_first_top_2_pct = select_percents(cccm_district_origin_first, 2, ., questions, choices, 'percent', group = !!get_group(.)),
    cccm_district_origin_first_top_3_name = select_percents(cccm_district_origin_first, 3, ., questions, choices, 'label', group = !!get_group(.)),
    cccm_district_origin_first_top_3_pct = select_percents(cccm_district_origin_first, 3, ., questions, choices, 'percent', group = !!get_group(.)),
    cccm_district_origin_first_top_4_name = select_percents(cccm_district_origin_first, 4, ., questions, choices, 'label', group = !!get_group(.)),
    cccm_district_origin_first_top_4_pct = select_percents(cccm_district_origin_first, 4, ., questions, choices, 'percent', group = !!get_group(.)),
    reason_displacement_top_1_name = select_percents(reason_displacement, 1, ., questions, choices, 'label', group = !!get_group(.)),
    reason_displacement_top_1_pct = select_percents(reason_displacement, 1, ., questions, choices, 'percent', group = !!get_group(.)),
    reason_displacement_top_2_name = select_percents(reason_displacement, 2, ., questions, choices, 'label', group = !!get_group(.)),
    reason_displacement_top_2_pct = select_percents(reason_displacement, 2, ., questions, choices, 'percent', group = !!get_group(.)),
    reason_displacement_top_3_name = select_percents(reason_displacement, 3, ., questions, choices, 'label', group = !!get_group(.)),
    reason_displacement_top_3_pct = select_percents(reason_displacement, 3, ., questions, choices, 'percent', group = !!get_group(.)),
    reason_displacement_top_4_name = select_percents(reason_displacement, 4, ., questions, choices, 'label', group = !!get_group(.)),
    reason_displacement_top_4_pct = select_percents(reason_displacement, 4, ., questions, choices, 'percent', group = !!get_group(.)),
    cccm_management_top_1_name = select_percents(cccm_management, 1, ., questions, choices, 'label', group = !!get_group(.)),
    cccm_management_top_1_pct = select_percents(cccm_management, 1, ., questions, choices, 'percent', group = !!get_group(.)),
    cccm_management_top_2_name = select_percents(cccm_management, 2, ., questions, choices, 'label', group = !!get_group(.)),
    cccm_management_top_2_pct = select_percents(cccm_management, 2, ., questions, choices, 'percent', group = !!get_group(.)),
    cccm_management_top_3_name = select_percents(cccm_management, 3, ., questions, choices, 'label', group = !!get_group(.)),
    cccm_management_top_3_pct = select_percents(cccm_management, 3, ., questions, choices, 'percent', group = !!get_group(.)),
    cccm_management_top_4_name = select_percents(cccm_management, 4, ., questions, choices, 'label', group = !!get_group(.)),
    cccm_management_top_4_pct = select_percents(cccm_management, 4, ., questions, choices, 'percent', group = !!get_group(.)),
    cccm_committees_top_1_name = select_percents(cccm_committees, 1, ., questions, choices, 'label', group = !!get_group(.)),
    cccm_committees_top_1_pct = select_percents(cccm_committees, 1, ., questions, choices, 'percent', group = !!get_group(.)),
    cccm_committees_top_2_name = select_percents(cccm_committees, 2, ., questions, choices, 'label', group = !!get_group(.)),
    cccm_committees_top_2_pct = select_percents(cccm_committees, 2, ., questions, choices, 'percent', group = !!get_group(.)),
    cccm_committees_top_3_name = select_percents(cccm_committees, 3, ., questions, choices, 'label', group = !!get_group(.)),
    cccm_committees_top_3_pct = select_percents(cccm_committees, 3, ., questions, choices, 'percent', group = !!get_group(.)),
    cccm_committees_top_4_name = select_percents(cccm_committees, 4, ., questions, choices, 'label', group = !!get_group(.)),
    cccm_committees_top_4_pct = select_percents(cccm_committees, 4, ., questions, choices, 'percent', group = !!get_group(.)),
    decision_making_committees_women = percent_response(decision_making_committees, .,'women', group = !!get_group(.)),
    decision_making_committees_minority_clan_members = percent_response(decision_making_committees, .,'minority_clan_members', group = !!get_group(.)),
    decision_making_committees_marginalised_clan_members = percent_response(decision_making_committees, .,'marginalised_clan_members', group = !!get_group(.)),
    decision_making_committees_pwd = percent_response(decision_making_committees, .,'pwd', group = !!get_group(.)),
    decision_making_committees_young_men = percent_response(decision_making_committees, .,'young_men', group = !!get_group(.)),
    decision_making_committees_young_women = percent_response(decision_making_committees, .,'young_women', group = !!get_group(.)),
    decision_making_committees_none_of_the_above = percent_response(decision_making_committees, .,'none_of_the_above', group = !!get_group(.)),
    foodsecurity_access_yes = percent_response(foodsecurity_access, .,'yes', group = !!get_group(.)),
    foodsecurity_access_no = percent_response(foodsecurity_access, .,'no', group = !!get_group(.)),
    foodsecurity_access_dnk = percent_response(foodsecurity_access, .,'dnk', group = !!get_group(.)),
    foodsecurity_access_distance_min_less_15 = percent_response(foodsecurity_access_distance_min, .,'less_15', group = !!get_group(.)),
    foodsecurity_access_distance_min_15_30 = percent_response(foodsecurity_access_distance_min, .,'15_30', group = !!get_group(.)),
    foodsecurity_access_distance_min_31_60 = percent_response(foodsecurity_access_distance_min, .,'31_60', group = !!get_group(.)),
    foodsecurity_access_distance_min_more_60 = percent_response(foodsecurity_access_distance_min, .,'more_60', group = !!get_group(.)),
    foodsecurity_access_distance_min_dnk = percent_response(foodsecurity_access_distance_min, .,'dnk', group = !!get_group(.)),
    foodsecurity_primary_top_1_name = select_percents(foodsecurity_primary, 1, ., questions, choices, 'label', group = !!get_group(.)),
    foodsecurity_primary_top_1_pct = select_percents(foodsecurity_primary, 1, ., questions, choices, 'percent', group = !!get_group(.)),
    foodsecurity_primary_top_2_name = select_percents(foodsecurity_primary, 2, ., questions, choices, 'label', group = !!get_group(.)),
    foodsecurity_primary_top_2_pct = select_percents(foodsecurity_primary, 2, ., questions, choices, 'percent', group = !!get_group(.)),
    foodsecurity_primary_top_3_name = select_percents(foodsecurity_primary, 3, ., questions, choices, 'label', group = !!get_group(.)),
    foodsecurity_primary_top_3_pct = select_percents(foodsecurity_primary, 3, ., questions, choices, 'percent', group = !!get_group(.)),
    foodsecurity_primary_top_4_name = select_percents(foodsecurity_primary, 4, ., questions, choices, 'label', group = !!get_group(.)),
    foodsecurity_primary_top_4_pct = select_percents(foodsecurity_primary, 4, ., questions, choices, 'percent', group = !!get_group(.)),
    foodsecurity_coping_food_top_1_name = select_percents(foodsecurity_coping_food, 1, ., questions, choices, 'label', group = !!get_group(.)),
    foodsecurity_coping_food_top_1_pct = select_percents(foodsecurity_coping_food, 1, ., questions, choices, 'percent', group = !!get_group(.)),
    foodsecurity_coping_food_top_2_name = select_percents(foodsecurity_coping_food, 2, ., questions, choices, 'label', group = !!get_group(.)),
    foodsecurity_coping_food_top_2_pct = select_percents(foodsecurity_coping_food, 2, ., questions, choices, 'percent', group = !!get_group(.)),
    foodsecurity_coping_food_top_3_name = select_percents(foodsecurity_coping_food, 3, ., questions, choices, 'label', group = !!get_group(.)),
    foodsecurity_coping_food_top_3_pct = select_percents(foodsecurity_coping_food, 3, ., questions, choices, 'percent', group = !!get_group(.)),
    foodsecurity_coping_food_top_4_name = select_percents(foodsecurity_coping_food, 4, ., questions, choices, 'label', group = !!get_group(.)),
    foodsecurity_coping_food_top_4_pct = select_percents(foodsecurity_coping_food, 4, ., questions, choices, 'percent', group = !!get_group(.)),
    foodsecurity_access_barriers_top_1_name = select_percents(foodsecurity_access_barriers, 1, ., questions, choices, 'label', group = !!get_group(.)),
    foodsecurity_access_barriers_top_1_pct = select_percents(foodsecurity_access_barriers, 1, ., questions, choices, 'percent', group = !!get_group(.)),
    foodsecurity_access_barriers_top_2_name = select_percents(foodsecurity_access_barriers, 2, ., questions, choices, 'label', group = !!get_group(.)),
    foodsecurity_access_barriers_top_2_pct = select_percents(foodsecurity_access_barriers, 2, ., questions, choices, 'percent', group = !!get_group(.)),
    foodsecurity_access_barriers_top_3_name = select_percents(foodsecurity_access_barriers, 3, ., questions, choices, 'label', group = !!get_group(.)),
    foodsecurity_access_barriers_top_3_pct = select_percents(foodsecurity_access_barriers, 3, ., questions, choices, 'percent', group = !!get_group(.)),
    foodsecurity_access_barriers_top_4_name = select_percents(foodsecurity_access_barriers, 4, ., questions, choices, 'label', group = !!get_group(.)),
    foodsecurity_access_barriers_top_4_pct = select_percents(foodsecurity_access_barriers, 4, ., questions, choices, 'percent', group = !!get_group(.)),
    food_sec_nc_index9_0 = percent_response(food_sec_nc_index9, .,'0', group = !!get_group(.)),
    food_sec_nc_index9_1 = percent_response(food_sec_nc_index9, .,'1', group = !!get_group(.)),
    foodsecurity_land_livestock_yes = percent_response(foodsecurity_land_livestock, .,'yes', group = !!get_group(.)),
    foodsecurity_land_livestock_no = percent_response(foodsecurity_land_livestock, .,'no', group = !!get_group(.)),
    foodsecurity_land_agriculture_yes = percent_response(foodsecurity_land_agriculture, .,'yes', group = !!get_group(.)),
    foodsecurity_land_agriculture_no = percent_response(foodsecurity_land_agriculture, .,'no', group = !!get_group(.)),
    food_sec_index2_2 = percent_response(food_sec_index2, .,'2', group = !!get_group(.)),
    food_sec_index2_3 = percent_response(food_sec_index2, .,'3', group = !!get_group(.)),
    food_sec_index2_4 = percent_response(food_sec_index2, .,'4', group = !!get_group(.)),
    nutrition_access_distance_min_less_15 = percent_response(nutrition_access_distance_min, .,'less_15', group = !!get_group(.)),
    nutrition_access_distance_min_15_30 = percent_response(nutrition_access_distance_min, .,'15_30', group = !!get_group(.)),
    nutrition_access_distance_min_31_60 = percent_response(nutrition_access_distance_min, .,'31_60', group = !!get_group(.)),
    nutrition_access_distance_min_more_60 = percent_response(nutrition_access_distance_min, .,'more_60', group = !!get_group(.)),
    nutrition_access_distance_min_dnk = percent_response(nutrition_access_distance_min, .,'dnk', group = !!get_group(.)),
    nutrition_distributions_top_1_name = select_percents(nutrition_distributions, 1, ., questions, choices, 'label', group = !!get_group(.)),
    nutrition_distributions_top_1_pct = select_percents(nutrition_distributions, 1, ., questions, choices, 'percent', group = !!get_group(.)),
    nutrition_distributions_top_2_name = select_percents(nutrition_distributions, 2, ., questions, choices, 'label', group = !!get_group(.)),
    nutrition_distributions_top_2_pct = select_percents(nutrition_distributions, 2, ., questions, choices, 'percent', group = !!get_group(.)),
    nutrition_distributions_top_3_name = select_percents(nutrition_distributions, 3, ., questions, choices, 'label', group = !!get_group(.)),
    nutrition_distributions_top_3_pct = select_percents(nutrition_distributions, 3, ., questions, choices, 'percent', group = !!get_group(.)),
    nutrition_distributions_top_4_name = select_percents(nutrition_distributions, 4, ., questions, choices, 'label', group = !!get_group(.)),
    nutrition_distributions_top_4_pct = select_percents(nutrition_distributions, 4, ., questions, choices, 'percent', group = !!get_group(.)),
    nutrition_services_top_1_name = select_percents(nutrition_services, 1, ., questions, choices, 'label', group = !!get_group(.)),
    nutrition_services_top_1_pct = select_percents(nutrition_services, 1, ., questions, choices, 'percent', group = !!get_group(.)),
    nutrition_services_top_2_name = select_percents(nutrition_services, 2, ., questions, choices, 'label', group = !!get_group(.)),
    nutrition_services_top_2_pct = select_percents(nutrition_services, 2, ., questions, choices, 'percent', group = !!get_group(.)),
    nutrition_services_top_3_name = select_percents(nutrition_services, 3, ., questions, choices, 'label', group = !!get_group(.)),
    nutrition_services_top_3_pct = select_percents(nutrition_services, 3, ., questions, choices, 'percent', group = !!get_group(.)),
    nutrition_services_top_4_name = select_percents(nutrition_services, 4, ., questions, choices, 'label', group = !!get_group(.)),
    nutrition_services_top_4_pct = select_percents(nutrition_services, 4, ., questions, choices, 'percent', group = !!get_group(.)),
    health_access_distance_min_less_15 = percent_response(health_access_distance_min, .,'less_15', group = !!get_group(.)),
    health_access_distance_min_15_30 = percent_response(health_access_distance_min, .,'15_30', group = !!get_group(.)),
    health_access_distance_min_31_60 = percent_response(health_access_distance_min, .,'31_60', group = !!get_group(.)),
    health_access_distance_min_more_60 = percent_response(health_access_distance_min, .,'more_60', group = !!get_group(.)),
    health_access_distance_min_dnk = percent_response(health_access_distance_min, .,'dnk', group = !!get_group(.)),
    health_women_unskilledhealthpersonnel_none = percent_response(health_women_unskilledhealthpersonnel, .,'none', group = !!get_group(.)),
    health_women_unskilledhealthpersonnel_few = percent_response(health_women_unskilledhealthpersonnel, .,'few', group = !!get_group(.)),
    health_women_unskilledhealthpersonnel_some = percent_response(health_women_unskilledhealthpersonnel, .,'some', group = !!get_group(.)),
    health_women_unskilledhealthpersonnel_many = percent_response(health_women_unskilledhealthpersonnel, .,'many', group = !!get_group(.)),
    health_women_unskilledhealthpersonnel_all = percent_response(health_women_unskilledhealthpersonnel, .,'all', group = !!get_group(.)),
    health_women_unskilledhealthpersonnel_dnk = percent_response(health_women_unskilledhealthpersonnel, .,'dnk', group = !!get_group(.)),
    health_services_top_1_name = select_percents(health_services, 1, ., questions, choices, 'label', group = !!get_group(.)),
    health_services_top_1_pct = select_percents(health_services, 1, ., questions, choices, 'percent', group = !!get_group(.)),
    health_services_top_2_name = select_percents(health_services, 2, ., questions, choices, 'label', group = !!get_group(.)),
    health_services_top_2_pct = select_percents(health_services, 2, ., questions, choices, 'percent', group = !!get_group(.)),
    health_services_top_3_name = select_percents(health_services, 3, ., questions, choices, 'label', group = !!get_group(.)),
    health_services_top_3_pct = select_percents(health_services, 3, ., questions, choices, 'percent', group = !!get_group(.)),
    health_services_top_4_name = select_percents(health_services, 4, ., questions, choices, 'label', group = !!get_group(.)),
    health_services_top_4_pct = select_percents(health_services, 4, ., questions, choices, 'percent', group = !!get_group(.)),
    health_facilities_top_1_name = select_percents(health_facilities, 1, ., questions, choices, 'label', group = !!get_group(.)),
    health_facilities_top_1_pct = select_percents(health_facilities, 1, ., questions, choices, 'percent', group = !!get_group(.)),
    health_facilities_top_2_name = select_percents(health_facilities, 2, ., questions, choices, 'label', group = !!get_group(.)),
    health_facilities_top_2_pct = select_percents(health_facilities, 2, ., questions, choices, 'percent', group = !!get_group(.)),
    health_facilities_top_3_name = select_percents(health_facilities, 3, ., questions, choices, 'label', group = !!get_group(.)),
    health_facilities_top_3_pct = select_percents(health_facilities, 3, ., questions, choices, 'percent', group = !!get_group(.)),
    health_facilities_top_4_name = select_percents(health_facilities, 4, ., questions, choices, 'label', group = !!get_group(.)),
    health_facilities_top_4_pct = select_percents(health_facilities, 4, ., questions, choices, 'percent', group = !!get_group(.)),
    male_health_problems_top_1_name = select_percents(male_health_problems, 1, ., questions, choices, 'label', group = !!get_group(.)),
    male_health_problems_top_1_pct = select_percents(male_health_problems, 1, ., questions, choices, 'percent', group = !!get_group(.)),
    male_health_problems_top_2_name = select_percents(male_health_problems, 2, ., questions, choices, 'label', group = !!get_group(.)),
    male_health_problems_top_2_pct = select_percents(male_health_problems, 2, ., questions, choices, 'percent', group = !!get_group(.)),
    male_health_problems_top_3_name = select_percents(male_health_problems, 3, ., questions, choices, 'label', group = !!get_group(.)),
    male_health_problems_top_3_pct = select_percents(male_health_problems, 3, ., questions, choices, 'percent', group = !!get_group(.)),
    male_health_problems_top_4_name = select_percents(male_health_problems, 4, ., questions, choices, 'label', group = !!get_group(.)),
    male_health_problems_top_4_pct = select_percents(male_health_problems, 4, ., questions, choices, 'percent', group = !!get_group(.)),
    female_health_problems_top_1_name = select_percents(female_health_problems, 1, ., questions, choices, 'label', group = !!get_group(.)),
    female_health_problems_top_1_pct = select_percents(female_health_problems, 1, ., questions, choices, 'percent', group = !!get_group(.)),
    female_health_problems_top_2_name = select_percents(female_health_problems, 2, ., questions, choices, 'label', group = !!get_group(.)),
    female_health_problems_top_2_pct = select_percents(female_health_problems, 2, ., questions, choices, 'percent', group = !!get_group(.)),
    female_health_problems_top_3_name = select_percents(female_health_problems, 3, ., questions, choices, 'label', group = !!get_group(.)),
    female_health_problems_top_3_pct = select_percents(female_health_problems, 3, ., questions, choices, 'percent', group = !!get_group(.)),
    female_health_problems_top_4_name = select_percents(female_health_problems, 4, ., questions, choices, 'label', group = !!get_group(.)),
    female_health_problems_top_4_pct = select_percents(female_health_problems, 4, ., questions, choices, 'percent', group = !!get_group(.)),
    health_index2_modified_0 = percent_response(health_index2_modified, .,'0', group = !!get_group(.)),
    health_index2_modified_1 = percent_response(health_index2_modified, .,'1', group = !!get_group(.)),
    health_index3_modified_0 = percent_response(health_index3_modified, .,'0', group = !!get_group(.)),
    health_index3_modified_1 = percent_response(health_index3_modified, .,'1', group = !!get_group(.)),
    hygiene_handwashingfacilities_modified_0 = percent_response(hygiene_handwashingfacilities_modified, .,'0', group = !!get_group(.)),
    hygiene_handwashingfacilities_modified_1 = percent_response(hygiene_handwashingfacilities_modified, .,'1', group = !!get_group(.)),
    adeqaute_health_men_none = percent_response(adeqaute_health_men, .,'none', group = !!get_group(.)),
    adeqaute_health_men_few = percent_response(adeqaute_health_men, .,'few', group = !!get_group(.)),
    adeqaute_health_men_some = percent_response(adeqaute_health_men, .,'some', group = !!get_group(.)),
    adeqaute_health_men_many = percent_response(adeqaute_health_men, .,'many', group = !!get_group(.)),
    adeqaute_health_men_all = percent_response(adeqaute_health_men, .,'all', group = !!get_group(.)),
    adeqaute_health_men_dnk = percent_response(adeqaute_health_men, .,'dnk', group = !!get_group(.)),
    adeqaute_health_women_none = percent_response(adeqaute_health_women, .,'none', group = !!get_group(.)),
    adeqaute_health_women_few = percent_response(adeqaute_health_women, .,'few', group = !!get_group(.)),
    adeqaute_health_women_some = percent_response(adeqaute_health_women, .,'some', group = !!get_group(.)),
    adeqaute_health_women_many = percent_response(adeqaute_health_women, .,'many', group = !!get_group(.)),
    adeqaute_health_women_all = percent_response(adeqaute_health_women, .,'all', group = !!get_group(.)),
    adeqaute_health_women_dnk = percent_response(adeqaute_health_women, .,'dnk', group = !!get_group(.)),
    protection_childfriendlyspace_yes = percent_response(protection_childfriendlyspace, .,'yes', group = !!get_group(.)),
    protection_childfriendlyspace_no = percent_response(protection_childfriendlyspace, .,'no', group = !!get_group(.)),
    protection_childfriendlyspace_dnk = percent_response(protection_childfriendlyspace, .,'dnk', group = !!get_group(.)),
    protection_womenspace_yes = percent_response(protection_womenspace, .,'yes', group = !!get_group(.)),
    protection_womenspace_no = percent_response(protection_womenspace, .,'no', group = !!get_group(.)),
    protection_restrictions_day_yes = percent_response(protection_restrictions_day, .,'yes', group = !!get_group(.)),
    protection_restrictions_day_no = percent_response(protection_restrictions_day, .,'no', group = !!get_group(.)),
    protection_restrictions_day_dnk = percent_response(protection_restrictions_day, .,'dnk', group = !!get_group(.)),
    protection_restrictions_night_yes = percent_response(protection_restrictions_night, .,'yes', group = !!get_group(.)),
    protection_restrictions_night_no = percent_response(protection_restrictions_night, .,'no', group = !!get_group(.)),
    protection_restrictions_night_dnk = percent_response(protection_restrictions_night, .,'dnk', group = !!get_group(.)),
    protection_incidents_top_1_name = select_percents(protection_incidents, 1, ., questions, choices, 'label', group = !!get_group(.)),
    protection_incidents_top_1_pct = select_percents(protection_incidents, 1, ., questions, choices, 'percent', group = !!get_group(.)),
    protection_incidents_top_2_name = select_percents(protection_incidents, 2, ., questions, choices, 'label', group = !!get_group(.)),
    protection_incidents_top_2_pct = select_percents(protection_incidents, 2, ., questions, choices, 'percent', group = !!get_group(.)),
    protection_incidents_top_3_name = select_percents(protection_incidents, 3, ., questions, choices, 'label', group = !!get_group(.)),
    protection_incidents_top_3_pct = select_percents(protection_incidents, 3, ., questions, choices, 'percent', group = !!get_group(.)),
    protection_incidents_top_4_name = select_percents(protection_incidents, 4, ., questions, choices, 'label', group = !!get_group(.)),
    protection_incidents_top_4_pct = select_percents(protection_incidents, 4, ., questions, choices, 'percent', group = !!get_group(.)),
    protection_index1_1 = percent_response(protection_index1, .,'1', group = !!get_group(.)),
    protection_index1_2 = percent_response(protection_index1, .,'2', group = !!get_group(.)),
    protection_index1_3 = percent_response(protection_index1, .,'3', group = !!get_group(.)),
    protection_index1_4 = percent_response(protection_index1, .,'4', group = !!get_group(.)),
    insecure_areas_top_1_name = select_percents(insecure_areas, 1, ., questions, choices, 'label', group = !!get_group(.)),
    insecure_areas_top_1_pct = select_percents(insecure_areas, 1, ., questions, choices, 'percent', group = !!get_group(.)),
    insecure_areas_top_2_name = select_percents(insecure_areas, 2, ., questions, choices, 'label', group = !!get_group(.)),
    insecure_areas_top_2_pct = select_percents(insecure_areas, 2, ., questions, choices, 'percent', group = !!get_group(.)),
    insecure_areas_top_3_name = select_percents(insecure_areas, 3, ., questions, choices, 'label', group = !!get_group(.)),
    insecure_areas_top_3_pct = select_percents(insecure_areas, 3, ., questions, choices, 'percent', group = !!get_group(.)),
    insecure_areas_top_4_name = select_percents(insecure_areas, 4, ., questions, choices, 'label', group = !!get_group(.)),
    insecure_areas_top_4_pct = select_percents(insecure_areas, 4, ., questions, choices, 'percent', group = !!get_group(.)),
    protection_index2_1 = percent_response(protection_index2, .,'1', group = !!get_group(.)),
    protection_index2_3 = percent_response(protection_index2, .,'3', group = !!get_group(.)),
    protection_index2_4 = percent_response(protection_index2, .,'4', group = !!get_group(.)),
    housing_property_incidences_top_1_name = select_percents(housing_property_incidences, 1, ., questions, choices, 'label', group = !!get_group(.)),
    housing_property_incidences_top_1_pct = select_percents(housing_property_incidences, 1, ., questions, choices, 'percent', group = !!get_group(.)),
    housing_property_incidences_top_2_name = select_percents(housing_property_incidences, 2, ., questions, choices, 'label', group = !!get_group(.)),
    housing_property_incidences_top_2_pct = select_percents(housing_property_incidences, 2, ., questions, choices, 'percent', group = !!get_group(.)),
    housing_property_incidences_top_3_name = select_percents(housing_property_incidences, 3, ., questions, choices, 'label', group = !!get_group(.)),
    housing_property_incidences_top_3_pct = select_percents(housing_property_incidences, 3, ., questions, choices, 'percent', group = !!get_group(.)),
    housing_property_incidences_top_4_name = select_percents(housing_property_incidences, 4, ., questions, choices, 'label', group = !!get_group(.)),
    housing_property_incidences_top_4_pct = select_percents(housing_property_incidences, 4, ., questions, choices, 'percent', group = !!get_group(.)),
    hlp_index1_modified_0 = percent_response(hlp_index1_modified, .,'0', group = !!get_group(.)),
    hlp_index1_modified_1 = percent_response(hlp_index1_modified, .,'1', group = !!get_group(.)),
    rate_likelihood_eviction_very_high = percent_response(rate_likelihood_eviction, .,'very_high', group = !!get_group(.)),
    rate_likelihood_eviction_moderate_moderate = percent_response(rate_likelihood_eviction, .,'moderate_moderate', group = !!get_group(.)),
    rate_likelihood_eviction_low = percent_response(rate_likelihood_eviction, .,'low', group = !!get_group(.)),
    rate_likelihood_eviction_pnta = percent_response(rate_likelihood_eviction, .,'pnta', group = !!get_group(.)),
    evictions_landowner_top_1_name = select_percents(evictions_landowner, 1, ., questions, choices, 'label', group = !!get_group(.)),
    evictions_landowner_top_1_pct = select_percents(evictions_landowner, 1, ., questions, choices, 'percent', group = !!get_group(.)),
    evictions_landowner_top_2_name = select_percents(evictions_landowner, 2, ., questions, choices, 'label', group = !!get_group(.)),
    evictions_landowner_top_2_pct = select_percents(evictions_landowner, 2, ., questions, choices, 'percent', group = !!get_group(.)),
    evictions_landowner_top_3_name = select_percents(evictions_landowner, 3, ., questions, choices, 'label', group = !!get_group(.)),
    evictions_landowner_top_3_pct = select_percents(evictions_landowner, 3, ., questions, choices, 'percent', group = !!get_group(.)),
    evictions_landowner_top_4_name = select_percents(evictions_landowner, 4, ., questions, choices, 'label', group = !!get_group(.)),
    evictions_landowner_top_4_pct = select_percents(evictions_landowner, 4, ., questions, choices, 'percent', group = !!get_group(.)),
    hlp_nc_index2_0 = percent_response(hlp_nc_index2, .,'0', group = !!get_group(.)),
    hlp_nc_index2_1 = percent_response(hlp_nc_index2, .,'1', group = !!get_group(.)),
    evictions_notice_yes = percent_response(evictions_notice, .,'yes', group = !!get_group(.)),
    evictions_notice_no = percent_response(evictions_notice, .,'no', group = !!get_group(.)),
    evictions_notice_dnk = percent_response(evictions_notice, .,'dnk', group = !!get_group(.)),
    nfi_index1_modified_0 = percent_response(nfi_index1_modified, .,'0', group = !!get_group(.)),
    nfi_index1_modified_1 = percent_response(nfi_index1_modified, .,'1', group = !!get_group(.)),
    nfi_nc_index3_0 = percent_response(nfi_nc_index3, .,'0', group = !!get_group(.)),
    nfi_nc_index3_1 = percent_response(nfi_nc_index3, .,'1', group = !!get_group(.)),
    shelter_types_top_1_name = select_percents(shelter_types, 1, ., questions, choices, 'label', group = !!get_group(.)),
    shelter_types_top_1_pct = select_percents(shelter_types, 1, ., questions, choices, 'percent', group = !!get_group(.)),
    shelter_types_top_2_name = select_percents(shelter_types, 2, ., questions, choices, 'label', group = !!get_group(.)),
    shelter_types_top_2_pct = select_percents(shelter_types, 2, ., questions, choices, 'percent', group = !!get_group(.)),
    shelter_types_top_3_name = select_percents(shelter_types, 3, ., questions, choices, 'label', group = !!get_group(.)),
    shelter_types_top_3_pct = select_percents(shelter_types, 3, ., questions, choices, 'percent', group = !!get_group(.)),
    shelter_types_top_4_name = select_percents(shelter_types, 4, ., questions, choices, 'label', group = !!get_group(.)),
    shelter_types_top_4_pct = select_percents(shelter_types, 4, ., questions, choices, 'percent', group = !!get_group(.)),
    shelter_fire_yes = percent_response(shelter_fire, .,'yes', group = !!get_group(.)),
    shelter_fire_no = percent_response(shelter_fire, .,'no', group = !!get_group(.)),
    shelter_fire_dnk = percent_response(shelter_fire, .,'dnk', group = !!get_group(.)),
    shelter_flood_yes = percent_response(shelter_flood, .,'yes', group = !!get_group(.)),
    shelter_flood_no = percent_response(shelter_flood, .,'no', group = !!get_group(.)),
    shelter_flood_dnk = percent_response(shelter_flood, .,'dnk', group = !!get_group(.)),
    shelter_publiclighting_yes = percent_response(shelter_publiclighting, .,'yes', group = !!get_group(.)),
    shelter_publiclighting_no = percent_response(shelter_publiclighting, .,'no', group = !!get_group(.)),
    nfi_nc_index1_0 = percent_response(nfi_nc_index1, .,'0', group = !!get_group(.)),
    nfi_nc_index1_1 = percent_response(nfi_nc_index1, .,'1', group = !!get_group(.)),
    nfi_items_available_top_1_name = select_percents(nfi_items_available, 1, ., questions, choices, 'label', group = !!get_group(.)),
    nfi_items_available_top_1_pct = select_percents(nfi_items_available, 1, ., questions, choices, 'percent', group = !!get_group(.)),
    nfi_items_available_top_2_name = select_percents(nfi_items_available, 2, ., questions, choices, 'label', group = !!get_group(.)),
    nfi_items_available_top_2_pct = select_percents(nfi_items_available, 2, ., questions, choices, 'percent', group = !!get_group(.)),
    nfi_items_available_top_3_name = select_percents(nfi_items_available, 3, ., questions, choices, 'label', group = !!get_group(.)),
    nfi_items_available_top_3_pct = select_percents(nfi_items_available, 3, ., questions, choices, 'percent', group = !!get_group(.)),
    nfi_items_available_top_4_name = select_percents(nfi_items_available, 4, ., questions, choices, 'label', group = !!get_group(.)),
    nfi_items_available_top_4_pct = select_percents(nfi_items_available, 4, ., questions, choices, 'percent', group = !!get_group(.)),
    nfi_nc_index4_0 = percent_response(nfi_nc_index4, .,'0', group = !!get_group(.)),
    nfi_nc_index4_1 = percent_response(nfi_nc_index4, .,'1', group = !!get_group(.)),
    water_access_distance_min_less_15 = percent_response(water_access_distance_min, .,'less_15', group = !!get_group(.)),
    water_access_distance_min_15_30 = percent_response(water_access_distance_min, .,'15_30', group = !!get_group(.)),
    water_access_distance_min_31_60 = percent_response(water_access_distance_min, .,'31_60', group = !!get_group(.)),
    water_access_distance_min_more_60 = percent_response(water_access_distance_min, .,'more_60', group = !!get_group(.)),
    water_access_distance_min_dnk = percent_response(water_access_distance_min, .,'dnk', group = !!get_group(.)),
    water_sources_primary_top_1_name = select_percents(water_sources_primary, 1, ., questions, choices, 'label', group = !!get_group(.)),
    water_sources_primary_top_1_pct = select_percents(water_sources_primary, 1, ., questions, choices, 'percent', group = !!get_group(.)),
    water_sources_primary_top_2_name = select_percents(water_sources_primary, 2, ., questions, choices, 'label', group = !!get_group(.)),
    water_sources_primary_top_2_pct = select_percents(water_sources_primary, 2, ., questions, choices, 'percent', group = !!get_group(.)),
    water_sources_primary_top_3_name = select_percents(water_sources_primary, 3, ., questions, choices, 'label', group = !!get_group(.)),
    water_sources_primary_top_3_pct = select_percents(water_sources_primary, 3, ., questions, choices, 'percent', group = !!get_group(.)),
    water_sources_primary_top_4_name = select_percents(water_sources_primary, 4, ., questions, choices, 'label', group = !!get_group(.)),
    water_sources_primary_top_4_pct = select_percents(water_sources_primary, 4, ., questions, choices, 'percent', group = !!get_group(.)),
    wash_nc_index3_0 = percent_response(wash_nc_index3, .,'0', group = !!get_group(.)),
    wash_nc_index3_1 = percent_response(wash_nc_index3, .,'1', group = !!get_group(.)),
    water_access_barriers_top_1_name = select_percents(water_access_barriers, 1, ., questions, choices, 'label', group = !!get_group(.)),
    water_access_barriers_top_1_pct = select_percents(water_access_barriers, 1, ., questions, choices, 'percent', group = !!get_group(.)),
    water_access_barriers_top_2_name = select_percents(water_access_barriers, 2, ., questions, choices, 'label', group = !!get_group(.)),
    water_access_barriers_top_2_pct = select_percents(water_access_barriers, 2, ., questions, choices, 'percent', group = !!get_group(.)),
    water_access_barriers_top_3_name = select_percents(water_access_barriers, 3, ., questions, choices, 'label', group = !!get_group(.)),
    water_access_barriers_top_3_pct = select_percents(water_access_barriers, 3, ., questions, choices, 'percent', group = !!get_group(.)),
    water_access_barriers_top_4_name = select_percents(water_access_barriers, 4, ., questions, choices, 'label', group = !!get_group(.)),
    water_access_barriers_top_4_pct = select_percents(water_access_barriers, 4, ., questions, choices, 'percent', group = !!get_group(.)),
    sanitation_access_distance_min_less_15 = percent_response(sanitation_access_distance_min, .,'less_15', group = !!get_group(.)),
    sanitation_access_distance_min_15_30 = percent_response(sanitation_access_distance_min, .,'15_30', group = !!get_group(.)),
    sanitation_access_distance_min_31_60 = percent_response(sanitation_access_distance_min, .,'31_60', group = !!get_group(.)),
    sanitation_access_distance_min_more_60 = percent_response(sanitation_access_distance_min, .,'more_60', group = !!get_group(.)),
    sanitation_access_distance_min_dnk = percent_response(sanitation_access_distance_min, .,'dnk', group = !!get_group(.)),
    latrines_accessible_pwd_none = percent_response(latrines_accessible_pwd, .,'none', group = !!get_group(.)),
    latrines_accessible_pwd_few = percent_response(latrines_accessible_pwd, .,'few', group = !!get_group(.)),
    latrines_accessible_pwd_some = percent_response(latrines_accessible_pwd, .,'some', group = !!get_group(.)),
    latrines_accessible_pwd_many = percent_response(latrines_accessible_pwd, .,'many', group = !!get_group(.)),
    latrines_accessible_pwd_all = percent_response(latrines_accessible_pwd, .,'all', group = !!get_group(.)),
    latrines_accessible_pwd_dnk = percent_response(latrines_accessible_pwd, .,'dnk', group = !!get_group(.)),
    sanitation_solidwastedisposal_top_1_name = select_percents(sanitation_solidwastedisposal, 1, ., questions, choices, 'label', group = !!get_group(.)),
    sanitation_solidwastedisposal_top_1_pct = select_percents(sanitation_solidwastedisposal, 1, ., questions, choices, 'percent', group = !!get_group(.)),
    sanitation_solidwastedisposal_top_2_name = select_percents(sanitation_solidwastedisposal, 2, ., questions, choices, 'label', group = !!get_group(.)),
    sanitation_solidwastedisposal_top_2_pct = select_percents(sanitation_solidwastedisposal, 2, ., questions, choices, 'percent', group = !!get_group(.)),
    sanitation_solidwastedisposal_top_3_name = select_percents(sanitation_solidwastedisposal, 3, ., questions, choices, 'label', group = !!get_group(.)),
    sanitation_solidwastedisposal_top_3_pct = select_percents(sanitation_solidwastedisposal, 3, ., questions, choices, 'percent', group = !!get_group(.)),
    sanitation_solidwastedisposal_top_4_name = select_percents(sanitation_solidwastedisposal, 4, ., questions, choices, 'label', group = !!get_group(.)),
    sanitation_solidwastedisposal_top_4_pct = select_percents(sanitation_solidwastedisposal, 4, ., questions, choices, 'percent', group = !!get_group(.)),
    sanitation_access_impediments_top_1_name = select_percents(sanitation_access_impediments, 1, ., questions, choices, 'label', group = !!get_group(.)),
    sanitation_access_impediments_top_1_pct = select_percents(sanitation_access_impediments, 1, ., questions, choices, 'percent', group = !!get_group(.)),
    sanitation_access_impediments_top_2_name = select_percents(sanitation_access_impediments, 2, ., questions, choices, 'label', group = !!get_group(.)),
    sanitation_access_impediments_top_2_pct = select_percents(sanitation_access_impediments, 2, ., questions, choices, 'percent', group = !!get_group(.)),
    sanitation_access_impediments_top_3_name = select_percents(sanitation_access_impediments, 3, ., questions, choices, 'label', group = !!get_group(.)),
    sanitation_access_impediments_top_3_pct = select_percents(sanitation_access_impediments, 3, ., questions, choices, 'percent', group = !!get_group(.)),
    sanitation_access_impediments_top_4_name = select_percents(sanitation_access_impediments, 4, ., questions, choices, 'label', group = !!get_group(.)),
    sanitation_access_impediments_top_4_pct = select_percents(sanitation_access_impediments, 4, ., questions, choices, 'percent', group = !!get_group(.)),
    hygiene_handwashingfacilities_none = percent_response(hygiene_handwashingfacilities, .,'none', group = !!get_group(.)),
    hygiene_handwashingfacilities_few = percent_response(hygiene_handwashingfacilities, .,'few', group = !!get_group(.)),
    hygiene_handwashingfacilities_some = percent_response(hygiene_handwashingfacilities, .,'some', group = !!get_group(.)),
    hygiene_handwashingfacilities_many = percent_response(hygiene_handwashingfacilities, .,'many', group = !!get_group(.)),
    hygiene_handwashingfacilities_all = percent_response(hygiene_handwashingfacilities, .,'all', group = !!get_group(.)),
    hygiene_handwashingfacilities_dnk = percent_response(hygiene_handwashingfacilities, .,'dnk', group = !!get_group(.)),
    wash_index1_modified_0 = percent_response(wash_index1_modified, .,'0', group = !!get_group(.)),
    wash_index1_modified_1 = percent_response(wash_index1_modified, .,'1', group = !!get_group(.)),
    education_access_distance_min_less_15 = percent_response(education_access_distance_min, .,'less_15', group = !!get_group(.)),
    education_access_distance_min_15_30 = percent_response(education_access_distance_min, .,'15_30', group = !!get_group(.)),
    education_access_distance_min_31_60 = percent_response(education_access_distance_min, .,'31_60', group = !!get_group(.)),
    education_access_distance_min_more_60 = percent_response(education_access_distance_min, .,'more_60', group = !!get_group(.)),
    education_access_distance_min_dnk = percent_response(education_access_distance_min, .,'dnk', group = !!get_group(.)),
    education_facilities_primary = percent_response(education_facilities, .,'primary', group = !!get_group(.)),
    education_facilities_secondary = percent_response(education_facilities, .,'secondary', group = !!get_group(.)),
    education_facilities_quoranic = percent_response(education_facilities, .,'quoranic', group = !!get_group(.)),
    education_facilities_basic_edu = percent_response(education_facilities, .,'basic_edu', group = !!get_group(.)),
    education_facilities_no_available = percent_response(education_facilities, .,'no_available', group = !!get_group(.)),
    education_facilities_dnk = percent_response(education_facilities, .,'dnk', group = !!get_group(.)),
    education_facilities_other = percent_response(education_facilities, .,'other', group = !!get_group(.)),
    education_barriers_boys_top_1_name = select_percents(education_barriers_boys, 1, ., questions, choices, 'label', group = !!get_group(.)),
    education_barriers_boys_top_1_pct = select_percents(education_barriers_boys, 1, ., questions, choices, 'percent', group = !!get_group(.)),
    education_barriers_boys_top_2_name = select_percents(education_barriers_boys, 2, ., questions, choices, 'label', group = !!get_group(.)),
    education_barriers_boys_top_2_pct = select_percents(education_barriers_boys, 2, ., questions, choices, 'percent', group = !!get_group(.)),
    education_barriers_boys_top_3_name = select_percents(education_barriers_boys, 3, ., questions, choices, 'label', group = !!get_group(.)),
    education_barriers_boys_top_3_pct = select_percents(education_barriers_boys, 3, ., questions, choices, 'percent', group = !!get_group(.)),
    education_barriers_boys_top_4_name = select_percents(education_barriers_boys, 4, ., questions, choices, 'label', group = !!get_group(.)),
    education_barriers_boys_top_4_pct = select_percents(education_barriers_boys, 4, ., questions, choices, 'percent', group = !!get_group(.)),
    education_barriers_girls_top_1_name = select_percents(education_barriers_girls, 1, ., questions, choices, 'label', group = !!get_group(.)),
    education_barriers_girls_top_1_pct = select_percents(education_barriers_girls, 1, ., questions, choices, 'percent', group = !!get_group(.)),
    education_barriers_girls_top_2_name = select_percents(education_barriers_girls, 2, ., questions, choices, 'label', group = !!get_group(.)),
    education_barriers_girls_top_2_pct = select_percents(education_barriers_girls, 2, ., questions, choices, 'percent', group = !!get_group(.)),
    education_barriers_girls_top_3_name = select_percents(education_barriers_girls, 3, ., questions, choices, 'label', group = !!get_group(.)),
    education_barriers_girls_top_3_pct = select_percents(education_barriers_girls, 3, ., questions, choices, 'percent', group = !!get_group(.)),
    education_barriers_girls_top_4_name = select_percents(education_barriers_girls, 4, ., questions, choices, 'label', group = !!get_group(.)),
    education_barriers_girls_top_4_pct = select_percents(education_barriers_girls, 4, ., questions, choices, 'percent', group = !!get_group(.)),
    education_index1_1 = percent_response(education_index1, .,'1', group = !!get_group(.)),
    education_index1_2 = percent_response(education_index1, .,'2', group = !!get_group(.)),
    education_index1_3 = percent_response(education_index1, .,'3', group = !!get_group(.)),
    education_index1_4 = percent_response(education_index1, .,'4', group = !!get_group(.)),
    education_index2_1 = percent_response(education_index2, .,'1', group = !!get_group(.)),
    education_index2_2 = percent_response(education_index2, .,'2', group = !!get_group(.)),
    education_index2_3 = percent_response(education_index2, .,'3', group = !!get_group(.)),
    education_index2_4 = percent_response(education_index2, .,'4', group = !!get_group(.)),
    education_facilities_segregated_none = percent_response(education_facilities_segregated, .,'none', group = !!get_group(.)),
    education_facilities_segregated_few = percent_response(education_facilities_segregated, .,'few', group = !!get_group(.)),
    education_facilities_segregated_some = percent_response(education_facilities_segregated, .,'some', group = !!get_group(.)),
    education_facilities_segregated_many = percent_response(education_facilities_segregated, .,'many', group = !!get_group(.)),
    education_facilities_segregated_all = percent_response(education_facilities_segregated, .,'all', group = !!get_group(.)),
    education_facilities_segregated_dnk = percent_response(education_facilities_segregated, .,'dnk', group = !!get_group(.)),
    education_facilities_fences_none = percent_response(education_facilities_fences, .,'none', group = !!get_group(.)),
    education_facilities_fences_few = percent_response(education_facilities_fences, .,'few', group = !!get_group(.)),
    education_facilities_fences_some = percent_response(education_facilities_fences, .,'some', group = !!get_group(.)),
    education_facilities_fences_many = percent_response(education_facilities_fences, .,'many', group = !!get_group(.)),
    education_facilities_fences_all = percent_response(education_facilities_fences, .,'all', group = !!get_group(.)),
    education_facilities_fences_dnk = percent_response(education_facilities_fences, .,'dnk', group = !!get_group(.)),
    number_schools_opened_modified_0 = percent_response(number_schools_opened_modified, .,'0', group = !!get_group(.)),
    number_schools_opened_modified_1 = percent_response(number_schools_opened_modified, .,'1', group = !!get_group(.)),
    aap_informationsources_top_1_name = select_percents(aap_informationsources, 1, ., questions, choices, 'label', group = !!get_group(.)),
    aap_informationsources_top_1_pct = select_percents(aap_informationsources, 1, ., questions, choices, 'percent', group = !!get_group(.)),
    aap_informationsources_top_2_name = select_percents(aap_informationsources, 2, ., questions, choices, 'label', group = !!get_group(.)),
    aap_informationsources_top_2_pct = select_percents(aap_informationsources, 2, ., questions, choices, 'percent', group = !!get_group(.)),
    aap_informationsources_top_3_name = select_percents(aap_informationsources, 3, ., questions, choices, 'label', group = !!get_group(.)),
    aap_informationsources_top_3_pct = select_percents(aap_informationsources, 3, ., questions, choices, 'percent', group = !!get_group(.)),
    aap_informationsources_top_4_name = select_percents(aap_informationsources, 4, ., questions, choices, 'label', group = !!get_group(.)),
    aap_informationsources_top_4_pct = select_percents(aap_informationsources, 4, ., questions, choices, 'percent', group = !!get_group(.)),
    aap_informationsources_pwd_top_1_name = select_percents(aap_informationsources_pwd, 1, ., questions, choices, 'label', group = !!get_group(.)),
    aap_informationsources_pwd_top_1_pct = select_percents(aap_informationsources_pwd, 1, ., questions, choices, 'percent', group = !!get_group(.)),
    aap_informationsources_pwd_top_2_name = select_percents(aap_informationsources_pwd, 2, ., questions, choices, 'label', group = !!get_group(.)),
    aap_informationsources_pwd_top_2_pct = select_percents(aap_informationsources_pwd, 2, ., questions, choices, 'percent', group = !!get_group(.)),
    aap_informationsources_pwd_top_3_name = select_percents(aap_informationsources_pwd, 3, ., questions, choices, 'label', group = !!get_group(.)),
    aap_informationsources_pwd_top_3_pct = select_percents(aap_informationsources_pwd, 3, ., questions, choices, 'percent', group = !!get_group(.)),
    aap_informationsources_pwd_top_4_name = select_percents(aap_informationsources_pwd, 4, ., questions, choices, 'label', group = !!get_group(.)),
    aap_informationsources_pwd_top_4_pct = select_percents(aap_informationsources_pwd, 4, ., questions, choices, 'percent', group = !!get_group(.)),
    aap_access_barriers_top_1_name = select_percents(aap_access_barriers, 1, ., questions, choices, 'label', group = !!get_group(.)),
    aap_access_barriers_top_1_pct = select_percents(aap_access_barriers, 1, ., questions, choices, 'percent', group = !!get_group(.)),
    aap_access_barriers_top_2_name = select_percents(aap_access_barriers, 2, ., questions, choices, 'label', group = !!get_group(.)),
    aap_access_barriers_top_2_pct = select_percents(aap_access_barriers, 2, ., questions, choices, 'percent', group = !!get_group(.)),
    aap_access_barriers_top_3_name = select_percents(aap_access_barriers, 3, ., questions, choices, 'label', group = !!get_group(.)),
    aap_access_barriers_top_3_pct = select_percents(aap_access_barriers, 3, ., questions, choices, 'percent', group = !!get_group(.)),
    aap_access_barriers_top_4_name = select_percents(aap_access_barriers, 4, ., questions, choices, 'label', group = !!get_group(.)),
    aap_access_barriers_top_4_pct = select_percents(aap_access_barriers, 4, ., questions, choices, 'percent', group = !!get_group(.)),
    aap_feedbackmechanism_yes = percent_response(aap_feedbackmechanism, .,'yes', group = !!get_group(.)),
    aap_feedbackmechanism_no = percent_response(aap_feedbackmechanism, .,'no', group = !!get_group(.)),
    aap_feedbackmechanism_dnk = percent_response(aap_feedbackmechanism, .,'dnk', group = !!get_group(.)),
    covid_issue_yes = percent_response(covid_issue, .,'yes', group = !!get_group(.)),
    covid_issue_no = percent_response(covid_issue, .,'no', group = !!get_group(.)),
    covid_issue_dnk = percent_response(covid_issue, .,'dnk', group = !!get_group(.)),
    action_to_prevent_top_1_name = select_percents(action_to_prevent, 1, ., questions, choices, 'label', group = !!get_group(.)),
    action_to_prevent_top_1_pct = select_percents(action_to_prevent, 1, ., questions, choices, 'percent', group = !!get_group(.)),
    action_to_prevent_top_2_name = select_percents(action_to_prevent, 2, ., questions, choices, 'label', group = !!get_group(.)),
    action_to_prevent_top_2_pct = select_percents(action_to_prevent, 2, ., questions, choices, 'percent', group = !!get_group(.)),
    action_to_prevent_top_3_name = select_percents(action_to_prevent, 3, ., questions, choices, 'label', group = !!get_group(.)),
    action_to_prevent_top_3_pct = select_percents(action_to_prevent, 3, ., questions, choices, 'percent', group = !!get_group(.)),
    action_to_prevent_top_4_name = select_percents(action_to_prevent, 4, ., questions, choices, 'label', group = !!get_group(.)),
    action_to_prevent_top_4_pct = select_percents(action_to_prevent, 4, ., questions, choices, 'percent', group = !!get_group(.)),
    snfi_score_1 = percent_response(snfi_score, .,'1', group = !!get_group(.)),
    snfi_score_2 = percent_response(snfi_score, .,'2', group = !!get_group(.)),
    snfi_score_3 = percent_response(snfi_score, .,'3', group = !!get_group(.)),
    snfi_score_4 = 0,
    wash_score_1 = percent_response(wash_score, .,'1', group = !!get_group(.)),
    wash_score_2 = percent_response(wash_score, .,'2', group = !!get_group(.)),
    wash_score_3 = percent_response(wash_score, .,'3', group = !!get_group(.)),
    wash_score_4 = percent_response(wash_score, .,'4', group = !!get_group(.)),
    protection_score_1 = percent_response(protection_score, .,'1', group = !!get_group(.)),
    protection_score_2 = percent_response(protection_score, .,'2', group = !!get_group(.)),
    protection_score_3 = percent_response(protection_score, .,'3', group = !!get_group(.)),
    protection_score_4 = percent_response(protection_score, .,'4', group = !!get_group(.)),
    fs_score_1 = percent_response(fs_score, .,'1', group = !!get_group(.)),
    fs_score_2 = percent_response(fs_score, .,'2', group = !!get_group(.)),
    fs_score_3 = percent_response(fs_score, .,'3', group = !!get_group(.)),
    fs_score_4 = percent_response(fs_score, .,'4', group = !!get_group(.)),
    health_score_1 = percent_response(health_score, .,'1', group = !!get_group(.)),
    health_score_2 = percent_response(health_score, .,'2', group = !!get_group(.)),
    health_score_3 = percent_response(health_score, .,'3', group = !!get_group(.)),
    health_score_4 = percent_response(health_score, .,'4', group = !!get_group(.)),
    nutrition_score_1 = percent_response(nutrition_score, .,'1', group = !!get_group(.)),
    nutrition_score_2 = percent_response(nutrition_score, .,'2', group = !!get_group(.)),
    nutrition_score_3 = percent_response(nutrition_score, .,'3', group = !!get_group(.)),
    nutrition_score_4 = 0,
    education_score_1 = percent_response(education_score, .,'1', group = !!get_group(.)),
    education_score_2 = percent_response(education_score, .,'2', group = !!get_group(.)),
    education_score_3 = percent_response(education_score, .,'3', group = !!get_group(.)),
    education_score_4 = percent_response(education_score, .,'4', group = !!get_group(.)),
    hlp_score_1 = percent_response(hlp_score, .,'1', group = !!get_group(.)),
    hlp_score_2 = percent_response(hlp_score, .,'2', group = !!get_group(.)),
    hlp_score_3 = percent_response(hlp_score, .,'3', group = !!get_group(.)),
    hlp_score_4 = percent_response(hlp_score, .,'4', group = !!get_group(.)),
    hlp_score_3_plus = percent_response(health_score_binary, .,'3+', group = !!get_group(.)),
    snfi_score_3_plus = percent_response(snfi_score_binary, .,'3+', group = !!get_group(.)),
    protection_score_3_plus = percent_response(protection_score_binary, .,'3+', group = !!get_group(.)),
    fs_score_3_plus = percent_response(fs_score_binary, .,'3+', group = !!get_group(.)),
    health_score_3_plus = percent_response(health_score_binary, .,'3+', group = !!get_group(.)),
    nutrition_score_3_plus = percent_response(nutrition_score_binary, .,'3+', group = !!get_group(.)),
    education_score_3_plus = percent_response(education_score_binary, .,'3+', group = !!get_group(.)),
    hlp_score_3_plus = percent_response(hlp_score_binary, .,'3+', group = !!get_group(.)),
    wash_score_3_plus = percent_response(wash_score_binary, .,'3+', group = !!get_group(.))
    
    
  )
#### Final sectoral scores ####

data_merge_national <- data_merge_national %>% mutate(
  snfi_score = case_when(
    snfi_score_4 >= 20 ~ 4,
    rowSums(across(c(snfi_score_4,snfi_score_3)),na.rm = T) >= 20 ~ 3,
    rowSums(across(c(snfi_score_4,snfi_score_3,snfi_score_2)),na.rm = T) >= 20 ~ 2,
    TRUE ~ 1
  ),
  
  
  wash_score = case_when(
    wash_score_4 >= 20 ~ 4,
    rowSums(across(c(wash_score_4,wash_score_3)),na.rm = T) >= 20 ~ 3,
    rowSums(across(c(wash_score_4,wash_score_3,wash_score_2)),na.rm = T) >= 20 ~ 2,
    TRUE ~ 1
  ),
  
  protection_score = case_when(
    protection_score_4 >= 20 ~ 4,
    rowSums(across(c(protection_score_4,protection_score_3)),na.rm = T) >= 20 ~ 3,
    rowSums(across(c(protection_score_4,protection_score_3,protection_score_2)),na.rm = T) >= 20 ~ 2,
    TRUE ~ 1
  ),
  
  fs_score = case_when(
    fs_score_4 >= 20 ~ 4,
    rowSums(across(c(fs_score_4,fs_score_3)),na.rm = T) >= 20 ~ 3,
    rowSums(across(c(fs_score_4,fs_score_3,fs_score_2)),na.rm = T) >= 20 ~ 2,
    TRUE ~ 1
  ),
  
  health_score = case_when(
    health_score_4 >= 20 ~ 4,
    rowSums(across(c(health_score_4,health_score_3)),na.rm = T) >= 20 ~ 3,
    rowSums(across(c(health_score_4,health_score_3,health_score_2)),na.rm = T) >= 20 ~ 2,
    TRUE ~ 1
  ),
  
  nutrition_score = case_when(
    nutrition_score_4 >= 20 ~ 4,
    rowSums(across(c(nutrition_score_4,nutrition_score_3)),na.rm = T) >= 20 ~ 3,
    rowSums(across(c(nutrition_score_4,nutrition_score_3,nutrition_score_2)),na.rm = T) >= 20 ~ 2,
    TRUE ~ 1
  ),
  
  education_score = case_when(
    education_score_4 >= 20 ~ 4,
    rowSums(across(c(education_score_4,education_score_3)),na.rm = T) >= 20 ~ 3,
    rowSums(across(c(education_score_4,education_score_3,education_score_2)),na.rm = T) >= 20 ~ 2,
    TRUE ~ 1
  ),
  
  hlp_score = case_when(
    hlp_score_4 >= 20 ~ 4,
    rowSums(across(c(hlp_score_4,hlp_score_3)),na.rm = T) >= 20 ~ 3,
    rowSums(across(c(hlp_score_4,hlp_score_3,hlp_score_2)),na.rm = T) >= 20 ~ 2,
    TRUE ~ 1
  ))


# map_chr(names(data_merge_national) %>% tail(8),
#  ~ sprintf('%s_phase = case_when(
#   %s == 4 ~ "Extreme",
#   %s == 3 ~ "Severe",
#   %s == 2 ~ "Stress",
#   %s == 1 ~ "None"
# ),
#            ',.x,.x,.x,.x,.x) ) %>% clipr::write_clip()



  
data_merge_national <- data_merge_national %>% mutate(
  
  snfi_score_phase = case_when(
    snfi_score == 4 ~ "Extreme",
    snfi_score == 3 ~ "Severe",
    snfi_score == 2 ~ "Stress",
    snfi_score == 1 ~ "None"
  ),
  
  wash_score_phase = case_when(
    wash_score == 4 ~ "Extreme",
    wash_score == 3 ~ "Severe",
    wash_score == 2 ~ "Stress",
    wash_score == 1 ~ "None"
  ),
  
  protection_score_phase = case_when(
    protection_score == 4 ~ "Extreme",
    protection_score == 3 ~ "Severe",
    protection_score == 2 ~ "Stress",
    protection_score == 1 ~ "None"
  ),
  
  fs_score_phase = case_when(
    fs_score == 4 ~ "Extreme",
    fs_score == 3 ~ "Severe",
    fs_score == 2 ~ "Stress",
    fs_score == 1 ~ "None"
  ),
  
  health_score_phase = case_when(
    health_score == 4 ~ "Extreme",
    health_score == 3 ~ "Severe",
    health_score == 2 ~ "Stress",
    health_score == 1 ~ "None"
  ),
  
  nutrition_score_phase = case_when(
    nutrition_score == 4 ~ "Extreme",
    nutrition_score == 3 ~ "Severe",
    nutrition_score == 2 ~ "Stress",
    nutrition_score == 1 ~ "None"
  ),
  
  education_score_phase = case_when(
    education_score == 4 ~ "Extreme",
    education_score == 3 ~ "Severe",
    education_score == 2 ~ "Stress",
    education_score == 1 ~ "None"
  ),
  
  hlp_score_phase = case_when(
    hlp_score == 4 ~ "Extreme",
    hlp_score == 3 ~ "Severe",
    hlp_score == 2 ~ "Stress",
    hlp_score == 1 ~ "None"
  )
  
)


if(Somaliland) {
  write.csv(data_merge_national,"output/Data merge/data_merge_output_3103_national_somaliland.csv",row.names = F,na = "NA")
} else {
  write.csv(data_merge_national,"output/Data merge/data_merge_output_3103_national_somalia.csv",row.names = F,na = "NA")
}




  