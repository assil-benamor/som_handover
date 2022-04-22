############################
############################
# Data Preparation 
############################
############################

if (!require("pacman")) install.packages("pacman")

p_load(
  rio,
  tidyverse,
  crayon,
  hypegrammaR,
  sjmisc,
  koboquest,
  reshape2
)

# clean_data <- readxl::read_xlsx("/Users/mac/Downloads/REACH_SOM2101_JMCNA_CleanData_v10.xlsx",
#                   sheet = "Clean_Data",
#                   na = c("","NA"),
#                   guess_max = 500000)
# 
# write_rds(clean_data,"input/clean_data.RDS")

source("./src/functions/functions.R")

clean_data <-  readRDS("input/clean_data.RDS") ### Copy of the clean data sheet from REACH_SOM2101_JMCNA_CleanData_v9.xlsx and takes less time to load

data <- clean_data %>% filter(consensus == "yes")

questions <- import("input/tool/SOM_JMCNA_HH_Tool_2021_v5.xlsx", sheet = "survey",guess_max=50000) %>%
  select(-1) %>%
  filter(!is.na(name))

questions$`label::English` <- gsub("<[^>]*>","",questions$`label::English`,perl = T)

## Deleting group names from the columns 

group_names <- questions %>% filter(type=="begin_group") %>% pull(name)

group_names_rgx <- sprintf("(%s)",
                           gsub(" ","|",paste(paste0("^",group_names,"\\."), collapse=" ")))

colnames(data) <- gsub(group_names_rgx,"",colnames(data))


## District names and strata


xml_names = c("abdulaziz", "aden_yabaal", "afgooye", "afmadow", "baardheere", "badhaadhe", "badhan", "baidoa", "baki", "balcad", "bandarbayla", "baraawe", "belet_weyne", "belet_xaawo", "berbera", "boondheere", "borama", "bossaso", "buaale", "bulo_burto", "burco", "burtinle", "buuhoodle", "buur_hakaba", "cabudwaaq", "cadaado", "cadale", "caluula", "caynabo", "ceel_afweyn", "ceel_barde", "ceel_buur", "ceel_dheer", "ceel_waaq", "ceerigaabo", "daynile", "dharkenley", "dhuusamarreeb", "diinsoor", "doolow", "eyl", "gaalkacyo_north", "gaalkacyo_south", "galdogob", "garbahaarey", "garowe", "gebiley", "hamar_jaab_jab", "hamar_weyne", "hargeysa", "hawl_wadaag", "heliwa", "hobyo", "hodan", "iskushuban", "jalalaqsi", "jamaame", "jariiban", "jowhar", "kahda", "karaan", "kismayo", "kurtunwaarey", "laas_caanood", "laasqoray", "lughaye", "luuq", "marka", "mataban", "owdweyne", "qandala", "qansax_dheere", "qardho", "qoryooley", "rab_dhuure", "sablaale", "shangaani", "sheikh", "shibis", "taleex", "tayeeglow", "waaberi", "waajid", "wadajir", "wanla_weyn", "wardhiigleey", "xudun", "xudur", "yaaqshiid", "zeylac")
pop_names = c("Banadir", "Adan_Yabaal", "Afgooye", "Afmadow", "Baardheere", "Badhaadhe", "Laasqoray", "Baidoa", "Baki", "Balcad", "Bandarbayla", "Baraawe", "Belet_Weyne", "Belet_Xaawo", "Berbera", "Banadir", "Borama", "Bossaso", "Bu'aale", "Bulo_Burto", "Burco", "Burtinle", "Buuhoodle", "Buur_Hakaba", "Cabudwaaq", "Cadaado", "Cadale", "Caluula", "Caynabo", "Ceel_Afweyn", "Ceel_Barde", "Ceel_Buur", "Ceel_Dheer", "Ceel_Waaq", "Ceerigaabo", "Banadir", "Banadir", "Dhuusamarreeb", "Diinsoor", "Doolow", "Eyl", "Gaalkacyo", "Gaalkacyo", "Galdogob", "Garbahaarey", "Garowe", "Gebiley", "Banadir", "Banadir", "Hargeysa", "Banadir", "Banadir", "Hobyo", "Banadir", "Iskushuban", "Jalalaqsi", "Jamaame", "Jariiban", "Jowhar", "Banadir", "Banadir", "Kismayo", "Kurtunwaarey", "Laas_Caanood", "Laasqoray", "Lughaye", "Luuq", "Marka", "Belet_Weyne", "Owdweyne", "Qandala", "Qansax_Dheere", "Qardho", "Qoryooley", "Rab_Dhuure", "Sablaale", "Banadir", "Sheikh", "Banadir", "Taleex", "Tayeeglow", "Banadir", "Waajid", "Banadir", "Wanla_Weyn", "Banadir", "Xudun", "Xudur", "Banadir", "Zeylac")



district_names_matching = data.frame(district=xml_names,
                                     pop_names=pop_names,
                                     stringsAsFactors = F)

data$district = left_join(data,district_names_matching) %>% pull(pop_names)

data <- data %>% mutate(
  population_group = case_when(
    idp_settlement == "yes" ~ "IDP",
    idp_settlement == "no" ~ "HC"
  ),
  
  strata = paste0(district,"_",population_group)
)



### Correct colname
colnames(data) <- gsub("support_online_","support_online_education.",colnames(data))
colnames(data) <- gsub("support_online_education.education","support_online_education",colnames(data))
colnames(data) <- gsub("covid_signs.Conjunctivitus_\\.red_eyes\\.","covid_signs.Conjunctivitus_red_eyes",colnames(data))
colnames(data) <- gsub("don't_know","do_not_know",colnames(data))
colnames(data) <- gsub("^construction_school$","education_support.construction_school",colnames(data))
colnames(data) <- gsub("^provision_qualified_teachers$","education_support.provision_qualified_teachers",colnames(data))


### Convert numerical questions
num_q = questions %>% filter(type %in% c("calculate","integer")) %>% pull(name)
num_q = num_q[8:length(num_q)] 
num_q = num_q[!num_q %in% c("member_position","age_wgq","number_years","number_months","child_not_liv_position")]

data <- mutate_at(data,num_q,as.numeric)

### Load questionnaire

choices <- import("input/tool/SOM_JMCNA_HH_Tool_2021_v5.xlsx", sheet = "choices",guess_max=50000)

questionnaire <- load_questionnaire(
  data = data,
  questions = questions,
  choices = choices,
  choices.label.column.to.use = "label::English"
)

data[data=="don't_know"]<- "do_not_know"

data <- data %>% mutate(
  persons_with_valid_ids = ifelse(persons_with_valid_ids=="do_not_know","dnk",persons_with_valid_ids),
  main_source_water = ifelse(main_source_water %in% c("un_protected_wel","un_protected_well"),"unprotected_well",main_source_water )
  
)

other_questions <- map(questions %>% filter(grepl("^select_multiple.* or_other$",type)) %>% select(name),~paste0(.x,"_other")) $name
other_questions_binary <- map(questions %>% filter(grepl("^select_multiple.* or_other$",type)) %>% select(name),~paste0(.x,".other")) $name

data <- replace_columns(data,correct_other_binaries(data,other_questions_binary,other_questions) ) 

select_multiple_questions <-c("common_type_ids", "reasons_for_not_having_ids", "hh_main_source_income", "main_reason_loss_employement", "disp_why1", "disp_why2", "disp_reasons_1", "disp_reason_2", "modalities_distance_learning", "education_barriers_boys", "education_barries_girls", "education_support", "support_online_education", "sanitation_features", "sanitation_facilities_problems", "adopt_sanitaiton_items", "adopt_lack_water", "adapt_sanitation_issues", "water_access_problems_yes", "main_sources_food", "type_information", "where_receive_information", "preferred_means_info", "aid_agency_feedback", "satisfied_aid_recieived", "aid_barriers", "aid_denail_yes", "aap_top_priority", "future_aid_assistance", "any_other_shelter_types_yes", "shelter_enclosure_issue", "shelter_damage", "shelter_issues", "land_dispute_yes", "shelter_problems", "shelter_top_priority", "currently_access_nfi", "what_vacine_barriers", "seek_healthcare", "unmet_health_care_needs", "no_unmet_health_care_needs", "no_health_care_needs", "main_safety_concerns_boys", "main_safety_concerns_girls", "main_safety_concerns_men", "main_safety_concerns_women", "members_faced_restrictions", "barriers_mental_healths", "barriers_gbv_servies", "barriers_legal_protec", "barriers_livelihoods_s", "unsafe_locations_girls_yes", "unsafe_locations_boys_yes", "effect_explosive_hazards_yes", "child_prot_barriers_mental_health_supports_boys", "child_prot_barriers_mental_health_supports_girls", "child_prot_barriers_social_services_boys", "child_prot_barriers_social_services_girls", "child_prot_barriers_support_group_activites_boys", "child_prot_barriers_support_group_activites_girls", "mobile_health_visit_yes", "nutrition_services", "nutrition_difficulties", "covid_info_channels", "type_covid_info", "covid_actions_taken", "not_taken_covid_actions", "covid_signs", "what_do_covid", "not_taking_covid_vacine")

data <- replace_columns(data, generate_from_binaries(data,select_multiple_questions)) 

sampling.frame <- load_samplingframe("input/sampling frame/sampling_frame_final.csv")

weighting_function <-map_to_weighting(sampling.frame = sampling.frame,
                                      data.stratum.column = "strata",
                                      sampling.frame.population.column = "population",
                                      sampling.frame.stratum.column = "strata.names",
                                      data = data)

data[["weights"]] <-  weighting_function(data)

data$uuid =data$`_uuid`

rm(list =  c("clean_data", "num_q", "xml_names", "pop_names", "group_names", "group_names_rgx","select_multiple_questions"))

# write.csv(data,"clean_data_w.csv",row.names = F)










