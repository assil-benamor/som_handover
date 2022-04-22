rm(list = ls())

if (!require("pacman")) install.packages("pacman")

p_load(
  rio,
  tidyverse,
  crayon,
  hypegrammaR,
  composr,
  sjmisc,
  matrixStats
  )


############ Data preparation ############ 

source("./src/functions/pin_functions.R")
source("./src/functions/pin_functions copy.R")

source("./src/functions/functions.R")
source("./src/functions/functinos_other.R")
source("./src/load_data.R")


##### To generate the data.RDS file and decrypt the dataset use :
## key <- cyphr::key_sodium(readRDS("key"))
## cyphr::decrypt_file("input/data.RDS.encrypted", key, "input/data.RDS")
##### Make sure cyphr library is installed



############ Education ###########
education_template <- import("input/PIN_HNO_Indicators.xlsx",sheet="Education")

indicators = education_template %>% filter(!is.na(Indicators)) %>% pull("Indicators")

education_df <- generate_template(
  cluster_name = education_template[1, 1],
  questions = education_template %>% filter(!is.na(Questions))%>% pull("Questions"),
  indicators = indicators
)

edu_indicators <- compute_indicators(data,"Education",indicators)

education_df <- replace_columns(education_df, edu_indicators ) 

# export(education_df, "output/Pin calculation/education/Indicators.csv", na = "NA")

HH_AL_Aggregation <- education_df %>% mutate(
  strata = paste0(Area,"_",`Population group`)
) %>% select(
  uuid,
  KEY = strata,
  Area = Area,
  `Population group`, 
  all_of(indicators),
  `Final HH Score`,
) 

dummy_strata = data.frame(x1 = stri_rand_strings(10, 36, pattern = "[A-Za-z0-9]"),
                          x2 = c(rep("Zdummy_HC",5),rep("Zdummy_IDP",5)),
                          x3 = rep("Zdummy",10),
                          x4 = c(rep("HC",5),rep("IDP",5)),
                          x5 = rep(1:5,2),
                          x6 = rep(1:5,2),
                          x7 = rep(1:5,2),
                          x8 = rep(1:5,2),
                          x9 = rep(1:5,2),
                          stringsAsFactors = F
)

colnames(dummy_strata) = colnames(HH_AL_Aggregation)

HH_AL_Aggregation <- rbind(HH_AL_Aggregation,dummy_strata)
HH_AL_Aggregation$`Final HH Score` = 99
write.csv(HH_AL_Aggregation,"output/Pin calculation/education/C-HH_AL_Aggregation_education.csv",row.names = F,na = "1")



############ Wash ###########
wash_template <- import("input/PIN_HNO_Indicators.xlsx",sheet="Wash")

indicators = wash_template %>% filter(!is.na(Indicators)) %>% pull("Indicators") %>% .[-c(6,7,8)]


wash_df <- generate_template(
  cluster_name = wash_template[1, 1],
  questions = wash_template %>% filter(!is.na(Questions))%>% pull("Questions"),
  indicators = indicators
)

wash_indicators <- compute_indicators(data,"Wash",indicators)

wash_df <- replace_columns(wash_df, wash_indicators ) 

# export(wash_df, "output/Pin calculation/wash/Indicators.csv", na = "NA")

HH_AL_Aggregation <- wash_df %>% mutate(
  strata = paste0(Area,"_",`Population group`)
) %>% select(
  uuid,
  KEY = strata,
  Area = Area,
  `Population group`, 
  all_of(indicators),
  `Final HH Score`,
) 

dummy_strata = data.frame(x1 = stri_rand_strings(10, 36, pattern = "[A-Za-z0-9]"),
                          x2 = c(rep("Zdummy_HC",5),rep("Zdummy_IDP",5)),
                          x3 = rep("Zdummy",10),
                          x4 = c(rep("HC",5),rep("IDP",5)),
                          x5 = rep(1:5,2),
                          x6 = rep(1:5,2),
                          x7 = rep(1:5,2),
                          x8 = rep(1:5,2),
                          x9 = rep(1:5,2),
                          x10 = rep(1:5,2),
                          x11 = rep(1:5,2),
                          stringsAsFactors = F
)

colnames(dummy_strata) = colnames(HH_AL_Aggregation)

HH_AL_Aggregation <- rbind(HH_AL_Aggregation,dummy_strata)
HH_AL_Aggregation$`Final HH Score` = 99

x <- pin_process(HH_AL_Aggregation,"round") [[2]] %>% View()

write.csv(HH_AL_Aggregation,"output/Pin calculation/wash/C-HH_AL_Aggregation_wash.csv",row.names = F,na = "1")


############ Protection ###########

protection_template <- import("input/PIN_HNO_Indicators.xlsx",sheet="Protection")

indicators = protection_template %>% filter(!is.na(Indicators)) %>% pull("Indicators")

protection_df <- generate_template(
  cluster_name = protection_template[1, 1],
  questions = protection_template %>% filter(!is.na(Questions))%>% pull("Questions"),
  indicators = indicators
)

protection_indicators <- compute_indicators(data,"Protection",indicators)

protection_df <- replace_columns(protection_df, protection_indicators) 


# export(protection_df, "output/Pin calculation/protection/Indicators.csv", na = "NA")

HH_AL_Aggregation <- protection_df %>% mutate(
  strata = paste0(Area,"_",`Population group`)
) %>% select(
  uuid,
  KEY = strata,
  Area = Area,
  `Population group`, 
  all_of(indicators),
  `Final HH Score`,
) 

dummy_strata = data.frame(x1 = stri_rand_strings(10, 36, pattern = "[A-Za-z0-9]"),
                          x2 = c(rep("Zdummy_HC",5),rep("Zdummy_IDP",5)),
                          x3 = rep("Zdummy",10),
                          x4 = c(rep("HC",5),rep("IDP",5)),
                          x5 = rep(1:5,2),
                          x6 = rep(1:5,2),
                          x7 = rep(1:5,2),
                          x8 = rep(1:5,2),
                          x9 = rep(1:5,2),
                          x10 = rep(1:5,2),
                          stringsAsFactors = F
)

colnames(dummy_strata) = colnames(HH_AL_Aggregation)

HH_AL_Aggregation <- rbind(HH_AL_Aggregation,dummy_strata)
HH_AL_Aggregation$`Final HH Score` = 99

# pin_process(HH_AL_Aggregation,"round") [[2]] %>% View()

write.csv(HH_AL_Aggregation,"output/Pin calculation/protection/C-HH_AL_Aggregation_protection.csv",row.names = F,na = "1")


# protection_df_scores <- protection_df %>% select(region,Key,idp_settlement,all_of(indicators)) 
# protection_df_scores <- protection_df_scores %>% mutate(
#   ##### calculation of the HH score goes in here
#   hh_score_ceiling = ceiling(rowMeans(.[-grep("^(region|Key|idp_settlement)$", names(.))], na.rm = T)),
# ) 



############ SNFI ###########

SNFI_template <- import("input/PIN_HNO_Indicators.xlsx",sheet="SNFI")


indicators = SNFI_template %>% filter(!is.na(Indicators)) %>% pull("Indicators")

SNFI_df <- generate_template(
  cluster_name = SNFI_template[1, 1],
  questions = SNFI_template %>% filter(!is.na(Questions))%>% pull("Questions"),
  indicators = indicators
)

SNFI_indicators <- compute_indicators(data,"SNFI",indicators)

SNFI_df <- replace_columns(SNFI_df, SNFI_indicators) 


# export(SNFI_df, "output/Pin calculation/snfi/Indicators.csv", na = "NA")

HH_AL_Aggregation <- SNFI_df %>% mutate(
  strata = paste0(Area,"_",`Population group`)
) %>% select(
  uuid,
  KEY = strata,
  Area = Area,
  `Population group`, 
  all_of(indicators),
  `Final HH Score`,
) 

dummy_strata = data.frame(x1 = stri_rand_strings(10, 36, pattern = "[A-Za-z0-9]"),
                          x2 = c(rep("Zdummy_HC",5),rep("Zdummy_IDP",5)),
                          x3 = rep("Zdummy",10),
                          x4 = c(rep("HC",5),rep("IDP",5)),
                          x5 = rep(1:5,2),
                          x6 = rep(1:5,2),
                          x7 = rep(1:5,2),
                          x8 = rep(1:5,2),
                          x9 = rep(1:5,2),
                          x10 = rep(1:5,2),
                          # x11 = rep(1:5,2),
                          stringsAsFactors = F
)

colnames(dummy_strata) = colnames(HH_AL_Aggregation)

HH_AL_Aggregation <- rbind(HH_AL_Aggregation,dummy_strata)
HH_AL_Aggregation$`Final HH Score` = 99

pin_process(HH_AL_Aggregation,"round") [[2]] %>% View()

write.csv(HH_AL_Aggregation,"output/Pin calculation/snfi/C-HH_AL_Aggregation_snfi.csv",row.names = F,na = "1")



############ GBV ###########

gbv_template <- import("input/PIN_HNO_Indicators.xlsx",sheet="GBV AoR")


indicators = gbv_template %>% filter(!is.na(Indicators)) %>% pull("Indicators")

gbv_df <- generate_template(
  cluster_name = gbv_template[1, 1],
  questions = gbv_template %>% filter(!is.na(Questions))%>% pull("Questions"),
  indicators = indicators
)


gbv_indicators <- compute_indicators(data,"GBV_AoR",indicators)

gbv_df <- replace_columns(gbv_df, gbv_indicators) 

# export(gbv_df, "output/Pin calculation/GBV/Indicators.csv", na = "NA")

HH_AL_Aggregation <- gbv_df %>% mutate(
  strata = paste0(Area,"_",`Population group`)
) %>% select(
  uuid,
  KEY = strata,
  Area = Area,
  `Population group`, 
  all_of(indicators),
  `Final HH Score`,
) 

dummy_strata = data.frame(x1 = stri_rand_strings(10, 36, pattern = "[A-Za-z0-9]"),
                          x2 = c(rep("Zdummy_HC",5),rep("Zdummy_IDP",5)),
                          x3 = rep("Zdummy",10),
                          x4 = c(rep("HC",5),rep("IDP",5)),
                          x5 = rep(1:5,2),
                          x6 = rep(1:5,2),
                          x7 = rep(1:5,2),
                          x8 = rep(1:5,2),
                          x9 = rep(1:5,2),
                          stringsAsFactors = F
)

colnames(dummy_strata) = colnames(HH_AL_Aggregation)

HH_AL_Aggregation <- rbind(HH_AL_Aggregation,dummy_strata)
HH_AL_Aggregation$`Final HH Score` = 99
write.csv(HH_AL_Aggregation,"output/Pin calculation/GBV/C-HH_AL_Aggregation_gbv.csv",row.names = F,na = "1")




############ HLP ###########

hlp_template <- import("input/PIN_HNO_Indicators.xlsx",sheet="HLP AoR")


indicators = hlp_template %>% filter(!is.na(Indicators)) %>% pull("Indicators")

hlp_df <- generate_template(
  cluster_name = hlp_template[1, 1],
  questions = hlp_template %>% filter(!is.na(Questions))%>% pull("Questions"),
  indicators = indicators
)

hlp_indicators <- compute_indicators(data,"HLP_AoR",indicators)

hlp_df <- replace_columns(hlp_df, hlp_indicators) 

# export(hlp_df, "output/Pin calculation/hlp/Indicators.csv", na = "NA")

HH_AL_Aggregation <- hlp_df %>% mutate(
  strata = paste0(Area,"_",`Population group`)
) %>% select(
  uuid,
  KEY = strata,
  Area = Area,
  `Population group`, 
  all_of(indicators),
  `Final HH Score`,
) 

dummy_strata = data.frame(x1 = stri_rand_strings(10, 36, pattern = "[A-Za-z0-9]"),
                          x2 = c(rep("Zdummy_HC",5),rep("Zdummy_IDP",5)),
                          x3 = rep("Zdummy",10),
                          x4 = c(rep("HC",5),rep("IDP",5)),
                          x5 = rep(1:5,2),
                          x6 = rep(1:5,2),
                          x7 = rep(1:5,2),
                          x8 = rep(1:5,2),
                          # x9 = rep(1:5,2),
                          stringsAsFactors = F
)

colnames(dummy_strata) = colnames(HH_AL_Aggregation)

HH_AL_Aggregation <- rbind(HH_AL_Aggregation,dummy_strata)
HH_AL_Aggregation$`Final HH Score` = 99
write.csv(HH_AL_Aggregation,"output/Pin calculation/hlp/C-HH_AL_Aggregation_hlp.csv",row.names = F,na = "1")


############ CP ###########

CP_template <- import("input/PIN_HNO_Indicators.xlsx",sheet="CP AoR")


indicators = CP_template %>% filter(!is.na(Indicators)) %>% pull("Indicators")

CP_df <- generate_template(
  cluster_name = CP_template[1, 1],
  questions = CP_template %>% filter(!is.na(Questions))%>% pull("Questions"),
  indicators = indicators
)

CP_indicators <- compute_indicators(data,"CP_AoR",indicators)

CP_df <- replace_columns(CP_df, CP_indicators) 

# export(CP_df, "output/Pin calculation/cp/Indicators.csv", na = "NA")

HH_AL_Aggregation <- CP_df %>% mutate(
  strata = paste0(Area,"_",`Population group`)
) %>% select(
  uuid,
  KEY = strata,
  Area = Area,
  `Population group`, 
  all_of(indicators),
  `Final HH Score`,
) 

dummy_strata = data.frame(x1 = stri_rand_strings(10, 36, pattern = "[A-Za-z0-9]"),
                          x2 = c(rep("Zdummy_HC",5),rep("Zdummy_IDP",5)),
                          x3 = rep("Zdummy",10),
                          x4 = c(rep("HC",5),rep("IDP",5)),
                          x5 = rep(1:5,2),
                          x6 = rep(1:5,2),
                          x7 = rep(1:5,2),
                          x8 = rep(1:5,2),
                          # x9 = rep(1:5,2),
                          stringsAsFactors = F
)

colnames(dummy_strata) = colnames(HH_AL_Aggregation)

HH_AL_Aggregation <- rbind(HH_AL_Aggregation,dummy_strata)
HH_AL_Aggregation$`Final HH Score` = 99
write.csv(HH_AL_Aggregation,"output/Pin calculation/cp/C-HH_AL_Aggregation_cp.csv",row.names = F,na = "1")



############ EH ###########

EH_template <- import("input/PIN_HNO_Indicators.xlsx",sheet="EH AoR")

indicators = EH_template %>% filter(!is.na(Indicators)) %>% pull("Indicators")

EH_df <- generate_template(
  cluster_name = EH_template[1, 1],
  questions = EH_template %>% filter(!is.na(Questions))%>% pull("Questions"),
  indicators = indicators
)

EH_indicators <- compute_indicators(data,"EH_AoR",indicators)

EH_df <- replace_columns(EH_df, EH_indicators) 


# export(EH_df, "output/Pin calculation/EH/Indicators.csv", na = "NA")

HH_AL_Aggregation <- EH_df %>% mutate(
  strata = paste0(Area,"_",`Population group`)
) %>% select(
  uuid,
  KEY = strata,
  Area = Area,
  `Population group`, 
  all_of(indicators),
  `Final HH Score`,
) 

dummy_strata = data.frame(x1 = stri_rand_strings(10, 36, pattern = "[A-Za-z0-9]"),
                          x2 = c(rep("Zdummy_HC",5),rep("Zdummy_IDP",5)),
                          x3 = rep("Zdummy",10),
                          x4 = c(rep("HC",5),rep("IDP",5)),
                          x5 = rep(1:5,2),
                          x6 = rep(1:5,2),
                          # x7 = rep(1:5,2),
                          # x8 = rep(1:5,2),
                          # x9 = rep(1:5,2),
                          stringsAsFactors = F
)

colnames(dummy_strata) = colnames(HH_AL_Aggregation)

HH_AL_Aggregation <- rbind(HH_AL_Aggregation,dummy_strata)
HH_AL_Aggregation$`Final HH Score` = 99
write.csv(HH_AL_Aggregation,"output/Pin calculation/EH/C-HH_AL_Aggregation_eh.csv",row.names = F,na = "1")


############ CCCM ###########
CCCM_template <- import("input/PIN_HNO_Indicators.xlsx",sheet="CCCM")

indicators = CCCM_template %>% filter(!is.na(Indicators)) %>% pull("Indicators")

CCCM_df <- generate_template(
  cluster_name = CCCM_template[1, 1],
  questions = CCCM_template %>% filter(!is.na(Questions))%>% pull("Questions"),
  indicators = indicators
)

cccm_indicators <- compute_indicators(data,"CCCM",indicators)

CCCM_df <- replace_columns(CCCM_df, cccm_indicators) %>% filter(`Population group` == "IDP")


# export(CCCM_df, "output/Pin calculation/cccm/Indicators.csv", na = "NA")

HH_AL_Aggregation <- CCCM_df %>% mutate(
  strata = paste0(Area,"_",`Population group`)
) %>% select(
  uuid,
  KEY = strata,
  Area = Area,
  `Population group`, 
  all_of(indicators),
  `Final HH Score`,
) 

dummy_strata = data.frame(x1 = stri_rand_strings(10, 36, pattern = "[A-Za-z0-9]"),
                          x2 = c(rep("Zdummy_HC",5),rep("Zdummy_IDP",5)),
                          x3 = rep("Zdummy",10),
                          x4 = c(rep("HC",5),rep("IDP",5)),
                          x5 = rep(1:5,2),
                          x6 = rep(1:5,2),
                          x7 = rep(1:5,2),
                          x8 = rep(1:5,2),
                          x9 = rep(1:5,2),
                          stringsAsFactors = F
)

colnames(dummy_strata) = colnames(HH_AL_Aggregation)

HH_AL_Aggregation <- rbind(HH_AL_Aggregation,dummy_strata)
HH_AL_Aggregation$`Final HH Score` = 99
write.csv(HH_AL_Aggregation,"output/Pin calculation/cccm/C-HH_AL_Aggregation_cccm.csv",row.names = F,na = "1")





############ Health ###########
Health_template <- import("input/PIN_HNO_Indicators.xlsx",sheet="Health")

indicators = Health_template %>% filter(!is.na(Indicators)) %>% pull("Indicators")

Health_df <- generate_template(
  cluster_name = Health_template[1, 1],
  questions = Health_template %>% filter(!is.na(Questions))%>% pull("Questions"),
  indicators = indicators
)

Health_indicators <- compute_indicators(data,"Health",indicators)

Health_df <- replace_columns(Health_df, Health_indicators) 


# export(Health_df, "output/Pin calculation/health/Indicators.csv", na = "NA")

HH_AL_Aggregation <- Health_df %>% mutate(
  strata = paste0(Area,"_",`Population group`)
) %>% select(
  uuid,
  KEY = strata,
  Area = Area,
  `Population group`, 
  all_of(indicators),
  `Final HH Score`,
) 

dummy_strata = data.frame(x1 = stri_rand_strings(10, 36, pattern = "[A-Za-z0-9]"),
                          x2 = c(rep("Zdummy_HC",5),rep("Zdummy_IDP",5)),
                          x3 = rep("Zdummy",10),
                          x4 = c(rep("HC",5),rep("IDP",5)),
                          x5 = rep(1:5,2),
                          x6 = rep(1:5,2),
                          # x7 = rep(1:5,2),
                          # x8 = rep(1:5,2),
                          # x9 = rep(1:5,2),
                          stringsAsFactors = F
)

colnames(dummy_strata) = colnames(HH_AL_Aggregation)

HH_AL_Aggregation <- rbind(HH_AL_Aggregation,dummy_strata)
HH_AL_Aggregation$`Final HH Score` = 99
write.csv(HH_AL_Aggregation,"output/Pin calculation/health/C-HH_AL_Aggregation_health.csv",row.names = F,na = "1")

############ Intersectoral ###########

indicators <- import("input/PIN_HNO_Indicators.xlsx",sheet="Inter")%>% pull(Indicators)



Inter_df <- generate_template(
  cluster_name = "Inter",
  questions = "no_improved_water_source",
  indicators = indicators
)

Inter_indicators <- compute_indicators(data,"Inter",indicators)

Inter_df <- replace_columns(Inter_df, Inter_indicators) 

HH_AL_Aggregation <- Inter_df %>% mutate(
  strata = paste0(Area,"_",`Population group`)
) %>% select(
  uuid,
  KEY = strata,
  Area = Area,
  `Population group`, 
  all_of(indicators),
  `Final HH Score`,
) 

HH_AL_Aggregation$`% of HHs by most common barriers to humanitarian aid` = protection_indicators$`% of HHs by most common barriers to humanitarian aid`
HH_AL_Aggregation$`% of HHs reporting concerns from any harm, physical threats or discrimination in the area where they are living` = protection_df$`% of HHs reporting concerns from any harm, physical threats or discrimination in the area where they are living.`
HH_AL_Aggregation$`% of HHs with access to medical, legal and social services for women and girls` = gbv_indicators$`% of HHs with access to medical, legal and social services for women and girls`
HH_AL_Aggregation$`% of households reporting the presence of children engaged in child labor outside of the home in the past 30 days` = CP_indicators[[3]]
HH_AL_Aggregation$`SNFI: % of HHs having adequate living space` = SNFI_indicators$`SNFI: % of HHs having adequate living space`
HH_AL_Aggregation$`SNFI: % of HHs living in sub-standard shelter` = SNFI_indicators$`SNFI: % of HHs living in sub-standard shelter`
HH_AL_Aggregation$`% of HHs by type of primary source of drinking water` = wash_indicators$`% of HHs by type of primary source of drinking water`
HH_AL_Aggregation$`% of HHs using a sanitation facility - by type of sanitation facility used` = wash_indicators$`% of HHs using a sanitation facility - by type of sanitation facility used`
HH_AL_Aggregation$`% of HHs reporting having enough water for drinking, cooking, bathing and washing` = wash_indicators$`% of HHs reporting having enough water for drinking, cooking, bathing and washing`
HH_AL_Aggregation$`% of school-aged children attending school regularly (at least 4 days a week) in the 2020-2021 school year while schools were open, per age and sex group.` = edu_indicators$`% of school-aged children attending school regularly (at least 4 days a week) in the 2020-2021 school year while schools were open, per age and sex group.`
HH_AL_Aggregation$`% of households reporting HLP disputes and insecure land tenure` = SNFI_indicators$`SNFI: % of HHs having security of tenure issues`



dummy_strata = data.frame(x1 = stri_rand_strings(10, 36, pattern = "[A-Za-z0-9]"),
                          x2 = c(rep("Zdummy_HC",5),rep("Zdummy_IDP",5)),
                          x3 = rep("Zdummy",10),
                          x4 = c(rep("HC",5),rep("IDP",5)),
                          x5 = rep(1:5,2),
                          x6 = rep(1:5,2),
                          x7 = rep(1:5,2),
                          x8 = rep(1:5,2),
                          x9 = rep(1:5,2),
                          x10 = rep(1:5,2),
                          x11 = rep(1:5,2),
                          x12 = rep(1:5,2),
                          x13 = rep(1:5,2),
                          x14 = rep(1:5,2),
                          x15 = rep(1:5,2),
                          x16 = rep(1:5,2),
                          x17 = rep(1:5,2),
                          # x18 = rep(1:5,2),
                          stringsAsFactors = F
)

colnames(dummy_strata) = colnames(HH_AL_Aggregation)

HH_AL_Aggregation <- rbind(HH_AL_Aggregation,dummy_strata)


write.csv(HH_AL_Aggregation,"output/Pin calculation/Inter/C-HH_AL_Aggregation_inter.csv",row.names = F,na = "1")

