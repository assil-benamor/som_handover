rm(list = ls())

if (!require("pacman")) install.packages("pacman")

p_load(
  rio,
  tidyverse,
  crayon,
  # hypegrammaR,
  # composr,
  sjmisc,
  # matrixStats
)


############ Data preparation ############ 


source("./src/load_data.R")


#### WASH ####

source("src/MSNI/wash_prep.R")


data_indicators <- data_prep %>% mutate (
  
  #### wash indicator 1 ####
  ## % of HHs by type of primary source of drinking water
  wash_idicator1 = case_when(
    ## Water comes directly from rivers, lakes, ponds, etc.
    main_source_water %in% surface_water ~ 4,
    
    ## Water comes from an unimproved water source
    main_source_water %in% no_improved_water_source ~ 3,

    
    ## dnk
    TRUE ~ 1,
  ),
  
  
  #### wash indicator 2 ####
  ## % of HHs using a sanitation facility - by type of sanitation facility used
  wash_idicator2 = case_when(
    ## No access to latrine - open defecation
    sanitation_facility %in% open_defecation ~ 4,
    
    ## HH: Access to sanitation facilities shared with more than 50 people
      sharing_sanitation_facilities %in% c("yes") &
      (sharing_sanitation_facilities_yes_int * 7) >=20 ~ 3,

    
    TRUE ~ 1
    
  ),
  
  #### wash indicator 3 ####
  ## % of HHs with access to soap
  wash_idicator3 = case_when(
    ## Soap is not available at home and no handwashing facility with soap and water on premise
    have_soap %in% c("no") &
      hand_washing_facility %in% c("no_specific","tippy_tap") ~ 3,

    ## Soap is available at home BUT no handwashing facility on premises with soap and water
    (have_soap %in% c("yes") & hand_washing_facility %in% c("no_specific","tippy_tap")) |
      (have_soap %in% c("no") & hand_washing_facility == "dnk" ) ~ 3,
    
  
    TRUE ~ 1
    
  ),
  
  #### wash non critical indicator 1 ####
  ## % of HHs reporting having enough water for drinking, cooking, bathing and washing
  wash_nc_idicator1 = case_when(
    ## Not enough water for drinking OR domestic uses
    drinking_water == "no" | (domestice_water == "no" & cooking_water == "no" & hygiene_water == "no") ~ 1,
    
    ## EITHER enough water for drinking OR for domestic uses PLUS The HH does not have jerry cans
    (drinking_water == "yes" | (domestice_water == "yes" & cooking_water == "yes" & hygiene_water == "yes") ) & currently_access_nfi.jerrycans_2 == 0 ~ 1,
    
    ## EITHER enough water for drinking OR for domestic uses PLUS The HH has jerry cans
    (drinking_water == "yes" | (domestice_water == "yes" & cooking_water == "yes" & hygiene_water == "yes") ) & currently_access_nfi.jerrycans_2 == 1 ~ 1,
    
    ## Enough water for drinking AND domestic uses (cooking, bathing, and cleaning, not agriculture or livestock) PLUS The HH does not have jerry cans
    (drinking_water == "yes" & domestice_water == "yes" & cooking_water == "yes" & hygiene_water == "yes") & currently_access_nfi.jerrycans_2 == 0 ~ 0,
    
    ## Enough water for drinking AND domestic uses (cooking, bathing, and cleaning, not agriculture or livestock)  PLUS The HH has jerry cans
    (drinking_water == "yes" & domestice_water == "yes" & cooking_water == "yes" & hygiene_water == "yes") & currently_access_nfi.jerrycans_2 == 1 ~ 0,
    
    ##
    TRUE ~ 0
    
  ),
  
  #### wash non critical indicator 2 ####
  ## % of HHs with access to a sanitation facility safe for all members to use
  
  wash_nc_idicator2 = case_when(
    ## HH using latrines with incomplete walls AND no door, locks or functioning lighting
    sanitation_features.walls == 0 &
      sanitation_features.doors == 0 &
      sanitation_features.locks == 0 &
      sanitation_features.inside_light == 0 &
      sanitation_features.outside_light == 0 ~ 1,
    
    ## HH: HH using latrines with walls, door, BUT NO locks and functioning lighting
    sanitation_features.doors == 1 &
      sanitation_features.walls == 1 &
      sanitation_features.locks == 0 &
      sanitation_features.inside_light == 0 &
      sanitation_features.outside_light == 0 ~ 1,
    
    ## HH: HH using latrines with walls, door, locks and functioning lighting
    sanitation_features.doors == 1 &
      sanitation_features.walls == 1 &
      sanitation_features.locks == 1 &
      sanitation_features.inside_light == 1 &
      sanitation_features.outside_light == 1~ 0,
    
    TRUE ~ 0,
    
  ),
  
  #### wash non critical indicator 3 - temporary 1 ####
  ## % Households using negative coping strategies to access water
  wash_tmp_idicator3_1 = case_when(
    
    ## 3
    adopt_lack_water.fetch_dangerous_places == 1 | 
      adopt_lack_water.drink_less  == 1 ~ 3,
    
    ## 2
    adopt_lack_water.rely_unimproved_sources_drinking == 1 |
      adopt_lack_water.rely_surface_water_drinking == 1 |
      adopt_lack_water.send_children_fetch  == 1 |
      adopt_lack_water.reduce_water_consumption == 1  ~ 2,
    
    ## 1
    adopt_lack_water.spend_money_water == 1 |
      adopt_lack_water.fetch_further_source == 1 |
      adopt_lack_water.rely_unimproved_sources_for_other == 1 |
      adopt_lack_water.rely_surface_water_other  ~ 1,
    
    ## 
    TRUE ~ 0
  ),
  
  
  #### wash non critical indicator 3- temporary 2 ####
  ## % Households using negative coping strategies to access sanitation facilities in the past 1 month/30 days
  wash_tmp_idicator3_2 = case_when(
    
    ## 3
    adapt_sanitation_issues.defecate_open == 1 |
      adapt_sanitation_issues.going_dangerous_place == 1 ~ 3 ,
    
    ## 2
    adapt_sanitation_issues.defecate_plastic_bags == 1 |
      adapt_sanitation_issues.going_night == 1 ~ 2 ,
    
    ## 1
    adapt_sanitation_issues.rely_unhygienic_facilities == 1 |
      adapt_sanitation_issues.rely_comunal_facilities == 1 |
      adapt_sanitation_issues.going_further_latrines == 1 ~ 1 ,
    
    ## 
    TRUE ~ 0,
    
  ),
  
  
  #### wash non critical indicator 3 - temporary 3 ####
  ## % Households using negative coping strategies to access hygienic or menstrual materials
  wash_tmp_idicator3_3 = case_when(
    
    ## 3
    adopt_sanitaiton_items.buyying_nfi_dangerous_market_places == 1 |
      adopt_sanitaiton_items.reduce_nfi_consumption_personal == 1 ~ 3 ,
    
    ## 2
    adopt_sanitaiton_items.reduce_nfi_consumption_other == 1 ~ 2,
    
    
    ## 1
    adopt_sanitaiton_items.rely_less_types_nfi == 1 | 
      adopt_sanitaiton_items.rely_soap_substitutes == 1 |
      adopt_sanitaiton_items.buyying_nfi_market == 1 |
      adopt_sanitaiton_items.borrow_nfi_freind_relative == 1 |
      adopt_sanitaiton_items.spend_money_nfi == 1 ~ 1,
    
    ## 
    TRUE ~ 0
    
  )  
)

data_indicators <- data_indicators %>% mutate(
  wash_tmp_idicator3_sum = rowSums(.[grep("wash_tmp_idicator3_", names(.))], na.rm = TRUE),
) %>% select(-c(wash_tmp_idicator3_1:wash_tmp_idicator3_3))

data_indicators <- data_indicators %>% mutate(
  ## % Households using negative coping strategies to access water, sanitation facilities, hygienic or menstrual materials
  wash_nc_idicator3 = case_when(
    wash_tmp_idicator3_sum == 9 ~ 1,
    wash_tmp_idicator3_sum >= 6 ~ 1,
    wash_tmp_idicator3_sum >= 3 ~ 1,
    wash_tmp_idicator3_sum >= 1 ~ 0,
    wash_tmp_idicator3_sum == 0 ~ 0,
  ) ) %>% select(-wash_tmp_idicator3_sum)



###### Health ###### 

data_indicators <- data_indicators %>% mutate(     
  nb_unmet_health_care_needs = rowSums(.[grep("^unmet_health_care_needs\\.(?!(none_barrier))",names(.),perl = T)], na.rm = TRUE),
)

data_indicators <- data_indicators %>% mutate(
  #### Health non critical indicator 1 ####
  ## Percentage of population that can access primary healthcare within one hour’s walk from dwellings
  health_nc_idicator1 = case_when(
    ## Access to primary healthcare exceeding 60 minutes’ walk for HH.
    # health_transportion == "foot" & 
    time_nearest_health > 30 ~ 1,
    
    ## Access to primary healthcare within 60 minutes’ walk for HH.
    TRUE ~ 0,
    
  ),
  
  #### Health non critical indicator 2 ####
  ## % of individuals an unmet health care need 
  health_nc_idicator2 = case_when(
    
    not_obtain_healthcare >= 1 ~ 3,
    
    health_problems_last_3_months == "no" |
      
    not_obtain_healthcare == 0 ~ 1,
    
    TRUE ~ 1 
    
  ),
  
  #### Health non critical indicator 3 ####
  ## % of HHs with barriers to healthcare by type 
  health_nc_idicator3 = case_when(
    
    nb_unmet_health_care_needs >= 1 ~ 1 ,
    TRUE ~ 0
  ),
  
  ) %>% select(-nb_unmet_health_care_needs)
  


###### SNFI ###### 

source("src/MSNI/snfi_prep.R")


data_indicators <- data_indicators %>% mutate(
  #### SNFI non critical indicator 1 ####
  ## SNFI: % of HHs having adequate living space
  SNFI_nc_idicator1 = case_when(

    ## 2m2 ≤ surface per person < 2.5m2
    how_many_shelters == 0 | surface_per_person < 2 ~ 1,
    
    ## 2.5m2 ≤ surface per person < 3.5m2
    surface_per_person < 2.5 ~ 1,
    
    ## 3.5m2 ≤ surface per person < 4.5m2
    surface_per_person < 4.5 ~ 0,
    
    ## surface per person ≥4.5m2
    surface_per_person >= 4.5 ~ 0,
    
  ), 
  
  
  
  #### SNFI critical indicator 1 ####
  ## SNFI: % of HHs living in sub-standard shelter
  SNFI_idicator1 = case_when(
    ## None (sleeping in open)OR (Shelter Type =”” AND No. of shelter = 0)
    how_many_shelters == 0  | snfi_ind2_sev == "5" ~ 5,
    
    ## Buul in an IDP SiteORMakeshift shelter
    snfi_ind2_sev == "4" ~ 4,
    
    ## Unfinished / non-enclosed buildingORTent
    snfi_ind2_sev == "3" ~ 3,
    
    ## Timber and plastic sheet with CGI roofOR CGI sheet wall and CGI roofORMud and stick wall and CGI roofORCollective shelter(school, government buildin)ORIf Buul outside an IDP Site / Traditional somali house
    snfi_ind2_sev == "2" ~ 1,
    
    ## Brick and concrete house (solid, finished house or apartment)ORStone/brick wall and CGI roof
    snfi_ind2_sev == "1" ~ 1,
    
  ),
  
  
  
  
  #### SNFI critical indicator 2 ####
  ## SNFI: % of HHs living in inadequate shelter conditions.
  SNFI_idicator2 = case_when(

    ## >8 No
    nb_shelter_issues > 8 ~ 4,
    
    ## >5 No. of issues ≤ 8
    nb_shelter_issues > 5 ~ 3,

    ## <=2 Issues
    nb_shelter_issues <=5 ~ 1,
    
  ),
  
  
  
  #### SNFI non critical indicator 2 ####
  ## SNFI: % of HHs having security of tenure issues
  SNFI_nc_idicator2 = case_when(
    
    
    ## Occupancy arrangement ="ownership" AND No HLP Problem AND Has written documentation
    shelter_occupancy == "ownership" & HLP_problems == "no" & shelter_format_doc == "yes" ~ 0,
    
    ## Occupancy arrangement ="ownership" OR No HLP Problem OR Has written documentation
    shelter_occupancy == "ownership" | HLP_problems == "no" | shelter_format_doc == "yes"  ~ 0,
    
    ## Occupancy arrangement  NOT "ownership"AND Has HLP Problem AND Does Not Have written documentation
    shelter_occupancy != "ownership" & HLP_problems == "yes" & shelter_format_doc != "yes"  ~ 1,
    
    
  ),
  
  
  
  #### SNFI non critical indicator 3 ####
  ## SNFI: % Households owning sufficient basic NFIs
  SNFI_nc_idicator3 = case_when(
    ## Less than 3 items (0, 1 or 2)
    nb_available_items <3 ~ 1,
    
    ## 4 or 3 items present at HH level
    nb_available_items <5 ~ 1,
    
    ## 5 items present at HH level
    nb_available_items == 5  ~ 0,
    
    ## 6 items present at HH level
    nb_available_items == 6 ~ 0,
    
    ## All items available at HH level
    nb_available_items == 7 ~ 0,
    
  ),
  
  
)

data_indicators <- data_indicators %>% select(-c(avg_size_shelter:nb_available_items))


###### Education ###### 

source("src/MSNI/edu_prep.R")

data_indicators <- data_indicators %>%
  mutate(
    #### education non-critical indicator 1 ####
    ### % of children dropping out of school in the previous year
    edu_nc_idicator1 = case_when(
      
      school_age_children_cal == 0  ~ 0,
      
      ## All school-aged children in the HH dropped out
      total_drop_out > 0 & total_remaining <= 0 ~ 1,
      
      ##Some school-aged children in the HH dropped out
      total_drop_out > 0 & total_remaining > 0 ~ 1,
      
      ##No school-aged children in the HH dropped out 
      total_drop_out == 0 ~ 0,
      
      
    ),
    
    #### education indicator 1 ####
    ### % of school-aged children enrolled in school for the 2020-2021 school year
    edu_idicator1 = case_when(
      
      school_age_children_cal == 0 ~ as.numeric(NA),
    
      (school_age_children_cal > 0 & (total_enrolled / school_age_children_cal < 0.5)) ~ 3,
      
      TRUE ~ 1,
      
      
    ),
    
    edu_idicator2 = case_when(
      
      education_barriers.schools_closed_covid19 == 1 |
        education_barriers.schools_closed_other_reasons == 1 |
        education_barriers.schools_overcrowded == 1 |
        education_barriers.security_concerns == 1 |
        education_barriers.far_school_distance == 1 |
        education_barriers.financail_issues == 1 |
        education_barriers.child_helping_home_farm == 1 |
        education_barriers.child_working_outside == 1 |
        education_barriers.children_psychologically_distressed == 1 |
        education_barriers.lack_child_documentation == 1 |
        education_barriers.flooding_weather_events == 1 |
        education_barriers.children_join_armed_groups == 1 |
        education_barriers.marriage_pregnancy == 1 |
        education_barriers.poor_school_infrastructure == 1 |
        education_barriers.lack_qualified_teachers == 1 |
        education_barriers.insufficient_wash_facilities == 1 |
        education_barriers.lack_male_female_seperation == 1 ~ 3,
      
      education_barriers.parents_unaware_educ_available == 1 |
        education_barriers.parents_dont_value_education == 1 |
        education_barriers.parents_dont_approve_curriculum == 1 |
        education_barriers.displacement == 1 |
        education_barriers.language_issue == 1 ~ 2 ,
      
      TRUE ~ 1
      
        
        
    ),
    
    
    #### education non-critical indicator 2 ####
    ### % of school-aged children attending school regularly (at least 4 days a week) in the 2020-2021 school year while schools were open, per age and sex group.
    edu_nc_idicator2 = case_when(
      
      school_age_children_cal == 0 ~ 0,
      
      ##No children attend regularly
      school_age_children_cal != 0 & total_attend == 0 ~ 1,
      
      ##Some children attending regularly
      total_attend >0 & (school_age_children_cal < total_attend)  ~ 1,
      
      
      ##All children attending regularly
      (school_age_children_cal == total_attend) ~ 0
    ),
    

  )

data_indicators <- data_indicators %>% select(-c(hh_adults_int:education_barriers.not_sure))

###### Protection ###### 

source("src/MSNI/protection_prep.R")

data_indicators <- data_indicators %>% mutate(
  #### protection critical indicator 1 ####
  ## % of HHs reporting concerns from any harm, physical threats or discrimination in the area where they are living.
  protection_idicator1 = case_when(
    ## being killed, Mine/UXOs, Being injured/killed by an explosive hazard
    killed_hh >= 1 | mine_uxo_hh  >= 1 | injured_hh >= 1 ~ 4,
    
    ## Discrimination or persecution , Being detained,Female Genital Mutilation (FGM)
    discriminaition_hh >= 1 | detained_hh  >= 1 |  fgm_hh >= 1 ~ 4,
    
    ## Suffering from physical harassment or violence (not sexual), 
    ## Suffering from verbal harassment Suffering from sexual harassment or violence
    ## Being recruited by armed groupsBeing forcibly marriedBeing exploited Being robbed
    
    pysical_harresment_hh >= 1 | verbal_haresment_hh >= 1 |  sexual_harassment_hh >= 1 |
      recruited_armed_hh >= 1 | forcibly_married_hh  >= 1 |  exploited_hh >= 1 ~ 1,
    
    ## being threatened with violence, Being sent abroad to find work
    sent_abroad_hh >= 1 ~ 0,
    
    ## DNK Prefer not answer (None of the above options is selected)
    TRUE  ~ 0,
    
  ),
  
  
  #### protection non-critical indicator 1 ####
  ## % of HHs without access to offical law enforcement authorities and/or judiciary system
  protection_nc_idicator1 = case_when(
    ## 1. Yes – [no formal access to justice or compensation in my location]
    access_justice_denied == "formal_access" ~ 1,
    
    ## 2. Yes and No – [no formal access to justice or compensation in my location, but traditional/informal justice mechanisms available to resolve issues].3. No and Yes – [no access to traditional or informal justice mechanisms but access to formal justice or compensation mechanisms in my location]
    access_justice_denied %in% c("no_formal_access", "no_access") ~ 1,
    
    
    ## 4. No – [no issue linked to access to any justice mechanism arose]5. No – [full access to formal justice mechanisms and fair compensation]
    access_justice_denied %in% c("no_issue", "full_access") ~ 0,
    
  ),
  
  
  
  #### protection non-critical indicator 2 ####
  ## % of HHs that have experienced movement restrictions in the last 3 months
  protection_nc_idicator2 = case_when(
    ## Yes
    safety_restrictions == "yes" ~ 1,
    
    ## No
    TRUE ~ 0,
    
  ),
  
  
  
  #### protection non-critical indicator 3 ####
  ## % of HHs by most common barriers to humanitarian aid
  protection_nc_idicator3 = case_when(
    ## Insecurity On Route To Points Of Aid DistributionInsecurity At Site Of Aid DistributionExclusion By Camp Managers/Gatekeepers
    aid_barriers.insecurity_route == 1 |
      aid_barriers.insecurity_site == 1 |
      aid_barriers.exclusion == 1 ~ 1,
    
    ## Lack Of InformationPhysically Unable To Access Points Of Aid Distribution
    aid_barriers.lack_information == 1 |
      aid_barriers.physically_unable_access == 1 ~ 1,
    
    ## No problems faced
    # aid_barriers.no_problem == 1 ~ 1,
    TRUE ~ 0,
  ),
  
  
  #### protection non-critical indicator 4 ####
  ## % of HHs  with women and girls reporting lack of freedom to attend go about their duties/businessess
  protection_nc_idicator4 = case_when(
    ## No
    women_move_freely %in% c("no","do_not_know") ~ 1,
    
    ## Yes
    women_move_freely == "yes" ~ 0,
    
  ),
  
  
  #### protection non-critical indicator 5 ####
  ## % of households reporting HLP disputes and insecure land tenure
  protection_nc_idicator5 = case_when(
    
    risk_eviction == "yes"  & land_dispute == "yes" &
      land_dispute_yes.multiple_claims == 1 ~ 1, 
    
    risk_eviction == "yes"  & land_disputes_count >= 1 ~ 1,
    
    risk_eviction == "yes"  & land_disputes_count == 0 ~ 1,
    
    TRUE ~ 0
  ),
  
) 

data_indicators <- data_indicators %>% select(-c(hh_members_with_diff:land_disputes_count))


###### Food Security ###### 

source("src/MSNI/fs_prep.R")

data_indicators <- data_indicators %>% mutate(
  #### Food security critical indicator 1 ####
  ## % of HHs in need according to the FCS ##
  fs_idicator1 = case_when(
    
    fcs_category == "poor" ~ 4,
    fcs_category == "borderline" ~ 3,
    fcs_category == "acceptable" ~ 1

  ),
  
  #### Food security critical indicator 2 ####
  ## % of HHs in need according to the HHS ##
  fs_idicator2 = case_when(
    
    hhs_score >= 4 ~ 4,
    hhs_score >= 2 ~ 3,
    hhs_score >= 1 ~ 2,
    TRUE ~ 1
  ),
  
  #### Food security critical indicator 3 ####
  ## % of HHs in need according to the HHS ##
  fs_idicator3 = case_when(
    main_sources_food.relaint_family_freinds == 1 | 
    main_sources_food.reliant_humanitarian_assistance == 1 | 
    main_sources_food.reliant_government_assistance == 1 ~ 3 ,
    
    main_sources_food.fishing == 1 | 
      main_sources_food.hunting == 1 | 
      main_sources_food.bartering == 1 ~ 2,
    
    TRUE ~ 1 
  ),
  
  #### Food security non-critical indicator 1 ####
  ## % of HHs in need according to the rCSI ##
  fs_nc_idicator1 = case_when(
    rcsi_category %in% c("high","medium") ~ 1,
    TRUE ~ 0
  ),
  
  #### Food security non-critical indicator 2 ####
  ## % of households that do not have access to a market within 1-hour walking distance ##
  fs_nc_idicator2 = case_when(
    how_long_reach_nearest_market %in% c("1_2_hours","more_than_2_hours") ~ 1,
    TRUE ~ 0
  ),
  
  
)

data_indicators <- data_indicators %>% select(-c(fcs:hhs_score))

#### Scores calculation #####

snfi_critical_indicators <- c("SNFI_idicator1","SNFI_idicator2")
snfi_non_critical_indicators <- c("SNFI_nc_idicator1", "SNFI_nc_idicator2","SNFI_nc_idicator3")


wash_critical_indicators <- c("wash_idicator1","wash_idicator2","wash_idicator3")
wash_non_critical_indicators <- c("wash_nc_idicator1", "wash_nc_idicator2", "wash_nc_idicator3")

health_critical_indicators <- c( "health_nc_idicator2")
health_non_critical_indicators <- c("health_nc_idicator1","health_nc_idicator3")


education_critical_indicators <- c("edu_idicator1","edu_idicator2")
education_non_critical_indicators <- c("edu_nc_idicator1",
                                       "edu_nc_idicator2")

protection_critical_indicators <- c("protection_idicator1")
protection_non_critical_indicators <- c("protection_nc_idicator1", "protection_nc_idicator2", "protection_nc_idicator3", "protection_nc_idicator4",
                                        "protection_nc_idicator5")

fs_critical_indicators <- c("fs_idicator1","fs_idicator2","fs_idicator3")
fs_critical_non_critical_indicators <- c("fs_nc_idicator1","fs_nc_idicator2")


score_mean <- function(x) {
  case_when(x >2/3 ~ 3,
            x > 1/3 ~ 2,
            x <= 1/3 ~ 1)
}




data_indicators <-  data_indicators %>% 
  mutate(
    mean_nc_snfi = score_mean(rowMeans(select(.,!!!syms(snfi_non_critical_indicators)), na.rm = T)),
    mean_nc_wash = score_mean(rowMeans(select(.,!!!syms(wash_non_critical_indicators)), na.rm = T)),
    mean_nc_hlt = score_mean(rowMeans(select(.,!!!syms(health_non_critical_indicators)), na.rm = T)),
    mean_nc_edu = score_mean(rowMeans(select(.,!!!syms(education_non_critical_indicators)), na.rm = T)),
    mean_nc_protection = score_mean(rowMeans(select(.,!!!syms(protection_non_critical_indicators)), na.rm = T)),
    mean_nc_fs = score_mean(rowMeans(select(.,!!!syms(fs_critical_non_critical_indicators)), na.rm = T))
    
  ) 



data_indicators <-  data_indicators %>% 
  
  mutate(
    
    snfi_score = pmax(!!!syms(snfi_critical_indicators),mean_nc_snfi,na.rm = T),
    
    wash_score = pmax(!!!syms(wash_critical_indicators),mean_nc_wash,na.rm = T),
    
    protection_score = pmax(!!!syms(protection_critical_indicators),mean_nc_protection,na.rm = T),
    
    health_score = pmax(!!!syms(health_critical_indicators),mean_nc_hlt,na.rm = T),
    
    # health_score = mean_nc_hlt,
    
    education_score = pmax(!!!syms(education_critical_indicators),mean_nc_edu,na.rm = T),
    
    fs_score = pmax(!!!syms(fs_critical_indicators),mean_nc_fs,na.rm = T),
    
    
  ) 



data_indicators <- data_indicators %>% 
  
  mutate(
    
    protection_need = case_when(
      protection_score <= 2 ~ 0,
      protection_score >= 3 ~ 1),
    
    education_need = case_when(
      education_score <= 2 ~ 0,
      education_score >= 3 ~ 1),
    
    snfi_need = case_when(
      snfi_score <= 2 ~ 0,
      snfi_score >= 3 ~ 1),
    
    wash_need = case_when(
      wash_score <= 2 ~ 0,
      wash_score >= 3 ~ 1),
    
    health_need = case_when(
      health_score <= 2 ~ 0,
      health_score >= 3 ~ 1),
    
    fs_need = case_when(
      fs_score <= 2 ~ 0,
      fs_score >= 3 ~ 1),
    
  )

data_indicators <- data_indicators %>% 
  mutate(
    number_of_needs = rowSums(select(., protection_need, wash_need, health_need, education_need, 
                                     snfi_need,fs_need), na.rm=T),

  )



data_indicators$snfi_score %>% table()


data_indicators <- data_indicators %>% 
  mutate(
    MSNI = pmax(!!!syms(c("protection_score", "education_score", "snfi_score", "wash_score", "health_score", "fs_score")),na.rm = T),
  )


output <- data_indicators %>% select(
  "uuid",
  matches("idicator[[:digit:]]"),
  ends_with("_score"),
  ends_with("_need"),
  number_of_needs,
  MSNI) 
 

write.csv(output,'output/data_plus_scores_2710.csv',row.names = F)



