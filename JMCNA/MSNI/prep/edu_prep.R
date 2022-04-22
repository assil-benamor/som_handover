data_indicators <- data_indicators %>%
  rowwise() %>%
  mutate(
    
    hh_adults_int = as.numeric(hh_adults),
    
    school_age_children_cal = sum(males_6_11y,
                                  females_6_11y,
                                  males_12_17y,
                                  females_12_17y,
                                  na.rm = T),
    
    total_enrolled = sum(
      # enrolled_girls_3_5,
      # enrolled_boys_3_5,
      enrolled_girls_6_11,
      enrolled_boys_6_11,
      enrolled_girls_12_17,
      enrolled_boys_12_17,
      na.rm = T
    ),
    total_attend = sum(
      # attend_school_girls_3_5,
      # attend_school_boys_3_5,
      attend_school_girls_6_11,
      attend_school_boys_6_11,
      attend_school_girls_12_17,
      attend_school_boys_12_17,
      na.rm = T
    ),
    total_drop_out = sum(
      # dropped_out_girls_3_5,
      # dropped_out_boys_3_5,
      dropped_out_girls_6_11,
      dropped_out_boys_6_11,
      dropped_out_girls_12_17,
      dropped_out_boys_12_17,
      na.rm = T
    ),
    
    total_remaining = total_enrolled - total_drop_out,
    
    total_edu_level_achieved = sum(tertiary_degree,
                                   vocational_degree,
                                   secondary_school,
                                   middle_school,
                                   primary_school,
                                   quranic_school,
                                   na.rm = T
    )
  ) %>%
  ungroup()


data_indicators<- data_indicators %>% mutate(
    education_barriers.no_barrier = rowSums(.[grep("education_barrie[r]{0,1}s_(boys|girls)\\.no_barrier$",names(.),perl = T)], na.rm = TRUE),
    education_barriers.schools_closed_covid19 = rowSums(.[grep("education_barrie[r]{0,1}s_(boys|girls)\\.schools_closed_covid19$",names(.),perl = T)], na.rm = TRUE),
    education_barriers.schools_closed_other_reasons = rowSums(.[grep("education_barrie[r]{0,1}s_(boys|girls)\\.schools_closed_other_reasons$",names(.),perl = T)], na.rm = TRUE),
    education_barriers.schools_overcrowded = rowSums(.[grep("education_barrie[r]{0,1}s_(boys|girls)\\.schools_overcrowded$",names(.),perl = T)], na.rm = TRUE),
    education_barriers.security_concerns = rowSums(.[grep("education_barrie[r]{0,1}s_(boys|girls)\\.security_concerns$",names(.),perl = T)], na.rm = TRUE),
    education_barriers.far_school_distance = rowSums(.[grep("education_barrie[r]{0,1}s_(boys|girls)\\.far_school_distance$",names(.),perl = T)], na.rm = TRUE),
    education_barriers.financail_issues = rowSums(.[grep("education_barrie[r]{0,1}s_(boys|girls)\\.financail_issues$",names(.),perl = T)], na.rm = TRUE),
    education_barriers.child_helping_home_farm = rowSums(.[grep("education_barrie[r]{0,1}s_(boys|girls)\\.child_helping_home_farm$",names(.),perl = T)], na.rm = TRUE),
    education_barriers.child_working_outside = rowSums(.[grep("education_barrie[r]{0,1}s_(boys|girls)\\.child_working_outside$",names(.),perl = T)], na.rm = TRUE),
    education_barriers.parents_unaware_educ_available = rowSums(.[grep("education_barrie[r]{0,1}s_(boys|girls)\\.parents_unaware_educ_available$",names(.),perl = T)], na.rm = TRUE),
    education_barriers.parents_dont_value_education = rowSums(.[grep("education_barrie[r]{0,1}s_(boys|girls)\\.parents_dont_value_education$",names(.),perl = T)], na.rm = TRUE),
    education_barriers.parents_dont_approve_curriculum = rowSums(.[grep("education_barrie[r]{0,1}s_(boys|girls)\\.parents_dont_approve_curriculum$",names(.),perl = T)], na.rm = TRUE),
    education_barriers.children_psychologically_distressed = rowSums(.[grep("education_barrie[r]{0,1}s_(boys|girls)\\.children_psychologically_distressed$",names(.),perl = T)], na.rm = TRUE),
    education_barriers.displacement = rowSums(.[grep("education_barrie[r]{0,1}s_(boys|girls)\\.displacement$",names(.),perl = T)], na.rm = TRUE),
    education_barriers.lack_child_documentation = rowSums(.[grep("education_barrie[r]{0,1}s_(boys|girls)\\.lack_child_documentation$",names(.),perl = T)], na.rm = TRUE),
    education_barriers.flooding_weather_events = rowSums(.[grep("education_barrie[r]{0,1}s_(boys|girls)\\.flooding_weather_events$",names(.),perl = T)], na.rm = TRUE),
    education_barriers.children_join_armed_groups = rowSums(.[grep("education_barrie[r]{0,1}s_(boys|girls)\\.children_join_armed_groups$",names(.),perl = T)], na.rm = TRUE),
    education_barriers.marriage_pregnancy = rowSums(.[grep("education_barrie[r]{0,1}s_(boys|girls)\\.marriage_pregnancy$",names(.),perl = T)], na.rm = TRUE),
    education_barriers.language_issue = rowSums(.[grep("education_barrie[r]{0,1}s_(boys|girls)\\.language_issue$",names(.),perl = T)], na.rm = TRUE),
    education_barriers.poor_school_infrastructure = rowSums(.[grep("education_barrie[r]{0,1}s_(boys|girls)\\.poor_school_infrastructure$",names(.),perl = T)], na.rm = TRUE),
    education_barriers.lack_qualified_teachers = rowSums(.[grep("education_barrie[r]{0,1}s_(boys|girls)\\.lack_qualified_teachers$",names(.),perl = T)], na.rm = TRUE),
    education_barriers.insufficient_wash_facilities = rowSums(.[grep("education_barrie[r]{0,1}s_(boys|girls)\\.insufficient_wash_facilities$",names(.),perl = T)], na.rm = TRUE),
    education_barriers.lack_male_female_seperation = rowSums(.[grep("education_barrie[r]{0,1}s_(boys|girls)\\.lack_male_female_seperation$",names(.),perl = T)], na.rm = TRUE),
    education_barriers.not_sure = rowSums(.[grep("education_barrie[r]{0,1}s_(boys|girls)\\.not_sure$",names(.),perl = T)], na.rm = TRUE)
) %>% mutate_at(grep("education_barriers\\..*",names(.)), ~ as.integer(.x >= 1)) 

  
  

