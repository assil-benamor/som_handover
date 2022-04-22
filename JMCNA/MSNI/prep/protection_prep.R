loops_data_washington <- import("input/Raw_data_loops.xlsx",sheet="consent_controller_washington_g") 

col_names <- c("consent_controller.washington_group_question.persons_having_difficulty_loop.difficulty_seeing",
               "consent_controller.washington_group_question.persons_having_difficulty_loop.difficulty_hearing",
               "consent_controller.washington_group_question.persons_having_difficulty_loop.difficulty_walking",
               "consent_controller.washington_group_question.persons_having_difficulty_loop.difficulty_remembering", 
               "consent_controller.washington_group_question.persons_having_difficulty_loop.difficulty_selftcare", 
               "consent_controller.washington_group_question.persons_having_difficulty_loop.difficult_communication")

data_indicators <- left_join(data_indicators, loops_data_washington %>% mutate_at(col_names,
                                                                                  .funs = ~ case_when(
                                                                                    .x %in% c("a_lot_difficulty", "cannot_do_at_all") ~ 1,
                                                                                    TRUE ~ 0
                                                                                  )
) %>% mutate(
  member_has_diff = ifelse(rowSums(.[col_names], na.rm = TRUE) >= 1, 1, 0)
) %>% group_by(`_parent_index`) %>% summarise(
  hh_members_with_diff = sum(member_has_diff),
) %>% select(index = `_parent_index`, hh_members_with_diff)) %>% mutate(
  pct_with_diff = hh_members_with_diff / hh_size
)


data_indicators <- data_indicators %>% 
  mutate( 
    sent_abroad_hh = rowSums(.[grep("main_safety_concerns_.*\\.sent_abroad", names(.))], na.rm = TRUE),
    pysical_harresment_hh = rowSums(.[grep("main_safety_concerns_.*\\.pysical_harresment", names(.))], na.rm = TRUE),
    verbal_haresment_hh = rowSums(.[grep("main_safety_concerns_.*\\.verbal_haresment", names(.))], na.rm = TRUE),
    sexual_harassment_hh = rowSums(.[grep("main_safety_concerns_.*\\.sexual_harassment", names(.))], na.rm = TRUE),
    recruited_armed_hh = rowSums(.[grep("main_safety_concerns_.*\\.recruited_armed", names(.))], na.rm = TRUE),
    forcibly_married_hh = rowSums(.[grep("main_safety_concerns_.*\\.forcibly_married", names(.))], na.rm = TRUE),
    exploited_hh = rowSums(.[grep("main_safety_concerns_.*\\.exploited", names(.))], na.rm = TRUE),
    discriminaition_hh = rowSums(.[grep("main_safety_concerns_.*\\.discriminaition", names(.))], na.rm = TRUE),
    detained_hh = rowSums(.[grep("main_safety_concerns_.*\\.detained", names(.))], na.rm = TRUE),
    fgm_hh = rowSums(.[grep("main_safety_concerns_.*\\.fgm", names(.))], na.rm = TRUE),
    killed_hh = rowSums(.[grep("main_safety_concerns_.*\\.killed", names(.))], na.rm = TRUE),
    mine_uxo_hh = rowSums(.[grep("main_safety_concerns_.*\\.mine_uxo", names(.))], na.rm = TRUE),
    injured_hh = rowSums(.[grep("main_safety_concerns_.*\\.injured", names(.))], na.rm = TRUE),
    dnk_prefer_not_answer_hh = rowSums(.[grep("main_safety_concerns_.*\\.(dnk$|prefer_not_answer$)", names(.))], na.rm = TRUE),
    members_with_diff_hh = rowSums(.[grep("_memb$", names(.))], na.rm = TRUE),
    land_disputes_count = rowSums(.[grep("land_dispute_yes\\.", names(.))], na.rm = TRUE)
  )  
