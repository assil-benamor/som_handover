 data_indicators <- data_indicators %>% rowwise() %>% mutate(
  
  fcs = cereals *2 +
    beans *3 +
    dairy * 4 +
    meats *4 +
    vegtables *1 +
    fruits * 1+
    oils  *0.5+
    sugary_foods * 0.5 ,
  
  
  fcs_category = case_when(
    fcs <= 21 ~ "poor",
    fcs > 21 & fcs <= 35 ~ "borderline",
    fcs > 35 ~ "acceptable"
  ),
  
  rcsi = rely_on_lesspreferred_food + 
    borrow_food*2 + 
    reduce_meals +
    limit_portion_size + 
    restrict_consumption_adults*3,
  
  rcsi_category = case_when(
    rcsi <= 18 ~ "low",
    rcsi > 18 & rcsi <= 42 ~ "medium",
    rcsi > 42 ~ "high"
  ),
  
  no_food_eat_score = case_when(
    no_food_eat == "no" ~ 0,
    how_often_fs_happen %in% c("rately_1_2", "sometimes_3_10") ~ 1 ,
    how_often_fs_happen == "often_10_plus" ~ 2
  ),
  
  go_sleep_night_hungry_score = case_when(
    go_sleep_night_hungry == "no" ~ 0,
    often_go_sleep_night_hungry %in% c("rately_1_2", "sometimes_3_10") ~ 1 ,
    often_go_sleep_night_hungry == "often_10_plus" ~ 2
  ),
  
  whole_day_night_withou_eating_score = case_when(
    whole_day_night_withou_eating == "no" ~ 0,
    often_not_eating_whole_day_night %in% c("rately_1_2", "sometimes_3_10") ~ 1 ,
    often_not_eating_whole_day_night == "often_10_plus" ~ 2
  ),
  
  hhs_score = sum(no_food_eat_score,go_sleep_night_hungry_score,whole_day_night_withou_eating_score)
  
) %>%
   ungroup() 
 
 
 