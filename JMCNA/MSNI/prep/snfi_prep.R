
shlter_loop <-  import("input/Raw_data_loops.xlsx",sheet="consent_controller_shelter_many")

shelter_average_size = data.frame(shelter_type = c("brick", "cgi", "buul", "mud", "timer_", "stone", "unfinished", "stick", "collective", "tent", "none", "not_sure"),
                                  avg_size_shelter = c(15, 16, 6, 20, 20, 15, 16, 9, 30, 16, 0, 0),
                                  stringsAsFactors = F
)



colnames(shlter_loop)[1]="shelter_type"

shlter_loop <- left_join(shlter_loop,shelter_average_size)


shelters_repeat <- shlter_loop %>% filter(shelter_type!="none") %>%  group_by(`_parent_index`) %>% summarise(
  #x1 = paste(shelter_type,collapse = "#"),
  avg_size_shelter = mean(avg_size_shelter,na.rm=T)
) 


data_indicators <- left_join(data_indicators, shelters_repeat %>% select(index=`_parent_index`,avg_size_shelter) )

data_indicators$avg_size_shelter[is.na(data_indicators$avg_size_shelter)] <- 0


shlter_loop <- left_join(shlter_loop,data %>% select(`_parent_index` = index,population_group)) 


shelters_repeat_HC <- shlter_loop %>% filter(population_group=="HC") %>% group_by(`_parent_index`) %>% summarise(
  x1 = paste(shelter_type,collapse = "#"),
  x2 = fn_select_one_mode_shelter_HC (shelter_type)
)

shelters_repeat_IDP <- shlter_loop %>% filter(population_group=="IDP") %>% group_by(`_parent_index`) %>% summarise(
  x1 = paste(shelter_type,collapse = "#"),
  x2 = fn_select_one_mode_shelter_IDP(shelter_type)
) 

shelters_repeat_2 <- rbind(shelters_repeat_HC,shelters_repeat_IDP) %>% select(index =`_parent_index`,snfi_ind2_sev = x2)

data_indicators <- left_join(data_indicators, shelters_repeat_2 )

data_indicators <- data_indicators %>%
  mutate(
    shelt_count.sum_rooms = as.numeric(sum_rooms)) %>%
  mutate(
    surface_per_person = (shelt_count.sum_rooms * avg_size_shelter) / hh_size,
    ###YS     grep for nb_shelter_issues includes shelter_issues.no_issue
    ### AB That's weird!! Looking at the tool shelter_issues doesn't have a no_issue option! But probably tool was updated at some point to remove the option
    nb_shelter_issues = rowSums(.[grep("^(shelter_enclosure_issue|shelter_damage|shelter_issues)\\.(?!(none$|dnk$|prefer_not_answer$|no_issue$))",names(.),perl = T)], na.rm = TRUE),
    HLP_problems = ifelse(rowSums(.[grep("^shelter_problems\\.(?!(none$|not_sure$))",names(.),perl = T)], na.rm = TRUE)>=1,"yes","no"),
    nb_available_items = rowSums(.[grep("^currently_access_nfi\\.",names(.))], na.rm = TRUE)
  ) 


