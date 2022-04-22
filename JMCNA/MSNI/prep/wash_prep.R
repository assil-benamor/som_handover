
improved_water_source <- c("piped_system", "protected_well_pump", "protected_well_hand", "borehole","water_tank", "water_trucking")
no_improved_water_source <- c("water_kiosk", "vendor_shops", "unprotected_well")
surface_water <- c("river_pond")


improved_sanitation_facilities <-  c("flush_toilet", "pit_latrine_slap", "pit_vip")
no_improved_sanitation_facilities <-  c("pit_latrine_without_slap", "open_hole","bucket_toilet", "plastic_bag", "hanging_latrine")
open_defecation <- c("none_of_the_above")



data_prep <- data %>% 
  mutate(
    sharing_sanitation_facilities_yes_int = as.integer(sharing_sanitation_facilities_yes)
  )