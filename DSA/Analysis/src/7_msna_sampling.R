rm(list = ls())

if (!require("pacman")) install.packages("pacman")

p_load(rio,
       tidyverse,
       koboquest,
       crayon,
       hypegrammaR,
       xlsx,
       composr,
       ggpubr)

# laod aggregated data
data <- read.csv("output/Aggregation/DSA_aggregated_data.csv", stringsAsFactors = F)

data <- data %>% mutate(
  site_category_1 = case_when (
    duration_site_established_in_months <= 12 ~ "1_year_or_less",
    duration_site_established_in_months > 12 ~ "more_than_1_year",
    TRUE ~ as.character(NA)
  ),
  
  site_category_2 = case_when (
    cccm_idps_arrival %in% c("lessthanonemonth", "onetothreemonths", "fourtosixmonths") ~ "1_year_or_less",
    cccm_idps_arrival %in% c("morethansixmonths") ~ "more_than_1_year",
    TRUE ~ as.character(NA)
  )
  
) %>% filter(!is.na(site_category_1),!is.na(site_category_2))



###### Testing  ######
p1 <- ggplot(data, aes(x=as.factor(site_category_1), fill=as.factor(site_category_1) )) + 
  geom_bar( ) +
  scale_fill_hue(c = 40) +
  theme(legend.position="none") +
  labs(title="Using 'How long has this site been established for (In months)'",
                                      x ="Categories", y = "Number of sites")

p2 <- ggplot(data, aes(x=as.factor(site_category_2), fill=as.factor(site_category_2) )) + 
  geom_bar( ) +
  scale_fill_hue(c = 40) +
  theme(legend.position="none") +
  labs(title="Using 'When did the majority of individuals arrive to this site? '",
       x ="Categories", y = "Number of sites")



ggarrange(p1, p2, labels = c("A", "B"),
          common.legend = TRUE, legend = "none")

ggplot()

with(data, table(site_category_1,site_category_2)) %>% View()

 data$cccm_idps_arrival %>% table()
 
table(data$site_category_2)
table(data$site_category_1)

data$cccm_populationestimates_families

data %>% group_by(site_category_2) %>% summarise(
  nb_ind = sum(cccm_populationestimates_individuals,na.rm = T),
  nb_familites = sum(cccm_populationestimates_families,na.rm = T)
  
) %>% View()



##### Exporting ######

grouped_data <- data %>% split(.$site_category_2)

filenames<- unique(data$site_category_2)
filenames <- filenames[order(filenames)]

purrr::map2(grouped_data,filenames,function(dataset, filename){
  
  write.csv(dataset,paste0("./output/MSNA sampling/",filename,".csv"),row.names = F)
 
})


mapping <- readRDS("districts_pcode_mapping.RDS")

data <- data %>% left_join(.,mapping)

data_1_year_or_less = data %>% filter(site_category_2 == "1_year_or_less")
data_more_than_1_year = data %>% filter(site_category_2 == "more_than_1_year")
  
list_1_year_or_less <- data_1_year_or_less %>% select(localisation_region,localisation_region_label,localisation_district_label,localisation_district = district,idp_code,gps_latitude,gps_longitude,settlement_name,cccm_populationestimates_individuals,
                cccm_populationestimates_families) %>% split(.$localisation_district_label)



filenames<- unique(data_1_year_or_less$localisation_district_label)
filenames <- filenames[order(filenames)]

purrr::map2(list_1_year_or_less,filenames,function(dataset, filename){
  
  write.csv(dataset,paste0("./output/MSNA sampling/districts/1_year_or_less/",filename,".csv"),row.names = F)
  
})
  

list_more_than_1_year <- data_more_than_1_year %>% select(localisation_region,localisation_region_label,localisation_district_label,localisation_district = district,idp_code,gps_latitude,gps_longitude,settlement_name,cccm_populationestimates_individuals,
                                                          cccm_populationestimates_families) %>% split(.$localisation_district_label)

filenames<- unique(data_more_than_1_year$localisation_district_label)
filenames <- filenames[order(filenames)]

purrr::map2(list_more_than_1_year,filenames,function(dataset, filename){
  
  write.csv(dataset,paste0("./output/MSNA sampling/districts/more_than_1_year/",filename,".csv"),row.names = F)
  
})

#### sampling #####

data <- data %>% mutate(
  site_category_3 = case_when (
    site_category_1 == "1_year_or_less" | site_category_1 == "more_than_1_year" & site_category_2 == "1_year_or_less" ~ "vulnerable",
    site_category_1 == "more_than_1_year" & site_category_2 == "more_than_1_year" ~ "non_vulnerable",
    TRUE ~ as.character(NA)
  )
  
) %>% filter(!is.na(site_category_3))



sampling <- data %>% mutate(strata = paste0(localisation_district_label,"_",site_category_3)) %>% select(strata,localisation_region,localisation_region_label,localisation_district_label,localisation_district = district,idp_code,gps_latitude,gps_longitude,settlement_name,cccm_populationestimates_individuals,
                          cccm_populationestimates_families,site_category_3)



write.csv(sampling,'output/MSNA sampling/sampling_data_2004.csv',row.names = F)

