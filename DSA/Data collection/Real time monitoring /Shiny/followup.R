library(httr)
library(jsonlite)
library(readr) 
library(rio)
library(devtools)
library(tidyverse)


conn <- dbConnect(MySQL(), user="ahmed", db="dsa", host="165.232.64.110", password="=mX4mE,G$#N{Njs4")

conn <- dbConnect(MySQL(), user="user", db="dsa", host="165.232.64.110", password="--lilos@404@Sql__")

df <- dbReadTable(conn, "idp") %>% filter(times>0)

df <- df %>% filter(times>0)

oversampling <- df %>% filter(times>4)


master <- read.csv("../../Clustering/clustered_data_all_v3.csv")
colnames(master)[1] <- "pcode"

data <- left_join(df,master)


data %>% group_by(Region,District) %>% summarise(
  surveys_received = sum(times)
) %>% View()

data %>% filter(times>4) %>% select(Region,District,pcode,surveys_received = times) %>% View()


write.csv(data %>% group_by(Region,District) %>% summarise(
  surveys_received = sum(times)
) ,"summary_25_11.csv",row.names = F)




write.csv(data %>% filter(times>4) %>% select(Region,District,pcode,surveys_received = times),"oversampling_25_11.csv",row.names = F)

sum(data$times)



kobo_server_url<-"https://kobo.humanitarianresponse.info/"
kc_server_url<-"https://kc.humanitarianresponse.info/"
kobo_user = "reachsomalia_do"
kobo_pw = "hypegrammar_magic"
form_id = "929319"
dataurl<- paste0("https://kc.humanitarianresponse.info/api/v1/data/",form_id,".json")



download_dsa_data <-function(dataurl,kobo_user,kobo_pw){
  rawdata<- GET(dataurl,authenticate(kobo_user,kobo_pw),progress())
  rawJsonOBJ <- content(rawdata,"text",encoding = "UTF-8")
  rawJsonOBJ <- gsub("(begin_group[^/]*|b_localisation_idp_site)",
                     "begin_group",
                     rawJsonOBJ,perl = T)
  
  d <- as.list(jsonlite::fromJSON(rawJsonOBJ))
  
  d$`_id` <- NULL
  d$`uuid` <- NULL
  d$`_attachments` <- NULL
  d$`_geolocation` <- NULL
  d$`_tags` <- NULL
  d$`_notes` <- NULL
  d$`_validation_status` <- NULL
  d$`formhub/uuid` <- NULL
  
  df<-data.frame(d)
  
  names(df) <- gsub('^(?:[^/]*/)*(.*)', '\\1',names(d))
  
  df
  
}

data_server <-  download_dsa_data (dataurl,kobo_user,kobo_pw)


sum(df$times)
