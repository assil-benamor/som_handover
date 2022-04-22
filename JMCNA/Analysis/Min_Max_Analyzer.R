
library(openxlsx)
path <- "input/Min Max Overview.xlsx"
sheetNames <- getSheetNames(path)


for (sheetName in sheetNames) {
  print(sheetName)
  res = readxl::read_xlsx(path,sheet = sheetName)
  res = res [1:(nrow(res)-10),]
  for (i in 3:5) {
     max_min <- left_join(driving_2(res,colnames(res),T,3),driving_2(res,colnames(res),F)) 
      tryCatch(
        expr = dir.create(paste0("output/max_min/",sheetName)),
        warning=function(w) { 
          TRUE
          }
      )
      write.csv(max_min,paste0("output/max_min/",sheetName,"/",sheetName,"_",i,".csv"),row.names = F)
    
  }
  
}



debug(driving_2)



driving_2 <- function(res,colnames_indi,max=T,thr=3) {
  # res[is.na(res)] <- 1
  n = nrow(res)
  if (max) {
    res$max = apply(res, 1, max ,na.rm = T)
    max_if <- res %>% mutate_all(.funs = ~ .x >= thr) %>% mutate_all(as.numeric) %>% select(-max)
    max_if <- apply(max_if, 2, sum, na.rm = T)
    names(max_if) <- colnames_indi 
    dr <- tibble::rownames_to_column(max_if %>% melt() %>% as.data.frame() , "Indicator") %>% mutate(
      value = paste0(round((value / n)*100,2),"%")
    ) 
    colnames(dr)[2] <- "max"
    return(dr)
  } else{
    res$max = apply(res, 1, min ,na.rm = T)
    max_if <- res %>% mutate_all(.funs = ~ .x < 3) %>% mutate_all(as.numeric) %>% select(-max)
    max_if <- apply(max_if, 2, sum, na.rm = T)
    names(max_if) <- colnames_indi 
    dr <- tibble::rownames_to_column(max_if %>% melt() %>% as.data.frame() , "Indicator") %>% mutate(
      value = paste0(round((value / n)*100,2),"%")
    ) 
    colnames(dr)[2] <- "min"
    return(dr)
  }}


