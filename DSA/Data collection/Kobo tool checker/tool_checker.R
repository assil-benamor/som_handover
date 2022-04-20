rm(list = ls())


library(readxl)
library(dplyr)
library(qdapRegex)
library(stringr)
library(purrr)


check_answer_in_list <- function(constraint) {
  
  if(!str_detect(constraint,",")){
    return(TRUE)
  }
  
  # print(constraint)

  question_regex <- "\\{([^()]+)\\}"
  answer_regex <- "\\'([^()]+)\\'"
  
  question <- gsub(question_regex, "\\1", str_extract_all(constraint, question_regex)[[1]])
  answer <- gsub(answer_regex, "\\1", str_extract_all(constraint, answer_regex)[[1]])
  
  questions$type <- gsub("\\s+or_other\\s*$","",questions$type)
  
  listname <- gsub("^.*\\s","",questions %>% 
                     filter(name==question) %>% 
                     filter(!grepl("^(begin|end)\\s+group$",type)) %>% 
                     pull(type))
  
  choices_list <- choices %>% filter(list_name==listname) %>% pull(name)
  
  answer %in% choices_list
  
}

check_constraints <- function(questions,choices) {
  
  questions$name <- str_trim(questions$name)
  questions$type <- str_trim(questions$type)
  choices$list_name  <- str_trim(choices$list_name)
  choices$name  <- str_trim(choices$name)
  
  all_contraints <- c(questions %>% filter(grepl("selected",relevant)) %>% pull(relevant),
  map2_chr(questions %>% filter(grepl("selected",constraint)) %>% pull(name),
       questions %>% filter(grepl("selected",constraint)) %>% pull(constraint),
       ~ gsub("\\.\\s*,",sprintf('${%s},',.x),.y)
       ))
  
  
  # all_contraints <- questions %>% filter(grepl("selected",constraint)) %>% pull(constraint)
  
  all_contraints <- gsub('"',"'",all_contraints)

  rs_list <- map(all_contraints,~map_lgl(unlist(ex_default(.x, pattern = "selected\\s*\\([^\\)]*\\)")),check_answer_in_list))
  
  # map2(rs_list,seq_along(rs_list), ~ if(length(which(!.x))!=0) {
  #   return(unlist(ex_default(all_contraints[.y], pattern = "selected\\s*\\([^\\)]*\\)"))[which(!.x)])
  # } ) %>% unlist() %>% unique()
  
  
  map_chr(map2(rs_list,seq_along(rs_list), ~ if(length(which(!.x))!=0) {
    return(unlist(ex_default(all_contraints[.y], pattern = "selected\\s*\\([^\\)]*\\)"))[which(!.x)])
  } ) %>% unlist() %>% unique(),
  ~sprintf("Question name: %s //// Wrong constraint: %s",
           # .x,
           gsub("\\$\\{|\\}","",str_extract(.x,"\\$\\{.*\\}")),
           gsub(",\\s*'|'","",str_extract(.x,",\\s*'.+'")))
  )
  
  }



koboToolPath  = "tool coding/XLS/V1_19_10_21/REACH_SOM_DSA_Survey_tool_V1.xlsx"

koboToolPath  = "/Users/mac/Downloads/WFP_REACH_ETH_Tool_FS HH Survey_FINAL_Nov12_v1.xlsx"

questions = read_xlsx(koboToolPath,sheet="survey",guess_max = 50000) %>% filter(!is.na(name))
choices = read_xlsx(koboToolPath,sheet="choices",guess_max = 50000)

# questions <- questions %>% filter(grepl("education_barries_girls",name))
# debug(check_constraints)
check_constraints(questions,choices) 


# 
# 
# unglue::unglue_vec(x = "selected(${camp_structure},'others')",
#                    patterns = "{},'{x}')")
# 
# 
# 
# unglue::unglue_vec(x = "selected(${camp_structure},'others')",
#                    patterns = "selected[=\\s*](${[x]},[]",open = '[',close = ']')

