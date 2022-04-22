map_to_master_table <- function(results_object, filename, questionnaire = NULL){
  summary_table_single <- function(x, questions = questionnaire){
    if(!is.null(questions)){
      x <- map_to_labeled(result = x, questionnaire = questions)
    }
    y <- NULL
    no_pvalue <- is.null(x$hypothesis.test$result$p.value)
    no_hypothesis.test <- is.null(x$hypothesis.test$name)
    if(no_pvalue|no_hypothesis.test){
      x$hypothesis.test$result$p.value <- NA
      x$hypothesis.test$name <- NA
    }
    if(!is.null(x$summary.statistic)){
      
      y <- data.frame(x$summary.statistic,
                      p.value = x$hypothesis.test$result$p.value
                      #test.name = x$hypothesis.test$name
      )
    }
    return(y)
  }
  results_object <- lapply(results_object,function(x){x$summary.statistic<-as.data.frame(x$summary.statistic,stringsAsFactors=F);x})
  df <- results_object %>% lapply(summary_table_single) %>% do.call(rbind, .)
  map_to_file(df, filename)
}
