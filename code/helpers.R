# col name data required
multi_model_matrix = function(df, emptycolname) {
  max_cat =  max(sapply(df$data, length))
  cat_cols = paste(rep("Category", max_cat), 1:max_cat,sep="")
  df$data = sapply(df$data,function(x) paste(x, collapse=","))
  df = separate(df,
                data,
                into=cat_cols,
                sep = ",", remove=T, extra="drop")
  df = gather_(df, 'dummy', 'category', cat_cols)
  df %<>% filter(!is.na(category))
  df$category = ifelse(df$category=="", emptycolname, df$category)
  df$indicator = T
  df$dummy = NULL
  spread(df, category, indicator, fill=F)
}

multi_json = function(filename) {
  json = paste("[",
        paste(readLines(filename),
              collapse=", "),
        "]")
  return(flatten(fromJSON(json)))
}