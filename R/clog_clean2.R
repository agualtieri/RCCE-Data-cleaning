clog_clean2<-function(df, clean_log){
  clean_log<-clean_log%>%
    dplyr::filter(change == TRUE)%>%
    mutate(new_values = if_else(is.na(new_values), "", new_values))%>%
    dplyr::filter(variables != "uuid_second_column")
  for(i in 1:nrow(clean_log)){
    j<-clean_log$ids[i]
    k<-clean_log$variables[i]
    var<-clean_log$new_values[i]
    print(i)
    col_num <- which(colnames(df)==k)
    if(class(df[[col_num]])=="character"){
      var<-as.character(var)
    }else if(class(df[[col_num]])=="integer"){
      var<-as.integer(var)
    }else if(class(df[[col_num]])=="logical"){
      var<-as.logical(var)
    }
    m<-which(grepl(j,df$X_uuid))
    n<-which(grepl(k,colnames(df)))
    df[m,n]<-var
  }
  return(df)
}