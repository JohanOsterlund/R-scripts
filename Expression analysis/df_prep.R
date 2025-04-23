### A script for preparing a data frame. 
# Removes values that are NA and/or zeros based on provided arguments and returns the data.

df_prep <- function(df, zeros=TRUE, NAs=TRUE, namelist=colnames(df)){ 
  
  resultarray <- NULL
  
  if(NAs==TRUE){ 
    all_inds_na <- unique(unlist(sapply(namelist,function(x){return(which(is.na(df[,x])==TRUE))})))
    if(length(all_inds_na)>0){ 
      resultarray[1:length(all_inds_na)] <- all_inds_na
    }}
  
  if(zeros==TRUE){ 
    all_inds_zero <- unique(unlist(sapply(namelist, function(x){return(which(df[,x]==0))})))
    if(length(all_inds_zero)>0){
      start <- length(resultarray)+1
      resultarray[start:(start+length(all_inds_zero))] <- all_inds_zero
    }}
  
  if(length(resultarray)>0){ # If there are NA and/or zero values, return only the unique row numbers where they exist. 
    all_inds <- unique(resultarray)
    message("The following rows will be removed:")
    print(df[all_inds,])
    df <- df[-all_inds,]
    return(df)
  }
  else{
    message("No values to be removed. Returning original data")
    return(df)
  }
}

random_df <- mock_df(n_genes = 5, nrow_max = 3,cnames = c("One", "Two", "Three")) # See file mock_df
random_df[1,2] <- 0
random_df[3,2] <- NA
random_df[3,4] <- NA

df_prep(random_df, c("One", "Two", "Three"), TRUE, TRUE)
