# A script for preparing a data frame. 
# Function 1 (rm_values) removes values that are either NA or zeros based on the given list of names.
# Function 2 (df_prep) sends the df to function 1 and updates the df if Function 1 compiled any row indices to remove. 

# Function 1
rm_values <- function(df, namelist, zeros=TRUE, NAs=TRUE){ 
  
  resultarray <- NULL
  
  if(NAs==TRUE){ 
    all_inds_na <- unique(unlist(sapply(namelist,function(x){return(which(is.na(df[,x])==TRUE))})))
    if(length(all_inds_na)>0){ 
      resultarray[1:length(all_inds_na)] <- all_inds_na
    }}
  
  if(zeros==TRUE){ 
    all_inds_zero <- unique(unlist(sapply(namelist, function(x){return(which(df[,x]==0))})))
    print(all_inds_zero)
    if(length(all_inds_zero)>0){
      start <- length(resultarray)+1
      resultarray[start:(start+length(all_inds_zero))] <- all_inds_zero
    }}
  
  if(length(resultarray)>0){ # If there are NA and/or zero values, return only the unique row numbers where they exist. 
    all_inds <- unique(resultarray)
    return(all_inds)}
} 

# Function 2
df_prep <- function(df, names, zeros, NAs){ 
  inds <- rm_values(df, names, zeros, NAs)
  if(length(inds)>=1){ 
    message("The following rows will be removed:")
    print(df[inds,])
    data <- df[-inds,]
    return(data)
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
