###
# Create a mock dataframe with numerical dummy values (sampled from the exponential distribution) and made up gene names (4 letters). 
# Resulting dataframe will have "n_genes" number of genes, independent variables determined by entered column names, and a random number of observations per gene from "1" to "nrow_max". 

df_mock <- function(n_genes, nrow_max=3, cnames){
  
  fill.matrix <- function(expr, nrow, ncol) {
    return(
      matrix(
        eval(
          expr),
        nrow=nrow, 
        ncol=ncol)
    )
  }
  
  my_list <- lapply(1:n_genes, function(i){ # A for loop for each new gene
    
    nrowS <- sample(1:nrow_max,1) # Random number of rows between 1 and nrow_max 
    ncolS <- length(cnames) # Number of columns based on the list of provided column names
    n_values <- ncolS*nrowS # Number of values to generate to fill matrix of size nrowS*ncolS
    
    my_matrix <- fill.matrix(expr=rexp(n_values, rate=.1), nrow=nrowS, ncol=ncolS) #Create matrix
    my_matrix <- as.data.frame(my_matrix, row.names=FALSE) #Change to type data frame.
    colnames(my_matrix) <- cnames # Add column names
    
    name <- stringr::str_flatten(sample(toupper(letters), 4)) #Generate random gene name
    my_matrix <- cbind(my_matrix, ID = rep(name, nrowS)) #Replicate n_row times and add in ID column 
    
    return(my_matrix)
  })
  
  tot_df <- Reduce(f = dplyr::full_join, x = my_list) # Join data frames for separate genes into one and return it
  
  return(tot_df)
}

random_df <- df_mock(n_genes = 5, nrow_max = 3,cnames = c("One", "Two", "Three"))
