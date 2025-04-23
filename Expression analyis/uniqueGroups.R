###
# A function to create unique combinations for a selected group of gene names (or other name lists), given a certain group size. 
# The resulting list could be used in e.g. lapply to calculate investigate relationships between the unique combinations. 

uniqueGroups <- function(genelist, groupsize){ 
  
  #Create a matrix of all unique combinations.
  unique_combinations <- as.matrix(gtools::combinations(n = length(genelist), r=groupsize))
  
  #This will be returned in the end: 
  comb_list <- list()
  
  for(i in 1:nrow(unique_combinations)){ 
    
    indices <- as.array(unique_combinations[i,]) #Indices of combination "i"
    genes <- genelist[indices] # Genes of combination "i"
    comb_list[[i]] <- genes #Append the combination to the resulting list of combinations
  }
  return(comb_list)
}

genes <- c("BRCA2", "TP53", "EPCAM", "EGFR", "CD8A", "NOTCH2", "CCL5")
n <- 3
result <- uniqueGroups(genes, n)
result
