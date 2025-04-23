# You have a list of names, and you remember only a part of the gene name that you want to look up.
# The function returns all matches and their positions in the name list. 

find_fullname <- function(namelist, pattern){
  result <- unlist(stringr::str_extract_all(namelist, paste(".*", pattern, ".*", sep=""))) # Retrieve all strings that include the requested search. 
  ind <- sapply(result, function(x)return(which(namelist == x))) #For each match, return the position (index) of that match in the namelist.
  return(list(matches=result, pos=ind))
}

people <- c("Dorothy", "Michael", "David")
fullnames <- find_fullname(people, "Dor")
fullnames
