# -- Functions that may or may not be helpful

abbreviated_name <- function(server_name){
  # Function: Takes a full name (First Name + Last Name), and outputs an abbreviated version
  # Ex: Harvey Dent --> H. Dent
  
  # Args:
  #-|-|-|
  # server_name [str]
  
  # Returns:
  #-|-|-|-|-|
  # str
  last_name <- strsplit(server_name, " ")[[1]][2]
  first_letter <- substr(server_name,1,1)
  short_name <- paste(first_letter, '. ', last_name, sep = "")
  
  final_label <- short_name
  return(final_label)
}

# ** Note: this function works with mapply()
# Ex: df$name_tag <- mapply(df$full_name, FUN = abbreviated_name)
