

simplifyModelName <- function(model){
  model <- gsub("v\\d*\\.?\\d*\\.?\\d*", "", model) # remove "v1.0.0"
  model <- gsub(" \\d*\\.?\\d*\\.?\\d*", "", model) # remove "1.0.0"
  model <- gsub(" \\d*\\.?\\d*", "", model) # remove "1.1"
  model <- gsub("\\s$*", "", model) # remove trailing spaces
  model <- gsub("\\_*$", "", model) # remove trailing underscores
  model <- gsub("MESSAGEix-GLOBIOM","MESSAGE",model)
  return(model)
}

cleanFileName <- function(string){
  string <- gsub("\\(\\*\\)","",string)
  string <- trimws(string)
  string <- path_sanitize(string, replacement = "_")
  return(as.character(string))
}