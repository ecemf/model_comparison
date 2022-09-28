
download_iiasa_db <- function(){
  
  source_python("./python/download_iiasa_db.py")
  
  #create data folder
  if(!(file.exists("./data")))
    dir.create("./data")
  
  download_iiasa_db_py('ecemf_internal')
}

