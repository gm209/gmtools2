## Get the performance stats
setwd(WORKING_DIR_OF_PROJECT)
files_to_process <- list.files(recursive = TRUE,pattern = "run_results")

perf <- do.call(rbind,lapply(files_to_process,function(f){
  
  stamp <- unlist(strsplit(x = f,split = '/',fixed = TRUE)[1])[1]
  rec   <- read.csv(file = paste0('./',f))
  rec$id <- rep(stamp,nrow(rec))
  rec$runNum <- which(files_to_process == f)
  return(rec)
  
}))

require(dplyr)
perf %>% arrange(meanMetric)



