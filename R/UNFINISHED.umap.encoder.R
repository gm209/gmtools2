##
## Check out the umap function from the uwot package
## 
##
##
##

##load package
  require(uwot)
  
##UMAP builder
  make.umap.model <- function(data,...){
    umap.model <- umap(X = data,...)
    return(umap.model)
  }
  
  umap.mdl <- make.umap.model(data = as.matrix(train.data[,1:64]))
  

  
  