##load data
  ksnap <- fread(input = 'C:/Users/George/Documents/Kaggle/Polish_Bankruptcy/Data/01_csv_data/train_data.csv')
  ksnap <- ksnap[complete.cases(ksnap)]
   
##compute PCA mapping pack
  make.pca.model <- function(data,thresh=0.95){
    
    ##Calculate the components
    pca <- prcomp(x = data,center = TRUE,scale. = TRUE)
    res <- t(summary(pca)$importance)
    
    ##What number should we keep
    n.comp <- nrow(res[res[,3]<thresh, ])
    
    ##Return the model object and number of components
    return(list(model = pca,n.comp = n.comp))
  } 
  
##apply PCA model
  apply.pca.model <- function(pca.model,data){
    
    ##Predict from the model
    pca.predict <- predict(object = pca.model$model,newdata = data)[,1:pca.model$n.comp]
    
    ##return the components
    return(pca.predict)
  }

##pca
  dat <- ksnap[,setdiff(names(ksnap),'class'),with=FALSE]
  pca1 <- make.pca.model(data = dat,thresh = 0.95)
  pca2 <- apply.pca.model(pca.model = pca1,data = dat)
  ncol(dat)
  ncol(pca2)
  
  
  
  
  