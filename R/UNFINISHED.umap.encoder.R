##
## Check out the umap function from the uwot package
## 
##
##
##

##load package
  require(uwot)
  require(data.table)
  require(MLmetrics)

##load data
  ksnap <- fread(input = 'C:/Users/George/Documents/Kaggle/Polish_Bankruptcy/Data/01_csv_data/train_data.csv')
  
##recode missing as mean
  recode.missing <- function(x){ x[is.na(x)] <- mean(x = x,na.rm = TRUE)*rnorm(n = length(x[is.na(x)])); return(x) }
  ksnap.recode <- as.data.frame(sapply(X = ksnap,FUN = recode.missing))
  
  train.idx <- sample(x = seq(1,nrow(ksnap.recode)),size = nrow(ksnap.recode)*0.7)
  train.data <- ksnap.recode[train.idx,]
  valid.data <- ksnap.recode[-train.idx,]
  
##Start with bases cases
  xgbParams <- list(nthreads = 6,objective = 'binary:logistic',eval_metric = 'auc',max_depth = 4,min_child_weight = 100)
  dtrain <- xgb.DMatrix(data = as.matrix(train.data[,1:64]),label = train.data[,65])
  xgbCV <- xgb.cv(params = xgbParams,data = dtrain,nrounds = 1000,nfold = 10,
                  stratified = TRUE,early_stopping_rounds = 10,print_every_n = 10)
  xgbFinal <- xgb.train(params = xgbParams,data = dtrain,nrounds = xgbCV$best_iteration,watchlist = list(train = dtrain))
  
  test.auc <- MLmetrics::AUC(y_true = valid.data$class,
                  y_pred = predict(object = xgbFinal,newdata = xgb.DMatrix(data = as.matrix(valid.data[,1:64]))))
  
  test.auc ## 0.9305314
  
##UMAP builder
  make.umap.model <- function(data){
    umap.model <- umap(X = data,n_neighbors = 15,n_components = 30,verbose = TRUE,n_threads = 6)
    return(umap.model)
  }
  
  umap.mdl <- make.umap.model(data = as.matrix(train.data[,1:64]))
  
##Train reduced model
  dtrain <- xgb.DMatrix(data = umap.mdl[,1:10],label = train.data[,65])
  
  xgbCV <- xgb.cv(params = xgbParams,data = dtrain,nrounds = 1000,nfold = 10,
                  stratified = TRUE,early_stopping_rounds = 10,print_every_n = 10)
  xgbFinal <- xgb.train(params = xgbParams,data = dtrain,nrounds = xgbCV$best_iteration,watchlist = list(train = dtrain))
  
  
  