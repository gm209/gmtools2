
# Function Defs -----------------------------------------------------------
  evaluate.models <- function(x,y,w=NULL,offset=NULL,train.data,foldCol=NULL,xgbParams=NULL,h2o.gbmParams=NULL,h2o.glmParams=NULL,h2o.dlearnParams=NULL){
  
  ### Create the switches for different models
  runH2O <- ((!is.null(h2o.gbmParams)) + (!is.null(h2o.glmParams)) + (!is.null(h2o.dlearnParams))) > 0
  runXGB <- !is.null(xgbParams)
  
  ### Run the XGBoost Procedures if required
  if(runXGB){
    ## Prepare the data for XGBoost
    dtrain <- xgboost::xgb.DMatrix(data = as.matrix(train.data[,x]),label = train.data[,y])
    if(!is.null(w)) xgboost::setinfo(object = dtrain,'weight',train.data[,w])
    if(!is.null(offset)) xgboost::setinfo(object = dtrain,'base_margin',train.data[,offset])
    
    xgbParams$data <- dtrain
    xgbParams$pred <- TRUE
    xgbParams$folds <- lapply(unique(train.data[,foldCol]),function(f){ which(train.data[,foldCol] != f) })
    
    ## Train the XGBoost model
    xgbCV <- do.call(xgboost::xgb.cv,xgbParams)
  } else {
    xgbCV <- NULL
  }
  
  ### Run the h2o Procedures if required
  if(runH2O){
    ## Export the data into the h2o cluster
    train.data.h2o <- h2o::as.h2o(train.data)
    
    ## Set up the model parameters
    if(!is.null(h2o.gbmParams)) { 
      h2o.gbmParams$seed <- 1988
      h2o.gbmParams$keep_cross_validation_predictions <- TRUE 
      h2o.gbmParams$training_frame <- train.data.h2o
      h2o.gbmParams$x <- x
      h2o.gbmParams$y <- y
      h2o.gbmParams$fold_column <- foldCol
      if(!is.null(w)) h2o.gbmParams$w <- w
      if(!is.null(offset)) h2o.gbmParams$offset <- offset
    }
    
    if(!is.null(h2o.glmParams)) { 
      h2o.glmParams$seed <- 1988
      h2o.glmParams$keep_cross_validation_predictions <- TRUE 
      h2o.glmParams$training_frame <- train.data.h2o
      h2o.glmParams$x <- x
      h2o.glmParams$y <- y
      h2o.glmParams$fold_column <- foldCol
      if(!is.null(w)) h2o.glmParams$w <- w
      if(!is.null(offset)) h2o.glmParams$offset <- offset
    }
    
    if(!is.null(h2o.dlearnParams)) { 
      h2o.dlearnParams$seed <- 1988
      h2o.dlearnParams$keep_cross_validation_predictions <- TRUE
      h2o.dlearnParams$training_frame <- train.data.h2o
      h2o.dlearnParams$x <- x
      h2o.dlearnParams$y <- y
      h2o.dlearnParams$fold_column <- foldCol
      if(!is.null(w)) h2o.dlearnParams$w <- w
      if(!is.null(offset)) h2o.dlearnParams$offset <- offset 
     }
    
    ## Train the h2o models
    if(!is.null(h2o.gbmParams)) h2oGBM <- do.call(h2o::h2o.gbm,h2o.gbmParams) else h2oGBM <- NULL
    if(!is.null(h2o.glmParams)) h2oGLM <- do.call(h2o::h2o.glm,h2o.glmParams) else h2oGLM <- NULL
    if(!is.null(h2o.dlearnParams)) h2oDEEPLEARNING <- do.call(h2o::h2o.deeplearning,h2o.dlearnParams) else h2oDEEPLEARNING <- NULL
  } else {
    ## Set all the models to null
    h2oGBM <- NULL
    h2oGLM <- NULL
    h2oDEEPLEARNING <- NULL
  }
  
  ### cplm models
  
  ### Sort out results
  mdls <- list(xgbCV,h2oGBM,h2oGLM,h2oDEEPLEARNING)
  results <- do.call(rbind,lapply(mdls,function(x){ 
    if(!is.null(x)) eval.fun(mdl = x,y_true = train.data[,y],fold_column = train.data[,foldCol]) else NULL
  }))
  return(list(results = results,xgbCV = xgbCV,h2oGBM = h2oGBM,h2oGLM = h2oGLM,h2oDEEPLEARNING = h2oDEEPLEARNING))
  
}
  eval.fun <- function(mdl,y_true,fold_column){
  
  ## Get the preds
  if(class(mdl) == "xgb.cv.synchronous"){ 
    preds <- mdl$pred
    mdlName <- 'xgboost'
  } else if(class(mdl) == "H2ORegressionModel") {
    preds <- as.numeric(unlist(as.data.frame(h2o::h2o.cross_validation_holdout_predictions(object = mdl))))
    mdlName <- mdl@algorithm
  }
  
  ## Aggregate them together
  dfWorking <- data.frame(y_true = y_true,y_pred = preds,fold_column = fold_column)
  
  ## Generate results
  results <- c()
  for(i in unique(fold_column)){
    temp <- dfWorking[ dfWorking$fold_column == i, ]
    results[i] <- gmtools::create_quantile_plot_data(y_pred = temp$y_pred,y_true = temp$y_true)$bMAE
    #MLmetrics::Gini(y_pred = temp$y_pred,y_true = temp$y_true) 
    #MLmetrics::RMSE(y_pred = temp$y_pred,y_true = temp$y_true)    
    #MLmetrics::MAE(y_pred = temp$y_pred,y_true = temp$y_true)
    
  }
  
  ## Aggregate
  stderr <- function(x) sd(x)/sqrt(length(x))
  result <- data.frame(mdlName = mdlName,meanMetric = mean(results),seMetric = stderr(results))
  return(result)
}

# Data Prep ---------------------------------------------------------------
 
  ## Load packages 
  require(cplm);require(ggplot2); require(gmtools); require(data.table);require(dplyr)
  require(h2o)
   
  ## Start h2o cluster
  local_h2o <- h2o::h2o.init(nthreads = 6)

  ## Load the data
  data(AutoClaim)
  
  ## null some rubbish columns
  AutoClaim$POLICYNO <- NULL
  AutoClaim$PLCYDATE <- NULL
  AutoClaim$IN_YY <- NULL
   
  ## Create frequency variable
  AutoClaim$CLM_FREQ <- AutoClaim$CLM_FREQ5 / AutoClaim$NPOLICY
   
  ## Feature lists and response column
  x <- setdiff(names(AutoClaim),c('CLM_FREQ5',"CLM_AMT5","CLM_AMT","CLM_FREQ","NPOLICY"))
  y <- 'CLM_FREQ'
  w <- "NPOLICY"
   
  ## Remove level names with spaces!
  levels(AutoClaim$JOBCLASS) <- gsub(pattern = " ",replacement = "_",x = levels(AutoClaim$JOBCLASS))
  levels(AutoClaim$CAR_TYPE) <- gsub(pattern = " ",replacement = "_",x = levels(AutoClaim$CAR_TYPE))
  levels(AutoClaim$MAX_EDUC) <- gsub(pattern = " ",replacement = "_",x = levels(AutoClaim$MAX_EDUC))
   
  ## Fill in missing values
  AutoClaim$YOJ[is.na(AutoClaim$YOJ)] <- mean(AutoClaim$YOJ,na.rm = TRUE)
  AutoClaim$INCOME[is.na(AutoClaim$INCOME)] <- mean(AutoClaim$INCOME,na.rm = TRUE)
  AutoClaim$HOME_VAL[is.na(AutoClaim$HOME_VAL)] <- mean(AutoClaim$HOME_VAL,na.rm = TRUE)
  AutoClaim$SAMEHOME[is.na(AutoClaim$SAMEHOME)] <- mean(AutoClaim$SAMEHOME,na.rm = TRUE)
  AutoClaim$SAMEHOME[AutoClaim$SAMEHOME<0] <- 0
  
# Feature Builder ---------------------------------------------------------
  feature.builder <- function(df,features){
    for(i in 1:length(features)){
      feature <- features[i]
      
      ## Power transforms
      df[,paste0(feature,'_SQ')] <- df[,feature]**2
      df[,paste0(feature,'_CB')] <- df[,feature]**3
      df[,paste0(feature,'_SQRT')] <- df[,feature]**0.5
      
      ## Standardize
      df[,paste0(feature,'_STD')] <- scale(x = df[,feature],center = TRUE,scale = TRUE)[,1]
      
      ## Min Max Scaling
      df[,paste0(feature,'_MMS')] <- (df[,feature] - min(df[,feature])) / (max(df[,feature]) - min(df[,feature]))
      
      ## Descritise
      df[,paste0(feature,'_BIN')] <- binnarise(x = df[,feature])    
    }
    return(df)
  }
  num.feats <- names(AutoClaim[,x])[sapply(AutoClaim[,x],function(f){ class(f) %in% c('numeric','integer')  })]
  engineered <- feature.builder(df = AutoClaim,features = num.feats)
  x <- c(x,setdiff(names(engineered),names(AutoClaim)))
  
# Encode the data & split train from test ---------------------------------------------------------

  ## Define encoding
  encode.data <- function(encoding){
     if(encoding == "label"){
       encode.df <- as.data.frame(sapply(engineered,as.numeric))
       x <- x
     } else if(encoding == "h2o"){
       encode.df <- engineered
       x <- x
     } else if(encoding == "ohe"){
       features <- as.data.frame(model.matrix(object = ~ .,data = engineered[,x])[,-1])
       x <- names(features)
       encode.df <- as.data.frame(cbind(features,CLM_FREQ = engineered[,y],NPOLICY = engineered[,w])) 
     }
     return(list(data = encode.df,x = x))
   }
  tmp <- encode.data(encoding = "ohe")
  x <- tmp$x; encode.df <- tmp$data
  rm(tmp); gc()
  
  ## Split train and test
  set.seed(2001); r <- runif(n = nrow(encode.df))
  encode.trn <- encode.df[ r < 0.70, ]
  encode.trn$foldCol <- rep(1:5,length.out = nrow(encode.trn))
   
  encode.tst <- encode.df[ r > 0.69, ]
  
# Some feature selection --------------------------------------------------

  feature.select <- function(method,cor.num = 10,threshold = 0.75){
   if(method == "cor"){
     corMat <- data.frame(names = x,cor = cor(x = encode.trn[,x],y = encode.trn$CLM_FREQ)) %>%
       dplyr::arrange(-abs(cor)) %>%
       mutate(cumPercCor = cumsum(abs(cor)) / sum(abs(cor)))
     plot(corMat$cumPercCor)
     x <- corMat %>% head(cor.num) %>% select(names) %>% unlist() %>% as.character()
   } else if(method == "colin"){
    x <- gmtools::remove_colinear_features(df = encode.trn,features = x,threshold = threshold)$included_features     
   } else if(method == "none") {
    x <- x 
   }  
   return(x)
  }
  x <- feature.select(method = "cor",cor.num = 10,threshold = 0.8); 
  
# Train Models ------------------------------------------------------------

  ## State model run parameters
  xgbParams <- list(objective = "count:poisson",nthread = 6,early_stopping_rounds = 5,nrounds = 250,print_every_n = 10)
  h2o.gbmParams <- list(distribution = 'poisson',ntrees = 250,stopping_rounds = 5,categorical_encoding = "SortByResponse") 
  h2o.glmParams <- list(family = 'poisson',remove_collinear_columns = TRUE)
  h2o.dlearnParams <- list(distribution = 'poisson')
   
  ## Evaluate models
  sc<- evaluate.models(x = x,y = y,w = w,train.data = encode.trn,foldCol = "foldCol"
                       #,xgbParams = xgbParams
                       #,h2o.gbmParams = h2o.gbmParams
                       ,h2o.glmParams = h2o.glmParams
                       ,h2o.dlearnParams = h2o.dlearnParams
                       )
   
  sc$results %>% arrange(meanMetric) 
  
  ##0.05737626 h2oGBM - label
  ##0.06478571 h2oGBM - label + correl feature select
  ##0.06172097 h2oGBM - ohe
  ##0.06709428 h2oGBM - h2o[SortByResponse]
  ##0.08006539 h2oGLM - ohe + correl top 10 features
  ##0.09012985 h2oGLM - ohe + correl top 20 features
  ##0.09529011 h2oGLM - ohe + removing features with above 0.90 correl
  ##0.09579445 h2oGLM - ohe + removing features with above 0.95 correl
  ##0.09707414 h2oGLM - label + correl top 40 features
  ##0.1085153  h2oGLM - ohe + removing features with above 0.7 correl
  
  ## Quick sense check on train data
  gbmPred <- as.numeric(unlist(as.data.frame(h2o::h2o.cross_validation_holdout_predictions(object = sc$h2oGLM))))
  gbmActual <- encode.trn$CLM_FREQ
  plt.dat <- create_quantile_plot_data(y_pred = gbmPred,y_true = gbmActual)
  ggplot(data = plt.dat$data,aes(x = bin,y = wmean_pred)) + geom_line() + geom_point() + geom_line(aes(y = wmean_true)) + geom_point(aes(y = wmean_true))
  
  ## see performance on test data
  test.preds <- as.numeric(unlist(as.data.frame(h2o::h2o.predict(object = sc$h2oGLM,newdata = h2o::as.h2o(encode.tst)))))
  test.label <- encode.tst$CLM_FREQ
  plt.tst <- create_quantile_plot_data(y_pred = test.preds,y_true = test.label)$data
  ggplot(data = plt.tst,aes(x = bin,y = wmean_pred)) + geom_line() + geom_point() + geom_line(aes(y = wmean_true)) + geom_point(aes(y = wmean_true))