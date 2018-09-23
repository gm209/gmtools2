##
## Program Name: SampleMLPipeline
## Description:  A pipeline for creating models on the AutoClaim dataset
## Version History >>>     
## 0.01 > Initial Code Developed 
##

## TODO: find a nicer way of telling the pipeline to not run certain algos


# Parameter Read ----------------------------------------------------------

  ## IsInteractive
     IsInteractive <- (length(commandArgs(trailingOnly = TRUE))==0)
     if(IsInteractive) nParam <- 1 else nParam <- nrow(read.csv(file = commandArgs(trailingOnly = TRUE)[1],stringsAsFactors = FALSE))
     if(!IsInteractive) h2o::h2o.no_progress()
  
### Parameter Loop
    for(i in 1:nParam){

      message('>>> Starting Iteration # ',i)
      possibleError <- tryCatch(expr = {
      
        # Parameter Read ----------------------------------------------------------
          ## User defined parameters - ONLY USED IF CALLED INTERACTIVELY
          if(IsInteractive){
          encoding_type   <- "ohe"
          create_new_features <- TRUE
          feature_select  <- list(method = "cor",cor.num = 10,threshold = 0.8)
          
          xgbParams <- list(objective = "count:poisson",nthread = 6,early_stopping_rounds = 5,nrounds = 250,print_every_n = 10)
          h2o_gbmParams <- list(distribution = 'poisson',ntrees = 250,stopping_rounds = 5,categorical_encoding = "SortByResponse") 
          h2o_glmParams <- list(family = 'poisson',remove_collinear_columns = TRUE)
          h2o_dlearnParams <- list(distribution = 'poisson',stopping_rounds = 5)
        } else {
          
          ### Procedure for reading params from config file
          ## Read the param file
          paramRead <- read.csv(file = commandArgs(trailingOnly = TRUE)[1],stringsAsFactors = FALSE)[i,]
          
          ## Extract the vanilla params
          encoding_type <- paramRead$encoding_type
          
          create_new_features <- paramRead$create_new_features
          
          ## function to make params a proper list
          make.param.list <- function(prefix){
            cols <- names(paramRead)[grepl(pattern = prefix,x = names(paramRead))]
            cols_select <- paramRead[,cols]
            names(cols_select) <- gsub(pattern = paste0(prefix,'.'),replacement = '',x = names(cols_select))
            cols_select <- as.list(cols_select)
            if(sum(sapply(cols_select,function(x){x == "NULL"} ))>0) cols_select <- NULL
            return(cols_select)
          }
          
          ## Extract the feature_select columns
          feature_select <- make.param.list(prefix = 'feature_select')
          
          ## Extract the xgbParams Columns
          xgbParams <- make.param.list(prefix = 'xgbParams')
          str(xgbParams)
          
          ## Extract the h2o_gbmParams
          h2o_gbmParams <- make.param.list(prefix = 'h2o_gbmParams')
          
          ## Extract the h2o_glmParams
          h2o_glmParams <- make.param.list(prefix = 'h2o_glmParams')   
          
          ## Extract the h2o_dlearnParams
          h2o_dlearnParams <- make.param.list(prefix = 'h2o_dlearnParams')
          
        }  
        
        # Set workspace ----------------------------------------------------------
          setwd('SOME_DIRECTORY')
        
        # Setup some custom functions ---------------------------------------------
          evaluate.models <- function(x,y,w=NULL,offset=NULL,train.data,foldCol=NULL,xgbParams=NULL,h2o_gbmParams=NULL,h2o_glmParams=NULL,h2o_dlearnParams=NULL){
          
          ### Create the switches for different models
          runH2O <- ((!is.null(h2o_gbmParams)) + (!is.null(h2o_glmParams)) + (!is.null(h2o_dlearnParams))) > 0
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
            if(!is.null(h2o_gbmParams)) { 
              h2o_gbmParams$seed <- 1988
              h2o_gbmParams$keep_cross_validation_predictions <- TRUE 
              h2o_gbmParams$training_frame <- train.data.h2o
              h2o_gbmParams$x <- x
              h2o_gbmParams$y <- y
              h2o_gbmParams$fold_column <- foldCol
              if(!is.null(w)) h2o_gbmParams$w <- w
              if(!is.null(offset)) h2o_gbmParams$offset <- offset
            }
            
            if(!is.null(h2o_glmParams)) { 
              h2o_glmParams$seed <- 1988
              h2o_glmParams$keep_cross_validation_predictions <- TRUE 
              h2o_glmParams$training_frame <- train.data.h2o
              h2o_glmParams$x <- x
              h2o_glmParams$y <- y
              h2o_glmParams$fold_column <- foldCol
              if(!is.null(w)) h2o_glmParams$w <- w
              if(!is.null(offset)) h2o_glmParams$offset <- offset
            }
            
            if(!is.null(h2o_dlearnParams)) { 
              h2o_dlearnParams$seed <- 1988
              h2o_dlearnParams$keep_cross_validation_predictions <- TRUE
              h2o_dlearnParams$training_frame <- train.data.h2o
              h2o_dlearnParams$x <- x
              h2o_dlearnParams$y <- y
              h2o_dlearnParams$fold_column <- foldCol
              if(!is.null(w)) h2o_dlearnParams$w <- w
              if(!is.null(offset)) h2o_dlearnParams$offset <- offset 
            }
            
            ## Train the h2o models
            if(!is.null(h2o_gbmParams)) h2oGBM <- do.call(h2o::h2o.gbm,h2o_gbmParams) else h2oGBM <- NULL
            if(!is.null(h2o_glmParams)) h2oGLM <- do.call(h2o::h2o.glm,h2o_glmParams) else h2oGLM <- NULL
            if(!is.null(h2o_dlearnParams)) h2oDEEPLEARNING <- do.call(h2o::h2o.deeplearning,h2o_dlearnParams) else h2oDEEPLEARNING <- NULL
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
          binnarise <- function(x,w=NULL,nbins=10,retLabel=FALSE){
            
            ## Make sure dplyr is loaded
            ## suppressPackageStartupMessages(requireNamespace("dplyr"))
            
            ## Deal with null weights
            if(is.null(w)) w = rep(1,length(x))
            
            ## Deal with any nan weights
            if(anyNA(w)) stop('ERROR: NANs in weight column')
            
            ## Deal with nbins 1 or less
            if(nbins < 2) stop('ERROR: nbins < 2')
            
            ## Bind info into a frame
            dfWorking = data.frame(x = x,w = w,Index = seq(1,length(x))) %>% 
              dplyr::arrange(x) %>% 
              dplyr::mutate(cumSumW = cumsum(w))
            
            ## Create the breaks for the binning - need a + 1 for fence post errors 
            breaks = quantile(x = dfWorking$cumSumW,probs = seq(0,1,length=nbins+1))
            
            ## Bin the result
            dfWorking$bin = cut(x = dfWorking$cumSumW,breaks = breaks,include.lowest = TRUE,dig.lab=5)
            
            ## Sort our the labels
            dtSummary = data.table(dfWorking)[ , list(max_x = max(x),min_x = min(x)) , by = bin][order(bin), ]
            dtSummary$lbl = paste0(round(dtSummary$min_x,digits = 4)," - ",round(dtSummary$max_x,digits=4))
            levels(dfWorking$bin) = dtSummary$lbl; rm(dtSummary)
            
            if(retLabel==FALSE) dfWorking$bin = as.numeric(dfWorking$bin)
            
            dfWorking = dfWorking %>% dplyr::arrange(Index)
            
            ## Return the result
            return(dfWorking$bin)
            
          }
          
        # Load Environment --------------------------------------------------------
          
          ## Load packages
          pkgs <- c('ggplot2','data.table','dplyr','h2o','cplm','magrittr') 
          silent <- sapply(pkgs,function(p){
            if(!p %in% rownames(installed.packages())) install.packages(pkgs = p,dependencies = TRUE)
            require(p,character.only = TRUE,warn.conflicts = FALSE,quietly = TRUE)
          })
          if(sum(silent)!=length(pkgs)) stop('ERROR: loading one or more packages failed')
          
          ## Start h2o cluster
          local_h2o <- h2o::h2o.init(nthreads = 6)
          
          ## Load the data
          data(AutoClaim,package = "cplm")   
          
        # Store the config for later use ----------------------------------------------------
         
          ## Coalesce the config into data.frame
          run.config <- data.frame(run.date.time        = as.character(Sys.time()),
                                   encoding_type        = encoding_type,
                                   create_new_features  = create_new_features,
                                   feature_select       = feature_select,
                                   xgbParams            = ifelse(is.null(xgbParams),"NotExecuted",xgbParams),
                                   h2o_gbmParams        = ifelse(is.null(h2o_gbmParams),"NotExecuted",h2o_gbmParams),
                                   h2o_glmParams        = ifelse(is.null(h2o_glmParams),"NotExecuted",h2o_glmParams),
                                   h2o_dlearnParams     = ifelse(is.null(h2o_dlearnParams),"NotExecuted",h2o_dlearnParams))
          
          ## Create the logging directory
          log.name <- run.config$run.date.time %>% 
                      gsub(pattern = '-',replacement = '_', .) %>%
                      gsub(pattern = ':',replacement = '_', .) %>%
                      gsub(pattern = ' ',replacement = "_")
          log.dir <- paste0('./',log.name)
          dir.create(path = log.dir)
          setwd(log.dir)   
          
          ## Write the config to disk
          fwrite(x = run.config,file = './run.config.csv')
        
        # Data Cleansing ----------------------------------------------------------
        
          ## remove some rubbish columns - we dont want to use these
          AutoClaim$POLICYNO <- NULL
          AutoClaim$PLCYDATE <- NULL
          AutoClaim$IN_YY <- NULL ## Could use this to make everything comparable to Yip & Yau 2005
          
          ## Create frequency variable
          AutoClaim$CLM_FREQ <- AutoClaim$CLM_FREQ5 / AutoClaim$NPOLICY
          
          ## Feature lists and response column
          x <- setdiff(names(AutoClaim),c('CLM_FREQ5',"CLM_AMT5","CLM_AMT","CLM_FREQ","NPOLICY"))
          y <- 'CLM_FREQ'
          w <- "NPOLICY"
          
          ## Remove spaces from level names
          levels(AutoClaim$JOBCLASS) <- gsub(pattern = " ",replacement = "_",x = levels(AutoClaim$JOBCLASS))
          levels(AutoClaim$CAR_TYPE) <- gsub(pattern = " ",replacement = "_",x = levels(AutoClaim$CAR_TYPE))
          levels(AutoClaim$MAX_EDUC) <- gsub(pattern = " ",replacement = "_",x = levels(AutoClaim$MAX_EDUC))
          
          ## We've got some missing values - we need to fill them in
          AutoClaim$YOJ[is.na(AutoClaim$YOJ)] <- mean(AutoClaim$YOJ,na.rm = TRUE)
          AutoClaim$INCOME[is.na(AutoClaim$INCOME)] <- mean(AutoClaim$INCOME,na.rm = TRUE)
          AutoClaim$HOME_VAL[is.na(AutoClaim$HOME_VAL)] <- mean(AutoClaim$HOME_VAL,na.rm = TRUE)
          AutoClaim$SAMEHOME[is.na(AutoClaim$SAMEHOME)] <- mean(AutoClaim$SAMEHOME,na.rm = TRUE)
          
          ## Fix some impossible SAMEHOME values
          AutoClaim$SAMEHOME[AutoClaim$SAMEHOME<0] <- 0
          
        # Create Generic Features -------------------------------------------------
          
          if(run.config$create_new_features == TRUE){
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
          }
          
        # Apply Encoding then split Train & Test ----------------------------------
          
          ## This encoding function is realtively simple - it could be made much more complicated...
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
          tmp <- encode.data(encoding = run.config$encoding_type)
          x <- tmp$x; encode.df <- tmp$data
          rm(tmp); gc()
          
          ## Create fold index, Split test and train
          set.seed(2001); r <- runif(n = nrow(encode.df))
          encode.trn <- encode.df[ r < 0.70, ]
          encode.trn$foldCol <- rep(1:5,length.out = nrow(encode.trn))
          encode.tst <- encode.df[ r > 0.69, ]
        
        # Feature Selection Protocol ----------------------------------------------
          
          feature.select <- function(method,cor.num = 10,threshold = 0.75){
            if(method == "cor"){
              corMat <- data.frame(names = x,cor = cor(x = encode.trn[,x],y = encode.trn$CLM_FREQ)) %>%
                dplyr::arrange(-abs(cor)) %>%
                mutate(cumPercCor = cumsum(abs(cor)) / sum(abs(cor)))
              x <- corMat %>% head(cor.num) %>% select(names) %>% unlist() %>% as.character()
            } else if(method == "colin"){
              x <- gmtools::remove_colinear_features(df = encode.trn,features = x,threshold = threshold)$included_features     
            } else if(method == "none") {
              x <- x 
            }  
            return(x)
          }
          x <- feature.select(method = run.config$feature_select.method,cor.num = run.config$feature_select.cor.num,threshold = run.config$feature_select.threshold)
        
        # Evaluate Models ---------------------------------------------------------
          
          modelRun <- evaluate.models(x = x,y = y,w = w,train.data = encode.trn,foldCol = "foldCol"
                                     ,xgbParams = xgbParams
                                     ,h2o_gbmParams = h2o_gbmParams
                                     ,h2o_glmParams = h2o_glmParams
                                     ,h2o_dlearnParams = h2o_dlearnParams)
          
        # Save results ------------------------------------------------------------
          fwrite(x = modelRun$results,file = paste0('./run_results.csv'))
          h2o.shutdown(prompt = FALSE)
          Sys.sleep(time = 20) ## need to give the cluster a chance to terminate
     
      },error = function(e) {e})
    }        