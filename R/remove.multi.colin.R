##load packages
  require(data.table)
  require(dplyr)

##load data
  ksnap <- fread(input = 'C:/Users/George/Documents/Kaggle/Polish_Bankruptcy/Data/01_csv_data/train_data.csv')
  ksnap <- ksnap[complete.cases(ksnap)]
  
##Find Correlated Vars
  check.multi.colin <- function(matrix,thresh=0.9){
    
    ## Set options
    old.scipen <- getOption(x = 'scipen'); old.stringsAsFactors <- getOption(x = 'stringsAsFactors')
    options(scipen = 999,stringsAsFactors = FALSE)  
    
    ## Build correlation table
    cor.table <- melt(cor(matrix)) %>% 
                  arrange(-abs(value)) %>% 
                  filter(Var1 != Var2) %>%
                  filter(abs(value) > thresh)
    cor.table$Var1 <- as.character(cor.table$Var1)
    cor.table$Var2 <- as.character(cor.table$Var2)

    ## Build list of features to remove
    rem.list <- c()
    for(i in 1:nrow(cor.table)){

      if(! cor.table[i,'Var1'] %in% rem.list) rem.list <- c(rem.list,cor.table[i,"Var1"])  
    }
    
    ## Reset options
    options(scipen = old.scipen,stringsAsFactors = old.stringsAsFactors)
    
    ## Return the rem.list
    return(rem.list)
  }
  
##Remove Correlated Vars
  remove.multi.colin <- function(data,rem.list){
    reduc.data <- data[,setdiff(seq(1,ncol(data)),which(names(data) %in% rem.list))]
    return(reduc.data)
  }
  
##Function tests
  mc <- check.multi.colin(matrix = ksnap,thresh = 0.7)
  length(unique(mc))==length(mc)
  
  rd <- remove.multi.colin(data = as.data.frame(ksnap),rem.list = mc)
  ncol(rd)==length(names(ksnap))-length(mc)
  sum(mc %in% names(rd))==0
  
  
  