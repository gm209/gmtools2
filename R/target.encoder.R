#' make.label.encode.pack
#'
#' Function to create label encoder mapping packs
#' @param features A list of features to create mappings for
#' @param data  A data.frame to target encode
#' @param y     The label column to encode
#' @param w     A vector of weights, defaults to NULL
#' @param min_samples_per_leaf an scalar representing the smallest credible class
#' @param smoothing a scalar showing the degree of smoothing to apply to the data
#' @param noise_level random noise level to be injected (UNIMPLEMENTED) 
#' @keywords    make label encoder pack
#' @import data.table
#' @export
#' @examples
#' 

## function def
make.target.encode.pack  <- function(features,data,y,w=NULL,min_samples_per_leaf=1,smoothing=1,noise_level=0){
  mapping.pack <- lapply(features,function(f){
    
    ## Create df working
    if(is.null(w)) w <- rep(1,nrow(data)) else w <- data[,w]
    
    dfWorking <- data.frame(feat = data[,f],y = data[,y],w = w)
    
    ## Aggregate table  
    agg <- data.table(dfWorking)[ , list(wmean.target = weighted.mean(x = y,w = w),sum.weight = sum(w)) , by = feat]
    prior <- weighted.mean(x = dfWorking$y,w = dfWorking$w)
    smoothing <- 1 / (1 + exp(-(agg$sum.weight - min_samples_per_leaf)/smoothing))
    agg$out.val <- prior*(1 - smoothing) + agg$wmean.target * smoothing
    mapping <- data.frame(in.val = agg$f,out.val = agg$out.val)
    
    return(list(variable = f,mapping = mapping,default = prior))
  })
  return(mapping.pack)
}

# ## run test data
# mapping.pack <- make.target.encode.pack(features = c('gender','area'),data = dataCar,y = "numclaims")
# target.encoded <- apply.mapping.pack(data = dataCar,mapping.pack = mapping.pack)

# apply.mapping.pack(data = data.frame(gender = "Q",area = "Z",foo = "gany"),mapping.pack = mapping.pack)

# ## tests
# dim(label.encoded)==dim(dataCar)
# names(table(sapply(label.encoded[,c('gender','area')],class)))=="integer"
# length(unique(dataCar$gender))==length(unique(label.encoded$gender))
# length(unique(dataCar$area))==length(unique(label.encoded$area))

