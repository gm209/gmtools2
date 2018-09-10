 #' binnarise
#'
#' Function that creates equal width bins of a numeric input
#' @param x        A vector containing the values to be binned
#' @param w        A vector that contains the row weights. Defaults to NULL.
#' @param nbins    How many bins do you wish to create.
#' @param retLabel Keep string labels from binnarise
#' @keywords binnarise
#' @importFrom dplyr arrange mutate
#' @export
#' @examples
#' 
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

