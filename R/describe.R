#' describe
#'
#' This function creates some high level descriptive metrics for columns in a data.frame. Similar to h2o.describe
#' @param df   A data.frame (or other coercable object) that contains the columns you wish to describe
#' @param cols A list of columns that you wish to describe. If the default value is used all columns will be processed. Defaults to NULL
#' @keywords describe
#' @export
#' @examples
#' 

describe <- function(df,cols=NULL){

  ## Make sure the input is a data.frame
  if(class(df)[1] != 'data.frame') df = as.data.frame(df)

  ## If null cols then make it all columns
  if(is.null(cols)) cols = names(df)

  ## Loop over all columns and check
  meta = do.call(rbind,lapply(cols,function(f){
    ret = data.frame(feature = f,
                     class   = class(df[,f]),
                     nlevels = length(unique(df[,f])),
                     numNA   = sum(is.na(df[,f])),stringsAsFactors = FALSE)
    return(ret)
  }))

  return(meta)

}