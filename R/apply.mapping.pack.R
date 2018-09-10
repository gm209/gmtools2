#' apply.mapping.pack
#'
#' Function to apply mapping packs created by label encoder and target encoder
#' @param data   a data.frame or coercible object
#' @param mapping.pack  A mapping pack that tells the mapper how to assign values
#' @keywords     apply mapping pack
#' @export
#' @examples
#' 


apply.mapping.pack <- function(data,mapping.pack){

  ## Check input make its coercible
  if(class(data)[1] != 'data.frame') data <- as.data.frame(data)
  
  ## Set up column lists
  cols.to.map <- unlist(lapply(mapping.pack,function(i){i[[1]]}))
  other.cols  <- setdiff(names(data),cols.to.map)
  
  ## Map the columns that need mapping
  other.dat <- data[,other.cols]
  mapped.cols <- as.data.frame(do.call(cbind,lapply(cols.to.map,function(m){
    
    mapping <- mapping.pack[which(cols.to.map == m)][[1]]$mapping
    default <- mapping.pack[which(cols.to.map == m)][[1]]$default
    mapped.val <- mapping[match(x = data[,m],table = mapping[,'in.val']),'out.val']
    mapped.val[is.na(mapped.val)] <- default
    return(mapped.val)
    
  })))
  
  ## rename the columns back to the original
  names(mapped.cols) <- cols.to.map
  
  ## bind the mapped and unmapped cols together
  all.cols <- as.data.frame(cbind(other.dat,mapped.cols))
  return(all.cols)
}