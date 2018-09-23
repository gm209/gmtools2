#' make.label.encode.pack
#'
#' Function to create label encoder mapping packs
#' @param features A list of features to create mappings for
#' @param data  A data.frame to label encode
#' @keywords    make label encoder pack
#' @export
#' @examples
#' 

## function defs
make.label.encode.pack  <- function(features,data){
  mapping.pack <- lapply(features,function(f){
    vals <- unique(data[,f])
    mapping <- data.frame(in.val = vals,out.val = seq(1,length(vals)))
    default.val <- mapping[match(x = names(table(data[,f]))[1],table = mapping$in.val),'out.val']
    return(list(variable = f,mapping = mapping,default = default.val))
  })
  return(mapping.pack)
}

## test application
# mapping.pack <- make.label.encode.pack(features = c('gender','area'),data = dataCar)
# label.encoded <- apply.label.encode.pack(data = dataCar,mapping.pack = mapping.pack)

# ## tests
# dim(label.encoded)==dim(dataCar)
# names(table(sapply(label.encoded[,c('gender','area')],class)))=="integer"
# length(unique(dataCar$gender))==length(unique(label.encoded$gender))
# length(unique(dataCar$area))==length(unique(label.encoded$area))

