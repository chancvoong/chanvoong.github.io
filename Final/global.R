get.ab.info <- function(){

tmp <- read.csv(file='Libraries.csv',header=TRUE,sep=",",stringsAsFactors = FALSE)

dimnames(tmp) <- list(rownames(tmp$X, do.NULL = FALSE, prefix = tmp$X),
                      colnames(tmp, do.NULL = FALSE, prefix = "col"))


as.data.frame(tmp)

}
