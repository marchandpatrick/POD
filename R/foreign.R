#' @title Support Other Platforms
#' @description Export formatted data or code for use by other platforms
#'
#' @param obj A list returned by \code{\link{analyzeSingleLab}}.
#' @param dest The path to write the excel makro to.
#'
#' @details The output of exportQuodata can be used on the QuoData website (<http://quodata.de/content/validation-qualitative-pcr-methods-single-laboratory>).
#'
#' @return Nothing is returned.
#'
#' @name foreign
#' @aliases exportQuodata exportSAS exportExcelMacro
#'
#' @examples
#' x <- cbind(
#'  X=c( 0.1,1,2,5,10,20 ),
#'  S=c( 0,5,6,6,6,6 ),
#'  N=c( 6,6,6,6,6,6 )
#' )
#' obj <- analyzeSingleLab(x=x)
#' exportQuodata(obj)
#' 
#'
NULL

#' @rdname foreign
#' @export
exportQuodata <- function(obj){
    x <- obj$x[, c("X", "S", "N")]

    cat("##################\n")
    for( i in 1:nrow(x) ){
        cat( paste(collapse=" ", x[i,]), "\n", sep="" )
    }
    cat("##################\n")
}

#' @rdname foreign
#' @export
exportSAS <- function(obj){
  x <- obj$x[, c("X", "S", "N")]
    cat("##################\n")
    cat("
data singlelab;
    input Group X S N;
    lX=log(X);
    label X='Log of nominal DNA concentrations';
    datalines;
")
    for( i in 1:nrow(x) ){
        cat( paste(collapse=" ", x[i,]), "\n", sep="" )
    }
cat(";\n")
cat("
proc logistic data=singlelab;
    model S/N= / offset=lX
        link=cloglog
        scale=1;
   title 'logisticfit';
   run;
")
    cat("##################\n")
}

#' @rdname foreign
#' @export
exportExcelMacro <- function(dest){
    dest <- path.expand(dest)
    fpath <- system.file(".", "pod.xlsm", package="POD")
    fpathdest <- file.path(dest, "pod.xlsm")
    cat("Creating a copy of Excel macro (", fpathdest, ")\n", sep="")
return(file.copy(fpath, fpathdest))
}
