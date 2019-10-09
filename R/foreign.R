#' @title Support Other Platforms
#' @description Export formatted data or code for use by other platforms
#'
#' @param obj A list returned by \code{\link{analyzeSingleLab}}.
#' @param dest The path to write the excel macro to.
#'
#' @details The output of exportQuodata can be used on the QuoData website (\url{http://quodata.de/content/validation-qualitative-pcr-methods-single-laboratory}).
#' Function \code{exportExcelMacro()} creates an Excel macro in the specified directory. Existing files (older versions for instance) will not be overwritten! To create the macro in the current directory, set destination to \code{""} (Windows) or \code{"."} (Linux), respectively.
#'
#' @seealso \code{\link{getwd}}, \code{\link{dir}}
#'
#' @return Nothing is returned by \code{exportQuodata()} and \code{exportSAS()}. Function \code{exportExcelMacro()} returns a boolean, \code{FALSE} if a file with name 'pod.xlsm' already exists, \code{TRUE} otherwise.
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
    if( missing( dest ) ){
        message(paste0("No destination specified, writing into current working directory (obtained from 'getwd()')."))
        dest <- getwd()
    }
    if( ! file.exists( dest ) ){
        warning(paste0("Unable to write into ", dest, ", directory does not exist."))
        return(FALSE)
    }

    dest <- path.expand(dest)
    fpath <- system.file(".", "pod.xlsm", package="POD")
    fpathdest <- file.path(dest, "pod.xlsm")
    cat("Creating a copy of Excel macro (", fpathdest, ")\n", sep="")
return(file.copy(fpath, fpathdest))
}

