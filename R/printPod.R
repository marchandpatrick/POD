#' @title Summary of POD objects
#'
#' @description Generate nicely formatted output of the POD object
#'
#' @param x An object of class 'pod'
#' @param ... Other parameters, not supported yet.
#' @return Nothing is returned.
#'
#' @export
#' @name print.pod
#'
#' @examples
#' x <- cbind(
#'  X=c( 0.1,1,2,5,10,20 ),
#'  S=c( 0,5,6,6,6,6 ),
#'  N=c( 6,6,6,6,6,6 )
#' )
#' obj <- analyzeSingleLab(x=x)
#' print(obj)


print.pod <- function(x, ...){
    obj <- x
  cat("\n#######################\nSummary of POD analysis\n#######################\n\n")
  cat("Data:\n")
  print(round(obj$x,3))
  cat("\n")
  #cat("LOD [95 % CI]: ", sprintf("%s: %s", names(x$const), x$const), "\n\n")
  
  cat("Full GLM:\n")
  fit <- obj$fit.glm.full
  tmp <- round(unlist(fit$LOD), 3)
  cat("\t LOD95 [95% CI]: ", sprintf("%.3f [%.3f,%.3f]", tmp[1], tmp[2], tmp[3]), "\n")
  cat("\t", sprintf("lambda=%.3f, b=%.3f", fit$lambda, fit$b), "\n")
  cat("\tWarnings:\n"); for(w in fit$warn){cat("\t\t", w, "\n", sep="")}
  cat("\n")
  
  cat("Simplified GLM (setting 'b' to fixed value):\n")
  fit <- obj$fit.glm.simple
  tmp <- round(unlist(fit$LOD), 3)
  cat("\t LOD95 [95% CI]: ", sprintf("%.3f [%.3f,%.3f]", tmp[1], tmp[2], tmp[3]), "\n")
  cat("\t", sprintf("lambda=%.3f, b=%.3f", fit$lambda, fit$b), "\n")
  cat("\tWarnings:\n"); for(w in fit$warn){cat("\t\t", w, "\n", sep="")}
  cat("\n")
}
