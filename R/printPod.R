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
#' # or just
#' obj
#'
#' obj <- analyzeSingleLab(x=x, qLOD=c(50, 70, 95))
#' obj


print.pod <- function(x, ...){
    if(class(x) != "pod"){
        warning("Variable 'obj' is not of class 'pod'. Did you use 'analyzeSingleLab' to generate 'obj'?")
        return(NULL)
    }

    obj <- x
    .shift <- function(k){ return( c( " ", "  ", "   ", "    ", "     ", "      " )[k] ) }
    
    cat("\n#######################\nSummary of POD analysis\n#######################\n\n")
    cat("Data:\n")
    xtmp <- round(obj$x,3); colnames(xtmp)[4] <- "ROD"
    print(xtmp)
    cat("\n")
    
    HEAD <- c("Full GLM", "Simplified GLM (setting 'b' to fixed value)")
    OBJ <- list(obj$fit.glm.full, obj$fit.glm.simple)
    for( i in 1:2 ){
        cat(HEAD[i], ":\n", sep="")
        fit <- OBJ[[i]]
        tmp <- round(unlist(fit$LOD), 3)
        k <- 2
        if(nrow(tmp)>1){
            k <- 4
            cat(.shift(2), "LOD:\n", sep="")
        }
        for(rn in rownames(tmp)){
            tmp0 <- tmp[rn,]
            cat(.shift(k), "LOD", rn, " [95% CI]: ", sprintf("%.3f [%.3f;%.3f]", tmp0[1], tmp0[2], tmp0[3]), "\n", sep="")
        }

        cat(.shift(2), sprintf("lambda=%.3f; b=%.3f", fit$lambda, fit$b), "\n", sep="")
        cat(.shift(2), "Warnings:\n", sep=""); for(w in fit$warn){cat(.shift(4), w, "\n", sep="")}
        cat("\n")
    }
     cat("#######################\n\n")
}

