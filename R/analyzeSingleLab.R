#' @title Analyze Single Lab Qualitative PCR Outcomes
#' @description Compute the POD curve and the LOD value to validate a qualitative PCR method of a single laboratory.
#'
#' @param x A matrix or dataframe with columns 'X', 'S' and 'N'.
#' @param X Nominal DNA concentration.
#' @param S Number of successfull PCR outcomes.
#' @param N Total number of PCR experiments.
#' @param qLOD The quantile(s) for the Limit Of Detection (LOD). Divided by \eqn{100} if greater than one.
#' @param b Fixed value for the corrective parameter
#' 
#' @details 
#' According to the suggestion of Uhlig et al. (2015), the corrective parameter \eqn{b} is set to \eqn{1} if it is close to \eqn{1} (simplified fit). However, if sensitivity is better than achievable according to the theoretical POD curve or average amplification probability is higher at higher dilution levels than at lower dilution levels, the \eqn{b} is estimated from the data (full fit). 
#' The value of \eqn{b} can be changed by the user. However, it is not recommended to do so.
#' 
#' @references
#' Uhlig et al. Accred Qual Assur (2015) 20: 75. https://doi.org/10.1007/s00769-015-1112-9
#' 
#' @return A list with following items
#' \describe{
#' \item{x}{Input data plus extra columns}
#' \item{b}{The parameter \eqn{b}, as provided by the user}
#' \item{fit.glm.simple}{Results for the simplified GLM}
#' \item{fit.glm.full}{Results for the full GLM}
#' }
#' where "fit.glm.simple" and "fit.glm.full" are lists with the following parameters
#'  \describe{
#'   \item{b}{The parameter \eqn{b} (estimated from the model)}
#'   \item{lambda}{The parameter \eqn{\lambda} (estimated from the model)}
#'   \item{model}{The generalized linear model (GLM) fit to the data}
#'   \item{lod}{A named vector of LOD values}
#'   \item{lodci}{The 95\% confidence interval of the LOD}
#'   \item{warn}{A character vector containing warnings that appeared during GLM fit}
#'  }
#'
#' @export
#' @name analyzeSingleLab
#' @importFrom stats binomial family glm predict
#'
#' @examples
#' x <- cbind(
#'  X=c(0.1,1,2,5,10,20),
#'  S=c( 0,5,6,6,6,6 ),
#'  N=c( 6,6,6,6,6,6 )
#' )
#' obj <- analyzeSingleLab(x=x)

analyzeSingleLab <- function(x=NULL, X=NULL, S=NULL, N=NULL, qLOD=95, b=1){
    obj <- list()
    .COLNAMES <- c('X', 'S', 'N')
    .b <- b
    if(qLOD[1] > 1){ qLOD <- qLOD/100 } # force quantiles to [0,1]
    qLOD <- sort(qLOD)
    
    # compute curve 95% confidence interval (cip95)
    .getcip95 <- function(GLM, b, v=NULL){
      ilink <- family(GLM)$linkinv
      fit <- se.fit <- NULL
      aux <- v
      if(is.null(v)){ aux <- c(seq(min(x$X), max(x$X), length = 100), x$X, seq(0.5, 1.5, 0.01)) }
      pd <- data.frame(X = sort(unique(aux)), b=b)
      pd <- cbind(pd, predict(GLM, pd, type = 'link', se.fit = TRUE)[1:2])
      pd <- transform(pd, Fitted = ilink(fit), Upper = ilink(fit + (1.96 * se.fit)), Lower = ilink(fit - (1.96 * se.fit)))
    return(pd[, c("X", "Upper", "Lower")])
    }

    # compute LOD and confidence intervals
     .getlod <- function(lambda, b, cip95, mod){
        vLOD <- ( -log(1-qLOD) / lambda )**(1/b)
        names(vLOD) <- as.character(qLOD*100)
        LODLL <- LODUL <- rep(0, length(vLOD))
        for( iq in seq(qLOD) ){
            up <- cip95[which.min( abs(cip95$Upper-qLOD[iq]) )+c(-1,0,1), 1]
            lo <- cip95[which.min( abs(cip95$Lower-qLOD[iq]) )+c(-1,0,1), 1]
            up <- up[!is.na(up)]; lo <- lo[!is.na(lo)]
            cip95up <- .getcip95(mod, b, seq(min(up), max(up), 0.001))
            cip95lo <- .getcip95(mod, b, seq(min(lo), max(lo), 0.001))
            LODUL[iq] <- cip95up[which.min( abs(cip95up$Upper-qLOD[iq]) ), 1]
            LODLL[iq] <- cip95lo[which.min( abs(cip95lo$Lower-qLOD[iq]) ), 1]
        }
        out <- do.call("cbind", list(vLOD, LODLL, LODUL))
        colnames(out) <- c("LOD", "LL", "UL")
    return( out )
    }

    .catchWarn <- function(w){WARN <<- append(WARN, conditionMessage(w)); invokeRestart("muffleWarning");}

    if( is.null(x) ){
        if( is.null(X) || is.null(S) || is.null(N) ){
            warning("Assign valid value to 'x' or 'X' and 'S' and 'N'.")
            return(obj)
        }else{
            if( length(X) != length(S) ){
                warning("'X' and 'S' has to be of same length.")
                return(obj)
            }
            if( (length(N)!=1 && length(N)!=length(X)) ){
                warning("length(N)==1 or length(N)=length(X).")
                return(obj)
            }
            x <- cbind(X=X, S=S, N=N)
        }
    }else{
        if( !is.matrix(x) && !is.data.frame(x) ){
            warning("'x' has to be matrix or data.frame.")
            return(obj)
        }
        if( length(intersect(colnames(x), .COLNAMES))<3 ){
            warning("'x' must have columns 'X', 'S' and 'N'.")
            return(obj)
        }
    }

    x <- as.data.frame(x[, .COLNAMES])
    x$Y <- x$S/x$N

    # fit via glm (binomial, cloglog)
    # catch warning 'glm.fit: algorithm did not converge'
    # catch warning 'glm.fit: fitted probabilities numerically 0 or 1 occurred'
    expWarnings <- c('glm.fit: algorithm did not converge', 'glm.fit: fitted probabilities numerically 0 or 1 occurred')
    ## simplified fit
    WARN <- NULL
    GLMSIMPLE <- withCallingHandlers( glm( Y ~ offset(b*log(X)), data=x, family=binomial(link="cloglog"), weights=N ), warning=function(w){.catchWarn(w)} )
    sGLMSIMPLE <- summary(GLMSIMPLE)$coefficients
    lambda <- exp(sGLMSIMPLE[1,1])
    cip95 <- .getcip95(GLMSIMPLE, b)
    FIT.SIMPLE <- list( b=.b, lambda=lambda, model=GLMSIMPLE, LOD=.getlod(lambda, b, cip95, GLMSIMPLE), cip95=cip95, warn=WARN )
    if( length(setdiff( WARN, expWarnings )) ){ warning(WARN) }
    
    ## full fit
    WARN <- NULL
    GLMFULL <- withCallingHandlers( glm( Y ~ log(X), data=x, family=binomial(link="cloglog"), weights=N ), warning=function(w){.catchWarn(w)} )
    sGLMFULL <- summary(GLMFULL)$coefficients
    lambda <- exp(sGLMFULL[1,1])
    b <- sGLMFULL[2,1]
    cip95 <- .getcip95(GLMFULL, b)
    FIT.FULL <- list( b=b, lambda=lambda, model=GLMFULL, LOD=.getlod(lambda, b, cip95, GLMFULL), cip95=cip95, warn=WARN )
    if( length(setdiff( WARN, expWarnings )) ){ warning(WARN) }
    
    obj[["x"]] <- x
    obj[["b"]] <- .b
    obj[["fit.glm.simple"]] <- FIT.SIMPLE
    obj[["fit.glm.full"]] <- FIT.FULL
    class(obj) <- "pod"
return(obj)
}
