#' @title Compute the Probability Of Detection (POD)
#'
#' @description Compute the Probability Of Detection (POD) in qualitative PCR experiments carried out by a single laboratory.
#'
#' @param x Nominal DNA concentrations (numeric vector)
#' @param lambda The fraction of detected DNA fragments (numeric scalar)
#' @param b correction parameter (numeric scalar)
#' @return The POD function as described in Uhlig et al., 2015
#'
#' @references
#' Uhlig et al. Accred Qual Assur (2015) 20: 75. https://doi.org/10.1007/s00769-015-1112-9
#'
#' @export
#' @name computePOD
#'
#' @examples
#' # the optimal POD
#' computePOD(exp(seq(1, 10, 1)), 1, 1)
#' # some other POD
#' computePOD(exp(seq(1, 10, 1)), 0.5, 1.29)


computePOD <- function(x, lambda=1, b=1){
    if( length(lambda)>1 ){
        warning("Taking only first value of 'lambda'")
        lambda <- lambda[1]
    }
    if( length(b)>1 ){
        warning("Taking only first value of 'b'")
        b <- b[1]
    }
return( 1-exp( (-1) * lambda * x**b ) )
}
