

.COLOR.LOD <- "darkgreen"
.COLOR.SHADE <- "grey"
.COLOR.OBS <- "darkblue"

.smoothLog <- function(x){
    y <- c()
    if( min(x) < 1 ){ y <- c(y, seq( min(x), 1, 0.05 )) }
    if( any(x<2) ){ y <- c(y, seq( 1, 2, 0.1 )) }
    if( any(x<5) ){ y <- c(y, seq( 2, 5, 0.25 )) }
    if( any(x<10) ){ y <- c(y, seq( 5, 10, 0.5 )) }
    if( any(x<100) ){ y <- c(y, seq( 10, 100, 5 )) }
    if( any(x<1000) ){ y <- c(y, seq( 100, 1000, 10 )) }
return( unique(sort(y[ which(y<=max(x)) ])) )
}
