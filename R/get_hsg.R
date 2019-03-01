get.hsg <- function(ksat,sl.depth){
    first.50 <- (1:length(sl.depth))[sl.depth<=50]
    if(sl.depth[tail(first.50,1)] < 50)
        first.50 <- c(first.50,tail(first.50)+1)
    first.100 <- (1:length(sl.depth))[sl.depth<=100]
    if(sl.depth[tail(first.100,1)] < 100)
        first.100 <- c(first.100,tail(first.100)+1)
    ksat.50 <- min(ksat[first.50])
    ksat.100 <- min(ksat[first.100])
    mx.depth <- max(sl.depth)
    if((ksat.50 > 40*0.36 & mx.depth > 50) ||
       (ksat.100 > 10*0.36 & mx.depth >= 100)){
        hsg <- "A"
    }else if((ksat.50 <= 40*0.36 & ksat.50 > 10*0.36 & mx.depth > 50) ||
             (ksat.100 <= 10*0.36 & ksat.100 > 4*0.36 & mx.depth >= 100)){
        hsg <- "B"
    }else if((ksat.50 <= 10*0.36 & ksat.50 > 1*0.36 & mx.depth > 50) ||
             (ksat.100 <= 4*0.36 & ksat.100 > 0.4*0.36 & mx.depth >= 100)){
        hsg <- "C"
    }else if((ksat.50 <= 1*0.36 | mx.depth <= 50) || 
             (ksat.100 <= 0.4*0.36 & mx.depth >= 100)){
        hsg <- "D"
    }

    return(hsg)
}
