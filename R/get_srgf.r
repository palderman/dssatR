get.srgf <- function(slb){
    srgf = vector(length=length(slb),mode='numeric')
    srgf[] = 1
    lyr.center = (slb - c(0,slb[1:(length(slb)-1)]))/2
    srgf[lyr.center > 20] = exp(-0.02*lyr.center[lyr.center > 20])
    return(srgf)
}

