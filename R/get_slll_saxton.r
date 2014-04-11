get.slll.saxton <- function(sand,clay,coarse){
    A = exp(-4.396-0.0715*clay-(4.880e-4)*sand^2-(4.285e-5)*sand*clay)*100
    B = -3.140-0.00222*clay^2-(3.484e-5)*sand^2*clay
    slll=exp(log(1500/A)/B)
#    na=is.na(coarse)
#    if(any(!na)){
#        slll[!na] = slll[!na]*(1-coarse[!na]/100)
#    }
    return(slll)
}

