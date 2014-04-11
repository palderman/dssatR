get.ssat <- function(som,sbdm,minBD,coarse){
    ssat = 0.95*(1/sbdm-1/(som/0.224+(100-som)/minBD))
#    na=is.na(coarse)
#    if(any(!na)){
#        ssat[!na] = ssat[!na]*(1-coarse[!na]/100)
#    }
    return(ssat)
}

