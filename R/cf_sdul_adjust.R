cf.sdul.adjust <- function(soil.profile){
    i=!is.na(soil.profile$layer$SLCF)
    coarse = soil.profile$layer$SLCF[i]
    slll = soil.profile$layer$SLLL[i]
    sdul = soil.profile$layer$SDUL[i]
    sdul = (sdul-slll)*(1-coarse/100)+slll
    soil.profile$layer$SDUL[i] = sdul
    return(soil.profile$SDUL)
}

