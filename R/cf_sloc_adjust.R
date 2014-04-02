cf.sloc.adjust <- function(soil.profile){
    i=!is.na(soil.profile$layer$SLCF)
    coarse = soil.profile$layer$SLCF[i]
    sloc = soil.profile$layer$SLCF[i]
    sloc = sloc*(1-coarse/100)
    soil.profile$layer$SLOC[i]=sloc
}

