cf.bd.adjust <- function(soil.profile){
    silt = soil.profile$layer$SLSI
    clay = soil.profile$layer$SLCL
    sand = 100 - silt - clay
    som = soil.profile$layer$SLOC*1.72
    coarse = soil.profile$layer$SLCF
    minBD = get.minBD(sand,clay)
    new.bd = get.sbdm(som,minBD)
    new.bd = new.bd/(1-coarse/100)
    
}

