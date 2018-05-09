fill.in.profile <- function(soil.profile){
    silt = soil.profile$layer$SLSI
    clay = soil.profile$layer$SLCL
    coarse = soil.profile$layer$SLCF
    coarse[is.na(coarse)] <- 0
    sand = 100 - silt - clay
    som = soil.profile$layer$SLOC*1.72
    minBD = get.minBD(sand,clay)
    na = is.na(soil.profile$layer$SBDM)
    if(any(na)){
        soil.profile$layer$SBDM[na] = get.sbdm(som,minBD)[na]
    }
    na = is.na(soil.profile$layer$SSAT)
    if(any(na)){
        soil.profile$layer$SSAT[na] = 
            get.ssat(som,soil.profile$layer$SBDM,minBD)[na]
    }
    na = is.na(soil.profile$layer$SSKS)
    if(any(na)){
        soil.profile$layer$SSKS[na] = get.ksat(sand,silt,clay)[na]
    }
    na = is.na(soil.profile$layer$SDUL)
    if(any(na)){
        soil.profile$layer$SDUL[na] = 
            get.sdul.saxton(sand,clay,coarse)[na]
    }
    na = is.na(soil.profile$layer$SLLL)
    if(any(na)){
        soil.profile$layer$SLLL[na] = 
            get.slll.saxton(sand,clay,coarse)[na]
    }
    na = is.na(soil.profile$layer$SRGF)
    if(any(na)){
        soil.profile$layer$SRGF[na] = get.srgf(soil.profile$layer$SLB)[na]
    }
#    na = is.na(soil.profile$layer$SLMH)
#    if(any(na)){
#        soil.profile$layer$SLMH[na] = ''
#    }
#    na = is.na(soil.profile$layer$SADC)
#    if(any(na)){
#        soil.profile$layer$SADC[na] = 0.0
#    }
#    for(i in 1:ncol(soil.profile$layer)){
#        if(any(is.na(soil.profile$layer[,i]))){
#            soil.profile$layer[,i] = -99.0
#        }
#    }
    return(soil.profile)
}

fill.in.profile.06 <- function(soil.profile){
    silt = soil.profile$layer$SLSI
    clay = soil.profile$layer$SLCL
    coarse = soil.profile$layer$SLCF
    coarse[is.na(coarse)] <- 0
    sand = 100 - silt - clay
    som = soil.profile$layer$SLOC*1.72
    minBD = get.minBD(sand,clay)
    na = is.na(soil.profile$layer$SBDM)
    if(any(na)){
        soil.profile$layer$SBDM[na] = get.sbdm(som,minBD)[na]
    }
    saxton.06.est <- saxton.rawls.06(sand,clay,soil.profile$layer$SBDM,
                                     coarse,som)
    na = is.na(soil.profile$layer$SSAT)
    if(any(na)){
        soil.profile$layer$SSAT[na] =
                        get.ssat(som,soil.profile$layer$SBDM,minBD)[na]
    }
    na = is.na(soil.profile$layer$SSKS)
    if(any(na)){
        soil.profile$layer$SSKS[na] = saxton.06.est$SSKS[na]
    }
    na = is.na(soil.profile$layer$SDUL)
    if(any(na)){
        soil.profile$layer$SDUL[na] = saxton.06.est$SDUL[na]
    }
    na = is.na(soil.profile$layer$SLLL)
    if(any(na)){
        soil.profile$layer$SLLL[na] = saxton.06.est$SLLL[na]
    }
    na = is.na(soil.profile$layer$SRGF)
    if(any(na)){
        soil.profile$layer$SRGF[na] = get.srgf(soil.profile$layer$SLB)[na]
    }
#    na = is.na(soil.profile$layer$SLMH)
#    if(any(na)){
#        soil.profile$layer$SLMH[na] = ''
#    }
#    na = is.na(soil.profile$layer$SADC)
#    if(any(na)){
#        soil.profile$layer$SADC[na] = 0.0
#    }
#    for(i in 1:ncol(soil.profile$layer)){
#        if(any(is.na(soil.profile$layer[,i]))){
#            soil.profile$layer[,i] = -99.0
#        }
#    }
    return(soil.profile)
}

