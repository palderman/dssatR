SOILest <- function(soil){
    minBDtab = dget('~/R/source/rawlsbrakensiek1985fig1.r')
    sand = 100-soil$clay-soil$silt
    clay = soil$clay
    som = soil$soc*1.72
    depth = soil$depth
    SLOC = soil$soc
    SLCL = soil$clay
    SLSI = soil$silt
    SBDM = vector(length=nrow(soil),mode='numeric')
    SSAT = vector(length=nrow(soil),mode='numeric')
    SSKS = vector(length=nrow(soil),mode='numeric')
    SLLL = vector(length=nrow(soil),mode='numeric')
    SDUL = vector(length=nrow(soil),mode='numeric')
    SRGF = vector(length=nrow(soil),mode='numeric')
    for(i in 1:nrow(soil)){
        if(i>1) layercenter = depth[i-1]+(depth[i]-depth[i-1])/2 else layercenter = depth[i]/2
        minBD=minBDtab[ceiling(clay[i]/5),ceiling(sand[i]/5)]
        SBDM[i] = get.sbdm(som[i],minBD)
        SSAT[i] = get.ssat(som[i],SBDM[I],minBD)
#        SSAT[i] = 0.332-7.251e-4*(sand[i])+0.1276*log10(clay[i])
#        SSKS[i] = 2.778e-6*exp((12.012-0.0755*sand[i]+(-3.8950+0.03671*sand[i]-0.1103*clay[i]+8.7546e-4*clay[i]^2))/SSAT[i])
        SKSS[i] = get.ksat(sand[i],silt[i],clay[i])
        A = exp(-4.396-0.0715*clay[i]-(4.880e-4)*sand[i]^2-(4.285e-5)*sand[i]*clay[i])*100
        B = -3.140-0.00222*clay[i]^2-(3.484e-5)*sand[i]^2*clay[i]
        SLLL[i]=exp(log(1500/A)/B)
        SDUL[i]=exp(log(33/A)/B)
        if(layercenter>20) SRGF[i] = exp(-0.02*layercenter) else SRGF[i] = 1
    }
    unknown=as.numeric(rep(-99,nrow(soil)))
    SLMH = unknown
    SLCF = unknown
    SLNI = unknown
    SLHW = unknown
    SLHB = unknown
    SCEC = unknown
    SADC = rep(0,nrow(soil))
    output=cbind(depth,SLMH,SLLL,SDUL,SSAT,SRGF,SSKS,SBDM,SLOC,SLCL,SLSI,SLCF,SLNI,SLHW,SLHB,SCEC,SADC)
    colnames(output)=c('@  SLB','  SLMH','  SLLL','  SDUL','  SSAT','  SRGF','  SSKS','  SBDM','  SLOC','  SLCL','  SLSI','  SLCF','  SLNI','  SLHW','  SLHB','  SCEC','  SADC')
    return(output)
}

