ritchie87 <- function(soil){
    # PTF from Ritchie et al. (1987)
    OMf = soil$layer$SLOC*1.72
    Df = soil$layer$SBDM
    clay = soil$layer$SLCL
    silt = soil$layer$SLSI
    sand = 100-silt-clay
    RFW = soil$layer$SLCF

    if(any(is.na(Df))){
        stop('Ritchie et al, 1987 can only be used with measured BD')
    }

    LOLm = vector(length=length(sand))
    DULm = vector(length=length(sand))
    DULc = vector(length=length(sand))
    PLEXWm = vector(length=length(sand))
    Dm = vector(length=length(clay))
    SV = vector(length=length(clay))
    RFV = rep(0,length(clay))
    Dmf = (100-OMf)/(100/Df-OMf/0.224)
    
    for(i in 1:length(sand)){
        if(sand[i]>=75){
            LOLm[i] = 18.8-0.168*sand[i]
            PLEXWm[i] = 42.3-0.381*sand[i]
        }else{
            if(silt[i]<70){
                LOLm[i] = 3.62+0.444*clay[i]
            }else{
                LOLm[i] = 5+0.0244*clay[i]^2
            }
            PLEXWm[i] = 10.79+0.05004*silt[i]
        }

        DULm[i] = LOLm[i]+PLEXWm[i]

        if(sand[i]>80){
            Dm[i] = 1.709 - 0.01134*clay[i]
        }else if(sand[i]>=20){
            Dm[i] = 1.118+0.00816*sand[i]+
                clay[i]*(0.00834-0.3606/(100-sand[i]))
        }else{
            Dm[i] = 1.453 - 0.00433*sand[i]
        }

        if(is.na(Df[i])) Df[i] = (OMf[i]*0.224+(100-OMf[i])*Dm)/100

        DULc[i] = DULm[i] -17*(Dm[i]-Df[i])+0.23*OMf[i]

        PLEXWc[i] = PLEXWm[i] + 3.5*(Dm[i]-Df[i])+0.55*OMf[i]

        if(!is.na(RFW[i])){
#######################################################            
# Original equation provided in Ritchie et al 1987 was
# (incorrectly) given as:
#   RFV[i] = 1/(1+2.65*(100-RFW[i]/RFW[i]*Df[i]))
#   SV[i] = 100 - RFV[i]
# Replaced by (assuming density of 2.65 g/cm3 for coarse material):
            Vr = RFW[i]/2.65
            Fs = (100-RFW[i])
            Ds = (Df[i]-RFW[i]/100*2.65)*100/Fs
            Vs = Fs/Ds
            SV[i] = Vs/(Vr+Vs)
########################################################
        }
        sdul[i] = DULc[i]*SV[i]
        slll[i] = sdul[i] - PLEXWc[i]*SV[i]
    }
    
    
    for(i in 1:nrow(soil$layer)){
    }
}
