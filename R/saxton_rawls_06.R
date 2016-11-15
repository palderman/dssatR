saxton.rawls.06 <- function(sand,clay,bd,coarse,soc){

    sand = sand/100
    clay = clay/100
    coarse = coarse/100
    
# Pedotransfer functions reported in:
#    K.E. Saxton and W.J. Rawls. 2006. Soil Water Characteristic Estimates by
#    Texture and Organic Matter for Hydrologic Solutions. Soil Sci. Soc. Am. J.
#    70:1569-1578
    
    som = soc
    DF = 1

# Moisture Regressions
    
    theta.1500t = -0.024*sand + 0.487*clay + 0.006*som + 0.005*sand*som -
        0.013*clay*som + 0.068*sand*clay + 0.031
    theta.1500 = theta.1500t + 0.14*theta.1500t - 0.02

    theta.33t = -0.251*sand + 0.195*clay + 0.011*som + 0.006*sand*som -
        0.027*clay*som + 0.452*sand*clay + 0.299
    theta.33 = theta.33t + 1.283*(theta.33t)^2 - 0.374*theta.33t - 0.015

    theta.S33t = 0.278*sand + 0.034*clay + 0.022*som - 0.018*sand*som -
        0.027*clay*som - 0.584*sand*clay + 0.078
    theta.S33 = theta.S33t + 0.636*theta.S33t-0.107
    
    psi.et = -21.67*sand - 27.93*clay - 81.97*theta.S33 +
        71.12*sand*theta.S33 + 8.29*clay*theta.S33 +
        14.05*sand*clay + 27.16
        
    psi.e = psi.et + 0.02*psi.et^2 - 0.113*psi.et - 0.70

    theta.S = theta.33 + theta.S33 - 0.097*sand + 0.043

    if(any(is.na(bd))){
        rho.N = bd
        rho.N[is.na(bd)] = (1 - theta.S[is.na(bd)])*2.65
    }else{
        rho.N = bd
    }
    
# density effects
    
    rho.DF = rho.N*DF
    theta.SDF = 1-(rho.DF/2.65)
    theta.33DF = theta.33 - 0.2*(theta.S-theta.SDF)
    theta.S33DF = theta.SDF - theta.33DF

# Moisture-Tension
    B = (log(1500)-log(33))/(log(theta.33)-log(theta.1500))
#    A = exp(log(33)+B*log(theta.33))
#    psi.1500.33 = A*theta.S^{-B}
#    psi.33.psi.e = 33-(theta.S-theta.33)*(33-psi.e)/(theta.S-theta.33)
#    theta.psi.e.0 = theta.S

# Moisture-Conductivity
    lambda = 1/B
    Ks = 1930*(theta.SDF-theta.33DF)^(3-lambda)
#    K.theta = Ks*(theta/theta.S)^(3+2/lambda)
    
# gravel effects

    alpha = rho.N/2.65
    Rv = (alpha*coarse/100)/(1-coarse/100*(1-alpha))
    rho.B = rho.N*(1-Rv) + Rv*2.65
#    PAW.B = PAW*(1-Rv)
    Kb = Ks*(1-coarse/100)/(1-coarse/100*(1-3*alpha/2))

    sat = theta.SDF*(1-Rv)
    dul = theta.33DF*(1-Rv)
    ll = theta.1500*(1-Rv)

    return(invisible(data.frame(SLLL=ll,SDUL=dul,SSAT=sat,SBDM=bd,SSKS=Kb)))
}
