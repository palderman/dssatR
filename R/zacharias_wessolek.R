zacharias.wessolek <- function(sand,clay,bd){
    i = sand < 66.5
    
    sat = vector(length=length(sand),mode='numeric')
    dul = vector(length=length(sand),mode='numeric')
    ll = vector(length=length(sand),mode='numeric')
    if(any(i)){
        theta.r = 0
        theta.s = 0.788 + 0.001*clay[i] - 0.263*bd[i]
        alpha = exp(-0.648 + 0.023*sand[i] + 0.044*clay[i] - 3.168*bd[i])
        n.p = 1.392 - 0.418*sand[i]^-0.024 + 1.21*clay[i]^-0.704
        m = 1 - 1/n.p
        sat[i] = theta.s
        dul[i] = theta.r + (theta.s-theta.r)/(1+(alpha*33)^n.p)^m
        ll[i] = theta.r + (theta.s-theta.r)/(1+(alpha*1500)^n.p)^m
    }
    if(any(!i)){
        theta.r = 0
        theta.s = 0.890 + 0.001*clay[!i] - 0.322*bd[!i]
        alpha = exp(-4.197 + 0.013*sand[!i] + 0.076*clay[!i] - 0.276*bd[!i])
        n.p = -2.562 - (7*10^-9)*sand[!i]^(-4.004) + 3.750*clay[!i]^(-0.016)
        m = 1 - 1/n.p
        sat[!i] = theta.s
        dul[!i] = theta.r + (theta.s-theta.r)/(1+(alpha*10)^n.p)^m
        ll[!i] = theta.r + (theta.s-theta.r)/(1+(alpha*1500)^n.p)^m
    }

    return(invisible(data.frame(SLLL=ll,SDUL=dul,SSAT=sat)))
    
}
