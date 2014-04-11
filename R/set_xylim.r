set.xylim <- function(xylim,data){
    if(is.null(xylim)){
        xylim = c(NA,NA)
    }else if(length(xylim)<2){
        xylim = c(NA,xylim)
    }
    if(is.na(xylim[2])){
        xylim[2]=signif(max(data),digits=1)
        if(xylim[2]<max(data)) xylim[2]=signif(max(data),digits=2)
        if(xylim[2]<max(data)) xylim[2] = xylim[2] + 10^(log10(signif(max(data),digits=1))-1)
    }
    if(is.na(xylim[1])) {
        ord.mag = 10^floor(log10(xylim[2]))
        xylim[1]= min(data)%/%ord.mag*ord.mag
    }
    return(xylim)
}

