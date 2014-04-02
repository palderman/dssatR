get.increment <- function(xylim){
    increment = NULL
    for(i in 4:2){
        if((xylim[2]-xylim[1])%/%i==(xylim[2]-xylim[1])/i){
            increment = (xylim[2]-xylim[1])/i
            break
        }
    }
    if(is.null(increment)) increment = (xylim[2]-xylim[1])/4
    return(increment)
}

