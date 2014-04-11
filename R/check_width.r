check.width <- function(number,width){
    nwwd = width
    while(nchar(as.character(number))>width){
        nwwd = nwwd - 1
        number = signif(number,digits = nwwd)
    }
    return(nwwd)
}

