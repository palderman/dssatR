get.pch <- function(nlevel){
    pch = c(21,21,22,22,23,23)#rep(c(21,22,24),2)
    pch = pch[1:nlevel]
    return(pch)
}

