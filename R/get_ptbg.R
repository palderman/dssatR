get.ptbg <- function(nlevel){
    pbg = rep(c('black','white'),12)#c(rep('black',3),rep('white',3),rep('grey',3),rep('white',3))
    pbg = pbg[1:nlevel]
    return(pbg)
}

