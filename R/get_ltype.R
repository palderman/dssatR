get.ltype <- function(nlevel){
    ltype = c(1:5,1:5)#c(rep(1,2),rep(2,2),rep(3,2),rep(4,2),rep(5,2))
    ltype = ltype[1:nlevel]
    return(ltype)
}

