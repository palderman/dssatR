get.col <- function(nlevel,bw){
    if(bw){
        plcol = rep('black',nlevel)#rep('black',ceiling(nlevel/4)),rep('gray60',ceiling(nlevel/4))
    }else{
        plcol = 1:nlevel
    }
    plcol = plcol[1:nlevel]
    return(plcol)
}

