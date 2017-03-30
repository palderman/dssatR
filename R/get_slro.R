get.slro <- function(hyd.group,slope){

    hyd.group <- substr(hyd.group,1,1)

    if(slope<0|!hyd.group%in%c('A','B','C','D')) return(NA)

    slro <- matrix(c(61,73,81,84,
                     64,76,84,87,
                     68,80,88,91,
                     71,83,91,94),
                   byrow=TRUE,
                   ncol=4)

    if(slope>0&slope<=2){
        i=1
    }else if(slope>2&slope<=5){
        i=2
    }else if(slope>5&slope<=10){
        i=3
    }else if(slope>10){
        i=4
    }
    j <- switch(hyd.group,
                A=1,
                B=2,
                C=3,
                D=4)
    return(slro[i,j])
}
