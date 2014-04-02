getpltdate <- function(filexname,trno=NULL,sqno=NULL){
    fname = ''
    filex = read.filex(filexname)
    fname = filexname
    sec = grep('TNAME',filex)
    subsec = grep('TNAME',filex[[sec]])
    if(is.null(sqno)){
        xtrno = filex[[sec]][[subsec]]$N
        xmthplt = filex[[sec]][[subsec]]$MP[xtrno%in%trno]
    }else{
        xsq = filex[[sec]][[subsec]]$R
        xmthplt = filex[[sec]][[subsec]]$MP[xsq%in%sqno]
    }
    sec = grep('PDATE',filex)
    subsec = grep('PDATE',filex[[sec]])
    pdate = filex[[sec]][[subsec]]$PDATE[filex[[sec]][[subsec]]$P%in%xmthplt]
    if(pdate<20000){
        pdate=pdate+2000000
    }else{
        pdate=pdate+1900000
    }
    pdate=as.POSIXct(as.character(pdate),format='%Y%j')
    return(pdate)
}

