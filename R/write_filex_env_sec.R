write.filex.env.sec <- function(sec){
    sec.out <- list()
    sec.out$E <- sprintf('%2i ',sec$E)
    if('POSIXt'%in%class(sec$ODATE)){
        sec.out$ODATE <- format(sec$ODATE,'%y%j')
    }else if(is.numeric(sec$ODATE)|is.integer(sec$ODATE)){
        sec.out$ODATE <- sprintf('%5i',sec$ODATE)
    }else if(is.character(sec$ODATE)){
        sec.out$ODATE <- substr(gsub('^  *','',sec$ODATE),1,5)
    }
    sec.out$ODATE <- paste0(sec.out$ODATE,' ')
    out.list <- c('DAY','RAD','MAX','MIN','RAIN','CO2','DEW','WIND')
    sec.list <- c('DAY','RAD','TX','TM','PRC','CO2','DPT','WND')
    for(i in 1:length(out.list)){
        sec.out[[sprintf('E%3s',out.list[i])]] <-
            paste0(
                substr(gsub('^  *','',sec[[sprintf('%sFAC',sec.list[i])]]),1,1),
                sprintf('%4s',format(sec[[sprintf('%sADJ',sec.list[i])]],scientific=FALSE)),
                ' '
            )
    }
    sec.out$ENVNAME <- sec$ENVNAME
    sec.out <- do.call(paste0,sec.out)
    return(sec.out)
}
