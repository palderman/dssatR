add.periods <- function(cnames){
    cnames = unlist(lapply(cnames, function(name) switch(name,
                                HARM='HARM.........',
                                TNAME='TNAME....................',
                                WSTA='WSTA....',
                                XCRD='...........XCRD',
                                YCRD='...........YCRD',
                                ELEV='.....ELEV',
                                AREA='.............AREA',
                                SLEN='.SLEN',
                                FLWR='.FLWR',
                                SLAS='.SLAS',
                                SNAME='SNAME....................',
                                name)))
    if(any(grepl('TIMPL',cnames))){
        cnames = gsub('\\.','',cnames)
    }
    return(cnames)
}

