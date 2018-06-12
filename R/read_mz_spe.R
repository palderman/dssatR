parse.mz.spe.line <- function(line,phos.sec=FALSE){
    tmp <- gsub('^ *','',line)
    tmp <- strsplit(tmp,split='  *')[[1]]
    if(phos.sec){
        char.start <- grep('[[:alpha:]]',tmp)[1]
        vals <- type.convert(tmp[1:(char.start-1)],
                             na.strings=dssat.na.strings())
        n1 <- switch(tmp[char.start],
                     Optimum='Opt',
                     Minimum='Min',
                     Maximum='Max',
                     tmp[char.start])
        n2 <- switch(tmp[char.start+1],
                     Shoot='Shut',
                     Shell='Shel',
                     tmp[char.start+1])
        if(n1%in%c('Min','Max','Opt')){
            if(n2=='Veg'){
                attr(vals,'vnames') <- paste0('N2P',n1)
            }else{
                attr(vals,'vnames') <- paste0('PC',n2,n1)
            }
        }else{
            vals <- as.list(vals)
            n.ind <- char.start:(char.start+length(vals)-1)
            names(vals) <- gsub(',','',tmp[n.ind])
        }
    }else{
        vals <-  type.convert(tmp[-1],na.strings=dssat.na.strings())
        attr(vals,'vnames') <- tmp[1]
    }
    
    return(vals)
}

read.mz.spe <- function(filename){

    require(dplyr)

    tmp <- readLines(filename)

    tmp <- gsub('!.*','',tmp) %>%
        {.[substr(.,1,6)!='*MAIZE']} %>%
        {.[!grepl('^ *$',.)]}

    hlines <- grep('^\\*',tmp)
    begin <- hlines + 1
    end <- c(hlines[-1]-1,length(tmp))

    spe <- vector(mode='list',length=length(hlines))

    for(i in 1:length(spe)){
        spe[[i]] <- lapply(tmp[begin[i]:end[i]],
                           parse.mz.spe.line,
                           phos.sec=grepl('PHOSPHORUS',tmp[hlines[i]]))
        sec.names <- sapply(spe[[i]],attr,which='vnames')
        l <- sapply(spe[[i]],is.list)
        sec.names <- c(sec.names[!l],unlist(lapply(spe[[i]][l],names)))
        spe[[i]] <- c(spe[[i]][!l],do.call(c,spe[[i]][l]))
        names(spe[[i]]) <- sec.names
        spe[[i]] <- lapply(spe[[i]],function(x){
            attr(x,'vnames') <- NULL
            return(x)
        })
    }

    spe <- do.call(c,spe)

    return(spe)
}
