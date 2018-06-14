parse.cropgro.spe.line <- function(line,sec.name='',drop.comments=TRUE){

    require(dplyr)

    tmp <- gsub('^ *','',line)
    vnames <- NULL

    if(grepl('PARTITIONING',sec.name)&&
             grepl('(XLEAF)|(YLEAF)|(YSTEM)',tmp)){
        p <- regexpr('(XLEAF)|(YLEAF)|(YSTEM)',tmp)
        vnames <- substr(tmp,p,p+attr(p,'match.length')-1)
        vals <- strsplit(substr(tmp,1,p[1]-1),'  *')[[1]]
        vals <- list(as.numeric(vals))
        names(vals) <- vnames
    }else if(grepl('ROOT',sec.name)){
        n.ind <- regexpr('[[:alpha:]]',tmp)
        name.list <- substr(tmp,n.ind,10000)
        vnames <- strsplit(name.list,' *((,)|( )) *')[[1]]
        if(grepl('XRTFAC',name.list)){
            rt.mat <- as.numeric(strsplit(substr(tmp,1,n.ind-1),'  *')[[1]])
            rt.mat <- matrix(rt.mat,ncol=2,byrow=TRUE)
            vals <- list(rt.mat[,1],rt.mat[,2])
            vnames <- c('XRTFAC','YRTFAC')
        }else{
            vals <- as.list(strsplit(substr(tmp,1,n.ind-1),'  *')[[1]])
            vals <- lapply(vals,function(v){
                v.out <- type.convert(v)
                if(is.factor(v.out)){
                    return(v)
                }else{
                    return(v.out)
                }
            })
        }
        names(vals) <- vnames
    }else{

        p <- regexpr(' *\\(',tmp) - 1

        if(p>0){
            p[2] <- p[1]
            p[1] <- tail(gregexpr(' ',substr(tmp,1,p[2]))[[1]],1) + 1
            vnames[1] <- substr(tmp,p[1],p[2])
            vals <- strsplit(substr(tmp,1,p[1]-1),'  *')[[1]]
            if(grepl('TYP',tmp)){
                p <- regexpr('TYP',tmp)
                vnames[2] <- substr(tmp,p,p+5)
                vals <- list(as.numeric(head(vals,-1)),tail(vals,1))
            }else{
                vals <- list(as.numeric(vals))
            }
            names(vals) <- vnames
        }else{
            found <- gregexpr('  *',tmp)[[1]]
            ml <- attr(found,'match.length')
            n.ind <- tail(found[ml==max(ml)]+ml[ml==max(ml)],1)
            name.list <- substr(tmp,n.ind,1000) %>%
                gsub('(^ *)|( *$)|(;.*)','',.)

            vnames <- strsplit(name.list,' *((,)|( )) *')[[1]]
            vals <- as.list(strsplit(substr(tmp,1,n.ind-1),'  *')[[1]])
            vals <- lapply(vals,function(v){
                v.out <- type.convert(v)
                if(is.factor(v.out)){
                    return(v)
                }else{
                    return(v.out)
                }
            })
            names(vals) <- vnames
        }
    }

    return(vals)
}

parse.phos.sec <- function(lines){
    omm <- grep('(Optimum)|(Minimum)|(Maximum)',lines)
    n.ind <- regexpr('(Optimum)|(Minimum)|(Maximum)',lines[omm])
    n1 <- substr(lines[omm],n.ind,n.ind+2)
    n2.ind <- regexpr('(Leaf)|(Shoot)|(Stem)|(Root)|(Shell)|(Seed)|(Veg)',lines[omm])
    n2.ml <- attr(n2.ind,'match.length')
    n2 <- sapply(substr(lines[omm],n2.ind,n2.ind+n2.ml-1),function(x){
        n.out <- switch(x,
                        Shoot='Shut',
                        Shell='Shel',
                        x)
        return(n.out)
    })
    omm.names <- ifelse(n2=='Veg',paste0('N2P',n1),paste0('PC',n2,n1))
    omm.vals <- gsub('(^  *)|(  *$)','',substr(lines[omm],1,n.ind-1)) %>%
        strsplit(.,'  *') %>%
        lapply(.,type.convert,na.strings=dssat.na.strings())
    names(omm.vals) <- omm.names

    tmp <- gsub('SRATPHOTO','         SRATPHOTO',lines[-omm])
    tmp <- gsub(' - .*','',tmp)

    phos.out <- c(list(omm.vals),lapply(tmp,parse.cropgro.spe.line))

    return(phos.out)
        char.start <- grep('[[:alpha:]]',tmp)[1]
        vals <- type.convert(tmp[1:(char.start-1)],
                             na.strings=dssat.na.strings())
        if(n1%in%c('Min','Max','Opt')){
            if(n2=='Veg'){
                attr(vals,'name') <- paste0('N2P',n1)
            }else{
                attr(vals,'name') <- paste0('PC',n2,n1)
            }
        }else{
            vals <- as.list(vals)
            n.ind <- char.start:(char.start+length(vals)-1)
            names(vals) <- gsub(',','',tmp[n.ind])
        }
}

parse.lf.sen.sec <- function(lines){

    tmp <- gsub('(^ *)|(\\(.*)','',lines)
    tmp <- tmp[tmp!='']

    xline <- grep('XSTAGE',tmp) + 1
    vals <- as.numeric(strsplit(tmp[xline],'  *')[[1]])
    x <- list(XSTAGE=vals[1:(length(vals)/2)],
              XSENMX=vals[(length(vals)/2):length(vals)])

    sline <- grep('SENPOR',tmp) + 1
    vals <- as.numeric(strsplit(tmp[sline],'  *')[[1]])
    s <- list(SENPOR=vals[1:(length(vals)/2)],
              SENMAX=vals[(length(vals)/2):length(vals)])

    drp <- c(xline-1,xline,sline-1,sline)

    sec <- lapply(tmp[-drp],parse.cropgro.spe.line)

    return(c(sec,list(x),list(s)))

}

parse.phen.sec <- function(lines){

    require(dplyr)

    temps <- grep('DEVELOPMENT',lines)
    t.card <- lapply(lines[temps],parse.cropgro.spe.line) %>%
        lapply(.,unlist) %>%
        do.call(rbind,.) %>%
        {list(TB=.[,1],
             TO1=.[,2],
             TO2=.[,3],
             TM=.[,4])}

    tmp <- lines[-temps] %>%
        gsub('^  *','',.) %>%
        substr(.,1,37) %>%
        {strsplit(.,'  *')} %>%
        do.call(rbind,.) %>%
        as.data.frame(.,stringsAsFactors=FALSE)

    colnames(tmp) <- c('J','NPRIOR','DLTYP','CTMP','TSELC','WSENP','NSENP','PSENP')

    for(n in c('J','NPRIOR','DLTYP','CTMP','TSELC','WSENP','NSENP','PSENP')){
        if(!n%in%c('DLTYP','CTMP')) tmp[[n]] <- as.numeric(tmp[[n]])
        tmp[[n]] <- tmp[[n]][order(tmp$J)]
    }

    tmp$J <- tmp$J[order(tmp$J)]

    tmp <- merge(tmp,data.frame(J=1:max(tmp$J)),all=TRUE) %>%
        dplyr::select(.,-J) %>%
        as.list(.)

    p.out <- list(t.card,tmp)

    return(p.out)
}

read.cropgro.spe <- function(filename,s=1){

    require(dplyr)

    tmp <- readLines(filename)

    tmp <- tmp[!grepl('SPECIES COEFFICIENTS',tmp)] %>%
        {.[!grepl('^ *$',.)]}

    hlines <- grep('^!*\\*',tmp)
    begin <- hlines + 1
    end <- c(hlines[-1]-1,length(tmp))

    sec.names <- gsub('^!*\\*','',tmp[hlines])

    sec <- list()
    for(i in 1:length(begin)){
        sec[[i]] <- tmp[begin[i]:end[i]]
        if(!grepl('LEAF SENESCENCE',sec.names[i]) &&
           !grepl('ROOT',sec.names[i]))
            sec[[i]] <- gsub(' *!.*','',sec[[i]])
        sec[[i]] <- sec[[i]][sec[[i]]!='']
    }

    spe <- vector(mode='list',length=length(sec.names))

    for(i in 1:length(spe)){
        if(grepl('LEAF SENESCENCE',sec.names[i])){
            spe[[i]] <- parse.lf.sen.sec(sec[[i]])
        }else if(grepl('PHENOLOGY',sec.names[i])){
            spe[[i]] <- parse.phen.sec(sec[[i]])
        }else if(grepl('PHOSPHORUS',sec.names[i])){
            spe[[i]] <- parse.phos.sec(sec[[i]])
        }else{
            spe[[i]] <- lapply(sec[[i]],
                               parse.cropgro.spe.line,
                               sec.name=sec.names[i])
        }
        spe[[i]] <- do.call(c,spe[[i]])
    }

    spe <- do.call(c,spe)

    names(spe) <- gsub('CH20','CH2O',names(spe))

    return(spe)

}
