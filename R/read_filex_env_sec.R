read.filex.env.sec <- function(lines){

    require(dplyr)

    head.line <- gsub('@',' ',lines[1])
#    init.spc.pos <- regexpr('[[:alpha:]]',head.line) - 1

    cnames <- gsub('^  *','',head.line) %>%
        {strsplit(.,'  *')[[1]]}

    p <- matrix(0,nrow=length(cnames),ncol=2)

    for(i in 1:length(cnames)){
        tmp <- regexpr(cnames[i],head.line)
        p[i,1] <- tmp
        p[i,2] <- tmp + attr(tmp,'match.length') - 1
    }
    p[1,1] <- 1

    i=1
    for(i in 1:nrow(p)){
        if(i < nrow(p)){
            while(any(substr(lines[-1],p[i,2]+1,p[i,2]+1)!=' ') &&
                  p[i,2]+1 < p[i+1,1]){
                      p[i,2] <- p[i,2] + 1
                  }
        }else{
            while(any(substr(lines[-1],p[i,2],p[i,2])!=' ') &&
                  p[i,2] < max(nchar(lines[-1]))){
                      p[i,2] <- p[i,2] + 1
                  }
        }
    }

    env.char <- matrix('',ncol=length(cnames),nrow=length(lines)-1)

    for(i in 1:(length(lines)-1)){
        for(j in 1:length(cnames)){
            env.char[i,j] <- substr(lines[i+1],p[j,1],p[j,2])
        }
    }

    colnames(env.char) <- cnames

    env.out <- data.frame(E=as.integer(env.char[,'E']),
                          ODATE=as.integer(env.char[,'ODATE']),
                          DAYFAC=substr(env.char[,'EDAY'],1,1),
                          DAYADJ=as.numeric(substr(env.char[,'EDAY'],2,100)),
                          RADFAC=substr(env.char[,'ERAD'],1,1),
                          RADADJ=as.numeric(substr(env.char[,'ERAD'],2,100)),
                          TXFAC=substr(env.char[,'EMAX'],1,1),
                          TXADJ=as.numeric(substr(env.char[,'EMAX'],2,100)),
                          TMFAC=substr(env.char[,'EMIN'],1,1),
                          TMADJ=as.numeric(substr(env.char[,'EMIN'],2,100)),
                          PRCFAC=substr(env.char[,'ERAIN'],1,1),
                          PRCADJ=as.numeric(substr(env.char[,'ERAIN'],2,100)),
                          CO2FAC=substr(env.char[,'ECO2'],1,1),
                          CO2ADJ=as.numeric(substr(env.char[,'ECO2'],2,100)),
                          DPTFAC=substr(env.char[,'EDEW'],1,1),
                          DPTADJ=as.numeric(substr(env.char[,'EDEW'],2,100)),
                          WNDFAC=substr(env.char[,'EWIND'],1,1),
                          WNDADJ=as.numeric(substr(env.char[,'EWIND'],2,100)),
                          ENVNAME=env.char[,'ENVNAME'],
                          stringsAsFactors=FALSE
                          )

    return(env.out)
}
