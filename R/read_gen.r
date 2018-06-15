read.gen <- function(file.name,model=NULL,type=NULL){

    require(dplyr)

    if(is.null(type)){
        type = substr(file.name,nchar(file.name)-2,nchar(file.name))
        type = tolower(type)
    }
    if(is.null(model)){
        if(grepl('GRO',file.name)){
            model='CROPGRO'
            fmt.list = cropgro.fmt()
            if(type=='eco'){
                i <- grep('ECONAME',fmt.list)
                names(fmt.list)[i] <- '%17s'
            }
        }else if(grepl('CRP',file.name)){
            model = 'CROPSIM'
            fmt.list = cropsim.fmt()
        }else if(grepl('CER',file.name)){
            model = 'CERES'
            fmt.list = ceres.fmt()
        }else if(grepl('APS',file.name)){
            model = 'NWHEAT'
            fmt.list = nwheat.fmt()
        }else if(grepl('FRM',file.name)){
            model='PRFRM'
        }#else{
         #   stop('Please provide model name.')
        #}
    }
    file = readLines(file.name)
    first.char = substr(file,1,1)
    title = file[first.char=='*']
    comments = file[first.char=='!']
    if(type == "spe"){
        hlines = grep('@',file)
        hlines = hlines[substr(file[hlines],1,1)!='!']
#    header = lapply(file[hlines],FUN=function(x){
#                  x=strsplit(x,split='  *')[[1]]
#                  x=gsub('@','',gsub('\\.','',x))
#                  return(x)
#              })
        parameters = vector(length=length(hlines),mode='list')
        for(i in 1:length(hlines)){
            if(i==length(hlines)){
                end = length(file)
            }else{
                end = hlines[i+1]-1
            }
            check = file[(hlines[i]+1):end]
            nrows = length(check[substr(check,1,1)!='!'&check!=''])
            params = read.tier(file[hlines[i]],hlines[i],nrows,
                               file.name=file.name,fmt.list=fmt.list)
            parameters[[i]]=params
        }
        parameters = lapply(parameters,function(x){
            for(c in 1:ncol(x)){
                if(is.character(x[,c])){
                    x[,c]=gsub('^ *','',gsub(' *$','',x[,c]))
                }
            }
            return(x)
        })
        gen <- do.call(c,lapply(parameters,as.list))
    }else{
        head.line <- grep('@',file,value=TRUE)
        file <- file[first.char != '*' &
                     first.char != '!' &
                     first.char != '@' &
                     gsub('^ *$','',file) != '']

        if( type == 'cul' ){
            cnames <- gsub('^.*( EXP((NO)|(#))) *','',head.line) %>%
                {strsplit(.,'  *')[[1]]} %>%
                {c('VAR#','VARNAME','EXPNO',.)}
            p <- matrix(0,nrow=length(cnames),ncol=2)
            p[1,1] <- 1
            p[1,2] <- 7
            p[2,1] <- 8
            tmp <- regexpr(' EXP((NO)|(#))',head.line)
            p[3,2] <- tmp + attr(tmp,'match.length') - 1
            p[3,1] <- p[3,2] - 1
            p[2,2] <- p[3,1] - 1
            for(i in 4:length(cnames)){
                tmp <- regexpr(paste0('  *',cnames[i],'(( )|($))'),head.line)
                p[i,1] <- tmp
                p[i,2] <- tmp + attr(tmp,'match.length') - 1
                if(substr(head.line,p[i,2],p[i,2])==' ') p[i,2] = p[i,2] - 1
            }
        }else if( type == 'eco' ){
            cnames <- gsub('^.* ECO-*NAME\\.* *','',head.line) %>%
                {strsplit(.,'  *')[[1]]} %>%
                {c('ECO#','ECONAME',.)}
            p <- matrix(0,nrow=length(cnames),ncol=2)
            p[1,1] <- 1
            p[1,2] <- 7
            p[2,1] <- 8
            tmp <- regexpr(' ECONAME\\.*',head.line)
            p[2,2] <- tmp + attr(tmp,'match.length') - 1
            for(i in 3:length(cnames)){
                tmp <- regexpr(paste0('  *',cnames[i],'(( )|($))'),head.line)
                p[i,1] <- tmp
                p[i,2] <- tmp + attr(tmp,'match.length') - 1
                if(substr(head.line,p[i,2],p[i,2])==' ') p[i,2] = p[i,2] - 1
            }
        }
        gen <- vector(length=length(cnames),mode='list')
        for(i in 1:length(cnames)){
            gen[[i]] <- substr(file,p[i,1],p[i,2]) %>%
                type.convert(.,as.is=TRUE,
                             na.strings=c('.',dssat.na.strings()))
            if(is.character(gen[[i]]))
                gen[[i]] <- gsub('(^  *)|(  *$)','',gen[[i]])
        }
        gen <- as.data.frame(gen,stringsAsFactors=FALSE)
        colnames(gen) <- cnames
    }
    attr(gen,'title') <- title
    attr(gen,'comments') <- comments
    return(gen)
}

