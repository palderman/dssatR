read.dssat <- function(file.name,fmt.list=NULL){
    ftype = tolower(gsub('\\.OUT','',basename(file.name)))
    if(is.null(fmt.list)){
        fmt.list = eval(parse(text=paste('fmt.',ftype,'()',sep='')))
    }
    out = readLines(file.name)
    first.char = substr(out,1,1)
    stars = grep('^\\*',out)
    comments = out[first.char=='!']
    hlines = grep('@',out)
    trts = grep('TREATMENT',out)
    trts = as.integer(substr(out[trts],11,13))
    runs = as.integer(substr(out[stars][substr(out[stars],2,4)=='RUN'],6,8))
    dssat.lines = grep('DSSAT',out)
    tier = vector(length=length(hlines),mode='list')
    for(i in 1:length(hlines)){
        if(i==length(hlines)){
            end = length(out)
        }else{
            end = min(c(stars[stars>hlines[i]][1]-1,hlines[i+1]-1))
        }
        check = out[(hlines[i]+1):end]
        nrows = length(check[substr(check,1,1)!='!'&nchar(check)>0])
        tier[[i]] = read.tier(out[hlines[i]],hlines[i],nrows,
                file.name=file.name,fmt.list=fmt.list)
        if(!'RUN'%in%colnames(tier[[i]])) tier[[i]]$RUN=runs[i]
        if(!'TRNO'%in%colnames(tier[[i]])) tier[[i]]$TRNO=trts[i]
        if('DOY'%in%colnames(tier[[i]])) tier[[i]]=year.doy.to.date(tier[[i]])
    }
    tier = lapply(tier,function(x){
        for(c in 1:ncol(x)){
            if(is.character(x[,c])){
                x[,c]=gsub('^ *','',gsub(' *$','',x[,c]))
            }
        }
        return(x)
    })
    while(length(tier)>1){
        tier[[length(tier)-1]] =
            merge(tier[[length(tier)-1]],tier[[length(tier)]],all=TRUE)
        tier=tier[1:(length(tier)-1)]
    }
    if(is.list(tier)&!is.data.frame(tier)) tier = tier[[1]]
    if(all(c('RUN','TRNO','DATE')%in%colnames(tier))){
        tier = tier[,
            c(c('RUN','TRNO','DATE'),
            colnames(tier)[!colnames(tier)%in%c('RUN','TRNO','DATE')])]
    }
    return(tier)
}

