read.gen <- function(file.name,model=NULL,type=NULL){
    if(is.null(model)){
        if(grepl('GRO',file.name)){
            model='CROPGRO'
        }else if(grepl('CRP',file.name)){
            model = 'CROPSIM'
            fmt.list = cropsim.fmt()
        }else if(grepl('CER',file.name)){
            model = 'CERES'
            fmt.list = ceres.fmt()
        }else if(grepl('APS',file.name)){
            model = 'NWHEAT'
            fmt.list = nwheat.fmt()
        }else{
            stop('Please provide model name.')
        }
    }
    if(is.null(type)){
        type = substr(file.name,nchar(file.name)-2,nchar(file.name))
        type = tolower(type)
    }
    gen = readLines(file.name)
    first.char = substr(gen,1,1)
    title = gen[first.char=='*']
    comments = gen[first.char=='!']
    hlines = grep('@',gen)
    hlines = hlines[substr(gen[hlines],1,1)!='!']
#    header = lapply(gen[hlines],FUN=function(x){
#                  x=strsplit(x,split='  *')[[1]]
#                  x=gsub('@','',gsub('\\.','',x))
#                  return(x)
#              })
    parameters = vector(length=length(hlines),mode='list')
    for(i in 1:length(hlines)){
        if(i==length(hlines)){
            end = length(gen)
        }else{
            end = hlines[i+1]-1
        }
        check = gen[(hlines[i]+1):end]
        nrows = length(check[substr(check,1,1)!='!'])
        params = read.tier(gen[hlines[i]],hlines[i],nrows,
            file.name=file.name,fmt.list=fmt.list)
        parameters[[i]]=na.omit(params)
    }
    parameters = lapply(parameters,function(x){
        for(c in 1:ncol(x)){
            if(is.character(x[,c])){
                x[,c]=gsub('^ *','',gsub(' *$','',x[,c]))
            }
        }
        return(x)
    })
    gen = list(title=title,comments=comments,parameters=parameters)
    return(gen)
}

