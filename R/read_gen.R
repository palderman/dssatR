read.gen <- function(file.name,model=NA,type=NA){
    if(is.na(model)){
        if(grepl('GRO',file.name)){
            model='CROPGRO'
        }else if(grepl('CRP',file.name)){
            model = 'CROPSIM'
        }else if(grepl('CER',file.name)){
            model = 'CERES'
        }else{
            stop('Please provide model name.')
        }
    }
    if(is.na(type)){
        type = substr(file.name,nchar(file.name)-2,nchar(file.name))
        type = tolower(type)
    }
    gen = readLines(file.name)
    first.char = substr(gen,1,1)
    title = gen[first.char=='*']
    comments = gen[first.char=='!']
    hlines = (1:length(gen))[first.char=='@']
    header = lapply(gen[hlines],FUN=function(x){
                  x=strsplit(x,split='  *')[[1]]
                  x=gsub('@','',gsub('\\.','',x))
                  return(x)
              })
    parameters = vector(length=length(hlines),mode='list')
    for (i in 1:length(hlines)){
        classes = get.fmt(header[[i]],
                      model=rep(model,length(header[[i]])),
                      type=rep(type,length(header[[i]])))
        spaces = nchar(classes)-nchar(gsub(' *','',classes))
        widths = gsub('%-*','',classes)
        widths = gsub('[a-z].*','',widths)
        widths = gsub('\\..*','',widths)
        widths = as.numeric(widths)+spaces
        classes = gsub('.*e.*','numeric',classes)
        classes = gsub('.*s.*','character',classes)
        classes = gsub('.*f.*','numeric',classes)
        classes = gsub('.*d.*','numeric',classes)
        section=gen[(hlines[i]+1):
                     ifelse(i==length(hlines),length(gen),hlines[i+1]-hlines[i])]
        section=section[section!='']
        section=section[substr(section,1,1)!='!']
        params=matrix(ncol=length(widths),nrow=length(section))
        pos1=cumsum(c(1,widths[1:(length(widths)-1)]))
        pos2=cumsum(widths)
        for(c in 1:length(widths)){
            params[,c]=substr(section,pos1[c],pos2[c])
        }
        params=as.data.frame(params)
        for(j in 1:ncol(params)){
            if(classes[j]=='numeric'){
                params[,j]=as.numeric(params[,j])
            }else{
                params[,j]=gsub(' *','',params[,j])
            }
        }
        parameters[[i]]=na.omit(params)
        colnames(parameters[[i]]) = header[[i]]
        econum=gsub('  *','',parameters[[i]]$`ECO#`)
        parameters[[i]]=
            parameters[[i]][econum!='      '&!is.na(econum),]
    }
    gen = list(title=title,comments=comments,parameters=parameters)
    return(gen)
}

