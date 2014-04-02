read.evaluate <- function(fileout,model='cropgro'){
    filelines = readLines(fileout)
    headers = grep('@RUN',filelines)
    for (i in 1:length(headers)){
        if(i<length(headers)){
            end = headers[i+1]-1
        }else{
            end = length(filelines)
        }
        temp = read.table(textConnection(filelines[headers[i]:end]),header=T,comment.char="!",na.strings='-99')
        if(exists('pdata')){
            pdata=merge(pdata,temp,all=T)
        }else{
            pdata=temp
        }
    }
    assign(substr(fileout,start=1,stop=nchar(fileout)-4),pdata,envir=parent.frame())
}

load.dssat = function(model='cropgro',sqno=NULL,type='OUT'){
    files = c('PlantGro','PlantN','PlantC','PlantP','ETPhot','Weather','SoilWat','SoilNi','SoilPi','Dormancy','SOMLITC','SOMLITN')
    dirfiles = list.files()
    if(exists('output')) rm('output',inherits=T)
    for (i in 1:length(files)) {
        temp = NULL
        if (type=='OUT'&&any(dirfiles==paste(files[i],'.',type,sep=''))) {
            read.dssat(fileout=paste(files[i],'.OUT',sep=''),
                       model,sqno=sqno)
            temp = eval(as.name(files[i]))
        }else if(type=='R'&&any(grepl(tolower(files[i]),dirfiles))){
#            read.dssat.binary(files[i])
            if(!is.null(sqno)) colnames(temp)[1] = 'SQNO'
        }
        if(!is.null(temp)&&!exists('output')){
            assign('output',temp)
        }else if(!is.null(temp)){
            if(is.null(sqno)){
                temp = temp[,!colnames(temp)%in%colnames(output)|
                    colnames(temp)%in%c('TRNO','DATE')]
                output = merge(output,temp,by=c('TRNO','DATE'),sort=T,all=T)
            }else{
                temp = temp[,!colnames(temp)%in%colnames(output)|
                    colnames(temp)%in%'DATE']
                output = merge(output,temp,by='DATE',sort=T,all=T)
            }
        }
    }
    if ('TRNO'%in%colnames(output)){
        output=output[order(output$TRNO,output$DATE),]
    }else{
        output=output[order(output$DATE),]
    }
    assign('output',output,envir=globalenv())
}

