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

