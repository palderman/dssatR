read.fileat <- function(filename,sqno=NULL){
    classes='numeric'
    filelines=readLines(filename)
    nlines=length(filelines)
    headlines=grep('@TRNO',filelines)
    for (i in 1:length(headlines)){
        cnames=parse.header(filelines[headlines[i]])
        if (i < length(headlines)){
            nrows = headlines[i+1] - headlines[i] - 1
        }else{
            nrows = nlines - headlines[i]
        }
        temp=matrix(nrow=nrows,ncol=length(cnames))
        for(j in 1:length(cnames)){
            temp[,j]=substr(filelines[(headlines[i]+1):(headlines[i]+nrows)],((j-1)*6+1),j*6)
        }
        temp=as.data.frame(temp)
        for(j in 1:length(cnames)){
            if(grepl('DAT',cnames[j])){
                temp[,j]=gsub(' ','',temp[,j])
                if(as.numeric(substr(temp[1,j],1,2))<20){
                    temp[,j]=paste(20,temp[,j],sep='')
                    temp[,j]=as.POSIXct(temp[,j],format='%Y%j')
                }else{
                    temp[,j]=paste(19,temp[,j],sep='')
                    temp[,j]=as.POSIXct(temp[,j],format='%Y%j')
                }
            }else{
                temp[,j]=as.numeric(temp[,j])
            }
        }
        colnames(temp)=cnames
        if(i>1){
            fileat=merge(fileat,temp,by=c('@TRNO','DATE'))
        }else{
            fileat=temp
        }
    }
#    assign(tolower(substr(filename,1,8)),fileat,envir=globalenv())
    colnames(fileat)=gsub('@','',colnames(fileat))
    if(!is.null(sqno)){
        colnames(fileat)[1]='SQNO'
    }
    return(invisible(fileat))
}

