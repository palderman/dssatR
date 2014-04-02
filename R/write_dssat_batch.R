write.dssat.batch <- function(filex,trno=1,v='v46',sqno=NULL,batchfilename=NULL){
    nlines = ifelse(!is.null(sqno),length(sqno),length(trno))+2
    batchlines = vector(length=nlines,mode='character')
    if(is.null(batchfilename)){
        batchfilename = paste('DSSBATCH.',toupper(v),sep='')
    }
    if(length(sqno)<1) sqno=1
    if(length(filex)==1){
        if(length(trno)>1){
           filex=rep(filex,length(trno))
        }else if(length(sqno)>1){
           filex=rep(filex,length(sqno))
        }
    }
    if(toupper(v)!='DV4'){
        batchlines[1] = '$BATCH'
        batchlines[2] = paste(sprintf('%-92s','@FILEX'),
            paste(sprintf('%7s',c('TRTNO','RP','SQ','OP','CO')),
                collapse='',sep=''),collapse='',sep='')
        batchlines[3:length(batchlines)] = 
            paste(sprintf('%-92s',filex),sprintf('%7i',trno),sprintf('%7i',1),sprintf('%7i',sqno),
                paste(sprintf('%7i',c(1,1)),collapse='',sep=''),sep='')
    }else{
        batchlines[1] = '*BATCH'
        batchlines[2] = '@FILEX       TRTNO'
        batchlines[3:length(batchlines)] = 
            paste(filex,sprintf('%6i',trno),sep='')
    }
    write(batchlines,file=batchfilename)
}

