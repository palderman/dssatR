run.dssat <- function(filex=NULL,trno=1,model='cropgro',clarg='b',v='V46',sqno=NULL,load=T,clear.previous=T){
    v=toupper(v)
    if(filex[1]=='gh'){
#        system("sed -i 's/UFGA8401.PPX/SAWA1001.PPX/g' DSSBATCH.V45")
#        system("sed -i 's/ITIN0301.PPX/SAWA1001.PPX/g' DSSBATCH.V45")
        filex = rep('SAWA1001.PPX',length(trno))
        read.filet('SAWA1001.PPT')
    }
    if(filex[1]=='ga'){
#        system("sed -i 's/SAWA1001.PPX/UFGA8401.PPX/g' DSSBATCH.V45")
#        system("sed -i 's/ITIN0301.PPX/UFGA8401.PPX/g' DSSBATCH.V45")
        filex = rep('UFGA8401.PPX',length(trno))
        read.filet('UFGA8401.PPT')
        read.filea('UFGA8401.PPA')
    }
    if(filex[1]=='in'){
        filex = rep('ITIN0301.PPX',length(trno))
        read.filet('ITIN0301.PPT')
        read.filea('ITIN0301.PPA')
    }
    if(length(filex)==1&length(trno)>1) filex = rep(filex,length(trno))
    if(length(list.files(pattern='*.OUT'))>0) system('rm *.OUT')
    if(clear.previous&&length(list.files(pattern='saved.*'))>0){
        system('rm saved.*')
    }
    if(model=='forage'){
        if (!is.null(filex)) write.dssat.batch(filex,trno,v='v4',sqno=sqno)
        if (clarg=='b') clarg='B DSSBATCH.DV4'
        system(paste('./DSCSM030.EXE ',clarg,' >/dev/null',sep=' '))
    }else{
        if (!is.null(filex)) write.dssat.batch(filex,trno,v=v,sqno=sqno)
        if (clarg=='b') clarg=paste('B DSSBATCH.',v,sep='')
        if (!is.null(sqno)) clarg=paste('Q DSSBATCH.',v,sep='')
        v = substr(v,2,3)
        system(paste('~/DSSAT',v,'/dscsm0',v,'.exe ',clarg,' >/dev/null',sep=''))
    }
    if(load) load.dssat(model=model,sqno=sqno)
}

