one21.dssat <- function(variable,trno=NULL,sqno=NULL,run=NULL,dap=NULL,new=FALSE,units='',
                        main=NULL,xylim=NULL,increment=NULL,axis.label='',
                        dstat=T,rmse=T,rrmse=F,ceff=F,trt.labels=NULL,bw=F,subplot=NULL,cimmyt=F){
    if(is.null(main)){
        main = variable
    }
    if(!is.null(run)|!is.null(trno)){
        if(is.null(trno)){
            trno = get.trno(run)
        }
        pttyp = get.pch(length(trno))
        ptbg = get.ptbg(length(trno))
        ptcol = get.col(length(trno),bw=T)
        data = NULL
        for (i in 1:length(trno)){
            sbdata = get.obs.pred(variable=variable,trno=trno[i],add=list(pttyp=pttyp[i],ptbg=ptbg[i],ptcol=ptcol[i]))
            data = rbind(data,sbdata)
        }
    }else{
        data = get.obs.pred(variable=variable,sqno=sqno)
    }
    one21(data=data,units=units,main=main,axis.label=axis.label,
            xylim=xylim,increment=increment,
            dstat=dstat,rmse=rmse,rrmse=rrmse,ceff=ceff,
            new=new,trt.labels=trt.labels,bw=bw,subplot=subplot,cimmyt=cimmyt)
}

