dssat.plot <- function(variable,trno=NULL,sqno=NULL,run=NULL,new=FALSE,file=NULL,bw=FALSE,
                    xlim=NULL,ylim=NULL,xincrement=NULL,yincrement=NULL,
                    xax.lim=NULL,yax.lim=NULL,
                    xaxis.lab=NULL,yaxis.lab=NULL,use.dap=F,filexname=NULL,
                    xlab=NULL,ylab=NULL,trt.lab=NULL,include.legend=T,las=0,
                    lgdpos='topleft',statpos='bottomright',dstat=FALSE,ceff=FALSE,rmse=FALSE,rrmse=FALSE,
                    subplot=NULL,cimmyt=F){
    output=get('output')
    if(length(variable)==1){
        if (variable=='alldm') variable = c('CWAD','LWAD','SWAD','PWAD','GWAD')
    }
    if (exists('filet')){
        filet=get('filet')
    }else{
        plotfilet=rep(0,length(trno))%*%t(rep(0,length(variable)))
        filet=NULL
        warning('File T has not been loaded.')
    }
    if ('SQNO'%in%colnames(output)){
        TRSQ = 'SQNO'
        trsq = sqno
    }else{
        TRSQ = 'TRNO'
        trsq = trno
    }
    if(is.null(trsq))
        trsq = as.integer(as.character(levels(as.factor(output[,TRSQ]))))
    output = output[,c(TRSQ,'DATE',variable)]
    if(!is.null(xlim)){
        if(is.character(xlim)) xlim = as.POSIXct(xlim)
        output = output[output$DATE>=xlim[1]&
                        output$DATE<=xlim[2],]
        filet = filet[filet$DATE>=xlim[1]&
                      filet$DATE<=xlim[2],]
    }
    if(length(variable)>1){
        ord.adj = unlist(lapply(output[,variable],
            function(x){ceiling(log10(max(x)))}))
        ord.adj = 10^abs(ord.adj-max(ord.adj))
        for(i in 1:length(variable)){
            if(ord.adj[i]>1){
                output[,variable[i]]=output[,variable[i]]*ord.adj[i]
                colnames(output)[colnames(output)==variable[i]]=
                    sprintf('%sx%i',variable[i],ord.adj[i])
                variable[i]=sprintf('%sx%i',variable[i],ord.adj[i])
            }
        }
    }
    if(!is.null(xlim)&is.character(xlim)) xlim = as.POSIXct(xlim,format='%Y%j')
    if(!exists('plotfilet')){
        plotfilet = trsq%in%filet[,TRSQ]%*%t(variable%in%colnames(filet))==1
    }
    for(i in 1:length(trsq)){
        if(i==1){

            if(use.dap&&TRSQ=='TRNO'){
                pdate = getpltdate(filexname,trno=trsq[1])
            }else if(use.dap){
                pdate = getpltdate(filexname,sqno=trsq[1])
            }
            pdata = output[output[,TRSQ]%in%trsq,colnames(output)%in%c(TRSQ,'DATE',variable)]
            if(any(plotfilet)){
                odata = filet[filet[,TRSQ]%in%trsq,colnames(filet)%in%c(TRSQ,'DATE',variable)]
                ylim = set.xylim(ylim,na.omit(c(unlist(pdata[,colnames(pdata)%in%variable]),
                                                unlist(odata[,colnames(odata)%in%variable]))))
                if(is.null(xlim)){
                    xlim = c(min(c(unlist(pdata[,'DATE']),unlist(odata[,'DATE'])),na.rm=T),max(c(unlist(pdata[,'DATE']),unlist(odata[,'DATE'])),na.rm=T))
                    if(use.dap){
                        xlim[1] = pdate
                    }
                }
            }else{
                ylim = set.xylim(ylim,na.omit(unlist(pdata[,colnames(pdata)%in%variable])))
                if(is.null(xlim)){
                    xlim = c(min(unlist(pdata[,'DATE']),na.rm=T),max(unlist(pdata[,'DATE']),na.rm=T))
                }
            }
            if(is.null(xax.lim)){
                xax.lim = xlim
            }else if(length(xax.lim)<2){
                if(is.character(xax.lim)){
                    xax.lim = as.POSIXct(xax.lim,format='%Y%j')
                }
                xax.lim = c(xlim[1],xax.lim)
            }
            if (is.null(file)){
                if (new|!any(charmatch(c('x11','X11'),names(dev.cur()),nomatch=-1))) X11()
            }else{
                if (grepl('.jpg',file)) jpeg(filename=file)
                if (grepl('.tif',file)) tiff(filename=file)
                if (grepl('.eps',file)) postscript(filename=file)
            }
            dev.scale = par('fin')[2]/6.99311
            par(mgp=c(3,1,0)*dev.scale)
            par(mar=c(2,2,2,.5))
            if(cimmyt) par(mar=c(4,5,2,1))
            if('PHAN'%in%variable) par(mar=par('mar')*c(1,2,1,1))
            if(is.null(xlab)){
                if(use.dap){
                    xlab = 'DAP'
                }else{
                    xlab = 'Date'
                }
            }else if(!grepl('%',xlab)){
                xlab = gsub(' ','~',xlab)
            }
            if(is.null(ylab)){
                ylab = ''
            }else if(!grepl('%',ylab)){
                ylab = gsub(' ','~',ylab)
            }
            if(!is.null(filet)){
                ptype = get.pch(length(variable)*length(trsq))
            }else{
                ptype = NULL
            }
            plcol = get.col(length(variable)*length(trsq),bw=bw)
            if(bw){
                ltype = get.ltype(length(variable)*length(trsq))
                pbg = get.ptbg(length(variable)*length(trsq))
            }else{
                ltype = rep(1,length(variable)*length(trsq))
                pbg = plcol
            }
            plot(1,1,xlim=xlim,ylim=ylim,xlab='',ylab='',type='n',axes=F)
            colnum = 1
        }
        for (j in 1:length(variable)){
            pdatasub = na.omit(pdata[pdata[,TRSQ]==trsq[i]&pdata$DATE>=min(xlim)&pdata$DATE<=max(xlim),
                                                            colnames(pdata)%in%c('DATE',variable[j])])
            if(plotfilet[i,j]){
                odatasub = na.omit(odata[odata[,TRSQ]==trsq[i]&odata$DATE>=min(xlim)&odata$DATE<=max(xlim),
                                                            colnames(odata)%in%c('DATE',variable[j])])
                points(odatasub[,'DATE'],odatasub[,variable[j]],col=plcol[colnum],pch=ptype[colnum],bg=pbg[colnum],cex=1.5*dev.scale)
            }
            lines(pdatasub[,'DATE'],pdatasub[,variable[j]],lty=ltype[colnum],col=plcol[colnum],lwd=2*dev.scale)
            if(is.null(trt.lab)){
                if(exists('leglab')){
                    leglab=c(leglab,paste(variable[j],'- ',TRSQ,as.character(trsq[i])))
                }else{
                    leglab=paste(variable[j],'- ',TRSQ,as.character(trsq[i]))
                }
            }
            colnum = colnum + 1
        }    
    }
    if(!is.null(trt.lab))    leglab = trt.lab
    if(include.legend){
        lgdpos = convert.pos(lgdpos,xlim,ylim)
        if(is.null(lgdpos$x)) lgdpos$x = xlim[1]
        if(is.null(lgdpos$y)) lgdpos$y = ylim[2]
        legend(x=lgdpos$x,y=lgdpos$y,
                        xjust=lgdpos$xjust,yjust=lgdpos$yjust,
                        pch=ptype,lty=ltype,pt.bg=pbg,col=plcol,
                        legend = leglab,cex=1.3*dev.scale,
                        lwd=2*dev.scale)
    }
    if(!is.null(subplot)) mtext(subplot,side=3,at=xlim[2],adj=0.75,padj=0,line=0,cex=3*dev.scale)
    if(is.null(yincrement)) yincrement = get.increment(ylim)
    if(is.null(yaxis.lab)) {
        axis(2,at=seq(ylim[1],ylim[2],yincrement),pos=xlim[1],cex.axis=1.5*dev.scale,las=las)
    }else{
        axis(2,at=seq(ylim[1],ylim[2],yincrement),labels=yaxis.lab,pos=xlim[1],cex.axis=1.5*dev.scale,las=las)
    }
    if(use.dap){
        daplim=as.integer(difftime(xax.lim,pdate,units='days'))
        if(is.null(xincrement)) xincrement = get.increment(daplim)
        dap.len = (daplim[2]-daplim[1])/xincrement + 1
        axis.POSIXct(1,at=seq(xax.lim[1],xax.lim[2],length.out=dap.len),
                labels=seq(daplim[1],daplim[2],length.out=dap.len),
                pos=ylim[1],cex.axis=1.5*dev.scale,las=las)
    }else{
#        axis(1,at=seq(xlim[1],xlim[2],by=ifelse(xincrement<300,'month','year')),pos=ylim[1],cex.axis=1.5*dev.scale)
        axis.POSIXct(1,x=xax.lim,pos=ylim[1],cex.axis=1.5*dev.scale,las=las)
#        xincrement = (xlim[2]-xlim[1])/(length(xaxis.lab)-1)
    }
    if(nchar(xlab)>0){
        if(!grepl('%',xlab)) xlab=parse(text=xlab)
        mtext(xlab,1,2*dev.scale,cex=1.5*dev.scale)
    }
    if(nchar(ylab)>0){
        if(any(!grepl('%',ylab))) ylab=parse(text=ylab)
        if(variable!='PHAN'){
            mtext(ylab,2,2*dev.scale,cex=1.5*dev.scale)
        }else{
            mtext(ylab[1],2,4*dev.scale,cex=1.5*dev.scale)
            mtext(ylab[2],2,2*dev.scale,cex=1.5*dev.scale)
        }
    }
    if(any(c(dstat,ceff,rmse,rrmse))){
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
            data = get.obs.pred(variable=variable)
        }
        statpos = convert.pos(statpos,xlim,ylim)
        if(is.null(statpos$x)) statpos$x = xlim[2]
        if(is.null(statpos$y)) statpos$y = ylim[1]
        add.stats(statpos,data,
            dstat=dstat,ceff=ceff,rmse=rmse,rrmse=rrmse,
            cex=1.5*dev.scale)
    }
    if(!is.null(file)) dev.off()
}

