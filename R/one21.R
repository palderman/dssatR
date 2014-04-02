one21 <- function(data,units='',main='',axis.label='',xylim=NULL,
                    increment=NULL,dstat=T,rmse=T,rrmse=F,ceff=F,new=F,
                    trt.labels=NULL,bw=F,subplot=NULL,cimmyt=F){
    obs = data[,1]
    pred = data[,2]
    xylim = set.xylim(xylim,unlist(data[,1:2]))
    if (units!=''){
        units = paste('~(',gsub(' ','~',units),')',collapse='')
        axis.label=paste('~',gsub(' ','~',axis.label),units,sep='')
    }else if(axis.label!=''&!grepl('%',axis.label)){
        axis.label=paste('~',gsub(' ','~',axis.label))
    }
    if(!grepl('%',axis.label)){
            xlabel = parse(text=paste('Observed',axis.label,sep=''))
    }else{
        xlabel = paste('Observed ',axis.label,sep='')
    }
    if(!grepl('%',axis.label)){
        ylabel = parse(text=paste('Predicted',axis.label,sep=''))        
    }else{
        ylabel = paste('Predicted ',axis.label,sep='')
    }
    if (new|!any(charmatch(c('x11','X11'),names(dev.cur()),nomatch=-1))) X11()
    dev.scale = dev.size()[2]/6.99311
    par(mgp=c(3,1,0)*dev.scale)
    par(mar=c(2,2,2,.5))
    if(cimmyt) par(mar=c(4,5,2,1))
    if('pttyp'%in%colnames(data)) pch = data$pttyp else pch = 19
    if('ptcol'%in%colnames(data)) pcol = data$ptcol else pcol = 'black'
    if('ptbg'%in%colnames(data)) ptbg = data$ptbg else ptbg = 'black'
    plot(obs,pred,typ='p',pch=pch,col=pcol,bg=ptbg,xlim=xylim,ylim=xylim,xlab='',ylab='',cex=1.5*dev.scale,axes=F)
    if(!is.null(trt.labels)) legend(xylim[1],xylim[2],
                                    legend=trt.labels,
                                    cex=1.5*dev.scale,
                                    pch=get.pch(length(trt.labels)),
                                    col=get.col(length(trt.labels),T),
                                    pt.bg=get.ptbg(length(trt.labels)))
    mtext(main,3,2*dev.scale,cex=1.5*dev.scale)
    if(is.null(increment)) increment = get.increment(xylim)
    axis(1,at=seq(xylim[1],xylim[2],increment),pos=xylim[1],cex.axis=1.5*dev.scale)
    axis(2,at=seq(xylim[1],xylim[2],increment),pos=xylim[1],cex.axis=1.5*dev.scale)
#    axis(3,at=seq(0,xylim[2],xylim[2]/4),label=rep('',5),pos=c(0,xylim[2]),lwd.ticks=0)
#    axis(4,at=seq(0,xylim[2],xylim[2]/4),label=rep('',5),pos=c(0,xylim[2]),lwd.ticks=0)
    mtext(xlabel,1,2*dev.scale,cex=1.5*dev.scale)
    mtext(ylabel,2,2*dev.scale,cex=1.5*dev.scale)
    lines(c(xylim[1],signif(xylim[2],digits=2)),c(xylim[1],signif(xylim[2],digits=2)),lwd=dev.scale)
#    text(x=rep(xylim[2]*.7,length(lgd))*dev.scale,y=seq(1,length(lgd))*xylim[2]/15/dev.scale,label=lgd,pos=4,cex=1.5*dev.scale)
    if(!is.null(subplot)) mtext(subplot,side=3,at=xylim[2]*.95,line=-.5,cex=3*dev.scale)
    if(any(c(dstat,ceff,rmse,rrmse))) {
        statpos = convert.pos('bottomright',xylim,xylim)
        add.stats(statpos,data,dstat=dstat,ceff=ceff,rmse=rmse,rrmse=rrmse,cex=1.5*dev.scale)
    }
}

