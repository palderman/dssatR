add.stats <- function(xypos,data,dstat=T,ceff=F,rmse=T,rrmse=F,cex=1){
    if(dstat){
        if(exists('lgd')){
            lgd = c(lgd,paste('D: ',dstat(data),sep=''))
        }else{
            lgd = paste('D: ',dstat(data),sep='')
        }
    }
    if(ceff){
        if(exists('lgd')){
            lgd = c(lgd,parse(text=paste('C[eff]: ',ceff(data),sep='')))
        }else{
            lgd = parse(text=paste('C[eff]: ',ceff(data),sep=''))
        }
    }
    if(rmse){
        if(exists('lgd')){
            lgd = c(lgd,paste('RMSE: ',signif(rmse(data),digits=2),sep=''))
        }else{
            lgd = paste('RMSE: ',signif(rmse(data),digits=2),sep='')
        }
    }
    if(rrmse){
        if(exists('lgd')){
            lgd = c(lgd,paste('RRMSE: ',signif(rrmse(data),digits=2),sep=''))
        }else{
            lgd = paste('RRMSE: ',signif(rrmse(data),digits=2),sep='')
        }
    }
    legend(x=xypos$x,y=xypos$y,xjust=xypos$xjust,yjust=xypos$yjust,
            legend=lgd,cex=cex,bty='n')
}

