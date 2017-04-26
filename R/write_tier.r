write.tier <- function(tier,file.name,fmt.list=NULL){
    if(is.data.frame(tier)){
        cnames = colnames(tier)
        fmt = vector(length=length(cnames),mode='character')
        if(is.null(fmt.list)) fmt.list = fmt.default()
        for(i in 1:length(fmt.list)){
            fmt[cnames%in%fmt.list[[i]]]=names(fmt.list)[i]
        }
        class = fmt2class(fmt)
        if(!grepl('@',cnames[1])){
            cnames[1] = paste('@',gsub(' ','',cnames[1]),sep='')
        }
        for(cl in 1:ncol(tier)){
            if(grepl('yrdoy',fmt[cl])){
                if('POSIXt'%in%class(tier[,cl])){
                    tier[,cl] = as.POSIXlt(tier[,cl])$year%%100*1000+
                        as.POSIXlt(tier[,cl])$yday+1
                }
                fmt[cl] = gsub('yrdoy','.5i',fmt[cl])
            }
            na.sub = is.na(tier[,cl])
            if(is.factor(tier[,cl])){
                tier[,cl] = as.character(tier[,cl])
                fmt[cl] = paste('%',fmt2width(fmt[cl]),'s',sep='')
            }else if(is.character(tier[,cl])&!grepl('s',fmt[cl])){
                fmt[cl] = paste('%',fmt2width(fmt[cl]),'s',sep='')
            }
            if(class(tier[,cl])=='character')
                tier[,cl]=gsub('^ *','',gsub('  *$','',tier[,cl]))
            tier[,cl] = sprintf(fmt[cl],tier[,cl])
            width = fmt2width(fmt[cl])
            if(!is.na(width)){
                tier[na.sub,cl] = sprintf(
                        gsub('[fi]','s',gsub('\\..*[fi]','s',fmt[cl])),
                        '-99')
            }else{
                tier[na.sub,cl] = sprintf(fmt[cl],'-99')
            }
            if(cl==1&&!grepl('-',fmt[1])){
                if(!is.na(width)) fmt[1] = paste('%-',width,'s',sep='')
            }
            cnames[cl] = sprintf(gsub('[fi]','s',gsub('\\..*[fi]','s',fmt[cl])),cnames[cl])
        }
        tier = rbind(cnames,tier)
        tier = do.call(paste,c(tier,sep=''))
    }
    write(tier,file=file.name,append=TRUE)
    return(invisible(tier))
}
