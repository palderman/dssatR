write.gen <- function(gen,file.name,model=NULL){
    if(grepl('GRO',file.name)){
        model='CROPGRO'
    }else if(grepl('CRP',file.name)){
        model = 'CROPSIM'
        fmt.list = cropsim.fmt()
    }else if(grepl('CER',file.name)){
        model = 'CERES'
        fmt.list = ceres.fmt()
    }else if(grepl('APS',file.name)){
        model = 'NWHEAT'
        fmt.list = nwheat.fmt()
    }else if(is.null(model)){
        stop('Please provide model name.')
    }
    write(gen$title,file=file.name)
    write(gen$comments,file=file.name,append=T)
    if(is.data.frame(gen$p)){
        colnames(gen$p)[1]=sprintf('@%s',gsub(' ','',colnames(gen$p)[1]))
        write.tier(gen$p,file.name=file.name,fmt.list=fmt.list)
    }else{
        for(i in 1:length(gen$p)){
            write.tier(gen$p[[i]],file.name=file.name,
                       fmt.list=fmt.list)
        }
    }
    return(invisible(NULL))
}

