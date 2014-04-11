write.gen <- function(gen,file.name,model=NA,type=NA){
    if(is.na(model)){
        if(grepl('GRO',file.name)){
            model='CROPGRO'
        }else if(grepl('CRP',file.name)){
            model = 'CROPSIM'
        }else if(grepl('CER',file.name)){
            model = 'CERES'
        }else{
            stop('Please provide model name.')
        }
    }
    if(is.na(type)){
        type = substr(file.name,nchar(file.name)-2,nchar(file.name))
        type = toupper(type)
    }
    tn = length(gen[[1]])
    cn = length(gen[[2]])
    pn = sum(unlist(lapply(cul[[3]],nrow)))+length(cul[[3]])
    gen.out = vector(length=tn+cn+pn,mode='character')
    linenum = 1
    for(i in 1:tn){
        gen.out[linenum] = gen[[1]][i]
        linenum = linenum + 1
    }
    for(i in 1:length(gen[[3]])){
        fmt = get.fmt(colnames(gen[[3]][[i]]),
                  model=rep(model,ncol(gen[[3]][[i]])),
                  type=rep(type,ncol(gen[[3]][[i]])))
        if(type=='CUL'){
            fmt[colnames(gen[[3]][[i]])=='ECO#']=' %6s'
        }
        hfmt = gsub('\\..*','s',fmt)
        orig = gsub(' *','',gsub('%','',gsub('\\..*','',
                   gsub('s','',hfmt[1]))))
        adj = as.numeric(orig)-1
        if(adj<0) adj = adj + 2
        hfmt[1] = gsub('%','@%',gsub(orig,adj,hfmt[1]))
        gen.out[linenum] = 
            paste(sprintf(hfmt,colnames(gen[[3]][[i]])),collapse='')
        linenum = linenum + 1
        section = gen[[3]][[i]]
        for(c in 1:ncol(gen[[3]][[i]])){
            section[,c] = sprintf(fmt[c],gen[[3]][[i]][,c])
        }
        for(l in 1:nrow(section)){
            gen.out[linenum] = paste(section[l,],collapse='')
            linenum = linenum + 1
        }
    }
    if(cn>0){
        for(i in 1:cn){
            gen.out[linenum] = gen[[2]][i]
            linenum = linenum + 1
        }
    }
    write(gen.out,file=file.name)
    return(invisible(gen.out))
}

