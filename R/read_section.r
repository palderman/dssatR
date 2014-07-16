read.section <- function(file){
    file = file[!file==''&!grepl('^[[:space:]]$',file)]
    headers = grep('@',file)
    section = vector('list',length(headers)+1)
    section[[1]] = file[1]
    for(i in 2:length(section)){
        if(i<length(section)){
            begin = headers[i-1]
            end = (headers[i]-1)
        }else{
            begin = headers[i-1]
            end = length(file)
        }
        if(begin<end){
            pos.names = get.pos.names(file,begin,end)
            if('NOTES' %in% pos.names[[3]]){
                section[[i]] = data.frame(file[(begin+1):end])
                colnames(section[[i]]) = pos.names[[3]]
            }else{
            if(!is.vector(pos.names[[1]])){
                section[[i]] = matrix(nrow=(end-begin),ncol=ncol(pos.names[[1]]))
                for(j in 1:ncol(pos.names[[1]])){
                    section[[i]][,j] = substring(file[(begin+1):end],pos.names[[1]][,j],pos.names[[2]][,j])
                }
                section[[i]] = data.frame(section[[i]],stringsAsFactors=F)
                colnames(section[[i]])=pos.names[[3]]
            }else{
                section[[i]] = matrix(nrow=(end-begin),ncol=length(pos.names[[1]]))
                for(j in 1:length(pos.names[[1]])){
                    section[[i]][,j] = substring(file[(begin+1):end],pos.names[[1]][j],pos.names[[2]][j])
                }
                section[[i]] = data.frame(section[[i]],stringsAsFactors=F)
                colnames(section[[i]])=pos.names[[3]]
            }
            }
            for(j in 1:ncol(section[[i]])){
                if(numeric.all(section[[i]][,j])){
                    section[[i]][,j] = as.numeric(section[[i]][,j])
                }
                if(colnames(section[[i]])[j]%in%
                    c('FDATE','SADAT','ICDAT','PDATE','EDATE','HDATE',
                        'SDATE','PFRST','PLAST','HFRST','HLAST')){
                    section[[i]][,j] = as.integer(section[[i]][,j])
                }
                section[[i]][section[[i]]==-99] = NA
                section[[i]][section[[i]]=='-99'] = NA
                section[[i]][section[[i]]==''] = NA
            }
        }else{
            section[[i]]=file[begin]
        }
    }
    return(section)
}

