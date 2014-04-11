nlines.filex <- function(filex){
    nlines = 0
    for (i in 1:length(filex)){
        for(j in 1:length(filex[[i]])){
            if(is.vector(filex[[i]][[j]])){
                nlines = nlines + 1
            }else if(is.data.frame(filex[[i]][[j]])){
                nlines = nlines + nrow(filex[[i]][[j]]) + 1
            }else if(is.list(filex[[i]][[j]])){
                for(k in 1:length(filex[[i]][[j]])){
                    if(is.vector(filex[[i]][[j]][[k]])){
                        nlines = nlines + 1
                    }else if(is.data.frame(filex[[i]][[j]][[k]])){
                        nlines = nlines + nrow(filex[[i]][[j]][[k]]) + 1
                    }
                }
            }
        }
    }
    nlines = nlines + length(filex)
    return(nlines)
}

