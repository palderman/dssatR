write.filex <- function(filex,filex.name=NULL){
    nlines = nlines.filex(filex)
    flx.lines = vector(length=nlines,mode='character')
    flx.lines[1] = filex[[1]]
    flx.lines[2] = ''
    linenum = 2
    fmt.list = fmt.filex()
    for (i in 2:length(filex)){
        for(j in 1:length(filex[[i]])){
            if(!grepl('SIMULATION CONTROLS',names(filex)[i])){
                sub.lines = write.subsection(filex[[i]][[j]])
#                sub.lines = write.tier(filex[[i]][[j]],fmt.list=fmt.list)
                flx.lines[(linenum+1):
                                (linenum+length(sub.lines))] = sub.lines
                linenum = linenum + length(sub.lines)
            }else{
                for(k in 1:length(filex[[i]][[j]])){
                    sub.lines = write.subsection(filex[[i]][[j]][[k]])
#                    sub.lines = write.tier(filex[[i]][[j]][[k]],fmt.list=fmt.list)
                    flx.lines[(linenum+1):
                                (linenum+length(sub.lines))] = sub.lines
                    linenum = linenum + length(sub.lines)
                }
                linenum = linenum + 1
                flx.lines[linenum] = ''
            }
        }
        linenum = linenum + 1
        flx.lines[linenum] = ''
    }
    if(!is.null(filex.name)){
        write(flx.lines,file=filex.name)
    }
    return(invisible(flx.lines))
}

