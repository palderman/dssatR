write.filex <- function(filex,filex.name){
#    nlines = nlines.filex(filex)
#    flx.lines = vector(length=nlines,mode='character')
    write(filex[[1]],filex.name)
#    flx.lines[1] = filex[[1]]
#    flx.lines[2] = ''
#    linenum = 2
    fmt.list = fmt.filex()
    for (i in 2:length(filex)){
        if(names(filex)[i]=='GENERAL'){
            fmt.tmp = fmt.filex.gen()
        }else if(names(filex)[i]=='TREATMENTS'){
            fmt.tmp = fmt.filex.trt()
        }else if(names(filex)[i]=='CULTIVARS'){
            fmt.tmp = fmt.filex.cul()
        }else if(names(filex)[i]=='FIELDS'){
            fmt.tmp = fmt.filex.fld()
        }else if(names(filex)[i]=='PLANTING DETAILS'){
            fmt.tmp = fmt.filex.plt()
        }else if(names(filex)[i]=='HARVEST DETAILS'){
            fmt.tmp = fmt.filex.harv()
        }else if(names(filex)[i]=='ENVIRONMENT MODIFICATIONS'){
            fmt.tmp = fmt.filex.env()
        }else if(names(filex)[i]=='IRRIGATION AND WATER MANAGEMENT'){
            fmt.tmp = fmt.filex.irr()
        }else if(names(filex)[i]=='SIMULATION CONTROLS'){
            fmt.tmp = fmt.filex.sim()
        }else{
            fmt.tmp = fmt.filex()
        }
        if(i>1) write('',filex.name,append=TRUE)
        for(j in 1:length(filex[[i]])){
            if(!grepl('SIMULATION CONTROLS',names(filex)[i])){
#                sub.lines = write.subsection(filex[[i]][[j]])
                write.tier(filex[[i]][[j]],file.name=filex.name,
                           fmt.list=fmt.tmp)
#                flx.lines[(linenum+1):
#                                (linenum+length(sub.lines))] = sub.lines
#                linenum = linenum + length(sub.lines)
            }else{
                for(k in 1:length(filex[[i]][[j]])){
#                    sub.lines = write.subsection(filex[[i]][[j]][[k]])
                    write.tier(filex[[i]][[j]][[k]],file.name=filex.name,
                               fmt.list=fmt.tmp)
#                    flx.lines[(linenum+1):
#                                (linenum+length(sub.lines))] = sub.lines
#                    linenum = linenum + length(sub.lines)
                }
#                linenum = linenum + 1
#                flx.lines[linenum] = ''
            }
        }
#        linenum = linenum + 1
#        flx.lines[linenum] = ''
    }
#    write(flx.lines,file=filex.name)
    return(invisible(0))
}

