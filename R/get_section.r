get.section <- function(sect.name,file.type='filex',file.name=NULL,level=NULL,file=NULL){
    if(is.null(file.name)&is.null(file)) stop('No file name or file supplied')
    sect.name = switch(sect.name,
                    ic='INITIAL CONDITIONS',
                    sa='SOIL ANALYSIS',
                    sect.name
                )
    if(is.null(file)) file = readLines(file.name)
    file = file[!substr(file,1,1)=='!']
    if(file.type=='filex'){
        sect.heading = grep(paste('\\*',sect.name,sep=''),file)
        next.heading = 
                grep('^\\*',file[(sect.heading[1]+1):length(file)])-1
    }else{
        
    }
    if(length(next.heading)>0){
        next.heading = sect.heading + next.heading[1]
    }else{
        next.heading = length(file)
    }
    file = file[sect.heading:next.heading[1]]
    section = read.section(file)
    return(section)
}

