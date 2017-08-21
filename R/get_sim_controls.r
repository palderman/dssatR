get.sim.controls <- function(filex.name=NULL,level=NULL,filex=NULL){
    if(is.null(filex.name)&is.null(filex)){
                                stop('No file name or filex supplied')}
    sect.name = 'SIMULATION CONTROLS'
    if(is.null(filex)) filex = readLines(filex.name)
    filex = filex[!substr(filex,1,1)=='!'&filex!='@  AUTOMATIC MANAGEMENT']
    sect.heading = grep(paste('\\*',sect.name,sep=''),filex)
    section = vector('list',length=length(sect.heading))
    for(i in 1:length(sect.heading)){
        next.heading = grep('^\\*',filex[(sect.heading[i]+1):length(filex)])-1
        if(length(next.heading)>0){
            next.heading = sect.heading[i] + next.heading[1]
        }else{
            next.heading = length(filex)
        }
        section[[i]] = read.section(filex[sect.heading[i]:next.heading])
    }
    section <- Reduce(function(...)merge(...,by='N',all=TRUE),section)
    return(section)
}

