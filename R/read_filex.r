read.filex <- function(filex.name){
    flx = readLines(filex.name)
    flx = gsub('  *$','',flx)
    flx = gsub('\032','',flx)
    sectnames = gsub('^\\*','',grep('^\\*',flx,value=T))
    sectnames = sectnames[!grepl('SIMULATION CONTROLS',sectnames)]
    sectnames = c(sectnames,'SIMULATION CONTROLS')
    filex = vector('list',length=length(sectnames))
    filex[[1]] = paste('*',sectnames[1],sep='')
    sectnames = sectnames[2:length(sectnames)]
    sectnames = gsub('\\(','\\\\(',sectnames)
    sectnames = gsub('\\)','\\\\)',sectnames)
    sectnames = gsub('FERTILIZERS .*','FERTILIZERS',sectnames)
    sectnames = gsub('RESIDUES .*','RESIDUES',sectnames)
    sectnames = gsub('TILLAGE .*','TILLAGE',sectnames)    
    for (i in 1:length(sectnames)){
        if(!sectnames[i]=='SIMULATION CONTROLS'){
            filex[[i+1]]=get.section(sectnames[i],file=flx[2:length(flx)])
        }else{
            filex[[i+1]] <- get.sim.controls(filex=flx[2:length(flx)])
            cnames <- colnames(filex[[i+1]])
            cnames <- cnames[!cnames%in%c('GENERAL','OPTIONS','METHODS','MANAGEMENT',
                                          'OUTPUTS','PLANTING','IRRIGATION',
                                          'NITROGEN','RESIDUES','HARVEST')]
            filex[[i+1]] <- filex[[i+1]][,cnames]
        }
      if(!sectnames[i]%in%c('INITIAL CONDITIONS',
                            'IRRIGATION AND WATER MANAGEMENT')&
         !is.data.frame(filex[[i+1]])){
        filex[[i+1]] <- Reduce(function(...)merge(...,all=TRUE),filex[[i+1]])
      }
    }
    names(filex) = gsub('^\\*','',
                        gsub('      *.*','',
                            c('EXP.DETAILS',sectnames)))
#    lapply(filex,function(x) gsub('^\\*','',gsub('      *.*','',x[[1]])))
    return(filex)
}

