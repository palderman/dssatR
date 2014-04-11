list.ljust <- function(x){
    ljust = c('HARM','ID_SOIL','WSTA','SLTX','GENERAL','OPTIONS',
            'METHODS','MANAGEMENT','OUTPUTS','PLANTING',
            'IRRIGATION','NITROGEN','RESIDUES','HARVEST','ID_FIELD',
            'SMODEL','PEOPLE','ADDRESS','SITE')
    ljust = c(ljust,grep('NAME',x,value=T))
    ljust = grep('FNAME',ljust,value=T,invert=T)
    return(ljust)
}

