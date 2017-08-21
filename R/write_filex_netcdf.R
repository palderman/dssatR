write.filex.netcdf <- function(filex,filename){
  require(ncdf4)
  dims <- list()
  vars <- list()
  snames <- gsub(' \\(INORGANIC\\)','',names(filex))
  names(filex) <- snames
  for(s in snames){
    if(s%in%c('INITIAL CONDITIONS','IRRIGATION AND WATER MANAGEMENT')){
      for(i in 1:2){
        if(i==1){
          dims[[paste0(s,i)]] <- ncdim_def(paste0(s,i),'count',as.integer(1:nrow(filex[[s]][[i]])))
        }else{
          mx.lev <- max(summary(as.factor(filex[[s]][[i]][,1])))
          dims[[paste0(s,i)]] <- ncdim_def(paste0(s,i),'count',as.integer(1:mx.lev))
        }
        if(i==2) vars[[lncode(s)]] <- ncvar_def(lncode(s),'',dims[paste0(s,i)],prec='integer',
                                                missval=-99)
        vnames <- names(filex[[s]][[i]])[-1]
        for(v in vnames){
          if(dssat.class(v)=='char'){
            if(!is.character(filex[[s]][[i]][[v]]))
              filex[[s]][[i]][[v]] <- as.character(filex[[s]][[i]][[v]])
            cname <- paste0(v,'len')
            filex[[s]][[i]][[v]] <- gsub('^ *','',gsub(' *$','',filex[[s]][[i]][[v]]))
            clen <- max(nchar(filex[[s]][[i]][[v]]))
            if(is.na(clen)) clen = 1
            dims[[cname]] <- ncdim_def(cname,'count',as.integer(1:clen))
            vars[[v]] <- ncvar_def(v,'',dims[c(cname,paste0(s,i))],prec=dssat.class(v),missval=' ')
          }else{
            if(is.character(filex[[s]][[i]][[v]]))
              warning(paste0(v,' is character not integer or numeric.'))
            vars[[v]] <- ncvar_def(v,'',dims[paste0(s,i)],
                               prec=dssat.class(v),missval=-99)
          }
        }
      }
    }else if(s%in%c('TILLAGE AND ROTATIONS','CHEMICAL APPLICATIONS',
               'RESIDUES AND OTHER ORGANIC MATERIALS',
               'FERTILIZERS',
               'HARVEST DETAILS','ENVIRONMENT MODIFICATIONS')){
        dims[[s]] <- ncdim_def(s,'count',as.integer(1:nrow(filex[[s]])))
        vnames <- names(filex[[s]])
        if(vnames[1] %in% c('C','E','H','R','T')) vnames <- vnames[-1]
        vars[[lncode(s)]] <- ncvar_def(lncode(s),'',dims[s],prec='integer',missval=-99)
        for(v in vnames){
          if(dssat.class(v)=='char'){
            if(!is.character(filex[[s]][[v]]))
              filex[[s]][[v]] <- as.character(filex[[s]][[v]])
            filex[[s]][[v]] <- gsub('^ *','',gsub(' *$','',filex[[s]][[v]]))
            clen <- max(nchar(filex[[s]][[v]]))
            if(is.na(clen)) clen = 1
            cname <- paste0('len',clen)
            if(!cname%in%names(dims)){
              dims[[cname]] <- ncdim_def(cname,'count',as.integer(1:clen))
            }
            vars[[v]] <- ncvar_def(v,'',dims[c(cname,s)],prec=dssat.class(v),missval=' ')
          }else{
            if(is.character(filex[[s]][[v]]))
              warning(paste0(v,' is character not integer or numeric.'))
            vars[[v]] <- ncvar_def(v,'',dims[s],prec=dssat.class(v),missval=-99)
          }
        }
    }else{
      if(!s%in%c('EXP.DETAILS','GENERAL')){
        dims[[s]] <- ncdim_def(s,'count',as.integer(1:nrow(filex[[s]])))
        vnames <- names(filex[[s]])
        vnames <- vnames[!vnames%in%c('C','P','L')]
        if(s == 'TREATMENTS') vnames = vnames[!vnames%in%c('R','O','C')]
        for(v in vnames){
          if(dssat.class(v)=='char'){
            if(!is.character(filex[[s]][[v]]))
              filex[[s]][[v]] <- as.character(filex[[s]][[v]])
            filex[[s]][[v]] <- gsub('^ *','',gsub(' *$','',filex[[s]][[v]]))
            clen <- max(nchar(filex[[s]][[v]]))
            if(is.na(clen)) clen = 1
            cname <- paste0('len',clen)
            if(!cname%in%names(dims)){
              dims[[cname]] <- ncdim_def(cname,'count',as.integer(1:clen))
            }
            vars[[v]] <- ncvar_def(v,'',dims[c(cname,s)],prec=dssat.class(v),missval=' ')
          }else{
            if(is.character(filex[[s]][[v]]))
              warning(paste0(v,' is character not integer or numeric.'))
            vars[[v]] <- ncvar_def(v,'',dims[s],prec=dssat.class(v),missval=-99)
          }
        }
      }
    }
  }
  nc_filex <- nc_create(filename,vars)
  for(s in snames){
    if(s%in%c('INITIAL CONDITIONS','IRRIGATION AND WATER MANAGEMENT')){
      for(i in 1:2){
        vnames <- names(filex[[s]][[i]])[-1]
        if(i==2) ncvar_put(nc_filex,lncode(s),filex[[s]][[i]][[1]])
        for(v in vnames){
          ncvar_put(nc_filex,v,filex[[s]][[i]][[v]])
        }
      }
    }else if(s%in%c('TILLAGE AND ROTATIONS','CHEMICAL APPLICATIONS',
               'RESIDUES AND OTHER ORGANIC MATERIALS',
               'FERTILIZERS',
               'HARVEST DETAILS','ENVIRONMENT MODIFICATIONS')){
        vnames <- names(filex[[s]])
        ncvar_put(nc_filex,lncode(s),filex[[s]][[1]])
        for(v in vnames){
          ncvar_put(nc_filex,v,filex[[s]][[v]])
        }
    }else{
      if(!s%in%c('EXP.DETAILS','GENERAL')){
        vnames <- names(filex[[s]])
        vnames <- vnames[!vnames%in%c('C','P','L')]
        if(s == 'TREATMENTS') vnames = vnames[!vnames%in%c('R','O','C')]
        for(v in vnames){
          ncvar_put(nc_filex,v,filex[[s]][[v]])
        }
      }
    }
  }
  nc_close(nc_filex)
}