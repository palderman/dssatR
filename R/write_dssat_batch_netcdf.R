write.dssat.batch.netcdf <- function(filex.name,trtno=1,rp=1,sq=0,op=0,co=0,file.name){

    require(ncdf4)
    require(dplyr)

    batch <- data.frame(FILEX=filex.name,TRTNO=trtno,RP=rp,
                        SQ=sq,OP=op,CO=co,
                        stringsAsFactors=FALSE)

    # Create NetCDF dimension variables
    nruns <- ncdim_def('nruns','count',1:nrow(batch))

    batch <- as.list(batch)

    # Determine lengths for all character parameters
    clen <- sapply(batch,function(x){
                           if(is.character(x)|is.factor(x)){
                               clen <- as.character(x) %>%
                                   gsub('(^ *)|( *$)','',.) %>%
                                   nchar(.) %>%
                                   max(.,na.rm=TRUE)
                               return(clen)
                           }else{
                               return(NA)
                           }
    }) %>%
        unique(.) %>%
        na.omit(.)

    clen[clen==Inf|clen==-Inf] <- 1

    all.len <- unique(clen)

    # Create NetCDF dimension variables for each length determined above
    len.dims <- list()
    for(l in all.len){
        l.name <- paste0('l',l)
        len.dims[[l.name]] <- ncdim_def(l.name,'count',1:l)
    }

    v.list <- list()

    # Create variables for other station information variables
    for(p in names(batch)){
        if(is.character(batch[[p]]) |
           is.factor(batch[[p]])){
            clen <- as.character(batch[[p]]) %>%
                gsub('(^ *)|( *$)','',.) %>%
                nchar(.) %>%
                max(.,na.rm=TRUE)
            v.list[[p]] <- ncvar_def(p,
                                     '',
                                     list(len.dims[[which(all.len==clen)]],nruns),
                                     missval=' ',
                                     prec='char')
        }else{
            v.list[[p]] <- ncvar_def(p,
                                     '',
                                     list(nruns),
                                     missval=-99,
                                     prec='float')
        }
    }

    # Create new NetCDF file
    ncid <- nc_create(file.name,v.list)

    # Add all other parameter values to NetCDF file
    for(p in names(batch)){
        if(is.character(batch[[p]]) |
           is.factor(batch[[p]])){
            ncvar_put(ncid,p,as.character(batch[[p]]))
        }else{
            ncvar_put(ncid,p,batch[[p]])
        }
    }

    # Write data and close NetCDF file
    nc_close(ncid)
}
