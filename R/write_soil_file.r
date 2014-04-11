write.soil.file <- function(soil.profile,filename,append=T){
        if(ncol(soil.profile$layer)<=17){
            nlines = 8+nrow(soil.profile$layer)
        }else{
            nlines = 10+2*nrow(soil.profile$layer)
        }
        soil.file = vector(length=nlines,mode='character')
        soil.file[1] = '*SOILS:'
        soil.file[2] = ''
        soil.file[3] = paste('*',soil.profile$ID_SOIL,'  ',
                        sprintf('%-12s',soil.profile$SOURCE),
                        sprintf('%3s',soil.profile$TEXTURE_CLASS),
                        sprintf('%8.0f',soil.profile$DEPTH),' ',
                        soil.profile$SOIL_NAME,sep='')
        soil.file[4] = '@SITE        COUNTRY          LAT     LONG SCS FAMILY'
        soil.file[5] = paste(' ',soil.profile$SITE,
                            paste(rep(' ',12-nchar(soil.profile$SITE)),collapse=''),
                            soil.profile$COUNTRY,
                            paste(rep(' ',13-nchar(soil.profile$COUNTRY)),collapse=''),
                            sprintf('%8.3f',soil.profile$LAT),
                            sprintf('%8.3f',soil.profile$LONG),
                            ' ',soil.profile$SCS_FAMILY,
                            sep='')
        soil.file[6] = '@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE'
        soil.file[7] = paste(sprintf('%6s',soil.profile$SCOM),
                            sprintf('%6.2f',soil.profile$SALB),
                            sprintf('%6.1f',soil.profile$SLU1),
                            sprintf('%6.2f',soil.profile$SLDR),
                            sprintf('%6.1f',soil.profile$SLRO),
                            sprintf('%6.2f',soil.profile$SLNF),
                            sprintf('%6.2f',soil.profile$SLPF),
                            sprintf('%6s',soil.profile$SMHB),
                            sprintf('%6s',soil.profile$SMPX),
                            sprintf('%6s',soil.profile$SMKE),
                            sep='')
        soil.file[8] = paste(sprintf('%6s',colnames(soil.profile$layer[,1:17])),collapse='')
        substr(soil.file[8],1,1)='@'
        fmt = c('%6.0f','%6s',rep('%6.3f',5),rep('%6.2f',2),rep('%6.1f',3),'%6.3f',rep('%6.1f',4))
        for (j in 1:nrow(soil.profile$layer)){
            lyr.fmt = fmt
            lyr.fmt[soil.profile$layer[j,1:17]==-99] = '%6.0f'
            lyr.fmt[2] = '%6s'
            soil.file[8+j] = paste(sprintf(lyr.fmt[1],soil.profile$layer[j,1]),
                                    sprintf(lyr.fmt[2],soil.profile$layer[j,2]),
                                    paste(sprintf(lyr.fmt[3:length(lyr.fmt)],
                                            soil.profile$layer[j,3:length(lyr.fmt)]),
                                    collapse=''),sep='')
        }
        if(ncol(soil.profile$layer)>17){
            soil.file[10+nrow(soil.profile$layer)] = paste(sprintf('%6s',c('SLB',colnames(soil.profile$layer[,18:ncol(soil.profile$layer)]))),collapse='')
            substr(soil.file[10+nrow(soil.profile$layer)],1,1)='@'
            for (j in 1:nrow(soil.profile$layer)){
                lyr.fmt = rep('%6.2f',ncol(soil.profile$layer)-17)
                lyr.fmt[soil.profile$layer[j,18:ncol(soil.profile$layer)]==-99] = '%6.0f'
                lyr.fmt = c(fmt[1],lyr.fmt)
                soil.file[10+nrow(soil.profile$layer)+j] = 
                                paste(sprintf(lyr.fmt[1],soil.profile$layer[j,1]),
                                        paste(sprintf(lyr.fmt[2:length(lyr.fmt)],
                                                soil.profile$layer[j,18:ncol(soil.profile$layer)]),
                                        collapse=''),sep='')
            }
        }
        soil.file=gsub(' NA','-99',soil.file)
        if (!append) {
            write(soil.file,filename)
        }else{
                soil.file = soil.file[2:length(soil.file)]
                write(soil.file,filename,append=append)                
         }
}

