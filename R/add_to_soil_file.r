add.to.soil.file <- function(soil.profile,filename){
    soil.file = vector(length=7+nrow(soil.profile$layer),mode='character')
    soil.file[1] = ''
    soil.file[2] = paste('*',soil.profile[[i]]$ID_SOIL,'  ',
                    sprintf('%-12s',soil.profile[[i]]$SOURCE),
                    sprintf('%3s',soil.profile[[i]]$TEXTURE_CLASS),
                    sprintf('%8.0f',soil.profile[[i]]$DEPTH),' ',
                    soil.profile[[i]]$SOIL_NAME,sep='')
    soil.file[3] = '@SITE        COUNTRY          LAT     LONG SCS FAMILY'
    soil.file[4] = paste(' ',soil.profile[[i]]$SITE,
                        paste(rep(' ',12-nchar(soil.profile[[i]]$SITE)),collapse=''),
                        soil.profile[[i]]$COUNTRY,
                        paste(rep(' ',13-nchar(soil.profile[[i]]$COUNTRY)),collapse=''),
                        sprintf('%8.3f',soil.profile[[i]]$LAT),
                        sprintf('%8.3f',soil.profile[[i]]$LONG),
                        ' ',soil.profile[[i]]$SCS_FAMILY,
                        sep='')
    soil.file[5] = '@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE'
    soil.file[6] = paste(sprintf('%6s',soil.profile[[i]]$SCOM),
                        sprintf('%6.2f',soil.profile[[i]]$SALB),
                        sprintf('%6.1f',soil.profile[[i]]$SLU1),
                        sprintf('%6.2f',soil.profile[[i]]$SLDR),
                        sprintf('%6.1f',soil.profile[[i]]$SLRO),
                        sprintf('%6.2f',soil.profile[[i]]$SLNF),
                        sprintf('%6.2f',soil.profile[[i]]$SLPF),
                        sprintf('%6s',soil.profile[[i]]$SMHB),
                        sprintf('%6s',soil.profile[[i]]$SMPX),
                        sprintf('%6s',soil.profile[[i]]$SMKE),
                        sep='')
    soil.file[7] = '@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC'
    fmt = c('%6.0f','%6s',rep('%6.3f',5),rep('%6.2f',2),rep('%6.1f',3),'%6.3f',rep('%6.1f',4))
    for (j in 1:nrow(soil.profile[[i]]$layer)){
        lyr.fmt = fmt
        lyr.fmt[soil.profile$layer[j,]==-99] = '%6.0f'
        soil.file[7+j] = paste(sprintf(lyr.fmt[1],soil.profile[[i]]$layer[j,1]),
                                sprintf(lyr.fmt[2],soil.profile[[i]]$layer[j,2]),
                                paste(sprintf(lyr.fmt[3:length(lyr.fmt)],
                                        soil.profile[[i]]$layer[j,3:length(lyr.fmt)]),
                                collapse=''),sep='')
    }
    write(soil.file,filename,append=T)
}

