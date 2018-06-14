write.filex <- function(filex,filex.name){
    write(filex$EXP.DETAILS,filex.name)
    fmt.list <- fmt.filex()
    write('',filex.name,append=TRUE)
    if('GENERAL'%in%names(filex)){
      write('*GENERAL',filex.name,append=TRUE)
      write(c('@PEOPLE',unique(as.character(filex$GENERAL$PEOPLE))),
            filex.name,append = TRUE)
      write(c('@ADDRESS',unique(as.character(filex$GENERAL$ADDRESS))),
            filex.name,append = TRUE)
      write(c('@SITE',unique(as.character(filex$GENERAL$SITE))),
            filex.name,append = TRUE)
      write(c('@NOTES',unique(as.character(filex$GENERAL$NOTES)),''),
            filex.name,append = TRUE)
    }
    if('TREATMENTS'%in%names(filex)){
      fmt.tmp <- fmt.filex.trt()
      write('*TREATMENTS',filex.name,append=TRUE)
      write.tier(filex$TREATMENTS,file.name=filex.name,
                 fmt.list=fmt.tmp)
      write('',filex.name,append=TRUE)
    }
    if('CULTIVARS'%in%names(filex)){
      fmt.tmp <- fmt.filex.cul()
      write('*CULTIVARS',filex.name,append=TRUE)
      write.tier(filex$CULTIVARS,file.name=filex.name,
                 fmt.list=fmt.tmp)
      write('',filex.name,append=TRUE)
    }
    if('FIELDS'%in%names(filex)){
      fmt.tmp <- fmt.filex.fld()
      write('*FIELDS',filex.name,append=TRUE)
      first <- c('L','ID_FIELD','WSTA','FLSA','FLOB','FLDT','FLDD',
                  'FLDS','FLST','SLTX','SLDP','ID_SOIL','FLNAME')
      second <- c('L','XCRD','YCRD','ELEV','AREA','SLEN','FLWR','SLAS')
      write.tier(filex$FIELDS[,first],
                 file.name=filex.name,fmt.list=fmt.tmp)
      write('',filex.name,append=TRUE)
      write.tier(filex$FIELDS[,second],
                 file.name=filex.name,fmt.list=fmt.tmp)
      write('',filex.name,append=TRUE)
    }
    if('INITIAL CONDITIONS'%in%names(filex)){
      fmt.tmp <- fmt.filex()
      write('*INITIAL CONDITIONS',filex.name,append=TRUE)
      write.tier(filex$`INITIAL CONDITIONS`[[1]],
                 file.name=filex.name,fmt.list=fmt.tmp)
      write('',filex.name,append=TRUE)
      write.tier(filex$`INITIAL CONDITIONS`[[2]],
                 file.name=filex.name,fmt.list=fmt.tmp)
      write('',filex.name,append=TRUE)
    }
    if('PLANTING DETAILS'%in%names(filex)){
      fmt.tmp <- fmt.filex.plt()
      write('*PLANTING DETAILS',filex.name,append=TRUE)
      write.tier(filex$`PLANTING DETAILS`,
                 file.name=filex.name,fmt.list=fmt.tmp)
      write('',filex.name,append=TRUE)
    }
    if('IRRIGATION AND WATER MANAGEMENT'%in%names(filex)){
      fmt.tmp <- fmt.filex.irr()
      write('*IRRIGATION AND WATER MANAGEMENT',filex.name,append=TRUE)
      write.tier(filex$`IRRIGATION AND WATER MANAGEMENT`[[1]],
                 file.name=filex.name,fmt.list=fmt.tmp)
      write('',filex.name,append=TRUE)
      write.tier(filex$`IRRIGATION AND WATER MANAGEMENT`[[2]],
                 file.name=filex.name,fmt.list=fmt.tmp)
      write('',filex.name,append=TRUE)
    }
    if(any(c('FERTILIZERS','FERTILIZERS (INORGANIC)')%in%names(filex))){
      fmt.tmp <- fmt.filex()
      write('*FERTILIZERS (INORGANIC)',filex.name,append=TRUE)
      write.tier(filex$`FERTILIZERS`,
                 file.name=filex.name,fmt.list=fmt.tmp)
      write('',filex.name,append=TRUE)
    }
    if('CHEMICAL APPLICATIONS'%in%names(filex)){
      fmt.tmp <- fmt.filex.chem()
      write('*CHEMICAL APPLICATIONS',filex.name,append=TRUE)
      write.tier(filex$`CHEMICAL APPLICATIONS`,
                 file.name=filex.name,fmt.list=fmt.tmp)
      write('',filex.name,append=TRUE)
    }
    if('ENVIRONMENT MODIFICATIONS'%in%names(filex)){
      write('*ENVIRONMENT MODIFICATIONS',filex.name,append=TRUE)
      write(c('@E ODATE EDAY  ERAD  EMAX  EMIN  ERAIN ECO2  EDEW  EWIND ENVNAME',
              write.filex.env.sec(filex$`ENVIRONMENT MODIFICATIONS`)),
            filex.name,append=TRUE)
      write('',filex.name,append=TRUE)
    }
    if('HARVEST DETAILS'%in%names(filex)){
      fmt.tmp <- fmt.filex.harv()
      write('*HARVEST DETAILS',filex.name,append=TRUE)
      write.tier(filex$`HARVEST DETAILS`,
                 file.name=filex.name,fmt.list=fmt.tmp)
      write('',filex.name,append=TRUE)
    }
    if('SIMULATION CONTROLS'%in%names(filex)){
        fmt.tmp <- fmt.filex.sim()
        if('S'%in%colnames(filex$`SIMULATION CONTROLS`)){
            s <- which(colnames(filex$`SIMULATION CONTROLS`)=='S')
            colnames(filex$`SIMULATION CONTROLS`)[s] <- 'N'
        }
      filex$`SIMULATION CONTROLS`$GENERAL <- 'GE'
      filex$`SIMULATION CONTROLS`$OPTIONS <- 'OP'
      filex$`SIMULATION CONTROLS`$METHODS <- 'ME'
      filex$`SIMULATION CONTROLS`$MANAGEMENT <- 'MA'
      filex$`SIMULATION CONTROLS`$OUTPUTS <- 'OU'
      filex$`SIMULATION CONTROLS`$PLANTING <- 'PL'
      filex$`SIMULATION CONTROLS`$IRRIGATION <- 'IR'
      filex$`SIMULATION CONTROLS`$NITROGEN <- 'NI'
      filex$`SIMULATION CONTROLS`$RESIDUES <- 'RE'
      filex$`SIMULATION CONTROLS`$HARVEST <- 'HA'
      first <- c('N','GENERAL','NYERS','NREPS','START','SDATE',
                 'RSEED','SNAME')
      second <- c('N','OPTIONS','WATER','NITRO','SYMBI','PHOSP',
                  'POTAS','DISES','CHEM','TILL','CO2')
      third <- c('N','METHODS','WTHER','INCON','LIGHT','EVAPO',
                 'INFIL','PHOTO','HYDRO','NSWIT','MESOM','MESEV','MESOL')
      fourth <- c('N','MANAGEMENT','PLANT','IRRIG','FERTI',
                  'RESID','HARVS')
      fifth <- c('N','OUTPUTS','FNAME','OVVEW','SUMRY','FROPT','GROUT',
                 'CAOUT','WAOUT','NIOUT','MIOUT','DIOUT','LONG','CHOUT',
                 'OPOUT')
      if('VBOSE'%in%colnames(filex$`SIMULATION CONTROLS`)){
          fifth <- gsub('LONG','VBOSE',fifth)
      }
      sixth <- c('N','PLANTING','PFRST','PLAST','PH2OL','PH2OU',
                 'PH2OD','PSTMX','PSTMN')
      seventh <- c('N','IRRIGATION','IMDEP','ITHRL','ITHRU','IROFF',
                   'IMETH','IRAMT','IREFF')
      eighth <- c('N','NITROGEN','NMDEP','NMTHR','NAMNT','NCODE','NAOFF')
      ninth <- c('N','RESIDUES','RIPCN','RTIME','RIDEP')
      tenth <- c('N','HARVEST','HFRST','HLAST','HPCNP','HPCNR')
      for (i in 1:nrow(filex$`SIMULATION CONTROLS`)){
        write('*SIMULATION CONTROLS',filex.name,append=TRUE)
        write.tier(filex$`SIMULATION CONTROLS`[i,first],
                 file.name=filex.name,fmt.list=fmt.tmp)
        write.tier(filex$`SIMULATION CONTROLS`[i,second],
                 file.name=filex.name,fmt.list=fmt.tmp)
        write.tier(filex$`SIMULATION CONTROLS`[i,third],
                 file.name=filex.name,fmt.list=fmt.tmp)
        write.tier(filex$`SIMULATION CONTROLS`[i,fourth],
                 file.name=filex.name,fmt.list=fmt.tmp)
        write.tier(filex$`SIMULATION CONTROLS`[i,fifth],
                 file.name=filex.name,fmt.list=fmt.tmp)
        write(c('','@  AUTOMATIC MANAGEMENT'),filex.name,append=TRUE)
        write.tier(filex$`SIMULATION CONTROLS`[i,sixth],
                 file.name=filex.name,fmt.list=fmt.tmp)
        write.tier(filex$`SIMULATION CONTROLS`[i,seventh],
                 file.name=filex.name,fmt.list=fmt.tmp)
        write.tier(filex$`SIMULATION CONTROLS`[i,eighth],
                 file.name=filex.name,fmt.list=fmt.tmp)
        write.tier(filex$`SIMULATION CONTROLS`[i,ninth],
                 file.name=filex.name,fmt.list=fmt.tmp)
        write.tier(filex$`SIMULATION CONTROLS`[i,tenth],
                 file.name=filex.name,fmt.list=fmt.tmp)
        write('',filex.name,append=TRUE)
      }
    }
    return(invisible(0))
}

