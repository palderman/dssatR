dssat.class <- function(vname){
  if(vname%in%c('CU','FL','SA','IC','MP','MI','MF','MR','MC','MT','ME','MH','SM',
                'ICDAT','PDATE','EDATE','IDATE','FDATE',
                'NYERS','NREPS','SDATE','RSEED','NSWIT','FROPT',
                'PFRST','PLAST','RTIME','HFRST','HLAST')){
    the.class <- 'integer'
  }else if(vname%in%c('TNAME','CR','INGENO','CNAME','ID_FIELD','WSTA','FLST',
                      'FLDT','SLTX','ID_SOIL','FLNAME',
                      'FLHST','HSTG','HNAME','SMODEL','VBOSE','CO2',
                      'ICNAME','PCR','PLME','PLDS','PLNAME',
                      'IROFF','IAME','IRNAME','IROP','IOFF',
                      'FMCD','FACD','FERNAME','MESEV','MESOL',
                      'START','SNAME','WATER','NITRO','SYMBI','PHOSP','POTAS',
                      'DISES','CHEM','TILL','CTEMP','WTHER','INCON','LIGHT',
                      'EVAPO','INFIL','PHOTO','HYDRO','MESOM','PLANT','IRRIG',
                      'FERTI','RESID','HARVS','FNAME','OVVEW','SUMRY','GROUT',
                      'CAOUT','WAOUT','NIOUT','MIOUT','DIOUT','LONG','CHOUT',
                      'OPOUT','IMETH','NCODE','NAOFF','FOCD','FMOPT')){
    the.class <- 'char'
  }else{
    the.class <- 'float'
  }
  return(the.class)
}
