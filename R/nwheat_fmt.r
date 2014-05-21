nwheat.fmt <- function(){
    fmt = list(
        `%6.0f`=c('DSGFT','VREQ','PLGP1','RTDP1','RTDP2'),
        `%6.1f`=c('P1','P5','PHINT','GRNO','SLAP1','SLAP2','P3AF','MXGWT',
            'TBASE','TOPT','ROPT','TTOP','P20','GDDE','RUE','KCAN'),
        `%6.2f`=c('VSEN','PPSEN','MXFIL','STMMX','TC1P1','TC1P2','PLGP2',
            'P2AF','P4AF','P5AF','P6AF','ADLAI','ADTIL','ADPHO','STEMN',
            'MXNUP','WFNU','EXNO3','MNNO3','EXNH4','MNNH4','INGWT','GPPSS',
            'GPPES','MNRTN','TSEN','CDAY'),
        `%6.3f`=c('DTNP1','MXNCR','PNUPR','INGNC','FREAR','MNNCR','NOMOB'),
        `%6s`=c('ECO#','VAR#','EXPNO'),
        `%-16s`=c('ECONAME........','VRNAME..........')
        )
    return(fmt)
}
