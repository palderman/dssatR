cropsim.fmt <- function(){
    fmt = list(
        `%6.0f`=c('P1','P2','P3','P4','P5','P6','P7','P8','PHINT','HTSTD',
            'RS%A','TKFH','GWTAT','G#WTS'),
        `%6.1f`=c('VREQ','VBASE','PPS1','PPS2','LA1S','SHWTS','GWTS',
            'PARUE','PARU2','PHL2','PHF3','SLAS','LSENI','LSPHS','LSPHE',
            'TIL#S','TIPHE','TIFAC','TDPHS','TDPHE','TDFAC','RDGS','AWNS',
            'GN%S','GN%MN','GM%H','SSPHS','SSPHE','GWTAF','NUPWF'),
        `%6.2f`=c('VEFF','LAFV','LAFR','TDSF','KCAN','G#RF','NUPNF'),
        `%6.3f`=c('RTNUP'),
        `%5s `=c('EXP#'),
        `%6s`=c('ECO#','VAR#','EXPNO'),
        ` %-16s`=c('ECONAME........','VRNAME..........'),
        ` %-17s`=c('VAR-NAME........'),
        ` %-18s`=c('ECONAME..........')
        )
    return(fmt)
}
