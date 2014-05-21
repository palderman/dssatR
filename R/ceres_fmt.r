ceres.fmt <- function(){
    fmt = list(
        `%6.0f`=c('P1','P2','P3','P4','PHL2','SLAS','HTSTD',
            'RS%S','TKFH','P1V','P1D','P5','G1','G2','PHINT'),
        `%6.1f`=c('VEFF','PARUE','PARU2','PHF3','LA1S','LSPHS',
            'LSPHE','TIL#S','TIPHE','TIFAC','TDPHS','TDPHE',
            'TDFAC','RDGS','AWNS','GN%S','GN%MN','G3'),
        `%6.2f`=c('P2FR1','P4FR1','P4FR2','LAFV','LAFR','KCAN'),
        `%6s`=c('ECO#','VAR#','EXP#'),
        `%-16s`=c('VAR-NAME........')
        )
    return(fmt)
}
