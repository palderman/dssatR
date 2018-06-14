cropgro.fmt <- function(){
    fmt = list(
        `%6.Of`=c('SLAVR'),
        `%6.1f`=c('THVAR','PL-EM','EM-V1','V1-JU','JU-R0','LNGSH','R7-R8',
                  'OPTBI','EM-FL','FL-SH','FL-SD','SIZLF','SFDUR','PODUR',
                  'THRSH'),
        `%6.2f`=c('PM06','PM09','FL-VS','TRIFL','RWDTH','RHGHT','KCAN',
                  'CSDL','PPSEN','SD-PM','FL-LF','LFMAX','XFRT','SDPDV'),
        `%3.2i`=c('MG','TM'),
        `%6.3f`=c('R1PPO','SLOBI','WTPSD','SDPRO','SDLIP'),
        `%2s`=c('EXPNO','EXP#'),
        `%-7s`=c('@ECO#','@VAR#'),
        `%7s`=c('ECO#','VAR#'),
        `%-20s`=c('ECONAME','VRNAME','VAR-NAME')
        )
    return(fmt)
}
