cropgro.fmt <- function(){
    fmt = list(
        `%6.1f`=c('THVAR','PL-EM','EM-V1','V1-JU','JU-R0','LNGSH','R7-R8','OPTBI'),
        `%6.2f`=c('PM06','PM09','FL-VS','TRIFL','RWDTH','RHGHT','KCAN'),
        `%3.2i`=c('MG','TM'),
        `%6.3f`=c('R1PPO','SLOBI'),
        `%6s`=c('EXPNO','EXP#'),
        `%-7s`=c('@ECO#','@VAR#'),
        `%7s`=c('ECO#','VAR#'),
        `%-16s`=c('ECONAME........','VRNAME..........','VAR-NAME........'),
        `%-18s`=c('ECONAME..........')
        )
    return(fmt)
}
