print.filea <- function(filea){
    print(filea[[1]])
    NextMethod('print')
}
