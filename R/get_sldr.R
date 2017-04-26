get.sldr <- function(drainage.class){
    sldr <- switch(drainage.class,
                   'Excessively drained'=0.85,
                   'Somewhat excessively drained'=0.75,
                   'Well drained'=0.60,
                   'Moderately well drained'=0.40,
                   'Somewhat poorly drained'=0.25,
                   'Poorly drained'=0.05,
                   'Very poorly drained'=0.01)
    return(sldr)
}
