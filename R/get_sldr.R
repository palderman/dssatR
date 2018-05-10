get.sldr <- function(drainage.class){
    sldr <- vector(mode='numeric',length=length(drainage.class))

    sldr[drainage.class=='Excessively drained']=0.85
    sldr[drainage.class=='Somewhat excessively drained']=0.75
    sldr[drainage.class=='Well drained']=0.60
    sldr[drainage.class=='Moderately well drained']=0.40
    sldr[drainage.class=='Somewhat poorly drained']=0.25
    sldr[drainage.class=='Poorly drained']=0.05
    sldr[drainage.class=='Very poorly drained']=0.01

    return(sldr)
}
