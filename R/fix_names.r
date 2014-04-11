fix.names <- function(names){
    names=unlist(lapply(names,FUN=function(x){
            switch(x,
                    LN.D='LN%D',
                    SN.D='SN%D',
                    VN.D='VN%D',
                    GN.D='GN%D',
                    RN.D='RN%D',
                    L.SD='L#SD',
                    P.AD='P#AD',
                    G.AD='G#AD',
                    SH.D='SH%D',
                    CL.D='CL%D',
                    CS.D='CS%D',
                    GL.D='GL%D',
                    GC.D='GC%D',
                    LI.D='LI%D',
                    LI.N='LI%N',
                    N.LN='N%LN',
                    N.HN='N%HN',
                    IR.C='IR#C',
                    SL.20D='SL%20D',
                    x)
            }))
    return(names)
}

