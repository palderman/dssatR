convert.pos <- function(lgdpos,xlim,ylim){
        switch(lgdpos,
                topleft={lgdx=xlim[1];lgdy=ylim[2];lgdxjust=0;lgdyjust=1},
                top={lgdx=(xlim[2]-xlim[1])*0.5+xlim[1]
                        lgdy=ylim[2];lgdxjust=0.5;lgdyjust=1},
                topright={lgdx=xlim[2];lgdy=ylim[2];lgdxjust=1;lgdyjust=1},
                middleleft={lgdx=xlim[1]
                            lgdy=(ylim[2]-ylim[1])*.5+ylim[1]
                            lgdxjust=0;lgdyjust=0.5},
                middleright={lgdx=xlim[2]
                            lgdy=(ylim[2]-ylim[1])*.5+ylim[1]
                            lgdxjust=1;lgdyjust=0.5},
                bottomleft={lgdx=xlim[1];lgdy=ylim[1];lgdxjust=0;lgdyjust=0},
                bottom={lgdx=(xlim[2]-xlim[1])*0.5+xlim[1]
                        lgdy=ylim[1];lgdxjust=0.5;lgdyjust=0},
                bottomright={lgdx=xlim[2];lgdy=ylim[1];lgdxjust=1;lgdyjust=0},
        )
        return(list(x=lgdx,y=lgdy,xjust=lgdxjust,yjust=lgdyjust))
}

