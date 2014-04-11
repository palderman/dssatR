lyr.depths <- function(sl.prof){
    depth = sl.prof$layer$SLB
    depth = depth-c(0,depth[1:(length(depth)-1)])
    return(depth)
}

