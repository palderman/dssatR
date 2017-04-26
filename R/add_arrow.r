add.arrow <- function(x,y,lbl,height=100,cex=1,hd.len=0.1,lwd=2){
arrows(x,y+height/2,x,y,length=hd.len,lwd=lwd)
text(x,y+height*.8,lbl,cex=cex)
return(invisible(NULL))
}
