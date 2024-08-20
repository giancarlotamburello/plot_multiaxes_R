plotMaxis <- function(df1=NULL,df2=NULL,df3=NULL,df4=NULL,xlab="",xlim=NULL,xaxt="s",
                   ylabels=c("DF1","DF2","DF3","DF4"),pch=c(1,1,1,1),lwd=c(1,1,1,1),
                   lty=c(1,1,1,1),
                   type=c("p","p","p","p"),color=c("black","red","blue","purple"),
                   y.num.tick=c(5,5,5,5),x.num.tick=20,vert.shift=0){

  normalize <- function(x,compl.cases=F,x.range=NULL) {
    if (is.null(dim(x))) {x=(x - min(x,na.rm = T)) / (max(x,na.rm = T) - min(x,na.rm = T))}
    else{ for (i in 1:ncol(x)) {x[,i]=((x[,i] - min(x[,i],na.rm = T)) / (max(x[,i],na.rm = T) - min(x[,i],na.rm = T)))}}
    if(!is.null(x.range)){x=x*diff(x.range)+x.range[1]}
    if (compl.cases) {return(x[complete.cases(x),])}
    else{return(x)}
  }
  dn1 <- df1
  dn2 <- df2
  dn3 <- df3
  dn4 <- df4
  
  dn1[,2] <- normalize(df1[,2])
  dn2[,2] <- normalize(df2[,2])+vert.shift
  dn3[,2] <- normalize(df3[,2])+vert.shift*2
  dn4[,2] <- normalize(df4[,2])+vert.shift*3
  
  par(mar=c(5.1,6.1,4.1,6.1))
  if (!is.null(df3) & is.null(df4)) {
    par(mar=c(5.1,10.1,4.1,6.1))  
  }else if (!is.null(df4)) {
    par(mar=c(5.1,10.1,4.1,10.1))  
  }
  
  
  # Plot number 1
  plot(dn1,yaxs="i",ylab="",xlab=xlab,ylim=c(-0.05,1.05+vert.shift+vert.shift*(!is.null(df3))+vert.shift*(!is.null(df4)))
       ,yaxt="n",xlim=xlim,xaxt=xaxt,
       type=type[1],col=color[1],pch=pch[1],lwd=lwd[1],lty=lty[1])
  
  mtext(ylabels[1],2,3,col=color[1])
  
  # Plot number 2
  points(dn2,type=type[2],col=color[2],pch=pch[2],lwd=lwd[2],lty=lty[2])
  axis(2,at = seq(0,1,length.out=y.num.tick[1]),col.axis=color[1],
       labels = format(seq(min(df1[,2],na.rm = T),max(df1[,2],na.rm = T),length.out=y.num.tick[1]),digits=1))
  
  axis(4,at = seq(0+vert.shift,1+vert.shift,length.out=y.num.tick[2]),col.axis=color[2],
       labels = format(seq(min(df2[,2],na.rm = T),max(df2[,2],na.rm = T),length.out=y.num.tick[2] ),digits=1))
  mtext(ylabels[2],4,3,col=color[2])
  
  # Plot number 3
  if (!is.null(df3)) {
    points(dn3,type=type[3],col=color[3],pch=pch[3],lwd=lwd[3],lty=lty[3])
    axis(2,at = seq(0+vert.shift*2,1+vert.shift*2,length.out=y.num.tick[3]),line=5,col.axis=color[3],
         labels = format(seq(min(df3[,2],na.rm = T),max(df3[,2],na.rm = T),length.out=y.num.tick[3] ),digits=1))
    mtext(ylabels[3],2,7.5,col=color[3])
  }
  
  # Plot number 4
  if (!is.null(df4)) {
    points(dn4,type=type[4],col=color[4],pch=pch[4],lwd=lwd[4],lty=lty[4])
    axis(4,at = seq(0+vert.shift*3,1+vert.shift*3,length.out=y.num.tick[4]),line=5,col.axis=color[4],
         labels = format(seq(min(df4[,2],na.rm = T),max(df4[,2],na.rm = T),length.out=y.num.tick[4] ),digits=1))
    mtext(ylabels[4],4,7.5,col=color[4])
  }
  
  par(mar=c(5.1,4.1,4.1,2.1))
}
