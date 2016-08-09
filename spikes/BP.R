## fancy bp

bp <- function(x,ylab,...){
  par=list(...)
  horizontal = FALSE
  if(!is.null(par$horizontal)){
    horizontal = par$horizontal
  }


  par(mar=c(3,5,0.1,1))

  if(horizontal){
    par(mar=c(5,3,0,1))
    boxplot(x,xlab=ylab,xlim=c(0.5,1.2),...);
  }else{
    par(mar=c(3,5,0,1))
    boxplot(x,ylab=ylab,xlim=c(0.5,1.2),...);
  }

  x <- sort(x)
  cs = 0.02
  while(TRUE){
    slots = floor(.25 / (2*cs))
    l = min(x)
    h = max(x)
    int = findInterval(x,l + (0:slots)*(h-l)/slots)
    if(max(table(int)) < slots) break
    cs = cs / 5 * 4
  }
  pos = as.numeric(unlist(sapply(table(int),function(x) sample(seq(x),x)))) - 1

  if(horizontal){
    asp.ratio = par("pin")[2]/par("pin")[1]
    xscale = diff(par("usr"))[1]
    cat("aspect ratio: ",asp.ratio,"\n")
    symbols(x,pos*cs*2+0.5,circles=rep(cs*asp.ratio*xscale,length(x)),inches=FALSE,fg=NA,bg="purple",add=TRUE)
  }else{
    symbols(pos*cs*2+0.5,x,circles=rep(cs,length(x)),inches=FALSE,fg=NA,bg="purple",add=TRUE)
  }

  #segments(0,x,0.6,x,lwd=2,col=rgb(160/255,32/255,240/255,128/255))
}

bp(rnorm(20,20,10)^2,"aa")

bp(rnorm(20,20,10)^2,"aa",horizontal=TRUE)

x = rnorm(20,20,10)^2
ylab = "tempo"

f <- function(x,...){
  pars=list(...)
  if(is.null(pars$Horizontal)){
    cat("Vertical\n")
  }else{
    cat("Horizontal:",pars$Horizontal,"\n")
  }
}
f(1)
f(1,Horizontal=TRUE)
f(1,Horizontal=FALSE)
