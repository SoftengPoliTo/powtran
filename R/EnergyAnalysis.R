##########################################################################
# Power Trace Analyzer
#
# Copyright Marco Torchiano <marco.torchiano@polito.it>, 2016
#
# This file is part of the `powtran` package
#
# Powtran is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# Foobar is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
#
#
##########################################################################
#library(plyr)

##########################################################################
## code by Martin Maechler <maechler at stat.math.ethz.ch>
## found on https://stat.ethz.ch/pipermail/r-help/2005-November/083376.html
## MTk added the extend option for peaks close the the edges
#
peaks <- function(series, span = 3, do.pad = FALSE, extend=TRUE) {
  if((span <- as.integer(span)) %% 2 != 1) stop("'span' must be odd")
  s1 <- 1:1 + (s <- span %/% 2)
  if(span == 1) return(rep.int(TRUE, length(series)))
  if(extend){
    series = c(rep(series[1],s),series,rep(tail(series,1),s))
    do.pad = FALSE
  }
  z <- embed(series, span)
  v <- apply(z[,s1] > z[, -s1, drop=FALSE], 1, all)
  if(do.pad) {
    pad <- rep.int(FALSE, s)
    c(pad, v, pad)
  } else v
}

###############################################
## Compute moving mean from a vector
##
moving.mean <- function(x,n,smoothed=TRUE){
  cx <- cumsum(x)
  if(smoothed){
      left = floor((n-1)/2)
      rsum <- c(cx[1:left+n/2-1]/(1:left+n/2-2),
                (cx[n:length(x)] - c(0,cx[1:(length(x)-n)]))/n,
                (cx[length(x)]-cx[(length(x)-n+1):(length(x)-n/2)]) / (n-1):(n/2)
                )
  }else{
    rsum <- (cx[n:length(x)] - c(0,cx[1:(length(x)-n)]))/n
  }
  return( rsum )
}

##################################################################
## Compute outliers using the Tuckey test
##
outliers <- function(x,iqm=3,index=F,logic=F){
  qs = quantile(x,c(.25,.5,.75),na.rm = T)
  iqr = abs(qs[1]-qs[3])

  l = x < qs[1]-iqm*iqr | x > qs[3]+iqm*iqr
  if(logic){
    return( l )
  }
  if(index){
    return(which(l))
  }
  x[l]
}

############################################################
#
# .ds downsample a time series
#
.ds <- function(x,t.sampling,ns=2048){
  n = length(x)
  step = floor(n/ns);
  samples = seq(round(step/2),n,by=step)
  data.frame(t = (samples-1)*t.sampling,
             P = x[samples]
  )
}


# dsplot <- function(x,ns=2000,main=NULL){
#   m = mean(x)
#   plot(P ~ t ,data=.ds(x,1),t="l")
#   abline(m,0,col="red")
# #   text(0,m,
# #        formatC(m,digits=3),col="red",adj = c(1,0),xpd=TRUE)
# #   if(!is.null(main)){
# #     title(main)
# #   }
# #   #xs = seq(0,2000,by=2000/30)
# #   #segments(xs,rep(1,length(xs)),xs,rep(1.7,length(xs)),col="orange",lty=2)
# #   mml = 40000
# #   mm = moving.mean(x,mml)
# #   lines(samples+mml/2,mm[samples],col="#ffffff7f",lty=1, lwd=2)
# #   lines(samples+mml/2,mm[samples],col="purple",lty=2)
# }
#

######################################################################################
#
# Computes the average power for the working tasks in a series of measures.
#
# The computation assumes the execution follows a well defined protocol
# to mark tasks begin and end. A typical profile is:
#
#       --------      --v-^v^--
#       |      |      |       |
#       |      |      |       |
# -------      --------       -....
#  SLEEP: WAIT :SLEEP : WORK  :
#
#  SLEEP : is a period of sleep for a given (marker.length) time
#  WAIT  : is a period of busy waiting for a given (marker.length) time
#  WORK : is a period of actual work (the one we aim to measure the energy consumption of)
#
# Parameters:
# ----------
# data: time series with power measures in the variable "P"
# adjust: parameter to identify levels in the input
# N: expected number of task repetition
# marker.length : length of task begin marker in samples
# marker.tolerance : percentage of tolerance for markers, used to match the markers
# generation-number : max number of iteration to generate markers
# noise.force : force a value for the noise length
#
# Return:
# ------
#
# If no intermediate results are required (intermediate=FALSE)
# a data frame containing the power and energy information for
# each execution run.
#
# A list containing the following elements:
#
# - work: a dataset, rows represent the work periods, for each period we report
#   start, end, length and P
#
# - baseline : the baseline power considered the idle level
#
# - noise: the estimated length of noise runs
#
utils::suppressForeignCheck(c("res", "task.id","keep","start","end"))
extract.power <- function(data,
                            t.sampling,            # sampling period [s]
                            N=30,                  # number of markers [#]
                            marker.length=5000,    # marker pulse widht [samples]
                            marker.tolerance=0.25, # marker width tolerance [%]
                            cutoff = 9,            # Cut-off freq. [Hz]
                            adjust = 2,            # density function smoothing
                            peakspan = 69,         # width of peak detection
                            intermediate=FALSE,    # returns intermediate computations
                            include.rawdata=FALSE, # should rawdata be included?
                            baseline = "both"
){
  #start <- end <- NULL # to mute down CMD check
  ## Arguments parsing and adjustment ##
  if(is.data.frame(data)){
    if(! "P" %in% names(data)){
      stop("data must have a 'P' column" )
    }
  }else{
    if(is.numeric(data)){
      data = data.frame(P = data)
    }
  }

  if(marker.length < 10){ ## too short, then it's supposed to be in secs
    marker.length <- round(marker.length / t.sampling)
#     warning("Marker lenght is too short, it is intended to be in seconds\n",
#             t.sampling," sec = ", marker.length, " samples")
  }else{

  }

  time.start = Sys.time()

  result = list()
  class(result) = "EnergyAnalysis"
  result$t = t.sampling
  result$n = length(data$P)
  if(include.rawdata){
      result$P = data$P
  }else{
      ns = 2048
      step = floor(result$n/ns);
      samples = seq(round(step/2),result$n,by=step)
      result$P.sample = data.frame(t = (samples-1)*t.sampling,
                                   P = data$P[samples])
  }


  ## 1. Density analysis and level identification ##

  ## Distribution density of values
  dens <- density(data$P,adjust=adjust,n=2048)

  ## Find peaks in the distribution (the most common power levels)

  ## Tries different span values to be able to find at least
  ## two distinct peaks
  spans = rev(seq(9,peakspan,by=2))
  for(sp in spans){
    dens$peaks <- which(peaks(dens$y,span=sp) & dens$y>mean(dens$y))
    if(length(dens$peaks) >= 2){
      break
    }
  }
  if(length(dens$peaks)<=1){
    stop("The data is unimodal: impossible to identify edges!")
  }

  ## Identify level thresholds between levels
  ## as the points of minimum density between peaks
  thresholds.ids = apply(cbind(head(dens$peaks,-1),
                               tail(dens$peaks,-1)),1,
                         function(x){
                           ss = dens$y[x[1]:x[2]]
                           x[1] + which(ss == min(ss))
                         })
  thresholds = dens$x[thresholds.ids]

  valid =
    dens$y[thresholds.ids]/head(dens$y[dens$peaks],-1)<0.66 &
    dens$y[thresholds.ids]/tail(dens$y[dens$peaks],-1)<0.66

  thresholds = thresholds[valid]
  if(intermediate){
    result$peaks = dens$x[dens$peaks]
    result$thresholds = thresholds
  }

  ## 2. Tagging samples ##
  ## tag levels are those corresponding to peaks
  tag.levels <- paste0("L",seq(dens$peaks))

  ## low pass filter (using moving average or FFT (this latter slower))
  #f.cut = dim(data)[1] * t.sampling * cutoff
  #lP = lowpass(data$P,f.cut)
  window.width = round(1/(t.sampling*2*cutoff))
  lP = moving.mean(data$P,window.width) ## 20 times faster!!


  ## tag the samples
  data$tag <- factor(tag.levels[findInterval(lP,c(0,thresholds))],
                     tag.levels,
                     ordered=T)

  ## detect edges
  edges = 1+which(diff(as.numeric(data$tag))!=0)
  if(length(edges)==0){
    stop("Could not find any transition edge in the signal!")
  }

  ## Locally improve edge detection
  edges1 <- sapply(edges, function(edge){
    from = max(1,edge-round(window.width/2))
    to = min(edge+round(window.width/2),length(data$P))
    if(to-from<window.width/2) return(edge)
    values = data$P[from:to]
    tlevels = c(
      median(values[1:round(window.width/4)]),
      median(values[(length(values)-round(window.width/4)):length(values)])#,
      #         min(values),
      #         max(values)
    )
    half= mean(tlevels)
    crosses = from+which(sign(diff(sign(values-half)))==sign(tlevels[2]-tlevels[1]))
    if(length(crosses)==1) return(crosses)

    lcross = from+which(diff(sign(lP[from:to]-half))!=0)
    if(length(lcross)==0) lcross = edge
    if(length(lcross)>1) lcross = lcross[1]
    ml.cross = crosses[which.min(abs(crosses-lcross))]
    return(ml.cross)
  })

  while(TRUE){
    overpass = which(head(edges1,-1)>tail(edges,-1)) + 1
    if(length(overpass)==0) break;
    edges <- edges[-overpass]
    edges1 <- edges1[-overpass]
  }
  edges <- unique(sort(edges1))

  id.tab = data.frame(start=c(1,edges),
                      end=c(edges-1,length(data$P)),
                      length = diff(c(1,edges,length(data$P)+1))
  )

  while(length(short.runs <- which(id.tab$length<window.width)) > 0){
    #short.runs = which(id.tab$length<window.width)
    if(short.runs[1]==1){
      short.runs.pre <- short.runs[-1]
    }else{
      short.runs.pre = short.runs
    }
    check.prev = sapply(short.runs.pre,function(r){
      diff(
        findInterval(c(mean(data$P[id.tab$start[r]:id.tab$end[r]]),
                       mean(data$P[id.tab$start[r-1]:id.tab$end[r-1]])),
                     thresholds))==0
    })
    if(!any(check.prev)) break
    id.tab$end[short.runs.pre[check.prev]-1] =
      id.tab$end[short.runs.pre[check.prev]]
    id.tab <- id.tab[-short.runs.pre[check.prev],]
    id.tab$length = id.tab$end - id.tab$start + 1
  }

  while(length(short.runs <- which(id.tab$length<window.width)) > 0){
    #short.runs = which(id.tab$length<window.width)

    if(tail(short.runs,1)==dim(id.tab)[1]){
      short.runs.post <- head(short.runs,-1)
    }else{
      short.runs.post = short.runs
    }
    check.post = sapply(short.runs.post,function(r){
      diff(
        findInterval(c(mean(data$P[id.tab$start[r]:id.tab$end[r]]),
                       mean(data$P[id.tab$start[r+1]:id.tab$end[r+1]])),
                     thresholds))==0
    })
    if(!any(check.post)) break
    id.tab$start[short.runs.post[check.post]+1] =
      id.tab$start[short.runs.post[check.post]]
    id.tab <- id.tab[-short.runs.post[check.post],]
    id.tab$length = id.tab$end - id.tab$start + 1
  }

  id.tab = within(id.tab,{
    length = end-start+1
    #    tag = data$tag[start]
    tag = factor(tag.levels[findInterval(apply(data.frame(start,end),1,
                                               function(x) mean(data$P[x[1]:x[2]])),
                                         c(0,thresholds))],
                 tag.levels,
                 ordered=T)
    runid = seq_along(start)
  })

#   ## assign unique id to runs
#   data$runid <- cumsum(c(1,abs(diff(as.numeric(data$tag))!=0) ) )
#
#   ## build run id summary table
#   ## (this will be used for further computations
#   ##  because it is much more compact)
#   id.tab = subset(melt(with(data,table(tag,runid)),id.vars="runid",value.name="length"),length!=0)
#   id.tab$start=cumsum(c(1,head(id.tab$length,-1)))
#   id.tab$end=cumsum(id.tab$length)
#   id.tab$tag <- factor(id.tab$tag,tag.levels,ordered=T)
  if(intermediate){
    result$id.tab = id.tab
  }


## 3. Identify markers ###

  ## rising and falling edges
  rising = c(FALSE,diff(as.integer(id.tab$tag))>0) # run after rising edge
  falling = c(FALSE,diff(as.integer(id.tab$tag))<0) # run after falling edge

  ## range of acceptable marker lengths
  l.min = marker.length*(1-marker.tolerance)
  l.max = marker.length*(1+marker.tolerance)

  ## initial marker candidates are all those
  ## after a rising edge and within the acceptable length range
  initial.markers = subset(id.tab,rising &
                     start > marker.length/2 &
                     length>=l.min &
                     length<=l.max)

  if(dim(initial.markers)[1] <= 1 ){
    stop("Could not find any marker")
    #return( data.frame(start=c(),end=c(),length=c(),P=c(),e=c() ))
  }

  ## Prune potentially extra markers
  min.marker.period = as.integer(3 * marker.length * ( 1 - marker.tolerance))
  max.marker.period = as.integer(length(data$P) / (N-1))

  mstart = initial.markers$start[1]
  mend = tail(initial.markers$start,1)
  w = mend - mstart

  n.min = round(w / max.marker.period)
  n.max = min(N+5,round(w / min.marker.period))

  nums = n.min:n.max

  fit <- function(x,offset,freq,order=3){
    (1+cos((x-offset)*freq))^order/2^order
  }

  conv = sapply(nums,function(n){
    freq = 2 * pi / w * n
    #(1+cos((initial.markers$start-mstart)*freq))^2/4
    fit(initial.markers$start,mstart,freq,2)
  })

  index.ml = which.max(apply(conv,2,sum))

  wave.ml = apply(conv,2,sum)
  names(wave.ml) <- nums
#    cat("Wave fit options:\n")
#    print(wave.ml)


  n = nums[index.ml] # best fitting number of runs
  freq = 2 * pi / w * n
  period = round(w/n)

  # find best shift
  mstart.opts = seq(from=mstart-marker.length,
                      to=mstart+marker.length,
                      by=1)
  fit.opts = sapply(mstart.opts,function(s)
                            sum(fit(initial.markers$start,s,freq)))

  mstart = mstart.opts[which.max(fit.opts)]

  conv = fit(initial.markers$start,mstart,freq,2)
  ## select only the markers fitting the best fitting number of runs
  #markers.ml.l = conv > .1
  #initial.markers <- subset(initial.markers,markers.ml.l)

#     cat("Initial markers:\n")
#     print(initial.markers)
  ## Try to find out other (missing markers)
  selected.markers = initial.markers
  selected.markers$Gen = 0

  potential.markers = subset(id.tab,rising &
                               ! runid %in% initial.markers$runid)

  conv =  fit(potential.markers$start,mstart,freq,2)

#    cat("Potential markers:\n")
#    print(data.frame(start=potential.markers$start,score=conv))
  #extended.markers = subset(potential.markers,conv>0.005&start>l.min)
  extended.markers = subset(potential.markers,start>l.min)

  if(all(!is.null(extended.markers), dim(extended.markers)[1]>0)){
    extended.markers$Gen = max(selected.markers$Gen) + 1
    selected.markers = rbind(selected.markers,extended.markers)
    selected.markers = selected.markers[order(selected.markers$start),]
  }


  ## remove fake markers

#   period = round(median(diff(selected.markers$start)))
#   if(period>min.marker.period & period <max.marker.period){
#     freq = 2*pi/period
#   }else{
#     period = round(2*pi/freq)
#   }

#   cat("mstart:" ,mstart,"\n")
#   cat("period:" ,period,"\n")
  mid.tasks = seq(mstart%%period+round(period/2),
                 max(id.tab$start)+period,
                 period)

#   cat("mid tasks (pre):\n")
#   print(mid.tasks)

  indexes = findInterval(initial.markers$start,mid.tasks)+1
  count.markers = table(factor(indexes,1:length(mid.tasks)))

  upper.task = mid.tasks
#  upper.task[which(count.markers==1)] = initial.markers$start[indexes %in% which(count.markers==1)] + l.max

  lower.task = mid.tasks-period
#  lower.task[which(count.markers==1)] = initial.markers$start[indexes %in% which(count.markers==1)] - l.max

  find.id = function(x,lower.task,upper.task){
    up = findInterval(x,upper.task)+1
    low = findInterval(x,lower.task)
    matching = up==low

    up[!matching] <- NA

    up
  }
  task.id <- NULL ## just to avoid a NOTE from R CMD check
  keep <- NULL
  best.markers = ddply(data.frame(
#    task.id = findInterval(selected.markers$start,mid.tasks),
    task.id = find.id(selected.markers$start,lower.task,upper.task),
    start = selected.markers$start,
    conv = fit(selected.markers$start,mstart,freq)
    ),.(task.id),transform,
    keep= conv==max(conv) & !is.na(task.id))
  best.markers <- best.markers[order(best.markers$start),]

#   cat("best markers:\n")
#   print(best.markers)

  selected.markers = selected.markers[best.markers$keep,]
  rownames(selected.markers) <- NULL

#   cat("selected markers:\n")
#   print(selected.markers)

  ## identify end of markers (matching a falling edge)

   ## first of all realign freq and shift to newly identified markers
#   period = round(median(diff(selected.markers$start)))
#   freq = 2*pi/period
#   mstart = selected.markers$start[1]
  mstart.opts = seq(from=mstart-marker.length,
                    to=mstart+marker.length,
                    by=1)
  fit.opts = sapply(mstart.opts,function(s)
                      sum(fit(initial.markers$start,s,freq)))
  mstart = mstart.opts[which.max(fit.opts)]



  potential.markers = subset(id.tab,falling)

  shift = median(selected.markers$length)

#   cat("mstart=",mstart,"\n")
#   cat("shift=",shift,"\n")

  upper.task <- selected.markers$start + 2*shift
  lower.task <- selected.markers$start + shift/2

  best.ends = ddply(data.frame(
#    task.id = findInterval(potential.markers$start,selected.markers$start),
    task.id = find.id(potential.markers$start,lower.task,upper.task),
    start = potential.markers$start,
    end = potential.markers$end,
    conv = fit(potential.markers$start,mstart+shift,freq)
  ),.(task.id),transform,
  keep=conv==max(conv) & !is.na(task.id))


  best.ends = subset(best.ends, task.id>0 & keep)


  selected.markers[best.ends$task.id,]$end = best.ends$start-1
  selected.markers$length = selected.markers$end - selected.markers$start


  # infer ends for markers with anomalous length
  anomalous.length = with(selected.markers,length<l.min|length>l.max)
  expected.length = median(selected.markers$length[!anomalous.length])
  selected.markers$end[anomalous.length] = selected.markers$start[anomalous.length] + expected.length
  selected.markers$length[anomalous.length] = expected.length
  #selected.markers = subset(selected.markers,length>l.min&length<l.max)

  if(intermediate){
    result$markers = selected.markers
  }


## 4. Compute Power used in work sections ##

  work = data.frame(
    start = head(apply(selected.markers[c("start","end")],1,
                        function(m) min(m["start"]+2*marker.length,
                                        m["end"]+marker.length)),-1),
    end = tail(selected.markers,-1)$start-marker.length
  )
  work <- within(work,
                 duration <- (end-start+1)*t.sampling
  )

  if(dim(work)[1]>0){
    work$P.real = apply(work,1,function(x)
                      mean(data$P[x["start"]:x["end"]])
                  )
    work$P.idle.left = apply(work,1,function(x)
                          median(data$P[(x["start"]-marker.length):(x["start"]-1)])
                        )
    work$P.idle.right = apply(work,1,function(x)
                          median(data$P[(x["end"]+1):(x["end"]+marker.length)])
                        )
    rownames(work) <- NULL ## renumber rows

    anomalous.duration = work$duration<0 |
                          work$duration>(period-marker.length)*t.sampling
    if(any(anomalous.duration)){
      work[anomalous.duration,]$duration <- NA
    }

    result$baseline = baseline
    work = effective.power(work,baseline)

  }else{
    work$P=c()
    work$E=c()
  }
  result$work = work
  result$n.valid = sum(!is.na(work$P))
  result$elapsed = Sys.time() - time.start
  return(result)
}


effective.power <- function(work,baseline="both",FUN=median){
  baseline.options = c("both","left","right", "gboth","gleft","gright","gmin")
  baseline <- tolower(baseline)
  bl.sel = grep(paste("^",baseline,sep=""),baseline.options)
  if(length(bl.sel)==0 || length(bl.sel)>1){
    stop("baseline should match exactly one of ( '",
         paste(baseline.options,collapse="', '"),"')")
  }

  P.idle =
  with(work,
  switch(bl.sel,
    #1: both
    apply(rbind(P.idle.left,P.idle.right),2,FUN),
    #2: left
    P.idle.left,
    #3: right
    P.idle.right,
    #4: gboth
    FUN(c(P.idle.left,P.idle.right)),
    #5: gleft
    FUN(P.idle.left),
    #6: gright
    FUN(P.idle.right),
    #7: gmin
    min(c(P.idle.left,P.idle.right))
  ))

  work$P = work$P.real - P.idle
  work$P[is.na(work$duration)] <- NA
  work$E = work$P * work$duration
  work$E[work$P<0] <- NA
  work$P[work$P<0] <- NA
  return( work )
}


summary.EnergyAnalysis <- function(object, ...){
  x <- object
  cat("Power Trace Analysis\n\n")
  d = as.difftime(x$n*x$t,units = "secs")
  if(d>=as.difftime("0:1:0")){
    units(d) <- "mins"
  }
  cat("Trace:",x$n,"samples, at ", 1/x$t, "Hz (elapsed,",format(d,digits=2),")","\n")
  cat("Identified:",dim(x$work)[1],"cycles")
  n.valid = sum(!is.na(x$work$P))
  if(n.valid!=dim(x$work)[1]){
    nd = sum(is.na(x$work$duration))
    np = dim(x$work)[1] - n.valid - nd
    cat(" (", n.valid, "with valid power values:\n\t\t\t",
        nd, "invalid due to anomalous duration,\n\t\t\t",
        np, "invalid due to negative effective power)\n\n"
        )
  }else{
    cat(" (all valid)\n\n")
  }
#  cat("\nAll data outliers:\n")
  cat("Baseline method: ",x$baseline,"\n")
  with(x$work,{
  cat("  Power: mean=",format(mean(P,na.rm=TRUE),digits=3)," sd=",format(sd(P,na.rm=TRUE),digits=3)," (",
      round(sd(P,na.rm=TRUE)/mean(P,na.rm=TRUE)*100,1),"%)\n",sep="")
  cat("         95%CI=(",paste(format(quantile(P,c(.025,.975),na.rm=T),digits=3),collapse=" ; "),")\n\n")
  cat("   Time: mean=",format(mean(duration,na.rm=TRUE),digits=3)," sd=",format(sd(duration,na.rm=TRUE),digits=3)," (",
      round(sd(duration,na.rm=TRUE)/mean(duration,na.rm=TRUE)*100,1),"%)\n",sep="")
  cat("         95%CI=(",paste(format(quantile(duration,c(.025,.975),na.rm=T),digits=3),collapse=" ; "),")\n\n")
  cat(" Energy: mean=",format(mean(E,na.rm=TRUE),digits=3)," sd=",format(sd(E,na.rm=TRUE),digits=3)," (",
      round(sd(E,na.rm=TRUE)/mean(E,na.rm=TRUE)*100,1),"%)\n",sep="")
  cat("         95%CI=(",paste(format(quantile(E,c(.025,.975),na.rm=T),digits=3),collapse=" ; "),")\n\n")
  }
  )
  pars=list(...)
  remove.outliers=FALSE
  if("remove.outliers" %in% names(pars)){
    remove.outliers = pars$remove.outliers
  }
  if(remove.outliers){
    cat("\nWithout outliers:\n")
    ol = outliers(x$work$P,logic=T) |
         outliers(x$work$duration,logic=T) |
         outliers(x$work$E,logic=T)
    with(subset(x$work,!ol),{
      cat("  Power: mean=",mean(P,na.rm=TRUE)," sd=",sd(P,na.rm=TRUE)," (",
          round(sd(P,na.rm=TRUE)/mean(P,na.rm=TRUE)*100,1),"%)\n",sep="")
      cat("         95%CI=(",paste(format(quantile(P,c(.025,.975),na.rm=T),digits=3),collapse=" ; "),")\n")
      cat(" Energy: mean=",mean(E,na.rm=TRUE)," sd=",sd(E,na.rm=TRUE)," (",
          round(sd(E,na.rm=TRUE)/mean(E,na.rm=TRUE)*100,1),"%)\n",sep="")
      cat("         95%CI=(",paste(format(quantile(E,c(.025,.975),na.rm=T),digits=3),collapse=" ; "),")\n")
    }
    )
  }
  #cat(" Thoughput: ", (x$n / as.double(x$elapsed,units="secs") * 1e-6), "M samples per sec\n" )
}

plot.EnergyAnalysis <- function(x,work.unit=NULL,highlight=NULL,main=NULL,...){
  P <- NULL ## to mute down NOTE from R CMD check
  start <- end <- NULL # to mute down CMD check

#   layout(matrix(c(1,1,1,1,
#                   2,4,4,5,
#                   2,4,4,5,
#                   6,3,3,6),4,byrow=TRUE))
  layout(matrix(c(1,1,
                  2,4,
                  5,3),3,byrow=TRUE))
  push.mar = par("mar")
  par(mar=c(3,2,0.2,0.2))

  if(is.null(work.unit)){
    l=1
    r=x$n
  }else{
    l=x$markers$start[min(work.unit)]-median(x$markers$length)
    r=x$markers$start[max(work.unit)+1]+median(x$markers$length)
  }

  if(!"P" %in% names(x)){
    if(is.null(work.unit)){
      dsd = x$P.sample
    }else{
      wu = c(min(work.unit),1+max(work.unit))
      indexes = which(x$P.sample$t %in% x$t*x$work[wu,]$start)
      dsd = x$P.sample[min(indexes):max(indexes),]
    }
  }else{
    n = r-l
    m = mean(x$P[l:r])
    dsd = .ds(x$P[l:r],x$t)
    dsd$t <- dsd$t + (l-1)*x$t
  }
  plot(dsd$t,dsd$P,t="l",col="purple",...)

  with(subset(x$work,!is.na(P)&start>l&end<r),{
      rect(start*x$t,min(dsd$P),end*x$t,max(dsd$P),
         col=rgb(1,.64,0,.2),border="darkorange",lty=3)
      text((start+end)/2*x$t,min(dsd$P),which(!is.na(x$work$P)),col="darkorange",xpd=T,cex=0.6)
    }
  )
  if(any(is.na(x$work$P))){
    with(subset(x$work,is.na(P)),{
      rect(start*x$t,min(dsd$P),end*x$t,max(dsd$P),
           col=rgb(.6,.6,.6,.2),border="lightgray",lty=3)
      text((start+end)/2*x$t,min(dsd$P),which(is.na(x$work$P)),col="gray",xpd=T,cex=0.6)
    }
  )
  }
  if("markers" %in% names(x)){
    rect(x$markers$start*x$t,min(dsd$P),x$markers$end*x$t,max(dsd$P),
         col=rgb(0,1,.6,.1),border=NA,lty=2)
    text((x$markers$start+x$markers$end)/2*x$t,max(dsd$P),col="navy",xpd=T,cex=0.6)
  }

  bp <- function(x,ylab,col.point=1,hollow=NULL,...){
    par=list(...)
    horizontal = FALSE
    if(!is.null(par$horizontal)){
      horizontal = par$horizontal
    }

    MINX = 0
    if(horizontal){
      boxplot(x,xlab=ylab,xlim=c(MINX,1.2),...);
    }else{
      boxplot(x,ylab=ylab,xlim=c(MINX,1.2),...);
    }
    xo = order(x)
    x <- x[xo]
    if(!is.null(hollow)){
      hollow = hollow[xo]
    }
    cs = 0.02
    while(TRUE){
      slots = floor((.75-MINX) / (2*cs))
      l = min(x,na.rm=T)
      h = max(x,na.rm=T)
      int = findInterval(x,l + (0:slots)*(h-l)/slots)
      if(max(table(int)) < slots) break
      cs = cs / 5 * 4
    }
    pos = as.numeric(unlist(sapply(table(int,useNA = "ifany"),function(x) sample(seq(x),x)))) - 1
    y = pos*cs*2+MINX

    if(horizontal){
      asp.ratio = par("pin")[2]/par("pin")[1]
      xscale = diff(par("usr"))[1]

      cs <- cs*asp.ratio*xscale
      tmp = x
      x = y
      y = tmp
    }
    if(is.null(hollow)) hollow <- rep(FALSE,length(x))

    if(!all(hollow))
    symbols(y[!hollow],x[!hollow],circles=rep(cs,length(x))[!hollow],
            inches=FALSE,fg=NA,bg=col.point,add=TRUE)
    if(any(hollow)){
      symbols(y[hollow],x[hollow],circles=rep(cs,length(x))[hollow],
              inches=FALSE,fg=col.point,bg=NA,add=TRUE)
    }

    #segments(0,x,0.6,x,lwd=2,col=rgb(160/255,32/255,240/255,128/255))
  }


  par(mar=c(3,4,0,2))
  bp(x$work$P,"Power [W]","purple")

  par(mar=c(4,3,0,3))
  bp(x$work$duration,"Duration [s]","purple",horizontal=TRUE,hollow=is.na(x$work$P))

  par(mar=c(3,3,0,3))
  plot(x$work$duration,x$work$P,xlab="Duration [s]")
  x.to = par("usr")[2]
  y.from = par("usr")[3]
  even = TRUE
  energy = pretty(x$work$E)
  if(0 %in% energy){
    zi = which(energy==0)
    energy[zi] = min(energy[-zi])/5
  }
  for(e in energy){
    if(even){
      line.col = rgb(1,.549,0,.6)
      text.col = rgb(1,.549,0,1)
    }else{
      line.col = rgb(.627,.125,.941,.6)
      text.col = rgb(.627,.125,.941,1)
    }

    curve(e / x,from=max(par("usr")[1],0),to=x.to,
          col=line.col,add=TRUE)
    y = e/x.to

    if(y>y.from){
      text(x.to,y,format(e,digits=2),xpd=TRUE,cex=0.7,col=text.col,
           adj = -0.1)
    }else{
      x0 = e/y.from
      text(x0,y.from,format(e,digits=2),xpd=TRUE,cex=0.7,col=text.col,
           adj = c(0,-0.1))
    }
    even <- !even
  }

  par(mar=c(4,4,0,2))
  bp(x$work$E,"Energy [J]","orange")

  if(!is.null(main)){
    x0 = par("usr")[1] - par("plt")[1]/(diff(par("plt"))[1])*.75 * diff(par("usr"))[1]
    mtext(main,side=1,at=x0,line=2,adj=c(0,0),xpd=TRUE,font=2)
  }
  layout(1)
  par(mar=push.mar)
}


