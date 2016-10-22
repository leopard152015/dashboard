findpeaks <- function(vec,bw=1,x.coo=c(1:length(vec)))
{
  pos.x.max <- NULL
  pos.y.max <- NULL
  pos.x.min <- NULL
  pos.y.min <- NULL 	
  for(i in 1:(length(vec)-1)) 	{ 		if((i+1+bw)>length(vec)){
    sup.stop <- length(vec)}else{sup.stop <- i+1+bw
    }
    if((i-bw)<1){inf.stop <- 1}else{inf.stop <- i-bw}
    subset.sup <- vec[(i+1):sup.stop]
    subset.inf <- vec[inf.stop:(i-1)]
    
    is.max   <- sum(subset.inf > vec[i]) == 0
    is.nomin <- sum(subset.sup > vec[i]) == 0
    
    no.max   <- sum(subset.inf > vec[i]) == length(subset.inf)
    no.nomin <- sum(subset.sup > vec[i]) == length(subset.sup)
    
    if(is.max & is.nomin){
      pos.x.max <- c(pos.x.max,x.coo[i])
      pos.y.max <- c(pos.y.max,vec[i])
    }
    if(no.max & no.nomin){
      pos.x.min <- c(pos.x.min,x.coo[i])
      pos.y.min <- c(pos.y.min,vec[i])
    }
  }
  return(list(pos.x.max,pos.y.max,pos.x.min,pos.y.min))
}
findUpPeaks <- function (x, m = 3){
  #find peaks of signal (as vector)

  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(data.frame((i + 1), x[i+1])) else return(numeric(0))
  })
  # transforme result  with samples and corresponded peaks
  data <-  as.data.frame(matrix(unlist(pks),  ncol=2, byrow=TRUE))
  
  
  # create data.frame for result + 1 colomn ( period - diff between samples )
  sample <- data$V1
  peak <- data$V2
  period <- sample[-1] - sample[-length(sample)]
  zerocross <- sum(diff(sign(x)) != 0) #function (x) { return (sum(diff(sign(x)) != 0)) }
#  peak2peak <- function (x) { return (max(x) - min(x)) }
#  rms <- function (x) { return (sqrt(mean(x^2))) }
  # add one element (for example 1) for equal number of rows with sample and peaks
  period <- c(period, period[1] )
  
  message("Zerocross")
 message(length(zerocross))
  pks <- data.frame( peak, sample, period)
  pks
  
}


findUpPeaks2 <- function(x) {
  # Use -Inf instead if x is numeric (non-integer)
  y <- diff(c(-.Machine$integer.max, x)) > 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }

  sample <- y
  peak <- x[y]
  period <- sample[-1] - sample[-length(sample)]
  # add one element (for example 1) for equal number of rows with sample and peaks
  period <- c(period, period[1] )
  pks <- data.frame( peak, sample, period)
  pks
  #y
}
