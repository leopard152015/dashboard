deleteBias_1 <- function(sigData){
  
  p <- sigData$z/sigData$x
  arc_angle <- atan(p)
  angle = sigData$x/cos(arc_angle)
  
  p1 <- sigData$y/sigData$x
  arc_angle_z <- atan(p1)
  angle2 = angle/cos(arc_angle_z)
  
  sigData$x <- angle2

  return(sigData)
  
}

deleteBias_2 <- function(sigData){
  g <- 9.81
  pfi <- 1/cos(asin(sigData$z/g))
  teta <- 1/cos(asin(sigData$y/g) ) 
 # p <- sigData$z/sigData$x
#  arc_angle <- atan(p)
#  angle = sigData$x/cos(arc_angle)
  acc_y <-  sigData$x * pfi * teta # 1/cos(pfi) * 1/cos(teta)  
  sigData$x <- acc_y
  
  return(sigData)
  
}

integral <- function(fragment, a, b, delta, const){
 
  sig<-fragment
  
  len <- length(fragment)
  N <- c(1:len)
  x<-c(1:len)
  f <- function(x) sig[x]*delta
  f1<-0
  area <- function(x){
    n <- len-(len-(x+1))
    
    f1<- quadgk(f,x,n)
    
  }
  v.area <- Vectorize(area)#,vectorize.args='x')(N)
  v1<- sapply(N, v.area)
  v1<-cumsum(v1)
  if (const == TRUE){
    med<-mean(v1)
    v1 <- v1 - med
  }  
  # v1<-v.area
  sig<-v1
  area1 <- function(x){
    n <- len-(len-(x+1))
    
    f1<- quadgk(f,x,n)
  }
  v.area1 <- Vectorize(area1)#,vectorize.args='x')(N)
  v2<- sapply(N, v.area1)
  v2<-cumsum(v2)
  
  x<-c(1:len)#seq(0, 2*pi, length.out =l) 
  

  x1 <- c(1:len)

  df <- data.frame(sig=fragment, y1=v1, y2=v2)

  return(df)
}