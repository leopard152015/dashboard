correctionX <- function( sig ){
  p <- sig$z/sig$x
  arc_angle <- atan( p )
  angle = sig$x/cos( arc_angle )
  
  p1 <- sig$y/sig$x
  arc_angle_z <- atan( p1 )
  angle2 = angle/cos( arc_angle_z )
  
  sig$x <- angle2

  return( sig )
}

correctionY <- function( sig ){
  return( sig )
}

correctionZ <- function( sig ){
  p <- sig$x/sig$z
  arc_angle <- atan( p )
  angle = sig$z/cos( arc_angle )
  
  sig$z <- angle
  
  return( sig )
}

integral1 <- function(fragment,a, b, delta){
  
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
  
  v2 <- 0
  df <- data.frame(sig=fragment, y1=v1, y2=v2)
  
  return(df)
}

integral2 <- function(fragment, a, b, delta, const){
 
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