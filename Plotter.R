library(dygraphs)
plotMovementData <- function(dev, name){
  
 # p  <- plot_ly( x = dev$counter, y = dev$x,type = "scatter" ,mode="lines", name="X")%>% 
#    add_trace(x = dev$counter, y =dev$y, name = "Y") %>%
#    add_trace(x = dev$counter, y =dev$z, name = "Z") %>%
#    layout(dragmode = 'select', xaxis = list(range = c(0, 5000)))
  d <- data.frame(dev$counter, dev$x, dev$y, dev$z)  
  p<-dygraph( d, main = name) %>% 
    dyRangeSelector() %>%
    dySeries("dev.x",   label = "x") %>%
    dySeries("dev.y",   label = "y") %>%
    dySeries("dev.z", label="z") %>%
    dyHighlight(highlightCircleSize = 5, 
                highlightSeriesBackgroundAlpha = 0.2,
                hideOnMouseOut = FALSE)
       print(p)
  
}

plotHealthData <- function(dev, name){

 # p  <- plot_ly( x = dev$counter, y = dev$x,type = "scatter", mode="lines", name="X")%>% 
  #p <- dygraph( x = dev$counter, y = dev$x) %>%
  d <- data.frame(dev$counter, dev$x)
    p<-dygraph( d, main = name) %>%
    dyAxis("x", drawGrid = TRUE) %>%
    dyAxis("y", label = "Time") %>%
    dyOptions(includeZero = TRUE, 
              axisLineColor = "navy", 
              gridLineColor = "lightblue")
# 
# p<- dygraph(  d, main = "Deaths from Lung Disease (UK)") %>%
#    dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2"))
  print(p)
}


plotIntegral <- function(integral, ispan){
  
 # p <- plot_ly(integral, x = 1:length(integral$sig)) %>%
#    add_lines(y = ~integral$sig, name = "Signal") %>%
#    add_lines(y = ~integral$y1, name = "First", visible = T) %>%
#    add_lines(y = ~integral$y2, name = "Second", visible = T          ) 
 
  d <- data.frame(1:length(integral$sig), integral$sig, integral$y1 , integral$y2)  
  p<-dygraph( d, main = "Integral") %>% 
    dyRangeSelector() %>%
    dySeries("integral.sig",   label = "signal") %>%
    dySeries("integral.y1",   label = "speed") %>%
    dySeries("integral.y2", label="h") %>%
    dyHighlight(highlightCircleSize = 5, 
                highlightSeriesBackgroundAlpha = 0.2,
                hideOnMouseOut = FALSE)
    print(p)
  

  
}


plotDensity <- function(sig, axis){
  switch(axis,
         "X" = {
            p <- ggplot(data = sig) +
            geom_histogram(aes(x=sig$x, y =..density..)) +
            geom_density(aes(x=sig$x), col=2)
            gg <- dyplot(p)#ggplotly(p)
            #print(gg)
         },
         "Y" = {
            p <- ggplot(data = sig) +
            geom_histogram(aes(x=sig$y, y =..density..)) +
            geom_density(aes(x=sig$y), col=2)
            gg <- dyplot(p)#ggplotly(p)
#  print(gg) 
         },
        "Z" = {
            p <- ggplot(data = sig) +
            geom_histogram(aes(x=sig$z, y =..density..)) +
            geom_density(aes(x=sig$z), col=2)
            gg <- dyplot(p)#gglotly(p)
 # print(gg)
        })
        print(gg)  
}