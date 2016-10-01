output$range <- renderUI({
    sliderInput("range", label = h6("Fragment range"), min = 1, max = sizeDb(), step=1,   value = c(25, 175))
})

output$axis <- rendreUI({
    selectInput('axis', label = 'Select a axis', choices = lookupAxis, selected = "X", width=100)
})