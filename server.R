source("utils.R")

shinyServer(function(input, output) {
  
  output$kpi_summary_box_1 <- renderValueBox({
    valueBox(
      value = sprintf("%s", compress(245923)),
      subtitle = sprintf("KPI 1 (%.1f%%)", 8.9202),
      icon = icon("arrow-up"),
      color = "green"
    )
  })
  
  output$kpi_summary_box_2 <- renderValueBox({
    valueBox(
      value = sprintf("%s", compress(190)),
      subtitle = sprintf("KPI 2 (%.1f%%)", -0.23),
      icon = icon("arrow-down"),
      color = "red"
    )
  })
  
  output$kpi_summary_box_3 <- renderValueBox({
    valueBox(
      value = sprintf("%s", compress(104924422)),
      subtitle = sprintf("KPI 3 (%.1f%%)", -5.422),
      icon = icon("arrow-down"),
      color = "green"
    )
  })
  
})