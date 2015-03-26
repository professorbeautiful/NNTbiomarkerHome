shinyServer(function(input, output) {
  output$myplot <- renderPlot({
    achievable.se.sp(the.prev = input$prevalence)
    })
  output$hoverinfo <- renderTable({
    if (is.null(input$myplot_hover))
      NULL
    else
      data.frame(x=input$myplot_hover$x, y=input$myplot_hover$y)
  })
})
