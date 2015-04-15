output$contraBayesPlot <- renderPlot({
  achievable.se.sp(the.prev = input$prevalence)
})

output$hoverinfo <- renderTable({
  if (is.null(input$contraBayesPlot_hover))
    NULL
  else {
    # TODO: currently assumes  axes = "pv"
    ppv = input$contraBayesPlot_hover$x
    npv = input$contraBayesPlot_hover$y
    the.prev = input$prevalence
    nnt = NNT.from.pv(ppv = ppv, npv=npv)
    sesp = sesp.from.pv(ppv = ppv, npv=npv, prev=the.prev)
    result = xtable(digits=3, t(data.frame(
      sensitivity=sesp["se"], specificity=sesp["sp"],
      PPV=ppv,
      NPV=npv,
      NNTpos=nnt[1],
      NNTneg=nnt[2]
    )))
    names(result) = "value"
    result
  }
})
