#####  shinyROC ui

shinyUI(fluidPage(
  tags$head(tags$script(HTML('
      Shiny.addCustomMessageHandler("jsCode",
        function(message) {
          console.log(message)
          eval(message.code);
        }
      );
    '))),
  uiOutput("debugTools"),
  fluidRow(
    column(7,
           h1("ROC-type curves")),
    column(5, a(
      href="Using_the_NNTbiomarker_package.htm", rel="help", target="_blank",
      #href="../doc/Using_the_NNTbiomarker_package.html", rel="help", target="_blank",
      #href="http://www.github.org/professorbeautiful/NNTbiomarkerHome/man/Using_the_NNTbiomarker_package.html",
      fluidRow(
        column(3,
               style="background:yellow",
               strong(em("Click for information:",
                         style="color:darkgreen")))
        ,
        column(1, style="background:yellow",
               actionButton(inputId = "Info", label="",
                            style="background:lightgreen",
                            icon=icon("info-sign", lib="glyphicon")))
      )
    )
    )
  ),
  hr(),
  fileInput(inputId = "data", label = "Choose data file"),
  numericInput(inputId = "N", "N", value=1000, min = 4, step = 1),
