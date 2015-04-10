options(error=function(){
  cat("Recovering!\n"); recover()
  })

#stepsTableInitial = read.csv("SteppingStoneTable.csv", stringsAsFactors=F)[-1,]  ## TODO
#data(stepsTableInitial)
source("stepsTableInitial.dmp", local=T)
rownames(stepsTableInitial) = NULL
buttonLabelValues = c("Not yet done", "Done")

completedToggle = function(number) {
  span(
    radioButtons("stepStatus" %&% number,
                 label="Is this step done?", choices=c("Not yet", "Done"))
    #       textOutput(outputId = "completedText" %&% number,
    #                  "Not yet done."),
    #       actionButton("completed" %&% number,
    #                    label = "")
  )
}

sectionHeader = function(number) {
  list(hr(),
       h2(paste0('(', number, ') ',
                 stepsTableInitial[number, "Stepping stone"])),
       h3("Question " %&% number, ": ",
          stepsTableInitial[number, "Question"]),
       completedToggle(number)
  )
}

