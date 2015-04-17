# options(error=function(){
#   cat("Recovering!\n"); recover()
#   })

#stepsTableInitial = read.csv("SteppingStoneTable.csv", stringsAsFactors=F)[-1,]  ## TODO
#data(stepsTableInitial)
source("conveniences.R", local=F)
source("stepsTableInitial.dmp", local=F)
rownames(stepsTableInitial) = NULL
stepsTableInitial[[1]] = as.character(stepsTableInitial[[1]])

disableActionButton <- function(id,session) {
  catn("Disabling button " %&% id)
  jsCode = list(code=
                  paste("$('#", id,
                        "').prop('disabled',true);"
                   , sep=""))
  catn("jsCode ", jsCode[[1]])
  #$("#reportButton").prop('disabled',true)
  session$sendCustomMessage(type="jsCode", jsCode)
}
enableActionButton <- function(id,session) {
  catn("Enabling button " %&% id)
  session$sendCustomMessage(type="jsCode",
                            list(code= paste("$('#",id,"').prop('disabled',false)"
                                             ,sep="")))
}



completedToggle = function(number) {
  span(
    radioButtons(  inline=TRUE, ### FAILS!
                 "stepStatus" %&% number,
                 label="Is this step done?", choices=c("Not yet", "Done"))
    #       textOutput(outputId = "completedText" %&% number,
    #                  "Not yet done."),
    #       actionButton("completed" %&% number,
    #                    label = "")
  )
}

sectionHeader = function(number, content) {
  list(hr(),
       h2(paste0('(', number, ') ',
                 stepsTableInitial[number, "Stepping stone"])),
       h3("Question " %&% number, ": ",
          stepsTableInitial[number, "Question"]),
       content,
       completedToggle(number),
       hr()
  )
}

