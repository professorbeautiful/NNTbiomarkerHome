options(error=function(){
  cat("Recovering!\n"); recover()
  })

#stepsTableInitial = read.csv("SteppingStoneTable.csv", stringsAsFactors=F)[-1,]  ## TODO
data(stepsTableInitial)

stepsTableInitial <- data.frame(`Done?`= "Not yet done", stepsTableInitial)
names(stepsTableInitial) = c("Done?", "Stepping stone", "Question")
rownames(stepsTableInitial) = paste0('(', 1:nrow(stepsTableInitial) , ')')

buttonLabelValues = c("Not yet done", "Done")

