stepsTable <- read.csv("SteppingStoneTable.csv", stringsAsFactors=F)[-1,]  ## TODO
stepsTable[2,2] <- "Who are the patients you want to help?\n What are the clinical options?"
stepsTable <- data.frame(`Done?`= "Not yet done", stepsTable[-1])
names(stepsTable) = c("Done?", "Stepping stone", "Question")
rownames(stepsTable) = paste0('(', 1:nrow(stepsTable) , ')')

rValues = reactiveValues(doneLabels = rep("Not yet done", nrow(stepsTable)))

sectionHeader = function(number) {
  list(h2(paste0('(', number, ') ',
                 stepsTable[number, "Stepping stone"])),
       h3(stepsTable[number, "Question"]))
}
buttonLabelValues = c("Not yet done", "Done")
completedButton = function(number) {
  actionButton("completed" %&% number, label = rValues$doneLabels[number])
}
