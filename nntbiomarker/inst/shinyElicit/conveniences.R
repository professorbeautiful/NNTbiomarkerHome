rValues = reactiveValues()
thisSession <<- session

wasClicked =  function(button) {
  if(exists("input"))
    if(!is.null(button) ) {
      if(button > 0) {
        return(TRUE)
      }
    }
  return(FALSE)
}
assign("%&%",  function (a, b) paste(a, b, sep = ""))
catn = function(...) cat(..., "\n")
