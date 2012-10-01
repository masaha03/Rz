tools <-
setRefClass("RzTools",
  fields = c("variableView", "infoBar", "window"),
  methods = list(
    initialize = function(...) {
      initFields(...)
      variableView <<- NULL
      infoBar      <<- NULL
      window       <<- NULL
    },
    
    clean = function(){
      variableView <<- NULL
      infoBar      <<- NULL
      window       <<- NULL
    }
  )
)
tools$accessors("variableView", "infoBar", "window")

