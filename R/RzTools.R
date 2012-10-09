tools <-
setRefClass("RzTools",
  fields = c("variableView", "infoBar", "window", "datasetName"),
  methods = list(
    initialize = function(...) {
      initFields(...)
      variableView <<- NULL
      infoBar      <<- NULL
      window       <<- NULL
      datasetName  <<- NULL
    },
    
    clean = function(){
      variableView <<- NULL
      infoBar      <<- NULL
      window       <<- NULL
      datasetName  <<- NULL
    }
  )
)
tools$accessors("variableView", "infoBar", "window", "datasetName")

