rzplot.geom <- 
setRefClass("RzPlotGeom",
  fields = c("main", "button", "rzPlotScript",
             "combo", "combo.theme",
             "hist.label1", "hist.combo1",
             "combo.x", "combo.y",
             "combo.position", "entry.width", "entry.height"),
  methods = list(
    initialize  = function(...) {
      initFields(...)
      
      # x
      label.x <- gtkLabelNew(show=TRUE)
      label.x$setText(gettext("x"))
      combo.x <<- new("RzCompletionCombo")
      
      # y
      label.y <- gtkLabelNew(show=TRUE)
      label.y$setText(gettext("y"))
      combo.y <<- new("RzCompletionCombo")
      
      # geom
      label <- gtkLabelNew("geom")
      combo <<- gtkComboBoxNewText()
      geoms <- c("none", "bar", "freqpoly", "histogram", "density", "boxplot", "violin",
                 "line", "area", "point", "jitter", "dotplot", "rug", "smooth", "quantile", "blank")
      for(i in geoms) combo$appendText(i)
      combo$setActive(0)
      
      image  <- gtkImageNewFromStock(GTK_STOCK_ADD, GtkIconSize["menu"])
      button <<- gtkButtonNew()
      button["tooltip-text"] <<- gettext("Add Layer and Redraw")
      button$setFocusOnClick(FALSE)
      button$setImage(image)
      button$setRelief(GtkReliefStyle["none"])
      
      # histogram
      hist.label1 <<- gtkLabelNew("binwidth", show=FALSE)
      hist.combo1 <<- gtkComboBoxEntryNewText()
      hist.combo1$hide()
      breaks <- c("default", "based on Sturges", "based on Scott", "based on Freedman-Diaconis", "(numeric)")
      for(i in breaks) hist.combo1$appendText(i)
      hist.combo1$setActive(0)
      
      # position
      label.position <- gtkLabelNew(gettext("position"))
      combo.position <<- gtkComboBoxNewText()
      combo.position$show()
      positions <- c("default", "identity", "dodge", "fill", "stack", "jitter")
      for(i in positions) combo.position$appendText(i)
      combo.position$setActive(0)
      
      label.width  <-  gtkLabelNew("width (position)")
      entry.width  <<- gtkEntryNew()
      label.height <-  gtkLabelNew("height (position)")
      entry.height <<- gtkEntryNew()
      
      table <- gtkTableNew(homogeneous=FALSE)
      table["border-width"] <- 5
      table$attach        (label,              0, 1, 0, 1, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo,              1, 2, 0, 1)
      table$attach        (button,             2, 3, 0, 1, "shrink", "shrink", 0, 0)
      table$attach        (label.x,            0, 1, 1, 2, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.x$getCombo(), 1, 3, 1, 2)
      table$attach        (label.y,            0, 1, 2, 3, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.y$getCombo(), 1, 3, 2, 3)
      table$attach        (hist.label1,        0, 1, 3, 4, "shrink", "shrink", 0, 0)
      table$attachDefaults(hist.combo1,        1, 3, 3, 4)
      table$attach        (label.position,     0, 1, 4, 5, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.position,     1, 3, 4, 5)
      table$attach        (label.width   ,     0, 1, 5, 6, "shrink", "shrink", 0, 0)
      table$attachDefaults(entry.width   ,     1, 3, 5, 6)
      table$attach        (label.height  ,     0, 1, 6, 7, "shrink", "shrink", 0, 0)
      table$attachDefaults(entry.height  ,     1, 3, 6, 7)
      
      table$setColSpacings(5)
      table$setRowSpacings(2)
      
      main <<- buildPlotOptionPage(table)

      .self$onGeomComboChanged()
      gSignalConnect(combo             , "changed", .self$onGeomComboChanged)
      gSignalConnect(combo.x$getCombo(), "changed", .self$generateScript)
      gSignalConnect(combo.y$getCombo(), "changed", .self$generateScript)
      gSignalConnect(hist.combo1       , "changed", .self$generateScript)
      gSignalConnect(combo.position    , "changed", .self$generateScript)
      gSignalConnect(entry.width       , "changed", .self$generateScript)
      gSignalConnect(entry.height      , "changed", .self$generateScript)
      
    },
    
    
    onGeomComboChanged = function(combo=NULL){
      if(is.null(combo)){
        geom <- "none"
      } else {
        geom <- localize(combo$getActiveText())
      }
      hist.label1$hide()
      hist.combo1$hide()
      
      if (geom == "histogram" || geom == "freqpoly") {
        hist.label1$showAll()
        hist.combo1$showAll()
      }
      .self$generateScript()
    },
    
    clear = function(){
      combo$setActive(0)
      hist.combo1$setActive(0)
      combo.x$clear()
      combo.y$clear()
      combo.position$setActive(0)
      entry.width$setText("")
      entry.height$setText("")
    },
    
    setX = function(txt){
      combo.x$setText(txt)
      .self$generateScript()
    },
    
    generateScript = function(...){
      geom <- combo$getActiveText()
      hist.break <- localize(hist.combo1$getActiveText())
      x       <- localize(combo.x$getActiveText())
      y       <- localize(combo.y$getActiveText())
      
      position  <- localize(combo.position$getActiveText())
      width     <- localize(entry.width$getText())
      height    <- localize(entry.height$getText())
      suppressWarnings(width  <- as.numeric(width))
      suppressWarnings(height <- as.numeric(height))
      if(is.na(width))  width  <- NULL
      else width <- sprintf("width=%s", width)
      if(is.na(height)) height <- NULL
      else height <- sprintf("height=%s", height)
      if (position=="default") position <- NULL
      else position <- sprintf("position_%s(%s)",
                               position, paste(c(width, height), collapse=","))

      if (geom=="none") {
        rzPlotScript$setScript("geom")
      } else if(geom=="histogram" || geom=="freqpoly") {
        if (hist.break=="default") {
          rzPlotScript$setScript("geom", type=geom,
                                 args=list(position=position))          
        } else {
          rzPlotScript$setScript("geom", type=geom,
                                 args=list(hist.break=deparse(hist.break), position=position))
        }
      } else {
        rzPlotScript$setScript("geom", type=geom,
                               args=list(position=position))
      }
      
      fac <- c("bar")
      num <- c("freqpoly", "histogram", "density")
      fac.num <- c("boxplot", "violin")
      num.num <- c("line", "area", "point", "jitter", "dotplot",
                   "rug", "smooth", "geom_quantile", "blank")
      
      if(geom %in% c(fac, num)) {
        rzPlotScript$setAes("x", x)        
      } else {
        rzPlotScript$setAes(c("x", "y"), c(x, y))        
      }
      
    },
    
    completionSetModel = function(model){
      combo.x$setModel(model)
      combo.y$setModel(model)
    }
  )
)
rzplot.geom$accessors("main", "button")
