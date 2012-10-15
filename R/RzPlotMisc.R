rzplot.misc <- 
setRefClass("RzPlotMisc",
  fields = c("main", "combo.theme", "rzPlotScript",
             "flip.togglebutton",
             "na.rm.togglebutton",
             "combo.scalex", "combo.scaley",
             "combo.coordx", "combo.coordy",
             "entry.xlim", "entry.ylim", "entry.fontsize",
             "title.entry", "xlab.combo", "ylab.combo"),
  methods = list(
    initialize  = function(...) {
      initFields(...)
            
      # misc options
      label.scale <- gtkLabelNew("scale")
      label.coord <- gtkLabelNew("coord")
      
      label.scalex <- gtkLabelNew("x")
      combo.scalex <<- gtkComboBoxNewText()
      scales <- c("default","log10", "reverse", "sqrt")
      for(i in scales) combo.scalex$appendText(i)
      combo.scalex$setActive(0)
      
      label.scaley <- gtkLabelNew("y")
      combo.scaley <<- gtkComboBoxNewText()
      for(i in scales) combo.scaley$appendText(i)
      combo.scaley$setActive(0)
      
      
      label.coordx <- gtkLabelNew("x")
      trans <- c("identity", "asn", "atanh","exp", "log",         
                 "log10", "log2", "logit", "probit", "reverse", "sqrt")
      combo.coordx <<- gtkComboBoxNewText()
      for(i in trans) combo.coordx$appendText(i)
      combo.coordx$setActive(0)
      
      label.coordy <- gtkLabelNew("y")
      combo.coordy <<- gtkComboBoxNewText()
      for(i in trans) combo.coordy$appendText(i)
      combo.coordy$setActive(0)
      
      label.limits <-  gtkLabelNew(gettext("axis limits"))
      label.xlim   <-  gtkLabelNew("x")
      label.ylim   <-  gtkLabelNew("y")
      entry.xlim   <<- gtkEntryNew()
      entry.ylim   <<- gtkEntryNew()
      entry.xlim["width-request"] <<- 1
      entry.ylim["width-request"] <<- 1
      
      #flip
      flip.togglebutton <<- gtkToggleButtonNewWithLabel(gettext("flip"))
      
      #remove NA
      na.rm.togglebutton <<- gtkToggleButtonNewWithLabel(gettext("remove NA"))
      na.rm.togglebutton$setActive(TRUE)
      na.rm.togglebutton$hide()
      
      #theme
      label.theme <-  gtkLabelNew("theme")
      combo.theme <<- gtkComboBoxNewText()
      themes <- c("grey", "bw")
      for(i in themes) combo.theme$appendText(i)
      combo.theme$setActive(0)
      
      label.fontsize   <-  gtkLabelNew(gettext("base font size"))
      entry.fontsize   <<- gtkEntryNew()
      entry.fontsize$setText("12")
                  
      table <- gtkTableNew(6, 5, FALSE)
      table["border-width"] <- 5
      table$attach        (label.scale       , 0, 1, 0, 1, "shrink", "shrink", 0, 0)
      table$attach        (label.scalex      , 1, 2, 0, 1, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.scalex      , 2, 3, 0, 1)
      table$attach        (label.scaley      , 3, 4, 0, 1, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.scaley      , 4, 5, 0, 1)
      table$attach        (label.coord       , 0, 1, 1, 2, "shrink", "shrink", 0, 0)
      table$attach        (label.coordx      , 1, 2, 1, 2, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.coordx      , 2, 3, 1, 2)
      table$attach        (label.coordy      , 3, 4, 1, 2, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.coordy      , 4, 5, 1, 2)
      table$attach        (label.limits      , 0, 1, 2, 3, "shrink", "shrink", 0, 0)
      table$attach        (label.xlim        , 1, 2, 2, 3, "shrink", "shrink", 0, 0)
      table$attachDefaults(entry.xlim        , 2, 3, 2, 3)
      table$attach        (label.ylim        , 3, 4, 2, 3, "shrink", "shrink", 0, 0)
      table$attachDefaults(entry.ylim        , 4, 5, 2, 3)
      table$attachDefaults(flip.togglebutton , 0, 5, 3, 4)
      table$attachDefaults(na.rm.togglebutton, 0, 5, 4, 5)
      table$attach        (label.theme       , 0, 2, 5, 6, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.theme       , 2, 5, 5, 6)
      table$attach        (label.fontsize    , 0, 2, 6, 7, "shrink", "shrink", 0, 0)
      table$attachDefaults(entry.fontsize    , 2, 5, 6, 7)
      
      table$setColSpacings(5)
      table$setRowSpacings(2)
            
      main <<- buildPlotOptionPage(table)
      
      .self$generateScript()
      
      gSignalConnect(combo.scalex, "changed", .self$generateScript)
      gSignalConnect(combo.scaley, "changed", .self$generateScript)
      gSignalConnect(combo.coordx, "changed", .self$generateScript)
      gSignalConnect(combo.coordy, "changed", .self$generateScript)
      gSignalConnect(entry.xlim  , "changed", .self$generateScript)
      gSignalConnect(entry.ylim  , "changed", .self$generateScript)
      gSignalConnect(flip.togglebutton , "toggled", .self$generateScript)
      gSignalConnect(na.rm.togglebutton, "toggled", .self$generateScript)
      gSignalConnect(combo.theme , "changed", .self$generateScript)
      gSignalConnect(entry.fontsize, "changed", .self$generateScript)
    },
    
    clear = function(){
      combo.scalex$setActive(0)
      combo.scaley$setActive(0)
      combo.coordx$setActive(0)
      combo.coordy$setActive(0)
      entry.xlim$setText("")
      entry.ylim$setText("")
      flip.togglebutton$setActive(FALSE)
      na.rm.togglebutton$setActive(TRUE)
      combo.theme$setActive(0)
      entry.fontsize$setText("12")
    },
    
    generateScript = function(...){
      scalex <- localize(combo.scalex$getActiveText())
      scaley <- localize(combo.scaley$getActiveText())
      coordx <- localize(combo.coordx$getActiveText())
      coordy <- localize(combo.coordy$getActiveText())
      xlim   <- localize(entry.xlim$getText())
      ylim   <- localize(entry.ylim$getText())
      xlim   <- strsplit(xlim, ",")[[1]]
      ylim   <- strsplit(ylim, ",")[[1]]
      xlim   <- suppressWarnings(as.numeric(xlim))
      ylim   <- suppressWarnings(as.numeric(ylim))
      
      if (coordx == "identity") coordx <- NULL
      if (coordy == "identity") coordy <- NULL
            
      if (any(is.na(xlim)) | length(xlim)==0) xlim <- NULL
      if (any(is.na(ylim)) | length(ylim)==0) ylim <- NULL
      
      flip   <- flip.togglebutton$getActive()
      na.rm  <- na.rm.togglebutton$getActive()

      theme    <- combo.theme$getActiveText()
      fontsize <- localize(entry.fontsize$getText())
      fontsize <- suppressWarnings(as.numeric(fontsize))
      if (any(is.na(fontsize)) | length(fontsize)==0) fontsize <- 12
      
      if (scalex != "default") rzPlotScript$setScript(layer="scale_x", type=scalex)
      else                     rzPlotScript$clearScript("scale_x")

      if (scaley != "default") rzPlotScript$setScript("scale_y", type=scaley)
      else                     rzPlotScript$clearScript("scale_y")
      
      if (!is.null(coordx) || !is.null(coordy)) {
        rzPlotScript$setScript("coord", type="trans",
                               args=list(xtrans=deparse(coordx), ytrans=deparse(coordy), limx=deparse(xlim), limy=deparse(ylim)))
        if (flip) rzPlotScript$setScript("coord", type="flip", add=TRUE)
        
      } else if (!is.null(xlim) || !is.null(ylim)) {
        rzPlotScript$setScript("coord", type="cartesian", args=list(xlim=deparse(xlim), ylim=deparse(ylim)))
        if (flip) rzPlotScript$setScript("coord", type="flip", add=TRUE)
        
      } else if (flip) {
        rzPlotScript$setScript("coord", type="flip")
        
      } else {
        rzPlotScript$clearScript("coord")
      }
      
      rzPlotScript$setBaseTheme(c(theme, fontsize))
    }
  )
)
rzplot.misc$accessors("main")
