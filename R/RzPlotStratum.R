rzplot.stratum <- 
setRefClass("RzPlotStratum",
  fields = c("combo.group", "combo.group.label",
             "combo.fill", "combo.fill.label",
             "combo.color", "combo.color.label",
             "combo.shape", "combo.shape.label",
             "combo.size","combo.size.label",
             "combo.line","combo.line.label",
             "combo.position", "position.entry",
             "combo.linetype", "combo.scale",
             "combo", "combo2", "combo.x",
             "combo.y", "entry3", "entry4", "entry5",
             "label.y", "label3", "label4", "label5",
             "rzPlotScript",
             "main"),
  methods = list(
    initialize  = function(...) {
      initFields(...)
      brewer    <- rownames(RColorBrewer::brewer.pal.info)
      treestore <- gtkTreeStoreNew("character", "character", "GdkPixbuf")

      pixbuf.path <- file.path(rzSettings$getRzPath(), "images", "palette",
                               "scale_hue.png")
      pixbuf <- gdkPixbufNewFromFile(pixbuf.path)$retval
      iter   <- treestore$append()$iter
      treestore$set(iter, 0, "hue", 2, pixbuf)
      
      pixbuf.path <- file.path(rzSettings$getRzPath(), "images", "palette",
                               "scale_grey.png")
      pixbuf <- gdkPixbufNewFromFile(pixbuf.path)$retval
      iter   <- treestore$append()$iter      
      treestore$set(iter, 0, "grey", 2, pixbuf)

      pixbuf.path <- file.path(rzSettings$getRzPath(), "images", "palette",
                               "scale_brewer.png")
      pixbuf <- gdkPixbufNewFromFile(pixbuf.path)$retval
      iter   <- treestore$append()$iter
      treestore$set(iter, 0, "brewer", 2, pixbuf)
      
      for (i in seq_along(brewer)) {
        iter.ch <- treestore$append(parent=iter)$iter
        pixbuf.path <- file.path(rzSettings$getRzPath(), "images", "palette",
                                 sprintf("%s.png", brewer[i]))
        pixbuf      <- gdkPixbufNewFromFile(pixbuf.path)$retval
        treestore$set(iter.ch, 1, brewer[i], 2, pixbuf)
      }
      
      renderer1 <- gtkCellRendererText()
      renderer2 <- gtkCellRendererText()
      renderer3 <- gtkCellRendererPixbuf()
      renderer3["width"]  <- 120
      renderer1["xalign"] <- 0.5
      color       <- renderer1["cell-background-gdk"]
      color$red   <- 65535L
      color$green <- 65535L
      color$blue  <- 65535L
      renderer1["cell-background-gdk"] <- color
      renderer2["cell-background-gdk"] <- color
      renderer3["cell-background-gdk"] <- color
      
      label.group <-  gtkLabelNew("group")
      combo.group <<- new("RzCompletionCombo")
      label.fill  <-  gtkLabelNew("fill")
      combo.fill  <<- new("RzCompletionCombo")
      label.color <-  gtkLabelNew("color")
      combo.color <<- new("RzCompletionCombo")
      label.shape <-  gtkLabelNew("shape")
      combo.shape <<- new("RzCompletionCombo")
      label.size  <-  gtkLabelNew("size")
      combo.size  <<- new("RzCompletionCombo")
      label.line  <-  gtkLabelNew("linetype")
      combo.line  <<- new("RzCompletionCombo")
      
      # group
      combo.group.label <<- gtkComboBoxEntryNewText()
      combo.group.label["width-request"] <<- 1
      combo.group.label$show()
      labels <- c(gettext("variable label"), gettext("variable name"), gettext("(free text)"))
      for(i in labels) combo.group.label$appendText(i)
      combo.group.label$setActive(0)

      # fill
      combo.fill.label <<- gtkComboBoxEntryNewText()
      combo.fill.label["width-request"] <<- 1
      combo.fill.label$show()
      for(i in labels) combo.fill.label$appendText(i)
      combo.fill.label$setActive(0)
      
      # color
      combo.color.label <<- gtkComboBoxEntryNewText()
      combo.color.label["width-request"] <<- 1
      combo.color.label$show()
      for(i in labels) combo.color.label$appendText(i)
      combo.color.label$setActive(0)

      # shape
      combo.shape.label <<- gtkComboBoxEntryNewText()
      combo.shape.label["width-request"] <<- 1
      combo.shape.label$show()
      for(i in labels) combo.shape.label$appendText(i)
      combo.shape.label$setActive(0)

      # size
      combo.size.label <<- gtkComboBoxEntryNewText()
      combo.size.label["width-request"] <<- 1
      combo.size.label$show()
      for(i in labels) combo.size.label$appendText(i)
      combo.size.label$setActive(0)

      # linetype
      combo.line.label <<- gtkComboBoxEntryNewText()
      combo.line.label["width-request"] <<- 1
      combo.line.label$show()
      for(i in labels) combo.line.label$appendText(i)
      combo.line.label$setActive(0)

      # scale
      label.scale <- gtkLabelNew("scale")
      combo.scale <<- gtkComboBoxNew(show=TRUE)
      combo.scale$setModel(treestore)
      combo.scale$clear()
      combo.scale$packStart(renderer1, expand=TRUE)
      combo.scale$packStart(renderer2, expand=TRUE)
      combo.scale$packStart(renderer3, expand=FALSE)
      combo.scale$addAttribute(renderer1, "text"  , 0)
      combo.scale$addAttribute(renderer2, "text"  , 1)
      combo.scale$addAttribute(renderer3, "pixbuf", 2)
      combo.scale$setActive(0)
      
      # position
      label.position <- gtkLabelNew(gettext("legend position"))
      combo.position <<- gtkComboBoxNewText()
      combo.position$show()
      labels <- c("top", "right", "bottom", "left", "specify", "none")
      for(i in labels) combo.position$appendText(i)
      combo.position$setActive(1)
      position.entry <<- gtkEntryNew()
      position.entry$setText("0,1")
      locator.button <- gtkButtonNew()
      mousepix       <- gdkPixbufNewFromFile(file.path(rzSettings$getRzPath(), "images/mouse.png"))$retval
      image          <- gtkImageNewFromPixbuf(mousepix)
      locator.button$setImage(image)
      position.hbox <- gtkHBoxNew()
      position.hbox$packStart(position.entry)
      position.hbox$packStart(locator.button, expand=FALSE)
      position.hbox["sensitive"] <- FALSE
      label.linetype <- gtkLabelNew(gettext("line type"))
      combo.linetype <<- gtkComboBoxNewText()
      linetypes <- c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
      for(i in linetypes) combo.linetype$appendText(i)
      combo.linetype$setActive(0)
      
      
      # facet
      label <- gtkLabelNew("facet")
      combo <<- gtkComboBoxNewText()
      combo$appendText("grid")
      combo$appendText("wrap")
      combo$setActive(0)
      label.x <-  gtkLabelNew("x")
      combo.x <<- new("RzCompletionCombo")
      label.y <<- gtkLabelNew("y")
      combo.y <<- new("RzCompletionCombo")
      label3 <<- gtkLabelNew("nrow", show=FALSE)
      entry3 <<- gtkEntryNew(show=FALSE)
      label4 <<- gtkLabelNew("ncol", show=FALSE)
      entry4 <<- gtkEntryNew(show=FALSE)
      label5 <<- gtkLabelNew("scale", show=FALSE)
      combo2 <<- gtkComboBoxNewText(show=FALSE)
      combo2$appendText("fixed")
      combo2$appendText("free")
      combo2$appendText("free_x")
      combo2$appendText("free_y")
      combo2$setActive(0)
      gSignalConnect(combo, "changed", function(combo){
        facet <- localize(combo$getActiveText())
        if(facet == "grid"){
          label.y$show()
          combo.y$getCombo()$show()
          label3$hide()
          entry3$hide()
          label4$hide()
          entry4$hide()
          label5$hide()
          combo2$hide()
        } else {
          label.y$hide()
          combo.y$getCombo()$hide()
          label3$show()
          entry3$show()
          label4$show()
          entry4$show()
          label5$show()
          combo2$show()
        }
        .self$generateScript()
      })
      gSignalConnect(entry3, "changed", function(entry){
        if(entry$getText() != ""){
          entry4$setSensitive(FALSE)
        } else {
          entry4$setSensitive(TRUE)
        }
        .self$generateScript()
      })
      gSignalConnect(entry4, "changed", function(entry){
        if(entry$getText() != ""){
          entry3$setSensitive(FALSE)
        } else {
          entry3$setSensitive(TRUE)
        }
        .self$generateScript()
      })
      
      
      table  <- gtkTableNew(10, 3, FALSE)
      table["border-width"] <- 5
      table$attach        (label.group           , 0, 1, 0,  1, "shrink", "shrink", 0, 0)
      table$attach        (combo.group$getCombo(), 1, 2,  0,  1, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.group.label     , 2, 3,  0,  1)
      table$attach        (label.fill            , 0, 1,  1,  2, "shrink", "shrink", 0, 0)
      table$attach        (combo.fill$getCombo() , 1, 2,  1,  2, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.fill.label      , 2, 3,  1,  2)
      table$attach        (label.color           , 0, 1,  2,  3, "shrink", "shrink", 0, 0)
      table$attach        (combo.color$getCombo(), 1, 2,  2,  3, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.color.label     , 2, 3,  2,  3)
      table$attach        (label.shape           , 0, 1,  3,  4, "shrink", "shrink", 0, 0)
      table$attach        (combo.shape$getCombo(), 1, 2,  3,  4, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.shape.label     , 2, 3,  3,  4)
      table$attach        (label.size            , 0, 1,  4,  5, "shrink", "shrink", 0, 0)
      table$attach        (combo.size$getCombo() , 1, 2,  4,  5, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.size.label      , 2, 3,  4,  5)
      table$attach        (label.line            , 0, 1,  5,  6, "shrink", "shrink", 0, 0)
      table$attach        (combo.line$getCombo() , 1, 2,  5,  6, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.line.label      , 2, 3,  5,  6)
      table$attach        (label.scale           , 0, 1,  6,  7, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.scale           , 1, 3,  6,  7)
      table$attach        (label.position        , 0, 1,  7,  8, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.position        , 1, 3,  7,  8)
      table$attachDefaults(position.hbox         , 0, 3,  8,  9)
      table$attach        (label.linetype        , 0, 1,  9, 10, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.linetype        , 1, 3,  9, 10)
      table$attach        (label                 , 0, 1, 10, 11, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo                 , 1, 3, 10, 11)
      table$attach        (label.x               , 0, 1, 11, 12, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.x$getCombo()    , 1, 3, 11, 12)
      table$attach        (label.y               , 0, 1, 12, 13, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.y$getCombo()    , 1, 3, 12, 13)
      table$attach        (label3                , 0, 1, 13, 14, "shrink", "shrink", 0, 0)
      table$attachDefaults(entry3                , 1, 3, 13, 14)
      table$attach        (label4                , 0, 1, 14, 15, "shrink", "shrink", 0, 0)
      table$attachDefaults(entry4                , 1, 3, 14, 15)
      table$attach        (label5                , 0, 1, 15, 16, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo2                , 1, 3, 15, 16)
      
      table$setColSpacings(5)
      table$setRowSpacings(2)

      main <<- buildPlotOptionPage(table)

      gSignalConnect(locator.button, "clicked", function(button){
        locate <- grid.locator(unit="npc")
        position.entry$setText(sprintf("%s,%s", as.numeric(locate$x), as.numeric(locate$y)))
      })
      
      gSignalConnect(combo.position, "changed", function(combo){
        position <- localize(combo$getActiveText())
        if(position=="specify") position.hbox["sensitive"] <- TRUE
        else                    position.hbox["sensitive"] <- FALSE
      })

      gSignalConnect(combo.group$getCombo(), "changed", .self$generateScript)
      gSignalConnect(combo.group.label     , "changed", .self$generateScript)
      gSignalConnect(combo.fill$getCombo() , "changed", .self$generateScript)
      gSignalConnect(combo.fill.label      , "changed", .self$generateScript)
      gSignalConnect(combo.color$getCombo(), "changed", .self$generateScript)
      gSignalConnect(combo.color.label     , "changed", .self$generateScript)
      gSignalConnect(combo.shape$getCombo(), "changed", .self$generateScript)
      gSignalConnect(combo.shape.label     , "changed", .self$generateScript)
      gSignalConnect(combo.size$getCombo() , "changed", .self$generateScript)
      gSignalConnect(combo.size.label      , "changed", .self$generateScript)
      gSignalConnect(combo.line$getCombo() , "changed", .self$generateScript)
      gSignalConnect(combo.line.label      , "changed", .self$generateScript)
      gSignalConnect(combo.scale           , "changed", .self$generateScript)
      gSignalConnect(combo.position        , "changed", .self$generateScript)
      gSignalConnect(position.entry        , "changed", .self$generateScript)
      gSignalConnect(combo.linetype        , "changed", .self$generateScript)
      
      gSignalConnect(combo.x$getCombo(), "changed", .self$generateScript)
      gSignalConnect(combo.y$getCombo(), "changed", .self$generateScript)
      gSignalConnect(combo2            , "changed", .self$generateScript)
      
    },
    
    clear = function(){
      combo.group$clear()
      combo.group.label$setActive(0)
      combo.fill$clear()
      combo.fill.label$setActive(0)
      combo.color$clear()
      combo.color.label$setActive(0)
      combo.shape$clear()
      combo.shape.label$setActive(0)
      combo.size$clear()
      combo.size.label$setActive(0)
      combo.line$clear()
      combo.line.label$setActive(0)
      combo.scale$setActive(0)
      combo.position$setActive(1)
      position.entry$setText("0,1")
      combo.linetype$setActive(0)
      combo$setActive(0)
      combo.x$clear()
      combo.y$clear()
      entry3$setText("")
      entry4$setText("")
      combo2$setActive(0)
    },
    
    completionSetModel = function(model){
      combo.group$setModel(model)
      combo.fill$setModel(model)
      combo.color$setModel(model)
      combo.shape$setModel(model)
      combo.size$setModel(model)
      combo.line$setModel(model)
    },
        
    generateScript = function(...){
      iter  <- combo.scale$getActiveIter()$iter
      model <- combo.scale$getModel()
      scale <- localize(unlist(model$get(iter, 0, 1)))
      if(scale=="brewer") scale <- "BrBG"
      group <- localize(combo.group$getActiveText())
      fill  <- localize(combo.fill$getActiveText())
      colour<- localize(combo.color$getActiveText())
      shape <- localize(combo.shape$getActiveText())
      size  <- localize(combo.size$getActiveText())
      line  <- localize(combo.line$getActiveText())
      group.label     <- localize(combo.group.label$getActiveText())
      fill.label      <- localize(combo.fill.label$getActiveText())
      colour.label    <- localize(combo.color.label$getActiveText())
      shape.label     <- localize(combo.shape.label$getActiveText())
      size.label      <- localize(combo.size.label$getActiveText())
      line.label      <- localize(combo.line.label$getActiveText())
      legend.position <- localize(combo.position$getActiveText())
      legend.linetype <- localize(combo.linetype$getActiveText())
      vals <- c(group, fill, colour, shape, size, line)
      labs <- c(group.label, fill.label, colour.label, shape.label, size.label, line.label)
      labs <- ifelse(nzchar(vals), labs, "")
      if(legend.position=="specify"){
        vec <- localize(position.entry$getText())
        vec <- strsplit(vec, ",")[[1]]
        vec <- suppressWarnings(as.numeric(vec))
        if (any(is.na(vec)) || length(vec) != 2) {
          legend.position <- ""
        } else {
          legend.position <- sprintf("c(%s, %s)", vec[1], vec[2])
        }
      } else if (legend.position=="right") {
        legend.position <- ""
      } else {
        legend.position <- sprintf('"%s"', legend.position)
      }
      
      if (legend.linetype=="blank") {
        legend.linetype <- ""
      } else {
        legend.linetype <- sprintf('element_rect(fill="white", linetype="%s")', legend.linetype)
      }
      
      rzPlotScript$setAes(c("group", "fill", "colour", "shape", "size", "linetype") , vals)
      rzPlotScript$setLabs(c("group", "fill", "colour", "shape", "size", "linetype"), labs)
      
      if(nzchar(fill)) {
        scale_fill <- switch(scale,
                             hue="",
                             grey="scale_fill_grey()",
                             sprintf("scale_fill_brewer(palette=%s)", scale))
        rzPlotScript$setScript("scale_fill", scale_fill)
      } else {
        rzPlotScript$setScript("scale_fill", "")        
      }
      if(nzchar(colour)) {
        scale_colour <- switch(scale,
                               hue="",
                               grey="scale_colour_grey()",
                               sprintf("scale_colour_brewer(palette=%s)", scale))
        rzPlotScript$setScript("scale_colour", scale_colour)
      } else {
        rzPlotScript$setScript("scale_colour", "")        
      }
      rzPlotScript$setTheme("legend.position"  , legend.position)
      rzPlotScript$setTheme("legend.background", legend.linetype)
      
      # facet
      facet  <- localize(combo$getActiveText())
      x      <- localize(combo.x$getActiveText())
      y      <- localize(combo.y$getActiveText())
      scale  <- localize(combo2$getActiveText())
      if (scale=="fixed") scale <- NULL
      else scale <- sprintf('scale="%s"', scale)
      
      on     <- FALSE
      if(x!="" || y!=""){
        on   <- TRUE
      }
      if(facet=="wrap" & x=="") {
        on   <- FALSE
      }
      
      if (on) {
        if (facet=="grid") {
          if(!nzchar(x)) x <- "."
          if(!nzchar(y)) y <- "."
          formula <- as.formula(sprintf("%s ~ %s", x, y))
          rzPlotScript$setScript(layer="facet", type="grid",
                                 args=list(deparse(formula), scale=deparse(scale)))
        } else {
          nrow <- localize(entry3$getText())
          ncol <- localize(entry4$getText())
          nrow <- suppressWarnings(as.numeric(nrow))
          ncol <- suppressWarnings(as.numeric(ncol))
          if(is.na(nrow)) nrow <- NULL
          else nrow <- sprintf("nrow=%s", nrow)
          if(is.na(ncol)) ncol <- NULL
          else ncol <- sprintf("ncol=%s", ncol)
          formula <- as.formula(sprintf("~ %s", x))
          args <- paste(c(, nrow, ncol, scale), collapse=",")
          rzPlotScript$setScript(layer="facet", type="wrap",
                                 args=list(deparse(formula), nrow=deparse(nrow), ncol=deparse(ncol), scale=deparse(scale)))
        }
      } else {
        rzPlotScript$setScript("facet")        
      }
    }

    )
)
rzplot.stratum$accessors("main")


