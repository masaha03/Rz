analysisStat <-
setRefClass("RzAnalysisStat",
  fields = c("main", "textview", "cross.combo1", "cross.combo2", "cross.combo3"),
  methods = list(
    initialize            = function(...) {
      initFields(...)
      
      methods <- c(gettext("Basic Statistics"),
                   gettext("Cross Tabulation"),
                   gettext("Correlation"))
      
      combo.methods <- gtkComboBoxNewText()
      for(i in methods) combo.methods$appendText(i)
      hbox.methods  <- gtkHBoxNew(spacing=5)
      hbox.methods$packStart(combo.methods)
      
      textview <<- gtkTextViewNew()
      textview$modifyFont(pangoFontDescriptionFromString(rzSettings$getMonospaceFont()))
      textview$setLeftMargin(5)
      textview$setRightMargin(5)
      scrolledwindow.textview <- gtkScrolledWindowNew()
      scrolledwindow.textview["shadow-type"] <- GtkShadowType["in"]
      scrolledwindow.textview$setPolicy(GtkPolicyType["automatic"], GtkPolicyType["automatic"])
      scrolledwindow.textview$add(textview)
      
      button.execute <- gtkButtonNewFromStock(GTK_STOCK_EXECUTE)
      button.clear   <- gtkButtonNewFromStock(GTK_STOCK_CLEAR)
      button.box     <- gtkHButtonBoxNew()
      button.box$packStart(button.clear)
      button.box$packStart(button.execute)
            
      cross.combo1 <<- new("RzCompletionCombo")
      cross.combo2 <<- new("RzCompletionCombo")
      cross.combo3 <<- new("RzCompletionCombo")
      cross.label1 <-  gtkLabelNew(gettext("Row"))
      cross.label2 <-  gtkLabelNew(gettext("Col"))
      cross.label3 <-  gtkLabelNew(gettext("Stratum"))

      cross.table <- gtkTableNew(homogeneous=FALSE)
      cross.table$setSizeRequest(250, -1)
      cross.table$attach(cross.label1           , 0, 1, 0, 1, xpadding=2, xoptions=2)
      cross.table$attach(cross.combo1$getCombo(), 1, 2, 0, 1)
      cross.table$attach(cross.label2           , 0, 1, 1, 2, xpadding=2, xoptions=2)
      cross.table$attach(cross.combo2$getCombo(), 1, 2, 1, 2)
      cross.table$attach(cross.label3           , 0, 1, 2, 3, xpadding=2, xoptions=2)
      cross.table$attach(cross.combo3$getCombo(), 1, 2, 2, 3)
      
      vbox <- gtkVBoxNew()
      hbox <- gtkHBoxNew()
      hbox$packStart(cross.table, padding=2, expand=FALSE)
      vbox$packStart(hbox       , padding=2, expand=FALSE)
      
      scrolledwindow.vbox <- gtkScrolledWindowNew()
      scrolledwindow.vbox$setPolicy(GtkPolicyType["automatic"], GtkPolicyType["automatic"])
      scrolledwindow.vbox$addWithViewport(vbox)
      
      hpaned <- gtkHPanedNew()
      hpaned$pack1(scrolledwindow.vbox    , resize=FALSE)
      hpaned$pack2(scrolledwindow.textview, resize=TRUE)
      hpaned$setPosition(260)
      
      main <<- gtkVBoxNew(spacing=0)
      main$packStart(hbox.methods, expand=FALSE, padding=2)
      main$packStart(hpaned      , expand=TRUE , padding=2)
      main$packStart(button.box  , expand=FALSE, padding=2)
      
      gSignalConnect(button.clear, "clicked", function(button){
        cross.combo1$clear()
        cross.combo2$clear()
        cross.combo3$clear()
      })
      
      gSignalConnect(cross.combo1$getCombo(), "changed", function(combo){ .self$setScript() })
      gSignalConnect(cross.combo2$getCombo(), "changed", function(combo){ .self$setScript() })
      gSignalConnect(cross.combo3$getCombo(), "changed", function(combo){ .self$setScript() })
      
      gSignalConnect(button.execute, "clicked", function(button){
        variable.view <- rzTools$getVariableView()
        if(is.null(variable.view)) return()

        buffer <- textview$getBuffer()
        iter   <- buffer$getBounds()
        script <- localize(buffer$getText(iter$start, iter$end))
        script <- sub("^([[:space:]]+)([^[:space:]]+)([[:space:]]+)$", "\\2", script)
        if(!nzchar(script)) return()
        
        cat(gettext("\n=============== Output by Rz ===============\n"), fill=TRUE)
        e <- try(eval(parse(text=script), envir=.GlobalEnv), silent=TRUE)
        info.bar <- rzTools$getInfoBar()
        if (class(e)=="try-error"){
          info.bar$setMessageType(GtkMessageType["error"])
          info.bar$setText(e[1])
          info.bar$show()
        } else {
          print(e)
          info.bar$hide()
        }
      })
    },
    
    toggled = function(){
      variable.view <- rzTools$getVariableView()
      if(!is.null(variable.view)){
        model  <- variable.view$getListstore()
        cross.combo1$setModel(model)
        cross.combo2$setModel(model)
        cross.combo3$setModel(model)        
      }
    },
    
    setScript = function(){
      script <- NULL
      data.set.name <- rzTools$getVariableView()$getData()$getData.set.name()
      x <- localize(cross.combo1$getActiveText())
      y <- localize(cross.combo2$getActiveText())
      z <- localize(cross.combo3$getActiveText())
      if(!nzchar(z)){
        script <- sprintf("with(%s,\n    summary(crossTable(%s, %s))\n)",
                          data.set.name, x, y)        
      } else {
        script <- sprintf("with(%s,\n    summary(crossTable(%s, %s, %s))\n)",
                          data.set.name, z, x, y)
      }
      
      textview$getBuffer()$setText(script)

    }
  )
)
analysisStat$accessors("main")

