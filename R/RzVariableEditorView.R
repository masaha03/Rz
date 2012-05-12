variableEditorView <-
setRefClass("RzVariableEditorView",
  fields = c("main", "textview", "data",
             "info.bar", "variable.view", "button.execute",
             "current.page", "script.prev"),
  methods = list(
    initialize            = function(...) {
      initFields(...)
      data <<- NULL
      variable.view <<- NULL
      script.prev <<- ""
      
      templates <- c(
          'measurement(var) <- "interval"',
          'description(var) <- "Variable Label"',
          'labels(var) <- c(\n  "a"=1,\n  "b"=2,\n  "c"=3\n  )',
          'missing.values(var) <- c(8, 9)',
          'var <- -var'
          )
      names(templates) <- c(gettext("Change Measurement"),
                            gettext("Edit Variable Labels"),
                            gettext("Edit Value Labels"),
                            gettext("Apply Missing Values"),
                            gettext("Reverse"))
      
      label.template <- gtkLabelNew(gettext("Templates"))
      combo.template <- gtkComboBoxNewText()
      for(i in names(templates)) combo.template$appendText(i)
      hbox.template  <- gtkHBoxNew(spacing=5)
      hbox.template$packStart(label.template, expand=FALSE)
      hbox.template$packStart(combo.template)

      textview <<- gtkTextViewNew()
      textview$modifyFont(pangoFontDescriptionFromString(rzSettings$getMonospaceFont()))
      textview$setLeftMargin(5)
      textview$setRightMargin(5)
      scrolledWindow2 <- gtkScrolledWindowNew()
      scrolledWindow2["shadow-type"] <- GtkShadowType["in"]
      scrolledWindow2$setPolicy(GtkPolicyType["automatic"], GtkPolicyType["automatic"])
      scrolledWindow2$add(textview)

      button.execute <<- gtkButtonNewFromStock(GTK_STOCK_EXECUTE)
      button.clear   <-  gtkButtonNewFromStock(GTK_STOCK_CLEAR)
      button.box2    <-  gtkHButtonBoxNew()
      button.box2$packStart(button.clear)
      button.box2$packStart(button.execute)

      vbox2 <- gtkVBoxNew(spacing=0)
#      vbox2$packStart(hbox.template, expand=FALSE)
      vbox2$packStart(scrolledWindow2, padding=2)
      vbox2$packStart(button.box2, expand=FALSE)
      
      vbox3 <- gtkVBoxNew(spacing=0)
      vbox3$packStart(hbox.template, expand=FALSE, padding=2)
      
      main <<- gtkNotebookNew()
      main$setTabPos("bottom")
      main$appendPage(vbox2, gtkLabelNew(gettext("Quick Editor")))
      main$appendPage(vbox3, gtkLabelNew(gettext("Quick Variable Editor")))
      current.page <<- 0
      
      gSignalConnect(button.clear, "clicked", function(button){
        buffer <- textview$getBuffer()
        buffer$setText("")
      })
      gSignalConnect(combo.template, "changed", function(combo){
        text <- localize(combo$getActiveText())
        buffer <- textview$getBuffer()
        buffer$setText(templates[text])
      })
      gSignalConnect(main, "switch-page", function(main, page, page_num){
        if(page_num==0){
          vbox3$remove(scrolledWindow2)
          vbox3$remove(button.box2)
          vbox2$packStart(scrolledWindow2, padding=2)
          vbox2$packStart(button.box2, expand=FALSE)
          buffer <- textview$getBuffer()
          iter   <- buffer$getBounds()
          script <- localize(buffer$getText(iter$start, iter$end))
          buffer$setText(script.prev)
          script.prev <<- script
        } else {
          vbox2$remove(scrolledWindow2)
          vbox2$remove(button.box2)
          vbox3$packStart(scrolledWindow2, padding=2)
          vbox3$packStart(button.box2, expand=FALSE)          
          buffer <- textview$getBuffer()
          iter   <- buffer$getBounds()
          script <- localize(buffer$getText(iter$start, iter$end))
          buffer$setText(script.prev)
          script.prev <<- script
        }
        current.page <<- page_num
      })
      gSignalConnect(button.execute, "clicked", function(button){
        if(is.null(data)) return()
        buffer <- textview$getBuffer()
        iter   <- buffer$getBounds()
        script <- localize(buffer$getText(iter$start, iter$end))
        script <- sub("^([[:space:]]+)([^[:space:]]+)([[:space:]]+)$", "\\2", script)
        if(!nzchar(script)) return()
        data.set.name <- data$getData.set.name()
        data.set      <- data$getData.set()
        data.frame    <- data$getData.frame()
        model         <- variable.view$getModel.selected()
        iter <- model$getIterFirst()
        vars <- character(0)
        while(iter$retval){
          var  <- localize(model$getValue(iter$iter, column.definition["vars"])$value)
          vars <- c(vars, var)
          iter$retval <- model$iterNext(iter$iter)
        }
        if(!nzchar(vars[1])) return()
#        script <- paste(data.set.name, "<- within(", data.set.name, ",{\n",
#                        "foreach(var=c(", vars, "), {\n",
#                        script,
#                        "\n}",
#                        ")", "}", ")")
        if (current.page==0) {
          env <- new.env()
          rz.tmp.path <- tempfile()
          env$df.orig <- data.frame
          env$df      <- data.frame[vars]
          cat(gettext("\n=============== Output from Quick Editor ===============\n"), fill=TRUE)
          e <- try(eval(parse(text=script), envir=env), silent=TRUE)
          if (class(e)=="try-error"){
            info.bar$setMessageType(GtkMessageType["error"])
            info.bar$setText(e[1])
            info.bar$show()
          } else {
            print(e)
            info.bar$hide()
          }
        } else if(current.page==1) {
          vars <- paste(vars, collapse=", ")
          script <- sprintf("foreach(var=c(%s), {\n%s\n})", vars, script)
          data.set <- try(within(data.set, eval(parse(text=script))), silent=TRUE)
          if (is.data.set(data.set)){
            data$setData.set(data.set)
            data$setData.frame(as.data.frame(data.set))
            variable.view$reload()
            data$linkDataFrame()
            info.bar$hide()
          } else {
            info.bar$setMessageType(GtkMessageType["error"])
            info.bar$setText(data.set[1])
            info.bar$show()
          }
        }

      })
    },
    
    setVariableView = function(variable.view){
      variable.view <<- variable.view
      if(is.null(variable.view)){
        data <<- NULL
      } else {
        data  <<- variable.view$getData()
      }
    },
    
    setAccel = function(accel.group){
      button.execute$setAccelPath("<Rz-Menu>/View/Quick Editor View/Execute", accel.group)
    }
  )
)
variableEditorView$accessors("main", "info.bar")
