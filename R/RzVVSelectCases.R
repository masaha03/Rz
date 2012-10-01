selectCases <-
  setRefClass("RzVVSelectCases",
  fields = c("main", "data", "textView", "textBuffer", "button.update"),
  methods = list(
    initialize = function(...) {
      initFields(...)
      
      toggleButton  <- gtkToggleButtonNewWithLabel(gettext("Enable Select Cases"))
      button.update <<- gtkButtonNewWithLabel(gettext("Update"))                  
      button.clear  <-  gtkButtonNewWithLabel(gettext("Clear"))                  
      textBuffer    <<- gtkTextBufferNew()
      textBuffer$setText(data$getSubset.condition())
      textView      <<- gtkTextViewNewWithBuffer(textBuffer)
      textView$setSensitive(FALSE)
      textView$setLeftMargin(5)
      textView$setRightMargin(5)
      scrolledWindow.textView <- gtkScrolledWindowNew()
      scrolledWindow.textView$setShadowType(GtkShadowType["in"])
      scrolledWindow.textView$setPolicy(GtkPolicyType["automatic"], GtkPolicyType["automatic"])
      scrolledWindow.textView$add(textView)
      scrolledWindow.textView$setSizeRequest(-1, 100)
      
      vbox.button1 <- gtkVBoxNew()
      vbox.button1$packStart(toggleButton, expand=FALSE)
      
      vbox.button2 <- gtkVBoxNew()
      vbox.button2$packStart(button.update, expand=FALSE)
      vbox.button2$packEnd(button.clear, expand=FALSE)

      hbox1 <- gtkHBoxNew(spacing=2)
      hbox1$packStart(vbox.button1, expand=FALSE)
      hbox1$packStart(scrolledWindow.textView)
      hbox1$packStart(vbox.button2, expand=FALSE)
            
      vbox <- gtkVBoxNew(spacing=4)
      vbox$setBorderWidth(2)
      vbox$packStart(hbox1, expand=FALSE)
      
      main <<- gtkScrolledWindowNew()
      main$setShadowType(GtkShadowType["none"])
      main$setPolicy(GtkPolicyType["automatic"], GtkPolicyType["automatic"])
      main$addWithViewport(vbox)
      
      gSignalConnect(toggleButton , "toggled", .self$onSelectCasesToggled)
      gSignalConnect(button.clear , "clicked", function(...){textBuffer$setText("")})
      gSignalConnect(button.update, "clicked", .self$onSelectCasesUpdated)
    },
    
    onSelectCasesToggled = function(button){
      if(button$getActive()){
        textView$setSensitive(TRUE)
        data$setSubset.on(TRUE)
      } else {
        textView$setSensitive(FALSE)
        data$setSubset.on(FALSE)
      }
      data$linkDataFrame()
    },
    
    onSelectCasesUpdated = function(button){
      iter <- textBuffer$getBounds()
      text <- textBuffer$getText(iter$start, iter$end)
      text <- localize(text)
      data$setSubset.condition(text)
      data$linkDataFrame()
    }
    
  ))
selectCases$accessors("main")
