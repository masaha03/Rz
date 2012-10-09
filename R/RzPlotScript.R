rzPlotScript <- 
setRefClass("RzPlotScript",
  fields = c("script", "aes","baseTheme", "theme", "labs", "data"),
  methods = list(
    initialize  = function(...) {
      initFields(...)
      script   <<- list()
      aes      <<- ""
      labs     <<- ""
      theme    <<- ""
      baseTheme <<- c("grey", "12")
      data     <<- NULL
    },
    
    setScript = function(layer, type=NULL, args=NULL, add=FALSE){
      if (is.null(type)) {
        script[[layer]] <<- NULL
        return()
      }
      
      if (add) {
        ind <- length(script[[layer]]) + 1
        script[[layer]][[ind]] <<- list(type=type, args=args)
      } else {
        script[[layer]]        <<- NULL
        script[[layer]][[ 1 ]] <<- list(type=type, args=args)
      }
    },
    
    getScript = function(){
      text <- character()
      if (any(nzchar(aes))) {
        aes.tmp <- aes[nzchar(aes)]
        aes.tmp.names <- names(aes.tmp)
        aes.tmp <- paste(aes.tmp.names, aes.tmp, sep="=", collapse=", ")
        text["data"] <- sprintf("p <- ggplot(data=%s, aes(%s))",
                                     data$getDataSetName(), aes.tmp)
      } else {
        text["data"] <- sprintf("p <- ggplot(data=%s)", data$getDataSetName())
      }
      if (any(nzchar(labs))) {
        labs.tmp <- labs[nzchar(labs)]
        title <- labs.tmp["title"]
        labs.tmp <- labs.tmp[ ! names(labs.tmp) %in% "title" ]
        labs.tmp.names <- names(labs.tmp)
        variableNames  <- data$getVariableNames()
        variableLabels <- data$getVariableLabels()
        names(variableLabels) <- variableNames
        vals <- aes[labs.tmp.names]
        labels <- variableLabels[vals]
        labs.tmp <- ifelse(labs.tmp==gettext("variable name") , vals  , labs.tmp)
        labs.tmp <- ifelse(labs.tmp==gettext("variable label"), labels, labs.tmp)
        names(labs.tmp) <- labs.tmp.names
        x     <- labs.tmp["x"]
        y     <- labs.tmp["y"]
        labs.tmp <- labs.tmp[ ! names(labs.tmp) %in% c("x", "y") ]
        labs.tmp.names <- names(labs.tmp)
        labs.tmp <- sprintf('"%s"', labs.tmp)
        labs.tmp <- paste(labs.tmp.names, labs.tmp, sep="=", collapse=", ")
        if(!nzchar(labs.tmp)) labs.tmp <- NULL
        if(!is.na(y)) {
          y        <- sprintf('y="%s"', y)
          labs.tmp <- paste(c(y, labs.tmp), collapse=", ")
        }
        if(!is.na(x)) {
          x        <- sprintf('x="%s"', x)
          labs.tmp <- paste(c(x, labs.tmp), collapse=", ")
        }
        if(!is.na(title)) {
          title    <- sprintf('title="%s"', title)
          labs.tmp <- paste(c(title, labs.tmp), collapse=", ")
        }
        if(length(labs.tmp) > 0 && nzchar(labs.tmp)) {
          text["labs"] <- sprintf("labs(%s)", labs.tmp)          
        }
      }
      if (any(nzchar(theme))) {
        theme.tmp <- theme[nzchar(theme)]
        theme.tmp.names <- names(theme.tmp)
        theme.tmp <- paste(theme.tmp.names, theme.tmp, sep="=", collapse=", ")
        text["theme"] <- sprintf("theme(%s)", theme.tmp)
      }

      
      script.tmp <- script
      
      if (all(!nzchar(aes[ ! names(aes) %in% c("x", "y") ]))) {
        script.tmp[["stat"]] <- lapply(script.tmp[["stat"]], function(x){
          if (x$type=="sum") {
            x$args <- list("aes(group=1)", x$args)
          }
          return(x)
        })
      }
      geom    <- .self$buildLayer("geom"   , script.tmp[["geom"]], var=data$getData.frame()[[aes["x"]]])
      stat    <- .self$buildLayer("stat"   , script.tmp[["stat"]])
      facet   <- .self$buildLayer("facet"  , script.tmp[["facet"]])      
      scale_x <- .self$buildLayer("scale_x", script.tmp[["scale_x"]])      
      scale_y <- .self$buildLayer("scale_y", script.tmp[["scale_y"]])      
      coord   <- .self$buildLayer("coord"  , script.tmp[["coord"]])

      text <- c(text["data"], geom, stat, facet, scale_x, scale_y, coord,
                      text["labs"], text["theme"])
      text <- text[!is.na(text)]

      font <- rzSettings$getPlotFontFamily()
      font.tmp <- character()
      if(grepl("mingw", R.Version()$os)){
        font.tmp[1] <- sprintf("windowsFonts(\"%s\" = windowsFont(\"%s\"))", font, font)
        font.tmp[2] <- sprintf("theme_set(theme_%s(base_size=%s, base_family=\"%s\"))",
                               baseTheme[1], baseTheme[2], font)
      } else if(grepl("darwin", R.Version()$os)){
        x11font     <- sprintf("-*-%s-*-*-*-*-*-*-*-*-*-*-*-*", font)
        quartzfont  <- rep(font, 4)
        font.tmp[1] <- sprintf("X11Fonts(\"%s\"=X11Font(\"%s\"))", font, x11font)
        font.tmp[2] <- sprintf("quartzFonts(\"%s\"=quartzFont(\"%s\"))", font, quartzfont)
        font.tmp[3] <- sprintf("theme_set(theme_%s(base_size=%s, base_family=\"%s\"))",
                               baseTheme[1], baseTheme[2], font)
      } else {
        font.tmp[1] <- sprintf("theme_set(theme_%s(base_size=%s, base_family=\"%s\"))",
                               baseTheme[1], baseTheme[2], font)
      }
      
      text <- paste(text, collapse="\np <- p +")
      text <- c(font.tmp, text, "print(p)")
      text <- tidy.source(text=text, output=FALSE, keep.blank.line=FALSE, width.cutoff=50)$text.tidy
      text <- paste(text, collapse="\n")
      return(text)
    },
    
    setAes = function(type, value){
      aes[type] <<- value
    },
    
    getAes = function(){
      return(aes)
    },
    
    setLabs = function(type, value){
      labs[type] <<- value
    },

    setTheme = function(type, value){
      theme[type] <<- value
    },
    
    setData = function(data) {
      script <<- list()
      aes      <<- ""
      labs     <<- ""
      theme    <<- ""
      baseTheme <<- c("grey", "12")
      data     <<- data
    },
    
    buildLayer = function(layer, list, var=NULL){
      layers <- NULL
      if (is.null(var)) {
        layers <- lapply(list, function(x) {
          args <- unlist(x$args)
          args.names <- names(args)
          args <- ifelse(nzchar(args.names), sprintf("%s=%s", args.names, args), args)
          args <- paste(args, collapse=",")
          return( sprintf("%s_%s(%s)", layer, x$type, args) )
        })    
      } else {
        layers <- lapply(list, function(x) {
          args <- unlist(x$args)
          args.names <- names(args)
          ind <- which(args.names=="hist.break")
          if (length(ind > 0)) {
            args[ind] <- sapply(args[ind], function(x) calc.hist.breaks(eval(parse(text=x)), var))
            args.names[ind] <- ""
          }
          args <- ifelse(nzchar(args.names), sprintf("%s=%s", args.names, args), args)
          args <- paste(args, collapse=",")
          return( sprintf("%s_%s(%s)", layer, x$type, args) )
        })
      }
      
      if (length(layers) > 0) {
        return(layers)        
      } else {
        return(NULL)
      }
    }
    
    )
)
rzPlotScript$accessors(c("baseTheme"))