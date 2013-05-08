VVPlot <-
setRefClass("RzVVPlot",
  fields = c("main", "data", "liststore"),
  methods = list(
    initialize            = function(...) {
      initFields(...)
      
      tw <- gtkTreeViewNewWithModel(liststore)
      sw <- gtkScrolledWindowNew()
      sw["shadow-type"] <- GtkShadowType["none"]
      sw$add(tw)
      sw$setPolicy(GtkPolicyType["automatic"], GtkPolicyType["automatic"])
      tw["enable-grid-lines"] <- GtkTreeViewGridLines["both"]
      tw["rules-hint"] <- TRUE
      tw["has-tooltip"] <- TRUE
      if(! grepl("darwin",R.Version()$os)) {
        tw$modifyFont(pangoFontDescriptionFromString(rzSettings$getVariableViewFont()))
      }
      
      rt.index    <- gtkCellRendererText()
      rtg.select  <- gtkCellRendererToggleNew()
      rt.vars     <- gtkCellRendererText()
      rt.var.labs <- gtkCellRendererText()
      rp.msr      <- gtkCellRendererPixbuf()
      color       <- rt.index["cell-background-gdk"]
      color$red   <- 45000L
      color$green <- 45000L
      color$blue  <- 45000L
      rt.index["cell-background-gdk"] <- color
      rt.index["xalign"] <- 0.5
      
      columns <- list(
        index   = gtkTreeViewColumnNewWithAttributes(""                     , rt.index   , "text"=column.definition[["index"]]   ),
        select  = gtkTreeViewColumnNewWithAttributes(""                     , rtg.select , "active"=column.definition[["select"]]),
        msr     = gtkTreeViewColumnNewWithAttributes(gettext("Measurement") , rp.msr     , "pixbuf"=column.definition[["msr.image"]]),
        vars    = gtkTreeViewColumnNewWithAttributes(gettext("Names")       , rt.vars    , "text"=column.definition[["vars"]]    ),
        labs    = gtkTreeViewColumnNewWithAttributes(gettext("Labels")      , rt.var.labs, "text"=column.definition[["var.labs"]])
      )
      
      lapply(columns, gtkTreeViewColumnSetSizing   , "fixed")
      lapply(columns, gtkTreeViewColumnSetResizable, TRUE)
      lapply(columns, gtkTreeViewColumnSetSpacing  , 1)
      
      columns$index$setData("attr", c(title="index"))
      columns$index$setMinWidth(30)
      columns$index$setSizing("automatic")
      columns$index$setResizable(FALSE)
      
      columns$select$setData("attr", c(title="select"))
      columns$select$setSizing("automatic")
      columns$select$setResizable(FALSE)
      
      columns$vars$setData("attr", c(title="vars"))
      columns$vars$setFixedWidth(50)
      
      columns$labs$setData("attr", c(title="labs"))
      columns$labs$setFixedWidth(250)
            
      columns$msr$setData("attr", c(title="msr"))
      columns$msr$setFixedWidth(30)
      columns$msr$setMinWidth(30)
      
      lapply(columns, function(column) tw$appendColumn(column))
      
      
      main <<- gtkHPaned()
      rzPlot <- new("RzPlot")
      rzPlot$setModel(liststore)
      rzPlot$setData(data)
      
      main$pack1(sw)
      main$pack2(rzPlot$getMain(), resize=TRUE)
      main$setPosition(400)
      rzPlot$construct()
      
    }
    
  )
)
VVPlot$accessors("main")
