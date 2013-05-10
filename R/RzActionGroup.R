ag <-
setRefClass("RzActionGroup",
  fields = c("action.group", "a.file", "a.open",
             "a.save", "a.ds", "a.duplicate", "a.delete", "a.selectall", "a.unselect",
             "a.quit", "a.edit","a.ch.name", "a.revert", "a.reload",
             "a.remove", "a.vlabs", "a.missing", 
             "a.recode", "a.value.lab", "a.settings",
             "a.help", "a.tutorial", "a.load.sample"),
  methods = list(
    initialize            = function(...) {
      initFields(...)
      
      gtkActionSetIconFromFile <- function(widget, filename) {
        image <- gFileIconNew(gFileNewForPath(file.path(rzSettings$getRzPath(), "images/oxygen", filename)))
        widget$setGicon(image)
      }

      a.file      <<- gtkActionNew("MenuFile", gettext("_File"))
      a.open      <<- gtkActionNew("Open", gettext("_Open"), gettext("Open"))
      a.save      <<- gtkActionNew("Save As", gettext("Save _As"), gettext("Save As"))
      a.ds        <<- gtkActionNew("ImportFromGlobalEnv", gettext("Import from _Grobal Environment"), gettext("Import from Grobal Environment"))
      a.quit      <<- gtkActionNew("Close", gettext("_Close"), gettext("Close"))
      a.edit      <<- gtkActionNew("MenuEdit", gettext("_Edit"))
      a.ch.name   <<- gtkActionNew("ChageDataSetName", gettext("_Change the Dataset Name"), gettext("Change the Dataset Name"))
      a.remove    <<- gtkActionNew("RemoveDataSet", gettext("_Remove Dataset"), gettext("Remove Dataset"))
      a.revert    <<- gtkActionNew("RevertToOriginal", gettext("Revert to Original DataSet"), gettext("Revert to Original DataSet"))
      a.reload    <<- gtkActionNew("ReloadFromGlobalEnv", gettext("Reload from Grobal Environment"), gettext("Reload from Grobal Environment"))
      a.vlabs     <<- gtkActionNew("ValueLabels", gettext("Value Labels"))
      a.missing   <<- gtkActionNew("Missing", gettext("Missing Values"))
      a.recode    <<- gtkActionNew("Recode", gettext("Recode"))
      a.delete    <<- gtkActionNew("Delete", gettext("Delete Variables"), gettext("Delete Variables"))
      a.duplicate <<- gtkActionNew("Duplicate", gettext("Duplicate Variables"), gettext("Duplicate Variables"))
      a.value.lab <<- gtkActionNew("EditValueLabels", gettext("Edit Value Labels"))
      a.settings  <<- gtkActionNew("Settings", gettext("_Preferences"), gettext("Preferences"))
      a.help      <<- gtkActionNew("MenuHelp", gettext("_Help"))
      a.tutorial  <<- gtkActionNew("Tutorial", gettext("Tutorial on Web"), gettext("Tutorial on Web"))
      a.load.sample <<- gtkActionNew("LoadSample", gettext("Load Sample Dataset"), gettext("Load Sample Dataset"))
      a.open$setIconFromFile("document-open.png")
      a.save$setIconFromFile("document-save-as.png")
      a.quit$setIconFromFile("window-close.png")
      a.settings$setIconFromFile("configure.png")      
      a.ds$setIconFromFile("table_add.png")
      a.ch.name$setIconFromFile("edit-rename.png")
      a.remove$setIconFromFile("table_delete.png")
      a.revert$setIconFromFile("revert.png")
      a.reload$setIconFromFile("table_refresh.png")
      a.delete$setIconFromFile("table_row_delete.png")
      a.duplicate$setIconFromFile("table_row_insert.png")
      a.open$setAccelPath("<Rz-Menu>/File/Open")
      a.save$setAccelPath("<Rz-Menu>/File/Save As")
      a.ds$setAccelPath("<Rz-Menu>/File/Import")
      a.quit$setAccelPath("<Rz-Menu>/File/Close")
      a.reload$setAccelPath("<Rz-Menu>/Edit/Reload")
      
      action.group  <<- gtkActionGroupNew()
      action.group$setTranslationDomain("pkg-RGtk2")
    },
    
    load = function(){
      action.group$addAction(a.file)
      action.group$addAction(a.open)
      action.group$addAction(a.save)
      action.group$addAction(a.ds)
      action.group$addAction(a.quit)

      action.group$addAction(a.edit)
      action.group$addAction(a.ch.name)
      action.group$addAction(a.remove)
      action.group$addAction(a.revert)
      action.group$addAction(a.reload)
#      action.group$addAction(a.selectall)
#      action.group$addAction(a.unselect)
      action.group$addAction(a.delete)
      action.group$addAction(a.duplicate)
      action.group$addAction(a.settings)
      
      action.group$addAction(a.help)
      action.group$addAction(a.tutorial)
      action.group$addAction(a.load.sample)

      action.group$addAction(a.recode)
      action.group$addAction(a.value.lab)
      
      
    }
  )
)
ag$accessors(c("action.group", "a.file", "a.open",
               "a.save", "a.ds", "a.quit",
               "a.edit", "a.ch.name", "a.remove",
               "a.revert", "a.reload",
               "a.vlabs", "a.duplicate", "a.delete", "a.selectall", "a.unselect",
               "a.missing", "a.recode", "a.value.lab", "a.settings",
               "a.tutorial", "a.load.sample"))

