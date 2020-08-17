#Install Package
install.packages("shiny")
install.packages("DT")

require("shiny")
require("dplyr")
require("DT")

baseline=read.table(file="D:\\Exported.txt", sep=";",dec=" ", header=T)
baseline$SEXE_SINGLE_FCT
type <- unique(baseline$SEXE_SINGLE_FCT)

## Details
sexe.details = function(data, typedata){
  sexe.details = list()
  count = list()
  taux = list()
  length = length(typedata)
  for(i in 1:length ){
	count[i]= (sum(data == type[i]))
	print(count[i])
	taux[i]=(as.numeric(count[i])/length(data)) * 100
  }
  sexe.details = cbind(count,taux)
  return (sexe.details)
}
 
sexe.details <- sexe.details(baseline$SEXE_SINGLE_FCT, type) 

## Data
input_data <- data.frame(Sexe = type,sexe.details)

## Module
modFunction <- function(input, output, session, data,reset) {
  
  v <- reactiveValues(data = data)
  
  proxy = dataTableProxy("mod_table")
  
  observeEvent(input$mod_table_cell_edit, {
    print(names(v$data))
    info = input$mod_table_cell_edit
    str(info)
    i = info$row
    j = info$col
    k = info$value
    str(info)
    
    isolate(
      if (j %in% match(c("count","taux"), names(v$data))) {
          v$data[i, j] <<- DT::coerceValue(k, v$data[i, j])
      } else {
        stop(".") 
      }
    )
    replaceData(proxy, v$data, resetPaging = FALSE)
  })
  
  observeEvent(reset(), {
    v$data <- data
  })
  
  print(isolate(colnames(v$data)))
  output$mod_table <- DT::renderDataTable({
    DT::datatable(v$data, editable = TRUE)
    
  })
}

modFunctionUI <- function(id) {
  ns <- NS(id)
  DT::dataTableOutput(ns("mod_table"))
  
}

### Shiny App
shinyApp(
  ui = basicPage(
	titlePanel("Résumé données colonne sexe_single_fct")
    mainPanel(
      actionButton("reset", "Reset"),
      tags$hr(),
      modFunctionUI("editable")
    )
  ),
  server = function(input, output) {
    demodata<-input_data
    callModule(modFunction,"editable", demodata,
               reset = reactive(input$reset))
  }
)

shinyApp(ui, server)
runApp("app")
