library(DT)
library(shinyjs)

#__ for data prepocess before plotting--------------------------------------
rvalues <- reactiveValues()
rvalues$ready_for_analysis<- FALSE


# Add sweet alert to inform that data has uploaded


# display the expression table
observe({
  if(input$sample_data >0){
  head(data_table)#  have to "use" this reactive expression once to make it work in the next module
  callModule(DATATABLE_Display, "data_table_display",
             data_table = data_table,
             filename_tag = "data_table",
             height = 300)
  }
})


# display the meta table
observe({
  if(input$sample_data >0){
  head(meta_table)#  have to "use" this reactive expression once to make it work in the next module
  callModule(DATATABLE_Display, "meta_table_display",
             data_table = meta_table,
             filename_tag = "meta_table",
             height = 300)
  }
})

observe({
  if (!is.null(data_table) && !is.null(meta_table)) {
    rvalues$ready_for_analysis <- TRUE
  }
  if (input$sample_data >0) {
  message = function(m){
    shinyjs::html("console", m$message, add = TRUE)
  }
  sendSweetAlert(
    session = session,
    title = "Dataset uploaded",
    text = "The dataset is loaded, you can view the dataset now. Then, please click \"Go to functional response analysis\" button to continue.",
    type = "success"
  )
  }
})


# Swith tabs
observeEvent(
  input$gotoanalysis, {
    updateTabItems(session, "tabs", "Analysis")
  }
)