
DATATABLE_Display_UI <- function(id, boxtitle = NULL,boxwidth = 12){
  ns <- NS(id)
  box(
    width = boxwidth,
    status = "primary",
    title = boxtitle,
    solidHeader = TRUE,
    # tableDownloadUI(ns("download_table"), 
    #                 label = "Download table"),
    DT::dataTableOutput(ns('table_output_data_for_network'))
  )
  
}



DATATABLE_Display <- function(input, output, session, data_table, filename_tag, height = 600){
  ns <- session$ns
  library(DT) # for table 
  callModule(tableDownload,"download_table", 
             data = data_table, 
             filename_tag = filename_tag) 
  
  output$table_output_data_for_network <- DT::renderDataTable(
    data_table,
    filter = 'top',
    extensions = c('Scroller'),
    options = list(
      autoWidth = TRUE,
      pageLength = 50,
      dom = 'Brtip',
      #buttons = c('colvis'),
      scrollY = height,
      scrollX = TRUE)
  )
  
}


## additional argument to define the label on the downloadButton
tableDownloadUI <- function(id, label = "Download CSV") {
  ns <- NS(id)
  
  downloadButton(ns("download_table"), label)
}

## allow users of the module to input a (reactive) data.frame to download as csv and a name for the file
tableDownload <- function(input, output, session, data, filename_tag = NULL
) {
  
  output$download_table <- downloadHandler(
    filename = function() {
      paste0(filename_tag,"_", Sys.time(), ".tsv")
    },
    content = function(file) {
      #write.csv(data, file)
      write.table(data,   # do not use data(),  here accepts either values or reactive expressions
                  file, 
                  sep = "\t",
                  col.names = NA)
    }
  )
}
