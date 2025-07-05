library(shiny)
library(shinyWidgets)

fluidPage(
  # ====== cover shown before clicking“Load dataset!” ======
  conditionalPanel(
    condition = "input.sample_data == 0",
    fluidRow(
      column(12,
             div(style = "padding: 40px; text-align: center;",
                 tags$img(src = "300drugs.png", height = "550px"),  
                 tags$h1("Welcome to MPR Viz"),
                 tags$hr(),
                 tags$p("This dashboard visualizes metaproteomic profiling of six individual gut microbiomes cultured with pharmaceutical compounds."),
                 tags$p("Click below to load the experimental dataset and start exploring."),
                 actionButton("sample_data", 
                              icon = icon("play"),
                              label = "Load dataset!",
                              style="color: white; background-color: #007bff; border-color: #007bff; font-size: 18px; padding: 10px 20px;")
             )
      )
    )
  ),
  
  # ====== Shown after clicking "Load dataset" ======
  conditionalPanel(
    condition = "input.sample_data > 0",
    fluidRow(
      column(3,
             box(title = "The result dataset",
                 status = "primary", 
                 width =  12,
                 solidHeader = TRUE,
                 collapsible = FALSE,
                 fluidRow(
                   tags$ul(
                     tags$li("Our experiment used six gut microbiome samples cultured against a library of pharmaceutical compounds"),
                     tags$li("The experiment followed the RapidAIM 2.0, a semi-automated culture and metaproteomic workflow."),
                     tags$li("Here we demonstrate functional responses of the in vitro gut microbiome based on the metaproteomic results."),
                     tags$li("Note that missing values were imputed with 1/5 of the lowest intensity in each feature. ")
                   ),
                   column(12,
                          align = "center",
                          actionButton("gotoanalysis", 
                                       icon = icon("arrow-right"),
                                       label = "Go to analysis!",
                                       style="color: #fff; background-color: #00cc00; border-color: #2e6da4"
                          ),
                          hr()
                   )
                 )
             )
      ),
      
      column(9,
             fluidRow(
               box(title = "View dataset in tables",
                   status = "primary", 
                   width =  12,
                   solidHeader = TRUE,
                   collapsible = TRUE,
                   DATATABLE_Display_UI("data_table_display", boxwidth = 12, boxtitle = "Data matrix"),
                   DATATABLE_Display_UI("meta_table_display", boxwidth = 12, boxtitle = "Meta data")
               )
             )
      )
    )
  )
)

