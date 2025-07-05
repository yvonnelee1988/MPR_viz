library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)
library(stringr)
library(colourpicker)
library(plotly)
library(DBI)
library(RSQLite)
library(latex2exp)
library(ggrepel)
library(shinycssloaders)

# The files are large, increase the limit of file upload size to 500MB
source("shiny_modules_for_RapidAIM.R")
options(shiny.maxRequestSize=500*1024^2)


#  __ get data_table ------------------------------------------------------
# connect to SQLite
conn <- dbConnect(SQLite(), "data/my_database.sqlite")
# return a dataframe
data_table <- dbGetQuery(conn, "SELECT * FROM my_table")
#disconnect
dbDisconnect(conn)


# replace round brackets with rectangle ones
for (i in 1:16) {
  data_table[,i] <- stringr::str_replace_all(data_table[,i], "\\(", "<")
  data_table[,i] <- stringr::str_replace_all(data_table[,i], "\\)", ">")}

# Read meta table
meta_table <- read.table("data/directLFQ_dataset_meta1.txt", header = TRUE, sep="\t", fill = TRUE)
rownames(meta_table) <- meta_table$Sample

#  _header ------------------------------------------------------


header <- dashboardHeader(title = span(img(src="logo_imetalab.png", width = 140), "MPR Viz"),
                          titleWidth = 460,
                          tags$li(class = "dropdown",
                                  tags$a(tags$img(height = "18px", alt="SNAP Logo", src="logo_M.png")
                                  )
                          )
)


#  _side bar ------------------------------------------------------

sidebar <- dashboardSidebar(
  width = 200,
  sidebarMenu(
    id = "tabs",
    menuItem("About the Dataset", tabName = "dashboard", icon = icon("file")),
    menuItem("Functional analysis", tabName = "Analysis", icon = icon("file")),
    menuItem("iMetaLab", icon = icon("home"), 
             href = "http://www.imetalab.ca")
    
  )
)

# _body --------------------------------------------------------------

body <- dashboardBody(
  tabItems(
    #  ___dashboard/starting  tab  ------------------------------------------------------
    tabItem(tabName = "dashboard",
            source(
              file = "ui_dashboard.R",
              local = TRUE,
              encoding = "UTF-8"
            )$value),
    tabItem(tabName = "Analysis",
            source(
              file = "ui_analyze.R",
              local = TRUE,
              encoding = "UTF-8"
            )$value)
  ),
  
  #   CSS section ignored for analysis------------------------------------------------------ 
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  #Semi-collapsible sidebar
  tags$script(HTML("$('body').addClass('sidebar-mini');"))             
)



# ------ UI ---------------------------
ui <- dashboardPage(
  title = "MPR_viz",
  header,
  sidebar,
  body
)


server <- function(input, output, session){
  source(file = "server_dashboard.R",
         local = TRUE,
         encoding = "UTF-8")
  source(file = "server_analyze.R",
         local = TRUE,
         encoding = "UTF-8")
  # output.fileUploaded1 = TRUE
}


shinyApp(ui, server)
