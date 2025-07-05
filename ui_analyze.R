

fluidRow( # this fluidrow is to ensure the extend the height
  column(4,
         box(width = 12,
             title = "Select Drug Selection Mode",
             solidHeader = TRUE,
             status = "primary",
             radioButtons("drug_selection_mode", "Choose how to select drugs:",
                          choices = c("By drug class (ATC Level_1)" = "by_class",
                                      "Manually select drugs" = "manual"),
                          selected = "by_class"),
             # select based on drug class 
             conditionalPanel(
               condition = "input.drug_selection_mode == 'by_class'",
               selectInput("selectLevel_1", "Select Drug Class (ATC Level_1):",
                           choices = unique(str_trim(meta_table$Level_1[!str_detect(meta_table$Level_1, "^(NA|DMSO|Kestose)$") & !is.na(meta_table$Level_1)])),
                           selected = "N",
                           multiple = FALSE)
             ),
             # user choose drugs
             conditionalPanel(
               condition = "input.drug_selection_mode == 'manual'",
               selectizeInput("manual_drug_list", "Manually Select Drugs:",
                              choices = unique(meta_table$Drug_first),
                              multiple = TRUE,
                              options = list(placeholder = 'Type to search drugs'))
             )
         ),
         
         box(width = 12,
             title = "Feature Selection",
             solidHeader = TRUE,
             status = "primary",
             radioButtons("feature_mode", "Select analysis type:",
                          choices = c("Analyze by Function" = "function",
                                      "Analyze by Taxonomy" = "taxon"),
                          selected = "function", inline = TRUE),
             # conditional UI: choose function_class or taxon_class
             conditionalPanel(
               condition = "input.feature_mode == 'function'",
               selectInput("function_class_func", "Choose functional class:", 
                           choices = c("Please choose" = "",
                                       "NOG category" = "NOG_category_name",
                                       "NOG accession" = "NOG_ID",
                                       "NOG name" = "NOG_name",
                                       "KEGG ko" = "KEGG_ko",
                                       "KEGG symbol" = "KEGG_symbol",
                                       "KEGG name" = "KEGG_name",
                                       "EC number" = "EC_number"))
             ),
             
             conditionalPanel(
               condition = "input.feature_mode == 'taxon'",
               selectInput("function_class_tax", "Choose taxonomic class:", 
                           choices = c("Please choose" = "",
                                       "Phylum" = "Phylum",
                                       "Class" = "Class",
                                       "Order" = "Order",
                                       "Family" = "Family",
                                       "Genus" = "Genus",
                                       "Species" = "Species"
                                       ))
             ),
             
             conditionalPanel(
               condition = "(input.feature_mode == 'function' && input.function_class_func) || 
               (input.feature_mode == 'taxon' && input.function_class_tax)",
               selectizeInput("selectFunction", label = "", choices = NULL)
             )
             
         ),
         
         # Boxplot Settings Panel (only show when box or all is selected)
         conditionalPanel(
           condition = "input.select_plot_view == 'box' ",
           box(width = 12,
               title = "Box plot settings",
               solidHeader = TRUE,
               status = "info",
               textInput("plot_title", "Add a plot title", value = "Drug vs control", placeholder = "Type your plot title here"),
               checkboxInput("size_cus", "Customize sizes?", FALSE),
               conditionalPanel(condition = "input.size_cus == 1",
                                sliderInput("plot_w", "Adjust plot width", min = 300, max = 1600, value = 800, step = 10),
                                sliderInput("plot_h","Adjust plot height", 500, min=200, max=1000, step = 10),
                                column(6,
                                       sliderInput("jitter_size","Jitter size", 1.5, min = 0.5, max = 2, step = 0.1),
                                       sliderInput("star_size","Asterisk size", 5, min = 2, max = 10),
                                       sliderInput("h_line_size","Horizontal line size", 0.2, min = 0.1, max = 1)
                                ),
                                column(6,
                                       sliderInput("title_size","Plot title size", 15, min = 5, max = 25),
                                       sliderInput("axis_size","Axis title size", 12, min = 5, max = 20),
                                       sliderInput("label_size","Axis label size", 11, min = 5, max = 20)
                                )
               ),
               checkboxInput("color_cus", "Customize colors?", FALSE),
               conditionalPanel(condition = "input.color_cus == 1",
                                column(6,
                                       colourpicker::colourInput("box_increased", "Fill color - sig. increased", "#ffb07b"),
                                       colourpicker::colourInput("box_decreased", "Fill color - sig. decreased", "#558fff"),
                                       colourpicker::colourInput("box_ns", "Fill color - not significant", "#cccccc"),
                                       colourpicker::colourInput("h_line", "Horizontal line color", "red")
                                ),
                                column(6,
                                       colourpicker::colourInput("line_increased", "Line color - sig. increased", "#ff6700"),
                                       colourpicker::colourInput("line_decreased", "Line color - sig. decreased", "#3300ff"),
                                       colourpicker::colourInput("line_ns", "Line color - not significant", "gray50"),
                                       colourpicker::colourInput("star_color", "Asterisk color", "red")
                                )
               )
           )
         ),
         
         # Volcano Plot Settings Panel (only show when volcano or all is selected)
         conditionalPanel(
           condition = "input.select_plot_view == 'volcano' ",
           box(width = 12,
               title = "Volcano Plot Settings",
               solidHeader = TRUE,
               status = "warning",
               checkboxInput("volcano_custom", "Customize volcano plot appearance?", value = FALSE),
               checkboxInput("volcano_show_labels", "Show drug names on plot?", value = FALSE),
               conditionalPanel(
                 condition = "input.volcano_custom == true",
                 sliderInput("volcano_width", "Volcano plot width", min = 300, max = 1600, value = 1000, step = 10),
                 sliderInput("volcano_height", "Volcano plot height", min = 300, max = 1000, value = 460),
                 sliderInput("volcano_point_size", "Point size", min = 1, max = 5, value = 3, step = 0.1),
                 sliderInput("volcano_label_size", "Drug label size", min = 6, max = 20, value = 15),
                 sliderInput("volcano_line_size", "Line width (dashed)", min = 0.2, max = 2, value = 0.5, step = 0.1),
                 sliderInput("volcano_title_size", "Title size", min = 8, max = 25, value = 20),
                 sliderInput("volcano_axis_size", "Axis title size", min = 8, max = 20, value = 15),
                 sliderInput("volcano_label_axis_size", "Axis label size", min = 8, max = 20, value = 12)
               )
           )
         )
         
  ),
  column(8,
         # Plot View Selector
         box(width = 12,
             title = "Select Visualization",
             status = "primary",
             solidHeader = TRUE,
             radioButtons("select_plot_view", "Choose plot to show:",
                          choices = c("Boxplot" = "box",
                                      "Volcano" = "volcano",
                                      "Heatmap" = "heatmap"),
                          selected = "box",
                          inline = TRUE)
         ),
         
         # --- Boxplot Panel ---
         conditionalPanel(
           condition = "input.select_plot_view == 'box' ",
           box(width = 12,
               title = "Boxplot of Drug Response",
               solidHeader = TRUE,
               status = "info",
               plotlyOutput("Box_plot_out", width = "100%", height = "auto")
           )
         ),
         
         # --- Volcano Plot Panel ---
         conditionalPanel(
           condition = "input.select_plot_view == 'volcano' ",
           box(width = 12,
               title = "Volcano Plot of Functional Response",
               solidHeader = TRUE,
               status = "warning",
               plotOutput("volcanoPlot", width = "100%", height = "auto")
           )
         ),
         
         # --- Heatmap Panel ---
         conditionalPanel(
           condition = "input.select_plot_view == 'heatmap' ",
           fluidRow(
               box(
                 width = 12, 
                 title = "Heatmap of Fold Change (Individuals × Drugs)",
                 solidHeader = TRUE,
                 status = "danger",
                 shinycssloaders::withSpinner(
                   imageOutput("heatmapPlot", width = "100%", height = "400px"),
                   type = 4, color = "#2c3e50", size = 1
                 )
             )
           )
         )
  )
  

)



