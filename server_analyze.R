library(dplyr)
library(ggpubr)
library(stringr)
library(scales)
library(pheatmap)
library(reshape2)
library(grid)

observe({
  isolate({
    if (input$feature_mode == "function" && input$function_class_func != "") {
      fc <- input$function_class_func
      if (fc == "NOG_category_name") {
        nog_choices <- unique(data_table$NOG_category_name)
        sorted <- sort(nog_choices[nog_choices != "Unassigned"])
        final_choices <- c(sorted, "Unassigned")
        updateSelectizeInput(session, 'selectFunction',
                             choices = final_choices,
                             label = "Select a NOG category (single selection)",
                             options = list(maxItems = 1, placeholder = 'Select a NOG category'),
                             server = TRUE)
      } else if (fc %in% names(data_table)) {
        updateSelectizeInput(session, 'selectFunction',
                             choices = unique(data_table[[fc]]),
                             label = paste("Select", fc),
                             options = list(maxItems = 20, placeholder = 'Type to search'),
                             server = TRUE)
      }
    } else if (input$feature_mode == "taxon" && input$function_class_tax != "") {
      tc <- input$function_class_tax
      if (tc %in% names(data_table)) {
        label_text <- paste("Select", tc)
        max_items <- if (tc %in% c("Phylum", "Class")) 1 else 20
        updateSelectizeInput(session, 'selectFunction',
                             choices = unique(data_table[[tc]]),
                             label = label_text,
                             options = list(maxItems = max_items, placeholder = paste("Type to search", tc)),
                             server = TRUE)
      }
    }
  })
})


observe({
  if (input$feature_mode == "function" && input$function_class_func != "") {
    # 功能类初始化（已写过）
  } else if (input$feature_mode == "taxon" && input$function_class_tax != "") {
    default_tax_class <- input$function_class_tax
    label_text <- paste("Select", default_tax_class)
    max_items <- if (default_tax_class %in% c("Phylum", "Class")) 1 else 20
    
    updateSelectizeInput(session, 'selectFunction',
                         choices = unique(data_table[[default_tax_class]]),
                         label = label_text,
                         options = list(maxItems = max_items, placeholder = paste("Type to search", default_tax_class)),
                         server = TRUE)
  }
})


# respond to feature_mode + function_class changes

observeEvent(input$function_class_func, {
  req(input$feature_mode == "function")
  req(input$function_class_func != "")
  
  fc <- input$function_class_func
  
  if (fc == "NOG_category_name") {
    nog_choices <- unique(data_table$NOG_category_name)
    sorted <- sort(nog_choices[nog_choices != "Unassigned"])
    final_choices <- c(sorted, "Unassigned")
    updateSelectizeInput(session, 'selectFunction',
                         choices = final_choices,
                         label = "Select a NOG category (single selection)",
                         options = list(maxItems = 1, placeholder = 'Select a NOG category'),
                         server = TRUE)
  } else if (fc == "NOG_ID") {
    updateSelectizeInput(session, 'selectFunction',
                         choices = unique(data_table$NOG_ID),
                         label = "Type NOG accession numbers to search and select",
                         options = list(maxItems = 20, placeholder = 'Type to search'),
                         server = TRUE)
  } else if (fc == "NOG_name") {
    updateSelectizeInput(session, 'selectFunction',
                         choices = unique(data_table$NOG_name),
                         label = "Type NOG names to search and select",
                         options = list(maxItems = 20, placeholder = 'Type to search'),
                         server = TRUE)
  } else if (fc == "KEGG_ko") {
    updateSelectizeInput(session, 'selectFunction',
                         choices = unique(data_table$KEGG_ko),
                         label = "Select a KEGG KO",
                         options = list(maxItems = 20, placeholder = 'Type to search'),
                         server = TRUE)
  } else if (fc == "KEGG_symbol") {
    updateSelectizeInput(session, 'selectFunction',
                         choices = unique(data_table$KEGG_symbol),
                         label = "Select a KEGG symbol",
                         options = list(maxItems = 20, placeholder = 'Type to search'),
                         server = TRUE)
  } else if (fc == "KEGG_name") {
    updateSelectizeInput(session, 'selectFunction',
                         choices = unique(data_table$KEGG_name),
                         label = "Select a KEGG name",
                         options = list(maxItems = 20, placeholder = 'Type to search'),
                         server = TRUE)
  } else if (fc == "EC_number") {
    updateSelectizeInput(session, 'selectFunction',
                         choices = unique(data_table$EC_number),
                         label = "Select an EC number",
                         options = list(maxItems = 20, placeholder = 'Type to search'),
                         server = TRUE)
    updateSelectizeInput(session, 'selectFunction',
                         choices = unique(data_table$NOG_ID),
                         label = "Type NOG accession numbers to search and select",
                         options = list(maxItems = 20, placeholder = 'Type to search'),
                         server = TRUE)
  }
})


observeEvent(input$function_class_tax, {
  req(input$feature_mode == "taxon")
  req(input$function_class_tax != "")
  
  class_name <- input$function_class_tax
  
  label_text <- paste("Select", class_name)
  placeholder_text <- paste("Type to search", class_name)
  max_items <- if (class_name %in% c("Phylum", "Class")) 1 else 20
  
  if (!is.null(data_table[[class_name]])) {
    updateSelectizeInput(session, 'selectFunction',
                         choices = unique(data_table[[class_name]]),
                         label = label_text,
                         options = list(
                           maxItems = max_items,
                           placeholder = placeholder_text  # placeholder
                         ),
                         server = TRUE
    )
  }
})


#Update Selectize input according to drug class
observe({
  updateSelectInput(session, "selectLevel_1", 
                    choices = unique(sort(str_trim(meta_table$Level_1[!str_detect(meta_table$Level_1, "^(NA|DMSO|Kestose)$") & !is.na(meta_table$Level_1)]))),
                    selected = "A")
})

meta_filtered <- reactive({
  req(meta_table)
  if (input$drug_selection_mode == "by_class") {
    req(input$selectLevel_1)
    na.omit(meta_table[meta_table$Level_1 == input$selectLevel_1 | meta_table$Drug_first == "DMSO", ])
  } else if (input$drug_selection_mode == "manual") {
    req(input$manual_drug_list)
    na.omit(meta_table[meta_table$Drug_first %in% input$manual_drug_list | meta_table$Drug_first == "DMSO", ])
  }
})

# filter data according to user selection of function or taxon
data_filtered <- reactive({
  #  feature_mode selection of function or taxon
  feature_class <- if (input$feature_mode == "function") input$function_class_func else input$function_class_tax
  
  req(feature_class, input$selectFunction)
  
  if (input$drug_selection_mode == "by_class") {
    req(input$selectLevel_1)
  }
  
  # Step 1: filter data_table feature columns
  filtered_data <- data_table %>%
    filter(str_detect(!!sym(feature_class), paste(input$selectFunction, collapse = "|"))) %>%
    select(16:711) %>%
    mutate(across(everything(), as.numeric))
  
  # Step 2: obrain meta_filtered()  Sample column
  filtered_samples <- meta_filtered()$Sample
  
  # Step 3: keep filtered_data matches to filtered_samples 
  filtered_data <- filtered_data[, colnames(filtered_data) %in% filtered_samples]
  
  return(filtered_data)
})

# sum
data_filtered_sum <- reactive({
  data_numeric <- data_filtered()[, sapply(data_filtered(), is.numeric)]
  as.matrix(colSums(data_numeric, na.rm = TRUE))
})

# combine meta with data_filtered_sum 
data_fc <- reactive({
  data_fc <- cbind(meta_filtered(), data_filtered_sum())
  names(data_fc)[names(data_fc) == 'data_filtered_sum'] <- 'data_filtered_sum'
  return(data_fc)
})


# # Get average value for each group
# data_fc_average <- suppressWarnings(aggregate(data_fc, by = list(data_fc$Individual, data_fc$Level_1), FUN=mean))

data_fc_average <- reactive({suppressWarnings(aggregate(data_fc(), by = list(data_fc()$Individual, data_fc()$Drug_first), FUN=mean))})

# # Calculate fold change for each volunteer
# data_fc_V1 <- data_fc_average %>% filter(Group.1 == "V45")
# data_dmso_V1 <- data_fc_V1 %>% filter(Group.2 == "DMSO") %>% select(23) %>% as.numeric()
# data_filtered_fc <- data_fc_V1$data_filtered_sum/data_dmso_V1
# data_fc_V1_calculated <- cbind(data_fc_V1, data_filtered_fc)
data_fc_V1 <- reactive({data_fc_average() %>% filter(Group.1 == "V45")})
data_dmso_V1 <- reactive({data_fc_V1() %>% filter(Group.2 == "DMSO") %>% select(23) %>% as.numeric()})
data_fc_V1_calculated <- reactive({
  data_filtered_fc <- data_fc_V1()$data_filtered_sum/data_dmso_V1()
  data_fc_V1_calculated <- cbind(data_fc_V1(), data_filtered_fc)
  return(data_fc_V1_calculated)
})

data_fc_V2 <- reactive({data_fc_average() %>% filter(Group.1 == "V46")})
data_dmso_V2 <- reactive({data_fc_V2() %>% filter(Group.2 == "DMSO") %>% select(23) %>% as.numeric()})
data_fc_V2_calculated <- reactive({
  data_filtered_fc <- data_fc_V2()$data_filtered_sum/data_dmso_V2()
  data_fc_V2_calculated <- cbind(data_fc_V2(), data_filtered_fc)
  return(data_fc_V2_calculated)
})

data_fc_V3 <- reactive({data_fc_average() %>% filter(Group.1 == "V47")})
data_dmso_V3 <- reactive({data_fc_V3() %>% filter(Group.2 == "DMSO") %>% select(23) %>% as.numeric()})
data_fc_V3_calculated <- reactive({
  data_filtered_fc <- data_fc_V3()$data_filtered_sum/data_dmso_V3()
  data_fc_V3_calculated <- cbind(data_fc_V3(), data_filtered_fc)
  return(data_fc_V3_calculated)
})

data_fc_V4 <- reactive({data_fc_average() %>% filter(Group.1 == "V48")})
data_dmso_V4 <- reactive({data_fc_V4() %>% filter(Group.2 == "DMSO") %>% select(23) %>% as.numeric()})
data_fc_V4_calculated <- reactive({
  data_filtered_fc <- data_fc_V4()$data_filtered_sum/data_dmso_V4()
  data_fc_V4_calculated <- cbind(data_fc_V4(), data_filtered_fc)
  return(data_fc_V4_calculated)
})

data_fc_V5 <- reactive({data_fc_average() %>% filter(Group.1 == "V49")})
data_dmso_V5 <- reactive({data_fc_V5() %>% filter(Group.2 == "DMSO") %>% select(23) %>% as.numeric()})
data_fc_V5_calculated <- reactive({
  data_filtered_fc <- data_fc_V5()$data_filtered_sum/data_dmso_V5()
  data_fc_V5_calculated <- cbind(data_fc_V5(), data_filtered_fc)
  return(data_fc_V5_calculated)
})

data_fc_V6 <- reactive({data_fc_average() %>% filter(Group.1 == "V52")})
data_dmso_V6 <- reactive({data_fc_V6() %>% filter(Group.2 == "DMSO") %>% select(23) %>% as.numeric()})
data_fc_V6_calculated <- reactive({
  data_filtered_fc <- data_fc_V6()$data_filtered_sum/data_dmso_V6()
  data_fc_V6_calculated <- cbind(data_fc_V6(), data_filtered_fc)
  return(data_fc_V6_calculated)
})




# Combine data for plot
data_plot <- reactive({
  rbind(data_fc_V1_calculated(),data_fc_V2_calculated(), data_fc_V3_calculated(), 
        data_fc_V4_calculated(), data_fc_V5_calculated(), data_fc_V6_calculated()) %>% filter(Group.2 != "DMSO" & Group.2 != "Kestose" )
})



# Calculate significance of change using Wilcoxon test
data_plot_greater <- reactive({
  data_plot() %>%
    dplyr::group_by(Group.2) %>%
    dplyr::summarise(
      Wil = if (sum(!is.na(data_filtered_fc)) > 1) {
        wilcox.test(data_filtered_fc, mu = 1, alternative = "greater")$p.value
      } else {
        NA  # Return NA if there are not enough observations
      }
    )
})

data_plot_less <- reactive({
  data_plot() %>%
    dplyr::group_by(Group.2) %>%
    dplyr::summarise(
      Wil = if (sum(!is.na(data_filtered_fc)) > 1) {
        wilcox.test(data_filtered_fc, mu = 1, alternative = "less")$p.value
      } else {
        NA  # Return NA if there are not enough observations
      }
    )
})
data_sig <- reactive({
  data_sig <- cbind(data_plot_greater(),data_plot_less()[,2])
  row.names(data_sig) <- data_sig[,1]
  data_sig <- data_sig[,-1]
  colnames(data_sig) <- c("greater", "less")
  return(data_sig)
})


# Assign color to significantly changed drug responses
# set color for boxes
my_color <- reactive({
  my_color <- data_sig()[,-1]
  my_color[] <- input$box_ns
  my_color[data_sig()$greater<=0.05] = input$box_increased
  my_color[data_sig()$less<=0.05] = input$box_decreased
  return(my_color)
})
# set darker color for lines around boxex
my_color_line <- reactive({
  my_color_line <- data_sig()[,-1]
  my_color_line[] <- input$line_ns
  my_color_line[data_sig()$greater<=0.05] = input$line_increased
  my_color_line[data_sig()$less<=0.05] = input$line_decreased
  return(my_color_line)
})

# Star sign - significance to appear on plot
my_stat <- reactive({
  my_stat <- data_sig()[,-1]
  my_stat[] <- c("")
  my_stat[data_sig()$greater<=0.05] = c("*")
  my_stat[data_sig()$less<=0.05] = c("*")
  return(my_stat)
})



# Star sign location on the plot -- slightly above highest value
data_plot_sign <- reactive({
  data_plot_sign <- data_plot() %>% dplyr::group_by(Group.2) %>% summarise(max = (max(data_filtered_fc)+0.1))
  data_plot_sign <- as.numeric(data_plot_sign$max)
  return(data_plot_sign)
})


# ### debug
# output$debug <- renderTable({
#   data_plot_sign()
#   # paste0("data_table$",input$function_class)
#   # paste(input$selectFunction, collapse = "|")
#   # paste0(input$selectFunction)
#   # data_plot_sign()
# }, rownames = TRUE)
# ### debug ends
# # 


output$Box_plot_out <- renderPlotly({
  tryCatch({
    p <- ggplot(data_plot(), aes(x = Group.2, y = data_filtered_fc)) +
      geom_boxplot(outlier.colour = "gray", outlier.shape = 1, outlier.size = 1.0,
                   fill = my_color(), color = my_color_line()) +
      geom_jitter(shape = 16, position = position_jitter(0.2), color = "gray30", size = input$jitter_size) +
      geom_hline(yintercept = 1, colour = input$h_line, size = input$h_line_size, linetype = "dashed") +
      annotate("text",
               x = 1:length(data_plot_sign()),
               y = data_plot_sign(),
               label = my_stat(),
               size = input$star_size,
               color = input$star_color) +
      labs(title = input$plot_title, x = "Drugs", y = "Fold change over control") +
      scale_y_log10() +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1, size = input$label_size),
        axis.title = element_text(size = input$axis_size, face = "bold"),
        plot.title = element_text(size = input$title_size, face = "bold")
      )
    
    ggplotly(p, width = input$plot_w, height = input$plot_h)
  })
})


#####newly added features
volcano_data <- reactive({
  df <- data_plot() %>%
    dplyr::group_by(Group.2) %>%
    dplyr::summarise(
      mean_fc = mean(data_filtered_fc, na.rm = TRUE),
      log2FC = log2(mean_fc)
    )
  
  df$greater_p <- data_sig()$greater[match(df$Group.2, rownames(data_sig()))]
  df$less_p <- data_sig()$less[match(df$Group.2, rownames(data_sig()))]
  df$pvalue <- pmin(df$greater_p, df$less_p, na.rm = TRUE)
  df$neglog10p <- -log10(df$pvalue)
  
  df$Significance <- "Not significant"
  df$Significance[df$greater_p <= 0.05] <- "Up"
  df$Significance[df$less_p <= 0.05] <- "Down"
  
  return(df)
})

output$volcanoPlot <- renderPlot({
  tryCatch({
    df <- volcano_data()
    req(nrow(df) > 0)
    
    df$Significance <- factor(df$Significance, levels = c("Up", "Down", "Not significant"))
    
    p <- ggplot(df, aes(x = log2FC, y = neglog10p)) +
      geom_point(aes(color = Significance), size = input$volcano_point_size) +
      scale_color_manual(values = c("Up" = "red", "Down" = "blue", "Not significant" = "grey")) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = input$volcano_line_size) +
      geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "black", size = input$volcano_line_size) +
      labs(
        title = "Volcano plot of drug responses",
        x = expression(log[2]*" Fold Change"),
        y = expression(-log[10]*"(p-value)")
      ) +
      theme_bw() +
      theme(
        legend.title = element_blank(),
        plot.title = element_text(size = input$volcano_title_size, face = "bold"),
        axis.title = element_text(size = input$volcano_axis_size),
        axis.text = element_text(size = input$volcano_label_axis_size)
      )
    
    if (input$volcano_show_labels) {
      p <- p + ggrepel::geom_text_repel(
        aes(label = Group.2),
        size = input$volcano_label_size / 3,
        max.overlaps = 30,
        box.padding = 0.4,
        point.padding = 0.3,
        show.legend = FALSE
      )
    }
    
    p
  })
}, width = function() input$volcano_width,
height = function() input$volcano_height)



#####heatmap

heatmap_data <- reactive({
  df <- data_plot()
  
  #  Individual as rows，Drugs as columns
  mat <- reshape2::dcast(df, Group.1 ~ Group.2, value.var = "data_filtered_fc")
  
  # remove first row (Group.1，sample names), keep rownames
  rownames(mat) <- mat$Group.1
  mat <- as.matrix(mat[ , -1])
  
  # calculate log2 fold change 
  mat <- log2(mat)
  
  return(mat)
})


output$heatmapPlot <- renderPlot({
  df <- data_plot()
  df <- as.data.frame(df)
  
  req(nrow(df) > 0)
  req(all(c("Group.1", "Group.2", "data_filtered_fc") %in% colnames(df)))
  
  #  matrix: Individual × Drug
  heatmap_matrix <- reshape2::dcast(df, Group.1 ~ Group.2, value.var = "data_filtered_fc")
  rownames(heatmap_matrix) <- heatmap_matrix$Group.1
  heatmap_matrix <- heatmap_matrix[, -1]
  
  # log2 transform
  log_matrix <- log2(as.matrix(heatmap_matrix))
  log_matrix[!is.finite(log_matrix)] <- NA  #  NA，shown as heatmap empty grid
  
  # change to tidy long format for ggplot
  df_long <- reshape2::melt(log_matrix, varnames = c("Individual", "Drug"), value.name = "log2FC")
  
  ggplot(df_long, aes(x = Drug, y = Individual, fill = log2FC)) +
    geom_tile(color = "white", size = 0.2) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                         midpoint = 0, na.value = "grey80", name = "log2 FC") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(size = 14, face = "bold")) +
    labs(title = "Heatmap of log2 Fold Change (Individual × Drug)",
         x = "Drug", y = "Individual")
})



output$heatmapPlot <- renderImage({
  df <- data_plot()
  df <- as.data.frame(df)
  
  req(nrow(df) > 0)
  req(all(c("Group.1", "Group.2", "data_filtered_fc") %in% colnames(df)))
  
  # Individual × Drug fold change matrix
  heatmap_matrix <- reshape2::dcast(df, Group.1 ~ Group.2, value.var = "data_filtered_fc")
  rownames(heatmap_matrix) <- heatmap_matrix$Group.1
  heatmap_matrix <- heatmap_matrix[, -1]
  
  # log2 transform
  log_matrix <- log2(as.matrix(heatmap_matrix))
  log_matrix[!is.finite(log_matrix)] <- NA
  
  my_palette <- colorRampPalette(c("blue", "white", "red"))(100)
  
  # Save to temporary file
  outfile <- tempfile(fileext = '.png')
  png(outfile, width = 800, height = 400, res = 120)
  pheatmap::pheatmap(
    mat = log_matrix,
    cluster_rows = TRUE,
    cluster_cols = TRUE,
    fontsize_row = 10,
    fontsize_col = 10,
    main = "Log2 Fold Change per Individual per Drug",
    color = my_palette,
    na_col = "grey80",
    border_color = "black"
  )
  dev.off()
  
  list(
    src = outfile,
    contentType = 'image/png',
    width = 800,
    height = 400,
    alt = "Heatmap"
  )
}, deleteFile = TRUE)
