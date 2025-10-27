## This module contains the UI and server code for the Rating Curves tool

## Create module UI function ######################################################

ratingCurvesToolUI <- function(id, pool, ...) {
# Create the UI for the ratingCurvesTool module
# Parameters:
#  - id: String, the module id
#  - pool: The pool connection to the database
# 
# Returns a div containing the layout
  
  # Create namespace
  ns <- NS(id)
  
  # Create layout with vertical panels
  div(
    class = 'rating-curves-tool tools-layout',
    
    # Panel 1: Site selection and data preview
    div(
      class = 'panel panel-default',
      style = 'margin-bottom: 30px;',
      div(
        class = 'panel-heading',
        h4('1. Select Site and View Data', class = 'panel-title')
      ),
      div(
        class = 'panel-body',
        fluidRow(
          column(4,
            # Station selection
            selectInput(
              ns('site'),
              'Station',
              choices = c(
                'Choose a station ...' = '',
                parseOptions(
                  getRows(pool, 'stations', columns = c('order', 'name')) %>%
                  arrange(order) %>% select(-order),
                  'name'
                )
              )
            )
          ),
          column(4,
            # Date range selection
            div(
              h5('Date Range', style = 'margin-bottom: 10px;'),
              dateRangeInput(
                ns('dateRange'),
                label = NULL,
                start = Sys.Date() - 365,
                end = Sys.Date(),
                format = 'yyyy-mm-dd'
              )
            )
          ),
          column(4,
            # Data summary display
            div(
              class = 'data-summary',
              h5('Data Summary', style = 'margin-bottom: 10px; color: #6c757d;'),
              div(
                class = 'summary-display',
                style = 'background-color: #f8f9fa; padding: 15px; border-radius: 5px; border: 1px solid #dee2e6;',
                div(
                  style = 'font-size: 14px; color: #495057;',
                  uiOutput(ns('dataSummary'))
                )
              )
            )
          )
        )
      )
    ),
    
    # Panel 2: Rating Curve Display
    div(
      class = 'panel panel-default',
      style = 'margin-bottom: 30px;',
      div(
        class = 'panel-heading',
        div(
          class = 'panel-title-with-button',
          h4('2. Rating Curve Visualization', class = 'panel-title', style = 'display: inline-block; margin: 0;'),
          actionButton(ns('generatePlot'), 'Generate Plot', 
                      class = 'btn btn-primary btn-lg',
                      style = 'float: right;')
        )
      ),
      div(
        class = 'panel-body',
        div(
          class = 'plot-container',
          fluidRow(
            column(12,
              div(
                class = 'rating-curve-plot',
                shinycssloaders::withSpinner(
                  plotOutput(ns('ratingCurvePlot'), height = '500px'),
                  type = 1,
                  color = '#0d6efd'
                )
              )
            )
          )
        )
      )
    ),
    
    # Panel 3: Data Table
    div(
      class = 'panel panel-default',
      style = 'margin-bottom: 30px;',
      div(
        class = 'panel-heading',
        h4('3. Data Table', class = 'panel-title')
      ),
      div(
        class = 'panel-body',
        div(
          class = 'data-table-container',
          DT::dataTableOutput(ns('dataTable'))
        )
      )
    ),
    
    # Add custom CSS for better styling
    tags$style(HTML("
      .rating-curves-tool {
        display: flex;
        flex-direction: column;
        width: 100%;
      }
      
      .rating-curves-tool .panel {
        box-shadow: 0 2px 8px rgba(0,0,0,0.1);
        border: 1px solid #ddd;
        display: block;
        width: 100%;
        clear: both;
        margin-bottom: 30px;
        flex-shrink: 0;
      }
      
      .rating-curves-tool .panel-heading {
        background-color: #f8f9fa;
        border-bottom: 1px solid #dee2e6;
      }
      
      .rating-curves-tool .panel-title {
        color: #495057;
        font-weight: 600;
      }
      
      .rating-curves-tool .data-summary {
        background-color: #f8f9fa;
        padding: 15px;
        border-radius: 5px;
        height: 100%;
      }
      
      .rating-curves-tool .plot-container {
        background-color: #f8f9fa;
        padding: 15px;
        border-radius: 5px;
        min-height: 500px;
      }
      
      .rating-curves-tool .rating-curve-plot {
        background-color: white;
        border-radius: 5px;
        padding: 10px;
      }
      
      .rating-curves-tool .data-table-container {
        background-color: #f8f9fa;
        padding: 15px;
        border-radius: 5px;
      }
      
      .rating-curves-tool .btn-lg {
        font-size: 16px;
        padding: 12px 20px;
      }
      
      .rating-curves-tool .panel-body {
        padding: 20px;
      }
      
      .rating-curves-tool .panel-title-with-button {
        width: 100%;
        overflow: hidden;
      }
      
      .rating-curves-tool .summary-display {
        min-height: 80px;
        display: flex;
        align-items: center;
        justify-content: center;
      }
      
      /* Ensure no floating or side-by-side panels */
      .rating-curves-tool .row {
        margin-left: 0;
        margin-right: 0;
      }
    "))
  )
}

## Create module server function ##################################################

ratingCurvesTool <- function(input, output, session, pool, ...) {
# Create the logic for the ratingCurvesTool module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - pool: The pool connection to the database
# 
# Returns a reactive expression containing the results
  
  ## Reactive values ##############################################################
  
  values <- reactiveValues(
    data = NULL,
    filteredData = NULL,
    plot = NULL
  )
  
  ## Data loading logic ###########################################################
  
  # Update data when site or date range changes
  observeEvent(c(input$site, input$dateRange), {
    req(input$site, input$dateRange)
    
    if (input$site == '') {
      values$data <- NULL
      values$filteredData <- NULL
      return()
    }
    
    tryCatch({
      # Get data for the selected station and date range
      selected_site <- input$site
      start_date <- as.character(input$dateRange[1])
      end_date <- as.character(input$dateRange[2])
      
      # Query for discharge (Q_Ls) and water level data
      # Assuming water level might be stored in columns like 'WaterLevel_m' or similar
      # For now, we'll focus on Q_Ls and any available level measurements
      data <- getRows(pool, 'data',
                     station == selected_site,
                     DATE_reading >= start_date,
                     DATE_reading <= end_date,
                     columns = c('DATE_reading', 'Q_Ls', 'WaterLevel_m', 'Stage_m', 'Depth_m')) %>%
        filter(!is.na(Q_Ls)) %>%  # Only include records with discharge data
        arrange(DATE_reading)
      
      # Create a water level column from available data
      # Priority: WaterLevel_m > Stage_m > Depth_m
      if ('WaterLevel_m' %in% names(data) && any(!is.na(data$WaterLevel_m))) {
        data$WaterLevel <- data$WaterLevel_m
        data$LevelType <- 'Water Level (m)'
      } else if ('Stage_m' %in% names(data) && any(!is.na(data$Stage_m))) {
        data$WaterLevel <- data$Stage_m
        data$LevelType <- 'Stage (m)'
      } else if ('Depth_m' %in% names(data) && any(!is.na(data$Depth_m))) {
        data$WaterLevel <- data$Depth_m
        data$LevelType <- 'Depth (m)'
      } else {
        data$WaterLevel <- NA
        data$LevelType <- 'No Level Data'
      }
      
      # Filter out records without both discharge and level data
      filtered_data <- data %>%
        filter(!is.na(Q_Ls), !is.na(WaterLevel)) %>%
        select(DATE_reading, Q_Ls, WaterLevel, LevelType)
      
      values$data <- data
      values$filteredData <- filtered_data
      
    }, error = function(e) {
      showNotification(paste("Error loading data:", e$message), type = "error")
      values$data <- NULL
      values$filteredData <- NULL
    })
  })
  
  ## Data summary output ##########################################################
  
  output$dataSummary <- renderUI({
    if (is.null(values$filteredData) || nrow(values$filteredData) == 0) {
      if (input$site == '') {
        return(div("Select a station to view data"))
      } else {
        return(div("No data available for selected period", style = "color: #dc3545;"))
      }
    }
    
    data <- values$filteredData
    n_points <- nrow(data)
    date_range <- range(data$DATE_reading)
    q_range <- range(data$Q_Ls, na.rm = TRUE)
    level_range <- range(data$WaterLevel, na.rm = TRUE)
    level_type <- unique(data$LevelType)[1]
    
    div(
      div(strong("Data Points: "), n_points),
      div(strong("Date Range: "), format(date_range[1], "%Y-%m-%d"), " to ", format(date_range[2], "%Y-%m-%d")),
      div(strong("Discharge Range: "), round(q_range[1], 3), " - ", round(q_range[2], 3), " L/s"),
      div(strong("Level Range: "), round(level_range[1], 3), " - ", round(level_range[2], 3), " m"),
      div(strong("Level Type: "), level_type)
    )
  })
  
  ## Generate rating curve plot ###################################################
  
  observeEvent(input$generatePlot, {
    req(values$filteredData)
    
    if (nrow(values$filteredData) == 0) {
      showNotification("No data available to plot", type = "warning")
      return()
    }
    
    tryCatch({
      data <- values$filteredData
      level_type <- unique(data$LevelType)[1]
      
      # Create the rating curve plot
      p <- ggplot(data, aes(x = WaterLevel, y = Q_Ls)) +
        geom_point(color = "#0d6efd", size = 2, alpha = 0.7) +
        geom_smooth(method = "loess", se = TRUE, color = "#dc3545", fill = "#dc3545", alpha = 0.2) +
        labs(
          title = paste("Rating Curve for", input$site),
          subtitle = paste("Data from", min(data$DATE_reading), "to", max(data$DATE_reading)),
          x = level_type,
          y = "Discharge (L/s)"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold", color = "#495057"),
          plot.subtitle = element_text(size = 12, color = "#6c757d"),
          axis.title = element_text(size = 12, face = "bold"),
          axis.text = element_text(size = 10),
          panel.grid.major = element_line(color = "#dee2e6", size = 0.5),
          panel.grid.minor = element_line(color = "#f8f9fa", size = 0.25),
          panel.background = element_rect(fill = "white", color = NA)
        )
      
      values$plot <- p
      showNotification("Rating curve plot generated successfully!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error generating plot:", e$message), type = "error")
    })
  })
  
  ## Plot output ##################################################################
  
  output$ratingCurvePlot <- renderPlot({
    if (is.null(values$plot)) {
      # Show placeholder when no plot is available
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                label = "Select a station and click 'Generate Plot' to view rating curve", 
                size = 6, color = "#6c757d") +
        theme_void() +
        theme(panel.background = element_rect(fill = "white", color = NA))
    } else {
      values$plot
    }
  })
  
  ## Data table output ############################################################
  
  output$dataTable <- DT::renderDataTable({
    if (is.null(values$filteredData) || nrow(values$filteredData) == 0) {
      # Return empty data frame with proper column names
      empty_df <- data.frame(
        Date = character(0),
        Discharge_L_s = numeric(0),
        Water_Level_m = numeric(0),
        Level_Type = character(0)
      )
      return(DT::datatable(empty_df, options = list(dom = 'tip')))
    }
    
    # Prepare data for display
    display_data <- values$filteredData %>%
      mutate(
        Date = as.character(DATE_reading),
        `Discharge (L/s)` = round(Q_Ls, 4),
        `Water Level (m)` = round(WaterLevel, 4),
        `Level Type` = LevelType
      ) %>%
      select(Date, `Discharge (L/s)`, `Water Level (m)`, `Level Type`)
    
    DT::datatable(
      display_data,
      options = list(
        scrollX = TRUE,
        pageLength = 15,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        columnDefs = list(
          list(className = 'dt-center', targets = 1:3)
        )
      ),
      extensions = 'Buttons',
      rownames = FALSE
    )
  })
  
  ## Return reactive ##############################################################
  
  return(reactive({
    list(
      data = values$filteredData,
      plot = values$plot
    )
  }))
}
