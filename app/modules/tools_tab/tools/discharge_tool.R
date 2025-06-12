## This module contains the UI and server code for the Discharge tool

## Create module UI function ######################################################

dischargeToolUI <- function(id, pool, ...) {
# Create the UI for the dischargeTool module
# Parameters:
#  - id: String, the module id
#  - pool: The pool connection to the database
# 
# Returns a div containing the layout
  
  # Create namespace
  ns <- NS(id)
  
  # Create layout with vertical panels
  div(
    class = 'discharge-tool tools-layout',
    
    # Panel 1: File upload and preview
    div(
      class = 'panel panel-default',
      style = 'margin-bottom: 30px;',
      div(
        class = 'panel-heading',
        h4('1. Upload Data File & Select Site/Date', class = 'panel-title')
      ),
      div(
        class = 'panel-body',
        fluidRow(
          column(3,
            # File upload button
            fileInput(ns('dataFile'), 'Choose CSV File',
                     accept = c('.csv'),
                     buttonLabel = 'Browse...',
                     placeholder = 'No file selected'),
            # Reset button
            div(
              style = 'margin-top: 10px;',
              actionButton(ns('reset'), 'Reset Tool', 
                          class = 'btn btn-warning btn-sm',
                          icon = icon('refresh'),
                          style = 'width: 100%;')
            )
          ),
          column(3,
            # Station selection (matching DOC tool pattern)
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
            ),
            # Date selection (matching DOC tool pattern)
            selectInput(
              ns('date'),
              'Date',
              choices = c(
                'Choose a date ...' = ''
              )
            )
          ),
          column(3,
            # Display WTW_Temp_degC_1 parameter
            div(
              class = 'parameter-display',
              h5('Water Temperature', style = 'margin-bottom: 10px; color: #6c757d;'),
              div(
                class = 'temperature-display',
                style = 'background-color: #f8f9fa; padding: 15px; border-radius: 5px; border: 1px solid #dee2e6;',
                div(
                  style = 'font-size: 18px; font-weight: bold; color: #495057;',
                  textOutput(ns('waterTemp'), inline = TRUE)
                ),
                div(
                  style = 'font-size: 12px; color: #6c757d; margin-top: 5px;',
                  'WTW_Temp_degC_1'
                )
              )
            )
          ),
          column(3,
            # Data table preview
            div(
              class = 'file-preview-container',
              DT::dataTableOutput(ns('filePreview'))
            )
          )
        )
      )
    ),
    
    # Panel 2: Parameters
    div(
      class = 'panel panel-default',
      style = 'margin-bottom: 30px;',
      div(
        class = 'panel-heading',
        h4('2. Configure Parameters', class = 'panel-title')
      ),
      div(
        class = 'panel-body',
        fluidRow(
          column(4,
            div(
              class = 'parameter-group',
              h5('Rhodamine Parameters', class = 'parameter-group-title'),
              div(
                class = 'parameter-inputs',
                numericInput(ns('initial_mass_rhodamine_wt'), 'Initial Mass Rhodamine (g)', 
                            value = 3.38019, min = 0, step = 0.01),
                numericInput(ns('concentration_rhodamine_wt'), 'Rhodamine Concentration (%)', 
                            value = 23.83, min = 0, max = 100, step = 0.01),
                numericInput(ns('initial_water_temp_degC'), 'Initial Water Temperature (°C)', 
                            value = 3.3, step = 0.1),
                numericInput(ns('n_rhodamine'), 'Temperature Correction Factor', 
                            value = 0.026, step = 0.001)
              )
            )
          ),
          column(4,
            div(
              class = 'parameter-group',
              h5('Salt Parameters', class = 'parameter-group-title'),
              div(
                class = 'parameter-inputs',
                numericInput(ns('initial_mass_salt'), 'Initial Mass Salt (g)', 
                            value = 2000, min = 0, step = 1),
                numericInput(ns('slope_conductivity'), 'Conductivity Slope', 
                            value = 1951.1, min = 0, step = 0.1)
              )
            )
          ),
          column(4,
            div(
              class = 'parameter-group',
              h5('Additional Parameters', class = 'parameter-group-title'),
              div(
                class = 'parameter-inputs',
                numericInput(ns('distance'), 'Distance (m)', 
                            value = 79, min = 0, step = 1),
                numericInput(ns('T_ref'), 'Reference Temperature (°C)', 
                            value = 25, step = 1)
              )
            )
          )
        )
      )
    ),
    
    # Panel 3: Calculation and Results
    div(
      class = 'panel panel-default',
      style = 'margin-bottom: 30px;',
      div(
        class = 'panel-heading',
        div(
          class = 'panel-title-with-button',
          h4('3. Calculate Discharge', class = 'panel-title', style = 'display: inline-block; margin: 0;'),
          actionButton(ns('calculate'), 'Calculate Discharge', 
                      class = 'btn btn-primary btn-lg',
                      style = 'float: right;')
        )
      ),
      div(
        class = 'panel-body',
        div(
          class = 'results-container',
          fluidRow(
            column(6,
              div(
                class = 'results-text',
                h5('Calculation Results'),
                verbatimTextOutput(ns('results'))
              )
            ),
            column(6,
              div(
                class = 'results-plot',
                plotOutput(ns('plots'), height = '400px')
              )
            )
          )
        )
      )
    ),
    
    # Panel 4: Database Update for Q_Ls parameter
    div(
      class = 'panel panel-default',
      style = 'margin-bottom: 30px;',
      div(
        class = 'panel-heading',
        h4('4. Update Database (Q_Ls Parameter for Selected Site/Date)', class = 'panel-title')
      ),
      div(
        class = 'panel-body',
        fluidRow(
          column(6,
            numericInput(ns('q_ls'), 'Q_Ls (L/s)',
                        value = NULL,
                        min = 0,
                        step = 0.001,
                        width = '100%')
          ),
          column(6,
            div(
              style = 'margin-top: 25px;',
              div(
                class = 'btn-group',
                style = 'width: 100%;',
                actionButton(ns('check'), 'Check', 
                           class = 'btn btn-default',
                           style = 'width: 48%; margin-right: 4%;'),
                actionButton(ns('update'), 'Update', 
                           class = 'btn btn-success',
                           style = 'width: 48%;')
              )
            )
          )
        ),
        fluidRow(
          column(12,
            div(
              class = 'database-feedback',
              style = 'margin-top: 15px;',
              uiOutput(ns('dbFeedback'))
            )
          )
        )
      )
    ),
    
    # Add custom CSS for better styling
    tags$style(HTML("
      .discharge-tool {
        display: flex;
        flex-direction: column;
        width: 100%;
      }
      
      .discharge-tool .panel {
        box-shadow: 0 2px 8px rgba(0,0,0,0.1);
        border: 1px solid #ddd;
        display: block;
        width: 100%;
        clear: both;
        margin-bottom: 30px;
        flex-shrink: 0;
      }
      
      .discharge-tool .panel-heading {
        background-color: #f8f9fa;
        border-bottom: 1px solid #dee2e6;
      }
      
      .discharge-tool .panel-title {
        color: #495057;
        font-weight: 600;
      }
      
      .discharge-tool .parameter-group {
        background-color: #f8f9fa;
        padding: 15px;
        border-radius: 5px;
        height: 100%;
      }
      
      .discharge-tool .parameter-group-title {
        color: #6c757d;
        font-weight: 600;
        margin-bottom: 15px;
        border-bottom: 1px solid #dee2e6;
        padding-bottom: 5px;
      }
      
      .discharge-tool .file-preview-container {
        max-height: 300px;
        overflow-y: auto;
        border: 1px solid #dee2e6;
        border-radius: 5px;
      }
      
      .discharge-tool .results-container {
        margin-top: 10px;
      }
      
      .discharge-tool .results-text {
        background-color: #f8f9fa;
        padding: 15px;
        border-radius: 5px;
        max-height: 400px;
        overflow-y: auto;
      }
      
      .discharge-tool .results-plot {
        background-color: #f8f9fa;
        padding: 10px;
        border-radius: 5px;
      }
      
      .discharge-tool .btn-lg {
        font-size: 16px;
        padding: 12px 20px;
      }
      
      .discharge-tool .panel-body {
        padding: 20px;
      }
      
      .discharge-tool .panel-title-with-button {
        width: 100%;
        overflow: hidden;
      }
      
      .discharge-tool .database-feedback {
        background-color: #f8f9fa;
        padding: 10px;
        border-radius: 5px;
        min-height: 40px;
      }
      
      .discharge-tool .btn-group .btn {
        border-radius: 4px !important;
      }
      
      /* Ensure no floating or side-by-side panels */
      .discharge-tool .row {
        margin-left: 0;
        margin-right: 0;
      }
      
      .discharge-tool .col-md-6,
      .discharge-tool .col-md-4,
      .discharge-tool .col-md-8 {
        float: none !important;
      }
    ")),
    
    # Add JavaScript for console debugging
    tags$script(HTML("
      Shiny.addCustomMessageHandler('console', function(message) {
        console.log('Discharge Tool Debug:', message.message);
      });
    "))
  )
}

## Create module server function ##################################################

dischargeTool <- function(input, output, session, pool, site, datetime, ...) {
# Create the logic for the dischargeTool module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - pool: The pool connection to the database
# 
# Returns a reactive expression containing the results
  
  ## Reactive values ##############################################################
  
  values <- reactiveValues(
    data = NULL,
    results = NULL,
    plots = NULL,
    checked = FALSE,
    dbError = NULL
  )
  
  ## Reset button logic ##########################################################
  
  # Reset tool when reset button is clicked
  observeEvent(input$reset, {
    # Clear reactive values
    values$data <- NULL
    values$results <- NULL
    values$plots <- NULL
    values$checked <- FALSE
    values$dbError <- NULL
    
    # Reset file input (this will trigger file upload logic to clear preview)
    # Note: fileInput cannot be directly reset, but clearing values$data will clear the preview
    
    # Reset site and date selections
    updateSelectInput(session, 'site', selected = '')
    updateSelectInput(session, 'date', choices = c('Choose a date ...' = ''), selected = '')
    
    # Reset Q_Ls input
    updateNumericInput(session, 'q_ls', value = NA)
    
    # Show notification
    showNotification('Discharge tool has been reset. You can now upload a new CSV file.', 
                    type = 'message', duration = 3)
    
    # Add debug message
    session$sendCustomMessage("console", list(message = "Discharge tool reset completed"))
  })
  
  ## File upload handling #########################################################
  
  observeEvent(input$dataFile, {
    req(input$dataFile)
    
    tryCatch({
      # Read the uploaded file
      data <- read.csv(input$dataFile$datapath, stringsAsFactors = FALSE)
      
      # Auto-detect time format and convert
      if ('time' %in% names(data)) {
        # Try different time formats
        time_formats <- c("%H:%M:%S", "%m/%d/%y %H:%M:%S", "%Y-%m-%d %H:%M:%S", "%d/%m/%Y %H:%M:%S")
        
        for (format in time_formats) {
          tryCatch({
            converted_time <- as.POSIXct(data$time, format = format)
            if (!all(is.na(converted_time))) {
              data$time <- converted_time
              showNotification(paste("Time format detected:", format), type = "message")
              break
            }
          }, error = function(e) {
            # Continue to next format
          })
        }
        
        # If no format worked, show warning
        if (all(is.na(data$time))) {
          showNotification("Could not detect time format. Please check your data.", type = "warning")
        }
      }
      
      values$data <- data
      
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
    })
  })
  
  ## File preview output ##########################################################
  
  output$filePreview <- DT::renderDataTable({
    req(values$data)
    
    DT::datatable(
      head(values$data, 100),
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        dom = 'tip'
      )
    )
  })
  
  ## Calculate discharge ###########################################################
  
  observeEvent(input$calculate, {
    req(values$data)
    
    tryCatch({
      data <- values$data
      
      # Get parameters from inputs
      initial_mass_rhodamine_wt <- input$initial_mass_rhodamine_wt
      concentration_rhodamine_wt <- input$concentration_rhodamine_wt / 100  # Convert percentage
      initial_water_temp_degC <- input$initial_water_temp_degC
      n_rhodamine <- input$n_rhodamine
      initial_mass_salt <- input$initial_mass_salt
      slope_conductivity <- input$slope_conductivity
      distance <- input$distance
      T_ref <- input$T_ref
      
      # Calculate initial mass of rhodamine
      initial_mass_rhodamine <- initial_mass_rhodamine_wt * concentration_rhodamine_wt * 1000
      
      # Initialize results
      results <- list()
      plots <- list()
      
      # Process Rhodamine data if available
      if ("ppb" %in% names(data)) {
        # Background correction
        background_indices <- c(1:min(15, nrow(data)), 
                               max(1, nrow(data)-9):nrow(data))
        background_model <- lm(ppb ~ as.numeric(time), data = data[background_indices, ])
        data$background_predicted <- predict(background_model, newdata = data)
        data$ppb_corrected <- data$ppb - data$background_predicted
        
        # Temperature correction
        data$ppb_temp_corrected <- data$ppb_corrected * exp(n_rhodamine * (initial_water_temp_degC - T_ref))
        
        # Smoothing
        data$ppb_smoothed <- sgolayfilt(data$ppb_temp_corrected, p = 3, n = min(11, nrow(data)))
        data$ppb_smoothed[data$ppb_smoothed < 0] <- 0
        
        # Calculate discharge
        time_seconds <- as.numeric(difftime(data$time, data$time[1], units = "secs"))
        auc_rhodamine <- trapz(time_seconds, data$ppb_smoothed)
        discharge_rhodamine <- initial_mass_rhodamine / (auc_rhodamine / 1000)
        
        # Calculate other metrics
        peak_concentration_rhodamine <- max(data$ppb_smoothed)
        peak_time_rhodamine <- data$time[which.max(data$ppb_smoothed)]
        travel_time_rhodamine <- as.numeric(difftime(peak_time_rhodamine, data$time[1], units = "secs"))
        velocity_rhodamine <- distance / travel_time_rhodamine
        
        # Store results
        results$rhodamine <- list(
          peak_concentration = peak_concentration_rhodamine,
          travel_time = travel_time_rhodamine,
          velocity = velocity_rhodamine,
          discharge = discharge_rhodamine
        )
        
        # Create plot
        plots$rhodamine <- ggplot(data, aes(x = time, y = ppb_smoothed)) +
          geom_line(color = "blue", size = 1) +
          geom_area(fill = "blue", alpha = 0.2) +
          labs(x = "Time", y = "Concentration (ppb)") +
          theme_minimal()
      }
      
      # Process Salt data if available
      if ("uScm" %in% names(data)) {
        # Background correction
        background_indices <- c(1:min(10, nrow(data)), 
                               max(1, nrow(data)-9):nrow(data))
        background_model_salt <- lm(uScm ~ as.numeric(time), data = data[background_indices, ])
        data$background_predicted_salt <- predict(background_model_salt, newdata = data)
        data$uScm_corrected <- data$uScm - data$background_predicted_salt
        
        # Convert to concentration
        data$salt_concentration <- data$uScm_corrected / slope_conductivity
        
        # Smoothing
        data$salt_concentration_smoothed <- sgolayfilt(data$salt_concentration, p = 3, n = min(11, nrow(data)))
        data$salt_concentration_smoothed[data$salt_concentration_smoothed < 0] <- 0
        
        # Calculate discharge
        time_seconds <- as.numeric(difftime(data$time, data$time[1], units = "secs"))
        auc_salt <- trapz(time_seconds, data$salt_concentration_smoothed)
        discharge_salt <- initial_mass_salt / auc_salt
        
        # Calculate other metrics
        peak_concentration_salt <- max(data$salt_concentration_smoothed)
        peak_time_salt <- data$time[which.max(data$salt_concentration_smoothed)]
        travel_time_salt <- as.numeric(difftime(peak_time_salt, data$time[1], units = "secs"))
        velocity_salt <- distance / travel_time_salt
        
        # Store results
        results$salt <- list(
          peak_concentration = peak_concentration_salt,
          travel_time = travel_time_salt,
          velocity = velocity_salt,
          discharge = discharge_salt
        )
        
        # Create plot
        plots$salt <- ggplot(data, aes(x = time, y = salt_concentration_smoothed)) +
          geom_line(color = "red", size = 1) +
          geom_area(fill = "red", alpha = 0.2) +
          labs(x = "Time", y = "Concentration (g/L)") +
          theme_minimal()
      }
      
      values$results <- results
      values$plots <- plots
      
      showNotification("Discharge calculation completed successfully!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error in calculation:", e$message), type = "error")
    })
  })
  
  ## Results output ###############################################################
  
  output$results <- renderText({
    req(values$results)
    
    result_text <- ""
    
    if (!is.null(values$results$rhodamine)) {
      r <- values$results$rhodamine
      result_text <- paste0(result_text,
        "RHODAMINE RESULTS:\n",
        "Peak concentration: ", round(r$peak_concentration, 3), " ppb\n",
        "Travel time: ", round(r$travel_time, 2), " seconds\n",
        "Velocity: ", round(r$velocity, 4), " m/s\n",
        "Discharge: ", round(r$discharge, 4), " L/s\n\n"
      )
    }
    
    if (!is.null(values$results$salt)) {
      s <- values$results$salt
      result_text <- paste0(result_text,
        "SALT RESULTS:\n",
        "Peak concentration: ", round(s$peak_concentration, 4), " g/L\n",
        "Travel time: ", round(s$travel_time, 2), " seconds\n",
        "Velocity: ", round(s$velocity, 4), " m/s\n",
        "Discharge: ", round(s$discharge, 4), " L/s\n"
      )
    }
    
    if (result_text == "") {
      result_text <- "No valid tracer data found. Please ensure your file contains 'ppb' (rhodamine) or 'uScm' (conductivity) columns."
    }
    
    result_text
  })
  
  ## Plots output #################################################################
  
  output$plots <- renderPlot({
    req(values$plots)
    
    plot_list <- values$plots
    
    if (length(plot_list) == 2) {
      grid.arrange(plot_list$rhodamine, plot_list$salt, ncol = 2)
    } else if (!is.null(plot_list$rhodamine)) {
      plot_list$rhodamine
    } else if (!is.null(plot_list$salt)) {
      plot_list$salt
    }
  })
  
  ## Reset date when site changes
  observeEvent(input$site, {
    updateSelectInput(session, 'date', selected = '')
  })
  
  # Update date choices when site changes
  observeEvent(input$site, {
    req(input$site != '')
    
    # Add JavaScript debug message
    session$sendCustomMessage("console", list(message = paste("Site changed to:", input$site)))
    
    # Get available dates for the selected station
    tryCatch({
      session$sendCustomMessage("console", list(message = "Querying database for dates..."))
      
      # Store the site value in a variable for use in getRows
      selected_site <- input$site
      
      dates <- getRows(pool, 'data', 
                      station == selected_site,
                      columns = 'DATE_reading') %>%
        distinct(DATE_reading) %>%
        arrange(desc(DATE_reading))
      
      session$sendCustomMessage("console", list(message = paste("Found", nrow(dates), "dates for station", input$site)))
      
      if (nrow(dates) > 0) {
        # parseOptions returns a simple vector, so we create the named list properly
        date_options <- parseOptions(dates, 'DATE_reading')
        date_choices <- c('Choose a date ...' = '', setNames(date_options, date_options))
        session$sendCustomMessage("console", list(message = paste("Date choices:", paste(date_options, collapse = ", "))))
        updateSelectInput(session, 'date', choices = date_choices)
      } else {
        session$sendCustomMessage("console", list(message = "No dates found for station"))
        updateSelectInput(session, 'date', choices = c('No dates available' = ''))
      }
    }, error = function(e) {
      session$sendCustomMessage("console", list(message = paste("Error loading dates:", e$message)))
      updateSelectInput(session, 'date', choices = c('Error loading dates' = ''))
    })
  })
  
  # Display water temperature when site and date are selected
  output$waterTemp <- renderText({
    req(input$site, input$date)
    
    if (input$site == '' || input$date == '') {
      return("Select site & date")
    }
    
    tryCatch({
      # Get water temperature for selected site and date
      selected_site <- input$site
      selected_date <- input$date
      
      temp_data <- getRows(pool, 'data',
                          station == selected_site,
                          DATE_reading == selected_date,
                          columns = 'WTW_Temp_degC_1')
      
      if (nrow(temp_data) > 0 && !is.na(temp_data$WTW_Temp_degC_1)) {
        paste(round(temp_data$WTW_Temp_degC_1, 1), "°C")
      } else {
        "No data"
      }
    }, error = function(e) {
      "Error loading"
    })
  })
  
  ## Database Check and Update Logic ##############################################
  
  # Check button logic
  observeEvent(input$check, {
    req(input$site, input$date)
    
    # Reset checked status and errors
    values$checked <- FALSE
    values$dbError <- NULL
    
    # Validate inputs
    if (input$site == '') {
      values$dbError <- "Please select a station."
      return()
    }
    
    if (is.null(input$q_ls) || is.na(input$q_ls)) {
      values$dbError <- "Please enter a Q_Ls value."
      return()
    }
    
    if (input$q_ls < 0) {
      values$dbError <- "Q_Ls value must be positive."
      return()
    }
    
    # Check if record exists
    tryCatch({
      # Store reactive values in variables
      selected_site <- input$site
      selected_date <- as.character(input$date)
      
      existing_record <- getRows(pool, 'data', 
                                station == selected_site, 
                                DATE_reading == selected_date,
                                columns = c('id', 'Q_Ls'))
      
      if (nrow(existing_record) == 0) {
        values$dbError <- paste("No data record found for station", input$site, "on", input$date)
      } else if (nrow(existing_record) > 1) {
        values$dbError <- paste("Multiple records found for station", input$site, "on", input$date, 
                               "- please contact administrator")
      } else {
        # Record found, check is successful
        values$checked <- TRUE
        current_q_ls <- if (is.na(existing_record$Q_Ls)) "NULL" else existing_record$Q_Ls
        values$dbError <- paste("✓ Check successful! Record ID:", existing_record$id, 
                               "| Current Q_Ls:", current_q_ls,
                               "| New Q_Ls:", input$q_ls)
      }
    }, error = function(e) {
      values$dbError <- paste("Database error:", e$message)
    })
  })
  
  # Update button logic
  observeEvent(input$update, {
    req(values$checked, input$site, input$date, input$q_ls)
    
    if (!values$checked) {
      values$dbError <- "Please check the record first before updating."
      return()
    }
    
    # Get the record to update
    tryCatch({
      # Store reactive values in variables
      selected_site <- input$site
      selected_date <- as.character(input$date)
      
      existing_record <- getRows(pool, 'data', 
                                station == selected_site,
                                DATE_reading == selected_date,
                                columns = c('id'))
      
      if (nrow(existing_record) == 1) {
        # Update the record
        result <- updateData(pool, existing_record$id, c('Q_Ls'), list(input$q_ls))
        
        if (result == '') {
          values$dbError <- paste("✅ Successfully updated Q_Ls to", input$q_ls, "L/s for station", 
                                 input$site, "on", input$date)
          values$checked <- FALSE  # Reset check status after successful update
        } else {
          values$dbError <- paste("Update failed:", result)
        }
      } else {
        values$dbError <- "Record not found for update. Please check again."
      }
    }, error = function(e) {
      values$dbError <- paste("Update error:", e$message)
    })
  })
  
  # Render database feedback
  output$dbFeedback <- renderUI({
    if (!is.null(values$dbError)) {
      if (grepl("^✓|^✅", values$dbError)) {
        # Success message
        div(style = "color: #155724; background-color: #d4edda; border: 1px solid #c3e6cb; padding: 10px; border-radius: 5px;",
            icon("check-circle"), " ", values$dbError)
      } else {
        # Error message
        div(style = "color: #721c24; background-color: #f8d7da; border: 1px solid #f5c6cb; padding: 10px; border-radius: 5px;",
            icon("exclamation-triangle"), " ", values$dbError)
      }
    } else {
      div(style = "color: #6c757d; font-style: italic;", "Select station, date, and Q_Ls value, then click Check to validate.")
    }
  })
  
  ## Return reactive ##############################################################
  
  return(reactive({
    values$results
  }))
}
