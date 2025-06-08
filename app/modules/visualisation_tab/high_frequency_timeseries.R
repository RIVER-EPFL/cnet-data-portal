## This module contains the code for the high frequency data timeserie visualisation

## Create the UI function of the module ###############################################

highFreqTimeSeriesUI <- function(id, pool) {
# Create the UI for the highFreqTimeSeries module
# Parameters:
#  - id: String, the module id
#  - pool: The pool connection to the database
# 
# Returns a list containing:
#  - inputs: the inputs UI elements of the module
#  - plots: the plots UI elements of the module
  
  # Create namespace
  ns <- NS(id)
  
  # Get site options
  siteOptions <- parseOptions(
    getRows(pool, 'stations', columns = c('order', 'name')) %>%
      arrange(order) %>% select(-order),
    'name'
  )
  
  # Create the UI list to be returned
  list(
    # Create the UI inputs
    'inputs' = div(
      # Set UI inputs id and class
      id = paste0('hf-time-serie-plot-input-', id),
      class = 'time-serie-input',
      # Create select input for catchment selection
      checkboxGroupInputWithClass(
        checkboxGroupInput(
          ns('sites'),
          'Station',
          choices = siteOptions,
          selected = siteOptions[[1]]
        ),
        class = 'checkbox-grid no-margin'        
      ),
      # Add information sentence about plotting efficiency
      p(
        class = 'tiny-warning',
        'BEWARE: loading simultaneously 10-min data of 4+ stations may take a while. Consider selecting a different frequency to speed up the visualization.'
      ),
      selectInput(
        ns('param'),
        # Create a label with an icon button
        tags$span(
          'Parameter',
          # Create an icon button that trigger a modal to display the parameter description
          actionButton(ns('paramHelper'), icon('question-circle'), class = 'icon-btn')
        ),
        parseOptionsWithSections(
          getRows(pool, 'sensor_params_plotting', active == TRUE, columns = c('order', 'section_name', 'option_name', 'param_name')) %>%
            arrange(order) %>% select(-order),
          'param_name'
        )
      ),
      # Create a checkbox to select or unselect modeled data
      checkboxInput(
        ns('showModeledData'),
        # Create a label with an icon button
        tags$span(
          'Show modeled data',
          # Create an icon button that trigger a modal to display the modeled data description
          actionButton(ns('modeledHelper'), icon('question-circle'), class = 'icon-btn')
        ),
        value = TRUE),
      # Create radio buttons to select the data frequency to display
      checkboxGroupInputWithClass(
        radioButtons(
          inputId = ns('dataFreq'),
          label = tags$span(
            'Data frequency',
            # Create an icon button that trigger a modal to display the parameter description
            actionButton(ns('freqHelper'), icon('question-circle'), class = 'icon-btn')
          ),
          choices = list('10min (raw)' = '10min', '6H', '12H', '24H'),
          selected = '10min'
        ),
        class = 'checkbox-grid'        
      ),
      # Show stats button
      actionButton(ns('showStats'), 'Show Stats', class = 'custom-style'),
      # Click-to-zoom button
      div(
        style = "margin-top: 10px;",
        actionButton(ns('startZoom'), 'Start Zoom', class = 'btn-primary'),
        br(),
        tags$small("1. Click 'Start Zoom' 2. Click 2 points on plot", style = "color: #666; font-size: 11px;")
      )
    ),
    # Create the UI plots
    'plots' = div(
      # Set UI plots id and class
      id = paste0('hf-time-serie-plots-', id),
      class = 'time-serie-plot point-hover-widget-plot',
      # Add JavaScript for console logging
      tags$script("
        Shiny.addCustomMessageHandler('console-log', function(message) {
          console.log(message);
        });
      "),
      # Create a plotOutput for the regular timeserie plot
      spinnerPlotOutput(
        ns('highfreq'),
        # Make data points hoverable
        hover = hoverOpts(ns('highfreq_hover')),
        # BRUSH DISABLED: Causes connection crashes - use click-to-zoom instead
        # brush = brushOpts(
        #   ns('highfreq_brush'),
        #   direction = 'x',
        #   delayType = 'debounce',
        #   resetOnNew = TRUE
        # ),
        # Click-to-zoom: Click two points to define zoom range
        click = clickOpts(ns('highfreq_click')),
        # Make plot double clickable
        dblclick = dblclickOpts(ns('highfreq_dblclick'))
      )
    )
  )
}



## Create the server function of the module ###############################################

highFreqTimeSeries <- function(input, output, session, df, dateRange, pool) {
# Create the logic for the highFreqTimeSeries module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - df: Named List of Data.frame, the sensors high frequency data at different frequency
#  - dateRange: Reactive expression that returns the date range to filter the data with.
#               Date range format must be a list containing:
#               + min: Date, the lower bound to filter the date
#               + max: Date, the upper bound to filter the data
#  - pool: The pool connection to the database
# 
# Returns a reactive expression containing the updated date range with the same format as the input
  
  ## Stations update logic ########################################################
  
  # Get the sites
  sites <- getRows(pool, 'stations', columns = c('order', 'name', 'full_name', 'catchment', 'color')) %>%
    arrange(order) %>% select(-order)
  
  # Create a reactive expression returning the selected sites
  selectedSites <- reactive({input$sites})
  
  # Create a debounced reactive expression returning the selected sites
  selectedSites_d <-  selectedSites %>% debounce(1000)
  
  
  
  ## Parameter logic ##############################################################
  
  # Create a reactive expression that returns the filtered parameters df
  param <- reactive(getRows(
    pool, 'sensor_params_plotting',
    active == TRUE,
    param_name == local(input$param),
    columns = c('order', 'param_name', 'units', 'data', 'description')
  ) %>% arrange(order) %>% select(-order))
  
  
  
  
  ## Modeled data selection logic #################################################
  
  # Create observeEvent that react to frequence update
  # Display showModeledData checkbox if the selected data frequence is 10min
  observeEvent(input$dataFreq, ignoreInit = TRUE, {
    toggleElement('showModeledData', condition = input$dataFreq == '10min')
  })
  
  
  
  
  ## Data manipulation logic ######################################################
  
  # Create a data reactive expression that return a subset of the data
  # Using the dateRange, selectedSites_d and param reactive expressions
  data <- reactive({
    # Wrap entire data processing in tryCatch to prevent crashes
    tryCatch({
      # Validate inputs before processing
      req(dateRange())
      req(selectedSites_d())
      req(param())
      
      # Additional validation for date range
      if (is.null(dateRange()$min) || is.null(dateRange()$max) ||
          !inherits(dateRange()$min, "Date") || !inherits(dateRange()$max, "Date") ||
          dateRange()$min >= dateRange()$max) {
        return(NULL)
      }
      
      # Select df
      df <- df[[input$dataFreq]]
      
      # Validate that df exists and has data
      if (is.null(df) || nrow(df) == 0) {
        return(NULL)
      }
      
      # If the raw data is selected filter also for modeled data
      if (input$dataFreq == '10min') {
        # Define data types to remove depending on the state of showModeledData
        # If nothing to remove, set to 'NULL' as string to avoid match error
        typesToRemove <- c('modeled')
        if (input$showModeledData) typesToRemove <- 'NULL'
        
        # Filter the data using the selected sites and the date range
        df %<>% filter(
          Site_ID %in% selectedSites_d(),
          date(Date) >= dateRange()$min,
          date(Date) <= dateRange()$max
        )
        
        # Check if filtering resulted in empty data
        if (nrow(df) == 0) return(NULL)
        
        # Continue with data transformation
        df %<>%
          # Select the date, Site_ID, all the parameter specific columns and remove modeled column not used
          select(Date, Site_ID, starts_with(param()$data), -ends_with(typesToRemove)) %>% 
          # Pivot longer the data to get a data_type and a value column
          pivot_longer(
            ends_with(c('measured', 'modeled')),
            names_to = 'data_type',
            names_pattern = '.*_(.*)',
            names_transform = list('data_type' = as.factor),
            values_to = 'value'
          ) %>% 
          # Rename with singlePoint column
          rename(singlePoint = ends_with('singlePoint'))
      } else {
        # Filter the data using the selected sites and the date range
        # Then select the parameter and rename the column to 'value'
        df %<>% filter(
          Site_ID %in% selectedSites_d(),
          date(Date) >= dateRange()$min,
          date(Date) <= dateRange()$max
        )
        
        # Check if filtering resulted in empty data
        if (nrow(df) == 0) return(NULL)
        
        # Check if the parameter column exists
        if (!param()$data %in% colnames(df)) {
          return(NULL)
        }
        
        # Select and rename columns
        df %<>% select(Date, Site_ID, 'value' = param()$data)
      }
      
      # Final validation of the processed data
      if (is.null(df) || nrow(df) == 0) return(NULL)
      
      return(df)
      
    }, error = function(e) {
      # Log error for debugging but don't crash
      warning("Error in data processing: ", e$message)
      return(NULL)
    })
  })
  
  ## Plots output logic ###########################################################
  
  # Render the regular timeserie plot
  output$highfreq <- renderPlot({
    # If there are no data return NULL
    if (data() %>% is.null()) return(NULL)
    
    # Additional validation for data structure
    if (nrow(data()) == 0) return(NULL)
    
    # Try to create the plot with error handling
    tryCatch({
      # Create and return a highFreqTimeSeriePlot
      highFreqTimeSeriePlot(
        df = data(),
        parameter = param(),
        plotTitle = 'Sensor High Frequency Time Serie',
        sites = sites,
        modeledData = 'data_type' %in% colnames(data())
      )
    }, error = function(e) {
      # Return a simple error plot instead of crashing
      plot(1, 1, type = "n", xlab = "", ylab = "", main = "Error rendering plot")
      text(1, 1, paste("Plot error:", e$message), cex = 0.8)
    })
  })
  
  ## Plot hovering logic ##########################################################
  
  # Activate the hover widget for the regular timeserie plot
  pointHoverWidgetServer(session, 'highfreq', data, reactive(input$highfreq_hover), y_label = 'Parameter')

  
  
  
  
  ## Parameter description modal logic ############################################
  
  # Create an observeEvent that react to the parameter helper icon button
  observeEvent(input$paramHelper, {
    # Render the description UI in the modal
    output$description <- renderUI(tags$p(
      class = 'description',
      param()$description
    ))
    
    # Create modal with the corresponding htmlOutput
    showModal(modalDialog(
      title = 'Parameter description',
      htmlOutput(session$ns('description')),
      footer = modalButtonWithClass('Dismiss', class = 'custom-style'),
      easyClose = TRUE
    ))
  })
  
  
  
  ## Data helpers logic ####################################################
  
  # Create an observeEvent that react to the data freq helper button
  observeEvent(input$freqHelper, ignoreInit = TRUE, {
    showModal(modalDialog(
      title = 'Sensor data frequency selection',
      htmlTemplate('./html_components/data_freq_help.html', icon = icon('exclamation-triangle')),
      footer = modalButtonWithClass('Dismiss', class = 'custom-style'),
      easyClose = TRUE
    ))
  })
  
  # Create an observeEvent that react to modeled data helper button
  observeEvent(input$modeledHelper, ignoreInit = TRUE, {
    showModal(modalDialog(
      title = 'Modeled data',
      htmlTemplate('./html_components/data_modeled_help.html'),
      footer = modalButtonWithClass('Dismiss', class = 'custom-style'),
      easyClose = TRUE
    ))
  })
  
  
  
  
  ## Show stats logic #############################################################
  
  # Create a reactive expression returning the the summarised data
  statsData <- reactive({
    # SHow spinner
    show_spinner('hf-stats')
    # Take the 10min data and filter by sites and date
    stats <- df$`10min` %>% filter(
      Site_ID %in% selectedSites_d(),
      date(Date) >= dateRange()$min,
      date(Date) <= dateRange()$max
      # Select the date, sites and selected parameter columns
    ) %>% select(
      Date,
      Site_ID,
      starts_with(param()$data),
      -ends_with('singlePoint')
      # Pivot longer to summarise easily
    ) %>% pivot_longer(
      ends_with(c('measured', 'modeled')),
      names_to = 'data_type',
      names_pattern = '.*_(.*)',
      names_transform = list('data_type' = as.factor),
      values_to = 'value'
      # Group by sites and data_type
    )%>% group_by(
      Site_ID, data_type
      # Get the number of values per data_type and site
    ) %>% summarise(
      n = sum(!is.na(value))
      # Pivot wider again to compute some info easily
    ) %>% pivot_wider(
      names_from = data_type,
      values_from = n
      # Add a total column
    ) %>% mutate(
      total = sum(measured, modeled)
      # Add percentage columns
    ) %>% mutate(
      measured_pct = measured / total * 100,
      modeled_pct = modeled / total * 100
      # Pivot wider to get the site in columns
    ) %>% pivot_wider(
      names_from = Site_ID,
      values_from = -c(Site_ID),
      names_glue = "{Site_ID}_{.value}"
      # Pivot longer to get the stats in rows
    ) %>% pivot_longer(
      everything(),
      names_to = c('.value', 'Stats'),
      names_pattern = '^([A-Z]*)_(.*)'
      # Capitalize the stats name
    ) %>% mutate(Stats = str_to_title(Stats))
    # Hide spinner
    hide_spinner('hf-stats')
    # Return stats table
    stats
  })
  
  # Render the stats tables in the modal
  output$sensorStats <- renderStatsTables(
    elements = unique(sites$catchment),
    data = statsData,
    sites = sites,
    tableFunction = createSensorStatsTable
  )
  
  # Create an observeEvent that react to the show stats button
  observeEvent(input$showStats, ignoreInit = TRUE, {
    # Create a moadal containing the stats output
    showModal(modalDialog(
      title = 'Sensor summary statistics',
      # Add spinner
      use_busy_spinner(spin = 'fading-circle', color = "#e24727", position = 'full-page', spin_id = 'hf-stats', margins = c(30, 10)),
      # Add UI output
      htmlOutput(session$ns('sensorStats'), class = 'stats-summary'),
      footer = modalButtonWithClass('Dismiss', class = 'custom-style'),
      easyClose = TRUE
    ))
  })
  
  
  
  
  
  ## Update dateRange with plot clicking and double click logic ####################################
  
  # Create a reactive value to store the updated date range
  updatedDateRange <- reactiveVal(NULL)
  
  # Create reactive values to store click-to-zoom state
  firstClick <- reactiveVal(NULL)
  zoomMode <- reactiveVal(FALSE)
  
  # BRUSH FUNCTIONALITY DISABLED TO PREVENT CONNECTION CRASHES
  # Alternative: Click-to-zoom functionality
  # Instructions: Click "Start Zoom" button, then click two points on the plot to define zoom range
  
  # Create a reactive expression that returns the updated date range
  # This reactive will be triggered whenever updatedDateRange() changes
  updateDateRange <- reactive({
    # Get the current value - this creates the reactive dependency
    result <- updatedDateRange()
    
    # Debug: Show when updateDateRange is triggered in browser console
    if (!is.null(result)) {
      session$sendCustomMessage("console-log", 
        paste("DEBUG: updateDateRange triggered with:", result$min, "to", result$max))
    } else {
      session$sendCustomMessage("console-log", "DEBUG: updateDateRange triggered with NULL")
    }
    
    # Return the result
    result
  })
  
  # Force the reactive to trigger by observing it
  observe({
    # This will force updateDateRange to be evaluated whenever updatedDateRange changes
    temp <- updateDateRange()
    session$sendCustomMessage("console-log", 
      paste("DEBUG: observe triggered, updateDateRange result:", 
            if(is.null(temp)) "NULL" else paste(temp$min, "to", temp$max)))
  })
  
  # Create a reactive value that update each time the plot is double clicked
  # Used as trigger to reset the date range in the outer module
  # Initialised to NULL to avoid a dateRange reset when a new unit is created
  resetDateRange <- reactiveVal(NULL)
  
  # Create an observe event that react on plot double click to reset the date range
  observeEvent(input$highfreq_dblclick, {
    if (is.null(resetDateRange())) {
      resetDateRange(1)
    } else {
      resetDateRange(resetDateRange() + 1)
    }
  })
  
  # Create an observe event that react on plot click to update the date range
  observeEvent(input$highfreq_click, {
    # Only process clicks if we have valid coordinates
    if (!is.null(input$highfreq_click) && !is.null(input$highfreq_click$x)) {
      click_x <- input$highfreq_click$x
      
      # Check if zoom mode is active
      if (zoomMode()) {
        # If first click is not set, store it
        if (is.null(firstClick())) {
          firstClick(click_x)
          showNotification("First point selected. Click second point to zoom.", type = "message", duration = 3)
        } else {
          # Second click - create zoom range
          tryCatch({
            # Convert coordinates to dates
            min_x <- min(firstClick(), click_x)
            max_x <- max(firstClick(), click_x)
            
            min_date <- as.Date(as.POSIXct(min_x, origin = "1970-01-01", tz = "GMT"))
            max_date <- as.Date(as.POSIXct(max_x, origin = "1970-01-01", tz = "GMT"))
            
            # Validate dates
            if (!is.na(min_date) && !is.na(max_date) && min_date < max_date) {
              # Additional validation - ensure dates are reasonable
              current_year <- as.numeric(format(Sys.Date(), "%Y"))
              min_year <- as.numeric(format(min_date, "%Y"))
              max_year <- as.numeric(format(max_date, "%Y"))
              
              # Check if dates are within reasonable range (not too far in past/future)
              if (min_year >= 1900 && min_year <= current_year + 10 &&
                  max_year >= 1900 && max_year <= current_year + 10) {
                
                # Instead of updating reactive value, store the zoom request
                # This will be handled by the return value
                updatedDateRange(list(
                  'min' = min_date,
                  'max' = max_date
                ))
                
                # Debug: Confirm the reactive value was set
                session$sendCustomMessage("console-log", 
                  paste("DEBUG: updatedDateRange set to:", 
                        if(is.null(updatedDateRange())) "NULL" else paste(updatedDateRange()$min, "to", updatedDateRange()$max)))
                
                showNotification("Zoom range selected. Applying zoom...", type = "message", duration = 2)
                
                # Debug: Send message to browser console
                session$sendCustomMessage("console-log", 
                  paste("DEBUG: Click-to-zoom setting date range:", min_date, "to", max_date))
              } else {
                showNotification("Date range is outside valid range. Please select dates within a reasonable timeframe.", type = "warning", duration = 4)
              }
            } else {
              showNotification("Invalid date range selected.", type = "warning", duration = 3)
            }
          }, error = function(e) {
            showNotification("Error selecting zoom range. Please try again.", type = "error", duration = 3)
          })
          
          # Reset zoom mode
          firstClick(NULL)
          zoomMode(FALSE)
        }
      }
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # Create an observe event that react to the start zoom button
  observeEvent(input$startZoom, {
    zoomMode(TRUE)
    firstClick(NULL)
    showNotification("Zoom mode activated. Click two points on the plot to define zoom range.", 
                     type = "message", duration = 4)
  })
  
  # Return the new dateRange values and date range reset trigger in order to update the outer module dateRangeInput
  return(list(
    'update' = updateDateRange,
    'reset' = resetDateRange
  ))
}
