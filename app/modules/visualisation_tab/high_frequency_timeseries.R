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
      actionButton(ns('showStats'), 'Show Stats', class = 'custom-style')
    ),
    # Create the UI plots
    'plots' = div(
      # Set UI plots id and class
      id = paste0('hf-time-serie-plots-', id),
      class = 'time-serie-plot point-hover-widget-plot',
      # Create a plotOutput for the regular timeserie plot
      spinnerPlotOutput(
        ns('highfreq'),
        # Make data points hoverable
        hover = hoverOpts(ns('highfreq_hover')),
        # Temporarily disabled to debug connection issues
        # brush = brushOpts(
        #   ns('highfreq_brush'),
        #   direction = 'x',
        #   delayType = 'debounce',
        #   resetOnNew = TRUE
        # ),
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
  
  ## Session validation and setup #################################################
  
  # Check if session is valid to prevent readyState errors
  if (is.null(session) || session$closed) {
    return(NULL)
  }
  
  # Add session onSessionEnded handler to clean up
  session$onSessionEnded(function() {
    # Clean up any reactive values or observers if needed
  })
  
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
  }) %>% debounce(300) # Add debouncing to prevent excessive updates
  
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
  }) %>% bindEvent(data(), ignoreNULL = TRUE, ignoreInit = FALSE) # Use bindEvent for more stable rendering
  
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
  
  
  
  
  
  ## Update dateRange with plot brushing and double click logic ####################################
  
  # Create a reactive value to store the updated date range
  updatedDateRange <- reactiveVal(NULL)
  
  # Temporarily disabled brush functionality to debug connection issues
  # # Debounce brush events to prevent overwhelming the reactive system
  # brushDebounced <- reactive({
  #   input$highfreq_brush
  # }) %>% debounce(800) # 800ms debounce for more stability
  # 
  # # Observe debounced brush changes and update the date range
  # observeEvent(brushDebounced(), {
  #   # Check if session is still valid
  #   if (is.null(session) || session$closed) {
  #     return()
  #   }
  #   
  #   brush <- brushDebounced()
  #   
  #   # Check if brush exists and has valid coordinates
  #   if (is.null(brush) || 
  #       is.null(brush$xmin) || 
  #       is.null(brush$xmax) ||
  #       !is.numeric(brush$xmin) ||
  #       !is.numeric(brush$xmax) ||
  #       is.na(brush$xmin) ||
  #       is.na(brush$xmax) ||
  #       brush$xmin >= brush$xmax ||
  #       abs(brush$xmax - brush$xmin) < 86400) { # Prevent selections smaller than 1 day (in seconds)
  #     return()
  #   }
  #   
  #   # Try to convert coordinates to dates with error handling
  #   tryCatch({
  #     min_date <- as.Date(as.POSIXct(brush$xmin, origin = "1970-01-01", tz = "GMT"))
  #     max_date <- as.Date(as.POSIXct(brush$xmax, origin = "1970-01-01", tz = "GMT"))
  #     
  #     # Validate that the converted dates are reasonable
  #     if (is.na(min_date) || is.na(max_date) || min_date >= max_date) {
  #       return()
  #     }
  #     
  #     # Prevent very narrow date ranges that might cause issues (minimum 1 day)
  #     if (as.numeric(max_date - min_date) < 1) {
  #       return()
  #     }
  #     
  #     # Update the reactive value only if it's different from current
  #     current <- updatedDateRange()
  #     if (is.null(current) || 
  #         current$min != min_date || 
  #         current$max != max_date) {
  #       updatedDateRange(list(
  #         'min' = min_date,
  #         'max' = max_date
  #       ))
  #     }
  #   }, error = function(e) {
  #     # Do nothing on error
  #     return()
  #   })
  # }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # Create a reactive expression that returns the updated date range
  updateDateRange <- reactive({
    updatedDateRange()
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
  
  # Return the new dateRange values and date range reset trigger in order to update the outer module dateRangeInput
  return(list(
    'update' = updateDateRange,
    'reset' = resetDateRange
  ))
}
