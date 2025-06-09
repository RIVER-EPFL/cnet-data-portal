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
        # Make plot brushable in the x direction with a debouncing delay type
        # Reset it when the plot is refreshed
        brush = brushOpts(
          ns('highfreq_brush'),
          direction = 'x',
          delayType = 'debounce',
          resetOnNew = TRUE
        ),
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
  selectedSites <- reactive({
    tryCatch({
      input$sites
    }, error = function(e) {
      runjs(paste0("console.error('High frequency selectedSites error: ", e$message, "');"))
      return(character(0))
    })
  })
  
  # Create a debounced reactive expression returning the selected sites
  selectedSites_d <-  selectedSites %>% debounce(1000)
  
  
  
  ## Parameter logic ##############################################################
  
  # Create a reactive expression that returns the filtered parameters df
  param <- reactive({
    tryCatch({
      getRows(
        pool, 'sensor_params_plotting',
        active == TRUE,
        param_name == local(input$param),
        columns = c('order', 'param_name', 'units', 'data', 'description')
      ) %>% arrange(order) %>% select(-order)
    }, error = function(e) {
      runjs(paste0("console.error('High frequency param error: ", e$message, "');"))
      return(data.frame(
        param_name = character(0),
        units = character(0),
        data = character(0),
        description = character(0),
        stringsAsFactors = FALSE
      ))
    })
  })
  
  
  
  
  ## Modeled data selection logic #################################################
  
  # Create observeEvent that react to frequence update
  # Display showModeledData checkbox if the selected data frequence is 10min
  observeEvent(input$dataFreq, ignoreInit = TRUE, {
    tryCatch({
      toggleElement('showModeledData', condition = input$dataFreq == '10min')
    }, error = function(e) {
      runjs(paste0("console.error('High frequency dataFreq toggle error: ", e$message, "');"))
    })
  })
  
  
  
  
  ## Data manipulation logic ######################################################
  
  # Create a data reactive expression that return a subset of the data
  # Using the dateRange, selectedSites_d and param reactive expressions
  data <- reactive({
    tryCatch({
      runjs("console.log('High frequency data reactive: Starting execution');")
      
      # Validate required inputs
      if (is.null(selectedSites_d()) || length(selectedSites_d()) == 0) {
        runjs("console.log('High frequency data reactive: No sites selected');")
        return(NULL)
      }
      
      if (is.null(param()) || is.null(dateRange())) {
        runjs("console.log('High frequency data reactive: Missing param or dateRange');")
        return(NULL)
      }
      
      # Validate date range has required properties
      if (is.null(dateRange()$min) || is.null(dateRange()$max)) {
        runjs("console.log('High frequency data reactive: Invalid date range');")
        return(NULL)
      }
      
      # Validate input$dataFreq exists
      if (is.null(input$dataFreq) || input$dataFreq == "") {
        runjs("console.log('High frequency data reactive: Missing dataFreq');")
        return(NULL)
      }
      
      runjs(paste0("console.log('High frequency data reactive: Using frequency ", input$dataFreq, "');"))
      
      # Get the data from the selected frequency
      filteredDf <- df[[input$dataFreq]]
      
      # Validate df exists and has data
      if (is.null(filteredDf) || nrow(filteredDf) == 0) {
        runjs(paste0("console.log('High frequency data reactive: No data for frequency ", input$dataFreq, "');"))
        return(NULL)
      }
      
      runjs(paste0("console.log('High frequency data reactive: Processing ", nrow(filteredDf), " rows');"))
      
      # Check if the raw data is selected (10min) to handle modeled data properly
      if (input$dataFreq == '10min') {
        runjs("console.log('High frequency data reactive: Processing 10min data');")
        
        # Validate showModeledData input exists
        if (is.null(input$showModeledData)) {
          input$showModeledData <- FALSE  # Default value
        }
        
        # Define data types to remove depending on the state of showModeledData
        # If nothing to remove, set to 'NULL' as string to avoid match error
        typesToRemove <- c('modeled')
        if (input$showModeledData) typesToRemove <- 'NULL'
        
        runjs(paste0("console.log('High frequency data reactive: Types to remove: ", paste(typesToRemove, collapse = ", "), "');"))
        
        # Filter the data using the selected sites and the date range
        filteredDf %<>% filter(
          Site_ID %in% selectedSites_d(),
          date(Date) >= dateRange()$min,
          date(Date) <= dateRange()$max
        ) %>%
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
        runjs("console.log('High frequency data reactive: Processing non-10min data');")
        
        # For non-10min data, filter the data using the selected sites and the date range
        # Then select the parameter and rename the column to 'value'
        filteredDf %<>% filter(
          Site_ID %in% selectedSites_d(),
          date(Date) >= dateRange()$min,
          date(Date) <= dateRange()$max
        ) %>% select(Date, Site_ID, 'value' = param()$data)
      }
      
      runjs(paste0("console.log('High frequency data reactive: Filtered to ", nrow(filteredDf), " rows');"))
      
      # If there is no data return NULL else df
      if (nrow(filteredDf) == 0) {
        runjs("console.log('High frequency data reactive: No data after filtering');")
        return(NULL)
      } else {
        runjs(paste0("console.log('High frequency data reactive: Returning ", nrow(filteredDf), " rows');"))
        return(filteredDf)
      }
      
    }, error = function(e) {
      # Log error for debugging but don't crash the app
      runjs(paste0("console.error('High frequency data filtering error: ", e$message, "');"))
      runjs(paste0("console.error('Error occurred at: ", paste(sys.calls(), collapse = " -> "), "');"))
      # Return empty data frame with correct structure to prevent further crashes
      return(data.frame(
        Date = as.POSIXct(character(0)),
        Site_ID = character(0),
        value = numeric(0),
        stringsAsFactors = FALSE
      ))
    })
  })
  
  
  ## Plots output logic ###########################################################
  
  # Render the regular timeserie plot
  output$highfreq <- renderPlot({
    # Add comprehensive error handling for high frequency plot rendering
    tryCatch({
      # If there are no data return NULL
      if (data() %>% is.null()) return(NULL)
      
      # Validate data has required structure
      if (nrow(data()) == 0) return(NULL)
      
      # Validate required columns exist
      required_cols <- c("Date", "Site_ID", "value")
      if (!all(required_cols %in% colnames(data()))) {
        return(NULL)
      }
      
      # Create and return a highFreqTimeSeriePlot
      highFreqTimeSeriePlot(
        df = data(),
        parameter = param(),
        plotTitle = 'Sensor High Frequency Time Serie',
        sites = sites,
        modeledData = 'data_type' %in% colnames(data())
      )
      
    }, error = function(e) {
      # Log error for debugging but don't crash the app
      runjs(paste0("console.error('High frequency plot rendering error: ", e$message, "');"))
      
      # Return a simple error plot instead of crashing
      ggplot() + 
        geom_text(aes(x = 0.5, y = 0.5, label = "Error rendering plot\nPlease try adjusting date range"), 
                  size = 6, color = "red") +
        xlim(0, 1) + ylim(0, 1) +
        theme_void() +
        ggtitle("High Frequency Plot Error")
    })
  })
  
  
  
  
  ## Plot hovering logic ##########################################################
  
  # Activate the hover widget for the regular timeserie plot
  tryCatch({
    pointHoverWidgetServer(session, 'highfreq', data, reactive(input$highfreq_hover), y_label = 'Parameter')
  }, error = function(e) {
    runjs(paste0("console.error('High frequency hover widget error: ", e$message, "');"))
  })
  
  
  
  
  ## Parameter description modal logic ############################################
  
  # Create an observeEvent that react to the parameter helper icon button
  observeEvent(input$paramHelper, {
    tryCatch({
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
    }, error = function(e) {
      runjs(paste0("console.error('High frequency paramHelper modal error: ", e$message, "');"))
    })
  })
  
  
  
  ## Data helpers logic ####################################################
  
  # Create an observeEvent that react to the data freq helper button
  observeEvent(input$freqHelper, ignoreInit = TRUE, {
    tryCatch({
      showModal(modalDialog(
        title = 'Sensor data frequency selection',
        htmlTemplate('./html_components/data_freq_help.html', icon = icon('exclamation-triangle')),
        footer = modalButtonWithClass('Dismiss', class = 'custom-style'),
        easyClose = TRUE
      ))
    }, error = function(e) {
      runjs(paste0("console.error('High frequency freqHelper modal error: ", e$message, "');"))
    })
  })
  
  # Create an observeEvent that react to modeled data helper button
  observeEvent(input$modeledHelper, ignoreInit = TRUE, {
    tryCatch({
      showModal(modalDialog(
        title = 'Modeled data',
        htmlTemplate('./html_components/data_modeled_help.html'),
        footer = modalButtonWithClass('Dismiss', class = 'custom-style'),
        easyClose = TRUE
      ))
    }, error = function(e) {
      runjs(paste0("console.error('High frequency modeledHelper modal error: ", e$message, "');"))
    })
  })
  
  
  
  
  ## Show stats logic #############################################################
  
  # Create a reactive expression returning the the summarised data
  statsData <- reactive({
    tryCatch({
      # Show spinner
      show_spinner('hf-stats')
      
      # Validate inputs
      if (is.null(selectedSites_d()) || length(selectedSites_d()) == 0) {
        hide_spinner('hf-stats')
        return(data.frame())
      }
      
      if (is.null(dateRange()) || is.null(dateRange()$min) || is.null(dateRange()$max)) {
        hide_spinner('hf-stats')
        return(data.frame())
      }
      
      if (is.null(param()) || nrow(param()) == 0) {
        hide_spinner('hf-stats')
        return(data.frame())
      }
      
      if (is.null(df) || is.null(df$`10min`) || nrow(df$`10min`) == 0) {
        hide_spinner('hf-stats')
        return(data.frame())
      }
      
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
      ) %>% 
        # Group by sites and data_type
      group_by(
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
      ) %>% pivot_longer(
        everything(),
        names_to = c('.value', 'Stats'),
        names_pattern = '^([A-Z]*)_(.*)'
        # Capitalize the stats name
      ) %>% mutate(Stats = str_to_title(Stats))
      
      # Hide spinner
      hide_spinner('hf-stats')
      # Return stats table
      return(stats)
      
    }, error = function(e) {
      runjs(paste0("console.error('High frequency statsData error: ", e$message, "');"))
      # Hide spinner on error
      hide_spinner('hf-stats')
      # Return empty data frame
      return(data.frame())
    })
  })
  
  # Render the stats tables in the modal
  output$sensorStats <- renderStatsTables({
    tryCatch({
      # Render the stats tables
      renderStatsTables(
        elements = unique(sites$catchment),
        data = statsData(),
        sites = sites,
        tableFunction = createSensorStatsTable
      )
    }, error = function(e) {
      # Log error for debugging but don't crash the app
      runjs(paste0("console.error('High frequency stats table rendering error: ", e$message, "');"))
      # Return a simple error message instead of crashing
      tags$p("Error rendering stats table. Please try adjusting date range.")
    })
  })
  
  # Create an observeEvent that react to the show stats button
  observeEvent(input$showStats, ignoreInit = TRUE, {
    tryCatch({
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
    }, error = function(e) {
      runjs(paste0("console.error('High frequency showStats modal error: ", e$message, "');"))
    })
  })
  
  
  
  
  
  ## Update dateRange with plot brushing and double click logic ###################
  
  # Create a dedicated reactive expression for high frequency brushing with robust error handling
  # This is separate from grab samples to avoid interfering with working functionality
  updateDateRangeHighFreq <- reactive({
    runjs("console.log('High frequency brushing: Starting execution');")
    
    # Validate brush input exists and has required properties
    if (is.null(input$highfreq_brush)) {
      runjs("console.log('High frequency brushing: No brush input');")
      return(NULL)
    }
    
    runjs("console.log('High frequency brushing: Brush input exists');")
    
    # Check if brush coordinates are valid numbers
    if (is.null(input$highfreq_brush$xmin) || is.null(input$highfreq_brush$xmax)) {
      runjs("console.log('High frequency brushing: Missing brush coordinates');")
      return(NULL)
    }
    
    runjs(paste0("console.log('High frequency brushing: Coordinates - xmin:", input$highfreq_brush$xmin, "xmax:", input$highfreq_brush$xmax, "');"))
    
    # Validate that brush coordinates are numeric
    if (!is.numeric(input$highfreq_brush$xmin) || !is.numeric(input$highfreq_brush$xmax)) {
      runjs("console.log('High frequency brushing: Non-numeric coordinates');")
      return(NULL)
    }
    
    # Check for infinite or NaN values
    if (!is.finite(input$highfreq_brush$xmin) || !is.finite(input$highfreq_brush$xmax)) {
      runjs("console.log('High frequency brushing: Infinite or NaN coordinates');")
      return(NULL)
    }
    
    # Check for extremely large or small values that might cause issues
    if (abs(input$highfreq_brush$xmin) > 1e10 || abs(input$highfreq_brush$xmax) > 1e10) {
      runjs("console.log('High frequency brushing: Extremely large coordinates');")
      return(NULL)
    }
    
    runjs("console.log('High frequency brushing: Coordinates validated, converting to dates');")
    
    # Try to convert brush coordinates to dates with error handling
    tryCatch({
      # Convert number to date using the Linux epoch time as origin
      minDate <- as.Date(as.POSIXct(input$highfreq_brush$xmin, origin = "1970-01-01", tz = "GMT"))
      maxDate <- as.Date(as.POSIXct(input$highfreq_brush$xmax, origin = "1970-01-01", tz = "GMT"))
      
      runjs(paste0("console.log('High frequency brushing: Converted dates - min:", minDate, "max:", maxDate, "');"))
      
      # Validate converted dates are not NA
      if (is.na(minDate) || is.na(maxDate)) {
        runjs("console.log('High frequency brushing: NA dates after conversion');")
        return(NULL)
      }
      
      # Ensure min is less than max
      if (minDate >= maxDate) {
        runjs("console.log('High frequency brushing: Invalid date range (min >= max)');")
        return(NULL)
      }
      
      # Check if dates are within reasonable bounds (not too far in past/future)
      currentDate <- Sys.Date()
      if (minDate < as.Date("1900-01-01") || maxDate > (currentDate + 365*10)) {
        runjs("console.log('High frequency brushing: Dates outside reasonable bounds');")
        return(NULL)
      }
      
      runjs("console.log('High frequency brushing: Returning valid date range');")
      
      # Return valid date range
      return(list(
        'min' = minDate,
        'max' = maxDate
      ))
    }, error = function(e) {
      # Log error for debugging but don't crash
      runjs(paste0("console.error('High frequency brushing date conversion error: ", e$message, "');"))
      return(NULL)
    })
  })
  
  # Create a dedicated reactive value for high frequency date range reset
  resetDateRangeHighFreq <- reactiveVal(NULL)
  
  # Create an observe event that react on plot double click to reset the date range
  # With error handling to prevent crashes
  observeEvent(input$highfreq_dblclick, {
    tryCatch({
      if (is.null(resetDateRangeHighFreq())) {
        resetDateRangeHighFreq(1)
      } else {
        resetDateRangeHighFreq(resetDateRangeHighFreq() + 1)
      }
    }, error = function(e) {
      # Log error for debugging but don't crash
      runjs(paste0("console.error('High frequency double-click reset error: ", e$message, "');"))
    })
  })
  
  # Return the new dateRange values and date range reset trigger in order to update the outer module dateRangeInput
  # Ensure we always return valid reactive expressions
  return(list(
    'update' = updateDateRangeHighFreq,
    'reset' = resetDateRangeHighFreq
  ))
}
