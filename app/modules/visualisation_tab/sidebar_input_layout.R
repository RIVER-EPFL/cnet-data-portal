## This module contains the UI and server code for the show/hide sidebar input layout
## with a dynamic interface to add and remove units of an inner module for visualisation

## Create module UI function ######################################################

sidebarInputLayoutUI <- function(id, minDate, maxDate, innerModuleUI, ...) {
# Create the UI for the sidebarInputLayout module
# Parameters:
#  - id: String, the module id
#  - minDate: Date, the lower bound for the dateRangeInput
#  - maxDate: Date, the upper bound for the dateRangeInput
#  - innerModuleUI: Function, the inner module UI function
#  - ...: All other arguments needed by the inner module function
# 
# Returns a div containing the layout
  
  # Create namespace
  ns <- NS(id)

  # Create the first unit UI elements of the innerModule
  innerModuleUIList <- innerModuleUI(ns('1'), ...)
  
  # Create the module layout
  div(
    # Add JavaScript for console logging
    tags$script("
      Shiny.addCustomMessageHandler('console-log', function(message) {
        console.log(message);
      });
    "),
    # First div containing the global inputs
    div(
      class = 'main-inputs',
      # Date Range to select the global dateRange
      dateRangeInput(ns('time'), 'Date range:',
                     start = "2024-05-15",
                     end = "2025-12-31",
                     min = minDate,
                     max = "2025-12-31",
                     format = 'dd/mm/yyyy',
                     separator = '-'),
      # Create a nutton to reset the date range
      actionButton(ns('resetDateRange'), 'Reset Date', class = 'custom-style'),
      # div grouping main actions to the left
      div(
        class = 'main-actions',
        # Button group containing the global actions
        div(
          class = 'btn-group',
          # Button to set plot width
          actionButton(ns('plotWidth'), 'Plot Width', class = 'custom-style'),
          # Button to toggle sidebar visibility
          actionButton(ns('toggleSidebar'), 'Hide inputs', class = 'custom-style'),
          # Button to add an additional unit
          actionButton(ns('addUnit'), 'Add Unit', class = 'custom-style'),
          # Button to remove an unit
          # Disable by default because at least one unit is displayed
          disabled(
            actionButton(ns('removeUnit'), 'Remove Unit', class = 'custom-style')
          )
        ),
        # Create an icon button that trigger a modal to display the global help
        actionButton(ns('help'), 'Help', class = 'custom-style custom-style--primary')
      )
    ),
    # Create the sidebarLayout
    sidebarLayout(
      # Create a sidebar with the innerModule first unit input UI elements inside
      sidebarPanel(
        id = ns('sidebar-inputs'),
        innerModuleUIList$inputs,
        width = 3
      ),
      # Create the main panel with the innerModule first unit plot UI elements inside
      mainPanel(
        id = ns('main-plots'),
        innerModuleUIList$plots,
        width = 9
      )
    )
  )
}



## Create module server function ##################################################

sidebarInputLayout <- function(input, output, session,
                               innerModule, innerModuleUI, innerModulePrefixIds,
                               minDate, maxDate, df = NULL,
                               plotDateRangeSelection = TRUE, ...) {
# Create the logic for the sidebarInputLayout module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - innerModule: Function, the inner module server function
#  - innerModuleUI: Function, the inner module UI function
#  - innerModulePrefixIds: Named list containing:
#                            + inputs: String, the prefix for the inner module inputs UI element
#                            + plots: String, the prefix for the inner module plots UI element
#  - df: Data.frame, the data to pass to the inner module, default NULL
#  - plotDateRangeSelection: Boolean, indicates if the inner module plots are modifying the date range, default TRUE
#  - minDate: Date, the lower bound for the dateRangeInput
#  - maxDate: Date, the upper bound for the dateRangeInput
#  - ...: All other arguments needed by the inner module function
# 
# Returns NULL
    
  ## DateRange input logic ########################################################
  
  # Create dateRange reactive expression containing the min and max values of the dateRangeInput
  dateRange <- reactive(list('min' = input$time[1], 'max' = input$time[2]))
  
  # Create an observeEvent that allows to reset the date range when resetDateRange is clicked
  observeEvent(input$resetDateRange, {
    updateDateRangeInput(session, 'time', start = minDate, end = maxDate)
  })
  
  
  # Create a reactive value that track if the plot are zoomed if the plot can be zoomed
  if (plotDateRangeSelection) zoomed <- reactiveVal(FALSE)
  
  
  
  ## First unit module calling ####################################################

  # Call the inner module
  if (plotDateRangeSelection) {
    callModule(innerModule, '1', dateRange, ...)
  } else {
    callModule(innerModule, '1', df, dateRange, ...)
  }
  
  # Note: Modules now handle date range updates directly, bypassing the return system
  
  
  ## Unit Nb tracking #############################################################
  
  # Create a reactive value that keep track of the number of displayed units
  # One is displayed by default
  unitsNb <- reactiveVal(1)
  
  
  
  ## Unit adding logic ############################################################
  
  # Add an observeEvent that will run upon click on the add unit button
  observeEvent(input$addUnit, {
    # If there is currently one unit enable remove unit button
    if (unitsNb() == 1) enable('removeUnit')
    # Increment the number of units
    unitsNb(unitsNb() + 1)
    
    # Create new unit UI elements
    innerModuleUIList <- innerModuleUI(session$ns(unitsNb()), ...)
    
    # Insert the new unit input UI elements in the sidebar
    insertUI(
      paste0('#', session$ns('sidebar-inputs')), where = 'beforeEnd',
      ui = innerModuleUIList$inputs,
      immediate = TRUE
    )
    
    # Insert the new unit plot UI elements in the main panel
    insertUI(
      paste0('#', session$ns('main-plots')), where = 'beforeEnd',
      ui = innerModuleUIList$plots,
      immediate = TRUE
    )
    
    # Call new unit module function and retrieve, if any, the named list containing:
    #  - update: Reactive expression containing the updated dateRange
    #  - reset: Reactive value, updated each time the plot is double clicked, used as dateRange reset trigger
    if (is.null(df)) {
      callModule(innerModule, "1", dateRange, ...)
    } else {
      callModule(innerModule, "1", df, dateRange, ...)
    }
  })
  
  
  
  ## Unit removing logic ##########################################################
  
  # Add an observeEvent that will run upon click on the remove unit button
  observeEvent(input$removeUnit, {
    # Define last unit inputs and plots ids
    inputsId <- paste0('#', innerModulePrefixIds$inputs, '-', session$ns(unitsNb()))
    plotsId <- paste0('#', innerModulePrefixIds$plots, '-', session$ns(unitsNb()))
    
    # Remove last unit plots
    removeUI(
      plotsId,
      immediate = TRUE
    )
    
    # Remove last unit inputs
    removeUI(
      inputsId,
      immediate = TRUE
    )
    
    # Decrement units nb
    unitsNb(unitsNb() - 1)
    
    # If only one unit left, disable remove unit button
    if (unitsNb() == 1) disable('removeUnit')
  })
  
  
  
  ## Sidebar inputs toggle logic ##################################################
  
  # Create a boolean reactive value that keep track of the sidebar visibility state
  sidebarVisible <- reactiveVal(TRUE)
    
  # Create the observeEvent that react to the sidebar toggling button
  observeEvent(input$toggleSidebar, {
    
    # Invert sidebarVisible value
    sidebarVisible(!sidebarVisible())
    
    # Create JSON message to send to client containing:
    #  - sidebarId: the sidebar id defined in the UI
    #  - mainPanelId: the main panel id defined in the UI
    #  - show: the new state of the sidebar
    messageJSON <- toJSON(list(
      'sidebarId' = session$ns('sidebar-inputs'),
      'mainPanelId' = session$ns('main-plots'),
      'show' = sidebarVisible()
    ), auto_unbox = TRUE)
    
    # Send the shiny custom message to toggle sidebar visibility
    # Linked to some JavaScript defined in './assets/js/sidebar_actions.js'
    session$sendCustomMessage('sidebarToggle', messageJSON)
    
    # Determine correct toggling button label
    newBtnLabel <- 'Show inputs'
    if (sidebarVisible()) newBtnLabel <- 'Hide inputs'
    
    # Update toggling button label
    updateActionButton(session, 'toggleSidebar', label = newBtnLabel)
  })
  
  
  
  
  
  
  ## Plot width logic #############################################################
  
  # Create an observeEvent that react to the plotWidth button
  observeEvent(input$plotWidth, ignoreInit = TRUE, {
    showModal(modalDialog(
      size = 's',
      title = 'Set plot width',
      p("Accepted values are valid CSS units, such as '100%' (default) or '400px'"),
      p('This setting affect all visible plots. Meaning that newly created plot will still by created with the default settings.'),
      p('Setting an empty string will reset to the default value.'),
      textInput(session$ns('newWidth'), 'New width', placeholder = 'Default: 100%'),
      footer = tagList(
        actionButton(session$ns('setWidth'), 'Set width', class = 'custom-style custom-style--primary'),
        modalButtonWithClass('Cancel', class = 'custom-style')
      )
    ))
  })
  
  # Create an observeEvent that react to the setWidth button
  observeEvent(input$setWidth, ignoreInit = TRUE, {
    # Parse new width
    width <- input$newWidth
    if (width == '') width <- '100%'
    
    # Create JSON message
    messageJSON <- toJSON(list(
      'containerId' = session$ns('main-plots'),
      'width' = width
    ), auto_unbox = TRUE)
    
    # Send the shiny custom message to toggle sidebar visibility
    # Linked to some JavaScript defined in './assets/js/custom_plot_width.js'
    session$sendCustomMessage('setPlotWidth', messageJSON)
    
    # Close modal
    removeModal()
  })
  
  
  
  
  
  ## Help modal logic #############################################################
  
  # Create an observeEvent that react to the help button
  observeEvent(input$help, {
    # Create modal with the corresponding htmlOutput
    showModal(modalDialog(
      title = 'Global Help',
      htmlTemplate('./html_components/help_table.html'),
      footer = modalButtonWithClass('Dismiss', class = 'custom-style'),
      easyClose = TRUE
    ))
  })
}
