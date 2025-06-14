# R version 4.0.2 (2020-06-22)
# Platform: x86_64-apple-darwin17.0 (64-bit)
# Running under: macOS Catalina 10.15.7
#
# Matrix products: default
# BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
# LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib
#
# locale:
#   [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
#
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base
#
# other attached packages:
#   [1] RMySQL_0.10.20        bigleaf_0.7.1         sodium_1.1            purrr_0.3.4
#   [5] readr_1.4.0           sass_0.3.1            future_1.20.1         promises_1.1.1
#   [9] rhandsontable_0.3.7   DT_0.16               dbplyr_1.4.4          pool_0.1.4.3
#  [13] DBI_1.1.0             dplyr_1.0.2           magrittr_1.5          tidyr_1.1.2
#  [17] forcats_0.5.0         lubridate_1.7.9       data.table_1.13.2     Cairo_1.5-12.2
#  [21] ggplot2_3.3.2         stringr_1.4.0         jsonlite_1.7.1        shinycssloaders_1.0.0
#  [25] shinybusy_0.2.2       shinyWidgets_0.5.4    shinyjs_2.0.0         shiny_1.5.0
#
# loaded via a namespace (and not attached):
#   [1] pkgload_1.1.0     splines_4.0.2     assertthat_0.2.1  blob_1.2.1        yaml_2.2.1
#   [6] robustbase_0.93-6 globals_0.13.1    backports_1.1.8   pillar_1.4.4      lattice_0.20-41
#  [11] glue_1.4.1        digest_0.6.25     colorspace_1.4-1  htmltools_0.5.1.1 httpuv_1.5.4
#  [16] Matrix_1.2-18     pkgconfig_2.0.3   listenv_0.8.0     xtable_1.8-4      scales_1.1.1
#  [21] processx_3.4.2    later_1.1.0.1     tibble_3.0.1      mgcv_1.8-31       generics_0.0.2
#  [26] farver_2.0.3      ellipsis_0.3.1    withr_2.2.0       solartime_0.0.1   cli_2.0.2
#  [31] crayon_1.3.4      mime_0.9          ps_1.3.3          fs_1.4.2          fansi_0.4.1
#  [36] parallelly_1.21.0 nlme_3.1-148      rsconnect_0.8.16  tools_4.0.2       hms_0.5.3
#  [41] lifecycle_0.2.0   munsell_0.5.0     packrat_0.5.0     compiler_4.0.2    rlang_0.4.8
#  [46] grid_4.0.2        rstudioapi_0.11   htmlwidgets_1.5.1 crosstalk_1.1.0.1 labeling_0.3
#  [51] testthat_2.3.2    gtable_0.3.0      codetools_0.2-16  R6_2.4.1          fastmap_1.0.1
#  [56] rprojroot_1.3-2   desc_1.2.0        stringi_1.4.6     parallel_4.0.2    Rcpp_1.0.4.6
#  [61] vctrs_0.3.4       DEoptimR_1.0-8    tidyselect_1.1.0


## Load Libraries #################################################################

# Function to safely check and load packages
safe_require <- function(packages) {
  missing_packages <- c()
  loaded_packages <- c()
  
  for (pkg in packages) {
    # First check if package is installed
    if (!requireNamespace(pkg, quietly = TRUE)) {
      missing_packages <- c(missing_packages, pkg)
    } else {
      # Package is installed, try to load it
      if (require(pkg, character.only = TRUE, quietly = TRUE)) {
        loaded_packages <- c(loaded_packages, pkg)
      } else {
        missing_packages <- c(missing_packages, pkg)
      }
    }
  }
  
  if (length(missing_packages) > 0) {
    return(FALSE)
  }
  return(TRUE)
}

# Check for required packages for discharge tool
required_packages <- c("pracma", "gridExtra", "signal")
discharge_packages_available <- safe_require(required_packages)

# Make it explicitly global
assign("discharge_packages_available", discharge_packages_available, envir = .GlobalEnv)

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinybusy)
library(shinycssloaders)
library(jsonlite)
library(stringr)
library(ggplot2)
library(Cairo)
library(data.table)
library(lubridate)
library(forcats)
library(tidyr)
library(magrittr)
library(dplyr)
library(DBI)
library(pool)
library(dbplyr)
library(DT)
library(rhandsontable)
library(promises)
library(future)
library(sass)
library(readr)
library(purrr)
library(sodium)
library(bigleaf)
library(RMySQL)

# Discharge packages are already loaded by safe_require if available

# Set locale to utf-8
Sys.setlocale('LC_ALL', 'en_US.UTF-8')

## Load Secret Global Variables ###################################################
source('./secrets.R')

## Load helper functions ##########################################################
source('./utils/helper_functions.R')


## Development environment specific ##########################################################

if (ENV == 'development') {
  # Remove old files
  file.remove(list.files('./www', pattern = 'main.css|metalpdataportal.js', full.names = TRUE))

  # Compile CSS from Sass
  sass::sass(
    sass::sass_file('assets/sass/main.scss'),
    output = 'www/main.css',
    options = sass::sass_options(output_style = 'compressed')
  )
  cssfile <- 'main.css'

  # Compile and minify JavaScript
  jsfile <- js_parser()

  # Create file with assets name
  readr::write_file(paste(
    paste0('STYLESHEET_FILE <- "', cssfile, '"'),
    paste0('JAVASCRIPT_FILE <- "', jsfile, '"'),
    sep = '\n'
  ), file = './assets_name.R')

  # Plan future strategy
  # Use multisession because the development version will run mainly within Rstudio
  plan(multisession)
} else {
  # Plan future strategy
  # Use multicore on the production server
  plan(multicore)
}

# Load env variables containing assets name
source('./assets_name.R')



## Load data ######################################################################

# Load data loading functions
source('./utils/data_preprocessing.R')

hfDf <- loadHighFreqDf()



## Connect to DB ##################################################################

# Load BD interactions functions
source('./utils/db_interaction.R')

# Create pool connection with the DB
pool <- connectToDB()



## Load Shiny extensions functions ################################################
source('./utils/shiny_extensions.R')



## Load tabs modules ##############################################################
source('./modules/login/login.R')
source('./modules/visualisation_tab/visualisation_tab.R')
source('./modules/download_tab/download_tab.R')
source('./modules/tools_tab/tools_tab.R')
source('./modules/data_management_tab/data_management_tab.R')
source('./modules/field_status_tab/field_status_tab.R')
source('./modules/portal_management/portal_management.R')
source('./modules/data_requests_management/data_requests_management.R')
source('./modules/editableDT/editableDT.R')
source('./modules/instruction_panel/intruction_panel.R')




## Create main UI #################################################################

ui <- tagList(
  # Load shinyjs
  useShinyjs(),
  # Load htmlwidgets.js at startup to avoid reference error problem in Firefox
  # when the datatables are loaded dynamically
  htmltools:: htmlDependency("htmlwidgets", packageVersion("htmlwidgets"),
                 src = system.file("www", package="htmlwidgets"),
                 script = "htmlwidgets.js"
  ),
  # Add stylesheet link and script tags to head
  tags$head(
    # Add link to main.css stylesheet
    tags$link(href = STYLESHEET_FILE, rel = 'stylesheet', type = 'text/css'),
    # Add link for js script
    tags$script(src = JAVASCRIPT_FILE)
  ),
  # Create the navbarPage using custom function to add a content-wrapper (defined in './utils/shiny_extensions.R')
  navbarPageWithWrapper(
    # Create Navabar page with login
    withLoginAction(
      # Pass in the output of shiny navbarPage()
      navbarPage(
        id = 'main-nav',
        # Load the custom logo for the navbar title
        htmlTemplate('./html_components/logo.html'),
        # Set a window browser window title
        windowTitle = 'C-NET PORTAL',
        # Create the about tab
        tabPanel(
          # Create a tab title with an icon
          tags$span(icon('campground'),tags$span('About', class = 'navbar-menu-name')),
          htmlTemplate(
            './html_components/about.html',
            dlTabLink = actionLink('aboutDlLink', 'Download tab'),
            extLinkIcon = icon('external-link-alt', class = 'ext-link')
          ),
          value = 'aboutTab'
        ),
        # Create the visualisation tab
        tabPanel(
          # Create a tab title with an icon
          tags$span(icon('chart-bar'),tags$span('Visualisation', class = 'navbar-menu-name')),
          # Load the visualisationTab module UI elements
          visualisationTabUI('visu', pool, hfDf),
          value = 'visuTab'
        ),
        # Create the download tab
        tabPanel(
          # Create a tab title with an icon
          tags$span(icon('download'),tags$span('Download', class = 'navbar-menu-name')),
          downloadTabUI(
            'dl',
            pool = pool,
            hfDfMinMaxDates = list(
              min = min(date(hfDf$`10min`$Date), na.rm = TRUE),
              max = max(date(hfDf$`10min`$Date), na.rm = TRUE)
            )
          ),
          value = 'dlTab'
        )
      ),
      # Add the login module UI
      loginUI('login')
    ),
    # Add footer to navbarPageWithWrapper
    footer = htmlTemplate(
      'html_components/footer.html',
      aboutLink = actionLink('aboutLink', 'About'),
      visuLink = actionLink('visuLink', 'Visualisation'),
      dlLink = actionLink('dlLink', 'Download'),
      creditsLink = actionLink('credits', 'Credits & Source code'),
      githubIcon = icon('github'),
      linkedinIcon = icon('linkedin'),
      twitterIcon = icon('twitter')
    )
  )
)



## Create server function #########################################################

server <- function(input, output, session) {

  ## Open specific tab when accessing the app #####################################
  isolate({
    if ('tab' %in% names(getQueryString())) {
      updateNavbarPage(session, 'main-nav', selected = getQueryString()$tab)
    }
  })


  ## Set maximum request size big enough for sensor data upload ###################
  # Set it to 200MB
  options(shiny.maxRequestSize=200*1024^2)




  ## Load login module server logic ###############################################
  user <- callModule(login, 'login', pool)


  ## Load visualisationTab module server logic ####################################
  callModule(visualisationTab, 'visu', pool, user, hfDf)



  ## Load downloadTab module server logic #########################################
  callModule(downloadTab, 'dl', pool, user, hfDf)



  ## Footer Navigation logic ######################################################

  observeEvent(input$aboutLink, ignoreInit = TRUE, updateNavbarPage(session, 'main-nav', selected = 'aboutTab'))

  observeEvent(input$visuLink, ignoreInit = TRUE, updateNavbarPage(session, 'main-nav', selected = 'visuTab'))

  observeEvent(input$dlLink | input$aboutDlLink, ignoreInit = TRUE, {
    req(input$dlLink != 0 | input$aboutDlLink != 0)
    updateNavbarPage(session, 'main-nav', selected = 'dlTab')
  })

  observeEvent(input$credits, ignoreInit = TRUE, {
    showModal(modalDialog(
      htmlTemplate(
        './html_components/credits.html',
        githubIcon = icon('github'),
        linkedinIcon = icon('linkedin'),
        twitterIcon = icon('twitter')
      ),
      footer = modalButtonWithClass('Close', class = 'custom-style'),
      easyClose = TRUE
    ))
  })




  ## Check authorizations #########################################################

  # Do it when the user role changes
  observeEvent(user$role, {
    if (user$role %in% c('intern', 'sber', 'admin')) {
      ## Generate toolsTab #################################################

      # Create the data management tab
      appendTab(
        'main-nav',
        tabPanel(
          # Create a tab title with an icon
          tags$span(icon('tools'),tags$span('Tools', class = 'navbar-menu-name')),
          toolsTabUI('toolsTab', pool),
          value = 'toolsTab'
        )
      )

      # Load data management server logic
      callModule(toolsTab, 'toolsTab', pool, user$role)

      # Add link to footer
      insertUI(
        selector = '#footer-nav-insertion',
        where = 'beforeBegin',
        ui = tags$li(actionLink('toolsLink', 'Tools')),
        immediate = TRUE
      )

      # Add nav update logic
      observeEvent(input$toolsLink, ignoreInit = TRUE, updateNavbarPage(session, 'main-nav', selected = 'toolsTab'))
    }




    if (user$role %in% c('sber', 'admin')) {
      ## Generate dataManagementTab #################################################

      # Create the data management tab
      appendTab(
        'main-nav',
        tabPanel(
          # Create a tab title with an icon
          tags$span(icon('database'),tags$span('Data', class = 'navbar-menu-name')),
          dataManagementTabUI('data', pool, user$role),
          value = 'dataTab'
        )
      )

      # Load data management server logic
      callModule(dataManagementTab, 'data', pool, user$role)

      # Add link to footer
      insertUI(
        selector = '#footer-nav-insertion',
        where = 'beforeBegin',
        ui = tags$li(actionLink('dataLink', 'Data')),
        immediate = TRUE
      )

      # Add nav update logic
      observeEvent(input$dataLink, ignoreInit = TRUE, updateNavbarPage(session, 'main-nav', selected = 'dataTab'))



      ## Generate fieldStatusTab #################################################

      # Create the data management tab
      appendTab(
        'main-nav',
        tabPanel(
          # Create a tab title with an icon
          tags$span(icon('map'),tags$span('Field status', class = 'navbar-menu-name')),
          fieldStatusTabUI('fieldStatusTab', pool),
          value = 'fieldStatusTab'
        )
      )

      # Load data management server logic
      callModule(fieldStatusTab, 'fieldStatusTab', pool)

      # Add link to footer
      insertUI(
        selector = '#footer-nav-insertion',
        where = 'beforeBegin',
        ui = tags$li(actionLink('fieldStatusLink', 'Field status')),
        immediate = TRUE
      )

      # Add nav update logic
      observeEvent(input$fieldStatusLink, ignoreInit = TRUE, updateNavbarPage(session, 'main-nav', selected = 'fieldStatusTab'))



      ## Generate data request tab ##########################################################

      # Get the UI
      requestUI <- requestsManagementUI('requests', pool)

      # Insert the link into navbar
      insertUI('#login-ui', 'beforeBegin', requestUI$navbarUI, immediate = TRUE)

      # Add data requests tab
      appendTab(
        'main-nav',
        tabPanel(
          # Create a tab title with an icon
          'Data Requests',
          requestUI$tabContent,
          value = 'requestsTab'
        )
      )

      # Call request tab module
      callModule(requestsManagement, 'requests',
                 pool = pool,
                 navbarSession = session,
                 navbarId = 'main-nav',
                 requestTabId = 'requestsTab')
    }


    if (user$role == 'admin') {
      ## Generate portalManagement tab ##########################################################

      # Create users tab
      insertTab(
        'main-nav',
        tabPanel(
          # Create a tab title with an icon
          tags$span(icon('empire'), tags$span('Portal', class = 'navbar-menu-name')),
          portalManagementUI('portal', pool),
          value = 'portalTab'
        ),
        target = 'requestsTab',
        position = 'before'
      )

      # Load users tab server logic
      callModule(portalManagement, 'portal', pool)

      # Add link to footer
      insertUI(
        selector = '#footer-nav-insertion',
        where = 'beforeBegin',
        ui = tags$li(actionLink('portalLink', 'Portal')),
        immediate = TRUE
      )

      # Add nav update logic
      observeEvent(input$portalLink, ignoreInit = TRUE, updateNavbarPage(session, 'main-nav', selected = 'portalTab'))
    }
  })
}



## Launch App #####################################################################

shinyApp(ui, server, onStart = function() {
  cat("Doing application setup\n")

  onStop(function() {
    cat("Doing application cleanup\n")
    poolClose(pool)
  })
})
