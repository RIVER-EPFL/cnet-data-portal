## Regroups all plotting functions used with ggplot2

## Plotting helpers ###############################################################

# Function to calculate Y-axis limits with adjustable margin
calculateYaxisLimits <- function(min_val, max_val, perc = 0.1) {
  if (min_val == max_val) {
    axisMargin <- min_val * perc
    return(c(min_val - axisMargin, max_val + axisMargin))
  }
  axisMargin <- (max_val - min_val) * perc
  return(c(min_val - axisMargin, max_val + axisMargin))
}

# Function to assign colors to sites, ensuring all sites have a color
siteColors <- function(df, sitesTable) {
  unique_sites <- unique(df$Site_ID)
  colors <- sitesTable %>% 
    filter(name %in% unique_sites) %>% 
    select(name, color) %>% 
    deframe()
  
  # Handle sites without a predefined color
  missing_sites <- setdiff(unique_sites, names(colors))
  if(length(missing_sites) > 0){
    # Generate distinct colors for missing sites
    default_colors <- RColorBrewer::brewer.pal(min(length(missing_sites), 12), "Set3")
    names(default_colors) <- missing_sites
    colors <- c(colors, default_colors)
  }
  
  return(colors)
}

## Plot types #####################################################################

# Basic scatter plot with LOESS curve
basicPlot <- function(df, x, param, plotTitle, sites) {
  # Ensure x is a symbol for ggplot2
  x_sym <- sym(x)
  
  # Create the plot
  p <- ggplot(df, aes(!!x_sym, value, color = Site_ID, linetype = parameter, shape = parameter)) +
    geom_point(size = 2, na.rm = TRUE) +
    # Replace geom_line with geom_smooth for LOESS
    geom_smooth(method = "loess", formula = y ~ x, 
                size = 1.2, alpha = 0.5, se = FALSE) +
    ggtitle(plotTitle) +
    ylab(paste0(param$param_name, ' [', param$units, ']')) +
    xlab('Date') +
    # Set color of the data groups and customize legend
    scale_color_manual(
      values = siteColors(df, sites),
      guide = guide_legend(override.aes = list(
        linetype = rep(0, length(unique(df$Site_ID))),
        shape = rep(16, length(unique(df$Site_ID))) # Ensure points are shown correctly
      ))
    ) +
    # Change the linetype legend label to 'LOESS curve'
    scale_linetype_manual(values = c("LOESS" = 1), labels = 'LOESS curve') +
    # Set the y axis limits
    scale_y_continuous(limits = calculateYaxisLimits(
      min(df$value, na.rm = TRUE), 
      max(df$value, na.rm = TRUE)
    )) +
    # Set theme
    theme_bw() +
    # Customize theme elements
    theme(
      plot.title = element_text(size = 16, face = 'bold'),
      legend.position = "bottom", 
      legend.title = element_blank(), 
      legend.text = element_text(size = 10),
      axis.title = element_text(size = 14), 
      axis.text.x = element_text(size = 10), 
      axis.text.y = element_text(size = 11)
    ) +
    # Remove the shape legend
    guides(shape = 'none', linetype = 'none')
  
  return(p)
}

# Scatter plot with LOESS and error bars
sdPlot <- function(df, x, param, plotTitle, sites) {
  # Create a basicPlot
  p <- basicPlot(df, x, param, plotTitle, sites)
  
  # Add error bars using the standard deviation
  p <- p + 
    geom_errorbar(aes(ymin = value - !!sym(param$sd), ymax = value + !!sym(param$sd)),
                  width = 0.2, alpha = 0.7)
  
  return(p)
}

# Scatter plot with LOESS and min/max values
minMaxPlot <- function(df, x, param, plotTitle, sites) {
  # Create a basicPlot
  p <- basicPlot(df, x, param, plotTitle, sites)
  
  p <- p + 
    # Define the linetypes: only average has a LOESS curve
    scale_linetype_manual(values = c("average" = 1, "min" = 0, "max" = 0), labels = c("Average", "Min", "Max")) +
    # Define the point shapes for average, min, and max
    scale_shape_manual(values = c("average" = 16, "min" = 1, "max" = 0), labels = c("Average", "Min", "Max")) +
    # Display the shape legend
    guides(linetype = 'none') # Remove linetype from legend as it's not needed for average only
  
  return(p)
}

# Multi-parameter scatter plot with LOESS curves
multiPlot <- function(df, x, param, plotTitle, sites) {
  # Create a basicPlot
  p <- basicPlot(df, x, param, plotTitle, sites)
  
  p <- p +
    # Define different linetypes for multiple parameters
    scale_linetype_manual(values = c("Param1" = 1, "Param2" = 2, "Param3" = 3), name = 'Parameter') +
    # Define different shapes for multiple parameters
    scale_shape_manual(values = c("Param1" = 16, "Param2" = 15, "Param3" = 17), name = 'Parameter') +
    # Display the shape legend
    guides(linetype = 'none') # Optionally hide linetype if not needed
  
  return(p)
}

# Time series plot using a specified plotting function
timeSeriePlot <- function(df, x, parameter, siteName, sites) {
  # Recover plotting function using string
  plottingFunc <- match.fun(parameter$plot_func)
  
  # Create plot title
  plotTitle <- paste0(siteName, ' Time Series')
  
  # Create plot using the plotting function
  p <- plottingFunc(df, x, parameter, plotTitle, sites)
  
  # Set the x axis as datetime with monthly minor breaks
  p <- p + scale_x_datetime(date_minor_breaks = '1 month')
  
  return(p)
}

# Day of Year (DOY) time series plot using a specified plotting function
DOYPlot <- function(df, x, parameter, siteName, sites) {
  # Recover plotting function using string
  plottingFunc <- match.fun(parameter$plot_func)
  
  # Create plot title
  plotTitle <- paste0(siteName, ' DOY Series')
  
  # Create plot using the plotting function
  p <- plottingFunc(df, x, parameter, plotTitle, sites)
  
  # Set the x axis as datetime with monthly breaks and specific limits
  p <- p + scale_x_datetime(
    date_breaks = '1 month',
    date_labels = '%b',
    limits = c(ymd_hms('2020-01-01 00:00:00 GMT'), ymd_hms('2020-12-10 00:00:00 GMT'))
  )
  
  return(p)
}

# High-frequency time series plot with optional modeled data
highFreqTimeSeriePlot <- function(df, parameter, plotTitle, sites, modeledData = FALSE) {
  # Create plot base
  p <- ggplot(df, aes(Date, value, color = Site_ID))
  
  # If df contains modeled data, add additional aesthetics
  if (modeledData) {
    p <- p + 
      geom_line(aes(linetype = data_type, alpha = data_type), size = 1, na.rm = TRUE) +
      geom_point(aes(size = singlePoint, shape = data_type, alpha = data_type), na.rm = TRUE) +
      scale_size_manual(values = c('0' = 0, '1' = 1.5), guide = 'none') +
      scale_shape_manual(values = c('measured' = 16, 'modeled' = 15)) +
      scale_linetype_manual(values = c('measured' = 1, 'modeled' = 2)) +
      scale_alpha_manual(values = c('measured' = 1, 'modeled' = 0.3))
  } else {
    p <- p + 
      geom_line(size = 1, na.rm = TRUE)
  }
  
  # Finished plot construction
  p <- p + 
    ggtitle(plotTitle) +
    ylab(paste0(parameter$param_name, ' [', parameter$units, ']')) +
    xlab('Date') +
    # Set the y axis limits
    scale_y_continuous(limits = calculateYaxisLimits(
      min(df$value, na.rm = TRUE), 
      max(df$value, na.rm = TRUE)
    )) +
    # Set color of the data groups
    scale_color_manual(values = siteColors(df, sites)) +
    # Set theme
    theme_bw() +
    # Customize theme elements
    theme(
      plot.title = element_text(size = 16, face = 'bold'),
      legend.position = "bottom", 
      legend.title = element_blank(), 
      legend.text = element_text(size = 10),
      axis.title = element_text(size = 14), 
      axis.text.x = element_text(size = 10), 
      axis.text.y = element_text(size = 11)
    )
  
  return(p)
}

# Function to add grab sample points to a high-frequency time series plot
addGrabSamplePoints <- function(p, df, minHf, maxHf) {
  # Add the points to the graph
  p <- p + 
    geom_point(data = df, mapping = aes(x = DATETIME_GMT, y = value), size = 3, color = 'black') +
    # Correct the y scale to display all the information
    scale_y_continuous(limits = calculateYaxisLimits(
      min(min(df$value, na.rm = TRUE), minHf),
      max(max(df$value, na.rm = TRUE), maxHf)
    ))
  
  return(p)
}

# One vs One plot with linear regression
onVsOnePlot <- function(df, x, y, parameterX, parameterY, plotTitle, color) {
  # Set axis labels with units if available
  xLabsUnits <- if(!is.null(parameterX$units)) paste0(' [', parameterX$units, ']') else ''
  yLabsUnits <- if(!is.null(parameterY$units)) paste0(' [', parameterY$units, ']') else ''
  
  # Create the plot
  p <- ggplot(df, aes(!!sym(x), !!sym(y))) +
    # Plot the linear model confidence interval
    geom_smooth(method = "lm", fill = color, alpha = 0.17, linetype = 0, na.rm = TRUE, fullrange = TRUE) +
    # Plot the linear model line
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, size = 1.5, color = color, alpha = 0.5, fullrange = TRUE) +
    # Plot the data points
    geom_point(size = 2.5, color = color, na.rm = TRUE) +
    # Add the linear model equation and R-squared
    annotate('text', -Inf, Inf, hjust = -0.1, vjust = 1.5,
             label = lm_eqn(df, x, y), parse = TRUE, size = 5) +
    # Set plot title and axis labels
    ggtitle(plotTitle) +
    ylab(paste0(parameterY$param_name, yLabsUnits)) +
    xlab(paste0(parameterX$param_name, xLabsUnits)) +
    # Set theme
    theme_bw() +
    # Customize theme elements
    theme(
      plot.title = element_text(size = 16, face = 'bold'),
      legend.position = "bottom", 
      legend.title = element_blank(), 
      legend.text = element_text(size = 10),
      axis.title = element_text(size = 14), 
      axis.text.x = element_text(size = 10), 
      axis.text.y = element_text(size = 11)
    )
  
  return(p)
}

# Function to add a one-to-one line to a one vs one plot
addOneToOneLine <- function(p, minData, maxData) {
  # Add the one-to-one line
  p <- p + 
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
    # Set the plot limits with adequate margins
    xlim(calculateYaxisLimits(minData, maxData, perc = 0.5)) +
    ylim(calculateYaxisLimits(minData, maxData, perc = 0.5)) +
    # Set the visible area of the plot
    coord_cartesian(
      xlim = calculateYaxisLimits(minData, maxData),
      ylim = calculateYaxisLimits(minData, maxData)
    )
  
  return(p)
}

# Function to highlight a subset of data in a one vs one plot
highlightDataSubset <- function(p, color, data, x, y, ...) {
  # Filter the subset of data
  dataSubset <- data %>% filter(...)
  
  # Add the subset to the plot with its own regression line and annotations
  p <- p + 
    geom_smooth(data = dataSubset, method = "lm", fill = color, alpha = 0.17, linetype = 0, na.rm = TRUE, fullrange = TRUE) +
    geom_smooth(data = dataSubset, method = "lm", formula = y ~ x, se = FALSE, size = 1.5, color = color, alpha = 0.5, fullrange = TRUE) +
    geom_point(data = dataSubset, size = 2.5, color = color, na.rm = TRUE) +
    annotate('text', -Inf, Inf, hjust = -0.1, vjust = 2.5,
             label = lm_eqn(dataSubset, x, y), parse = TRUE, size = 5, color = color)
  
  return(p)
}

# Function to plot distribution with comparison to a specific row
plotDistribution <- function(distribution, row, distColumn, rowColumn, plotTitle) {
  # Create the plot with distribution points and LOESS curve
  p <- ggplot(distribution, aes(DATETIME_month_day_time_GMT, !!sym(distColumn))) +
    geom_point(size = 2, na.rm = TRUE, color = 'black') +
    geom_smooth(method = "loess", formula = y ~ x, 
                color = 'black', size = 1.2, alpha = 0.5, se = FALSE) +
    # Add new point from the row data
    geom_point(data = row, aes(x = DATETIME_month_day_time_GMT, y = !!sym(rowColumn)), 
               size = 3, color = '#e24727') +
    ggtitle(plotTitle) +
    ylab(distColumn) +
    xlab('Date') +
    # Set the y axis limits
    scale_y_continuous(limits = calculateYaxisLimits(
      min(
        pull(distribution, distColumn),
        pull(row, rowColumn),
        na.rm = TRUE
      ),
      max(
        pull(distribution, distColumn),
        pull(row, rowColumn),
        na.rm = TRUE
      )
    )) +
    # Set theme
    theme_bw() +
    # Customize theme elements
    theme(
      plot.title = element_text(size = 16, face = 'bold'),
      legend.position = "bottom", 
      legend.title = element_blank(), 
      legend.text = element_text(size = 10),
      axis.title = element_text(size = 14), 
      axis.text.x = element_text(size = 10), 
      axis.text.y = element_text(size = 11)
    )
  
  return(p)
}
