library(ggplot2)
library(pracma)
library(gridExtra)
library(signal)

data <- read.csv("C:/Users/nicol/Desktop/q_rho_salt.csv")
data$time <- as.POSIXct(data$time, format="%H:%M:%S")   # check if time is defined at "time" or "date". Also check if it has datetime or just time
#data$time <- as.POSIXct(data$time, format="%m/%d/%y %H:%M:%S")

initial_mass_rhodamine_wt <- 3.38019
initial_water_temp_degC <- 3.3


initial_mass_salt <- 2000
slope_conductivity <- 1951.1

distance <- 79
T_ref <- 25
n_rhodamine <- 0.026

concentration_rhodamine_wt <- 23.83 / 100
initial_mass_rhodamine <- initial_mass_rhodamine_wt * concentration_rhodamine_wt * 1000

p1 <- NULL
p2 <- NULL

if ("ppb" %in% names(data)) {
  background_indices <- c(1:15, (nrow(data)-9):nrow(data))
  background_model <- lm(ppb ~ as.numeric(time), data = data[background_indices, ])
  data$background_predicted <- predict(background_model, newdata = data)
  data$ppb_corrected <- data$ppb - data$background_predicted
  data$ppb_temp_corrected <- data$ppb_corrected * exp(n_rhodamine * (initial_water_temp_degC - T_ref))
  data$ppb_smoothed <- sgolayfilt(data$ppb_temp_corrected, p = 3, n = 11)
  data$ppb_smoothed[data$ppb_smoothed < 0] <- 0
  time_seconds <- as.numeric(difftime(data$time, data$time[1], units = "secs"))
  auc_rhodamine <- trapz(time_seconds, data$ppb_smoothed)
  discharge_rhodamine <- initial_mass_rhodamine / (auc_rhodamine / 1000)
  peak_concentration_rhodamine <- max(data$ppb_smoothed)
  peak_time_rhodamine <- data$time[which.max(data$ppb_smoothed)]
  travel_time_rhodamine <- as.numeric(difftime(peak_time_rhodamine, data$time[1], units = "secs"))
  velocity_rhodamine <- distance / travel_time_rhodamine
  
  p1 <- ggplot(data, aes(x = time, y = ppb_smoothed)) +
    geom_line(color = "blue") +
    geom_area(fill = "blue", alpha = 0.2) +
    labs(title = "T-adjusted rhodamine concentration over time",
         x = "Time", y = "Concentration (ppb)")
}

if ("uScm" %in% names(data)) {
  background_indices <- c(1:10, (nrow(data)-9):nrow(data))
  background_model_salt <- lm(uScm ~ as.numeric(time), data = data[background_indices, ])
  data$background_predicted_salt <- predict(background_model_salt, newdata = data)
  data$uScm_corrected <- data$uScm - data$background_predicted_salt
  data$salt_concentration <- data$uScm_corrected / slope_conductivity
  data$salt_concentration_smoothed <- sgolayfilt(data$salt_concentration, p = 3, n = 11)
  data$salt_concentration_smoothed[data$salt_concentration_smoothed < 0] <- 0
  time_seconds <- as.numeric(difftime(data$time, data$time[1], units = "secs"))
  auc_salt <- trapz(time_seconds, data$salt_concentration_smoothed)
  discharge_salt <- initial_mass_salt / auc_salt
  peak_concentration_salt <- max(data$salt_concentration_smoothed)
  peak_time_salt <- data$time[which.max(data$salt_concentration_smoothed)]
  travel_time_salt <- as.numeric(difftime(peak_time_salt, data$time[1], units = "secs"))
  velocity_salt <- distance / travel_time_salt
  
  p2 <- ggplot(data, aes(x = time, y = salt_concentration_smoothed)) +
    geom_line(color = "red") +
    geom_area(fill = "red", alpha = 0.2) +
    labs(title = "Salt concentration over time",
         x = "Time", y = "Concentration (g/L)")
}

if (!is.null(p1) & !is.null(p2)) {
  grid.arrange(p1, p2, ncol = 2)
} else if (!is.null(p1)) {
  print(p1)
} else if (!is.null(p2)) {
  print(p2)
}

if ("ppb" %in% names(data)) {
  cat("Rhodamine Results:\n")
  cat("Peak concentration:", round(peak_concentration_rhodamine, 3), "ppb\n")
  cat("Travel time:", round(travel_time_rhodamine, 2), "seconds\n")
  cat("Velocity:", round(velocity_rhodamine, 4), "m/s\n")
  cat("Discharge:", round(discharge_rhodamine, 4), "L/s\n")
}

if ("uScm" %in% names(data)) {
  cat("\nSalt Results:\n")
  cat("Peak concentration:", round(peak_concentration_salt, 4), "g/L\n")
  cat("Travel time:", round(travel_time_salt, 2), "seconds\n")
  cat("Velocity:", round(velocity_salt, 4), "m/s\n")
  cat("Discharge:", round(discharge_salt, 4), "L/s\n")
}
