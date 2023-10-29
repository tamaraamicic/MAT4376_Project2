mimic_data = read.csv("C:\\Users\\micic\\OneDrive - University of Ottawa\\4th Year\\Fall 2023\\Topics in Statistics\\Projects\\Project 2\\oct 11\\mimic3d.csv")
library("carData")
library("car")

LOSdays = mimic_data$LOSdays

qqPlot(LOSdays)

log_LOSdays = log(LOSdays)
qqPlot(log_LOSdays, ylim = c(-8, 8))

library(MASS)
#library(ggplot2)

LOSdays_nonzero <- LOSdays + 1e-10

# Applying the Box-Cox transformation
boxcox_results <- boxcox(lm(LOSdays_nonzero ~ 1))

# Find the lambda value that maximizes the log-likelihood
lambda_max <- boxcox_results$LOSdays[which.max(boxcox_results$y)]

# Transform the data using the selected lambda value
transformed_data <- if (lambda_max != 0) {
  (LOSdays_nonzero^lambda_max - 1) / lambda_max
} else {
  log(LOSdays_nonzero)
}

# View the transformed data
print(transformed_data)

# Check the normality of the transformed data using a QQ plot
#qqnorm(transformed_data)
#qqline(transformed_data)
qqPlot(transformed_data)

shapiro.test(log_LOSdays)
ks.test(log_LOSdays, "pnorm")
