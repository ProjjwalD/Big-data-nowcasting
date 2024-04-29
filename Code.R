#*********************Use Libraries********************************************
#******************************************************************************
library(readr)
library(dplyr) 
library(lubridate) 
library(ggplot2)
library(scales)
library(forecast)
library(tidyr)
library(stringr)
library(readxl)

#********************* Import Google Trends data ******************************
#******************************************************************************

# Read the CSV file, skipping the first 2 rows, and the last three rows and converting "<1" to NA
google_trends_data <- read.csv("multiTimeline.csv", skip = 2, na.strings = "<1", nrows = length(count.fields("multiTimeline.csv", skip = 2)) - 3)
google_trends_data$Month <- paste0(google_trends_data$Month, "-01")
google_trends_data$Month <- as.Date(google_trends_data$Month, format="%Y-%m-%d")
names(google_trends_data)[names(google_trends_data) == "download.phonepe...India."] <- "search_volume"

# Shift the months to match the desired quarters:
# April, May, June -> Q1; July, Aug, Sep -> Q2; Oct, Nov, Dec -> Q3; Jan, Feb, Mar -> Q4.
# For this, subtract 3 months from the Month column to align April with Q1.
google_trends_data <- google_trends_data %>%
  mutate(
    Month = Month %m-% months(3),
    Year = year(Month),
    Quarter = ceiling(month(Month) / 3)
  )

# Group by Year and Quarter and calculate the mean search volume
google_trends_data <- google_trends_data %>%
  group_by(Year, Quarter) %>%
  summarize(Average_Search_Volume = mean(search_volume, na.rm = TRUE)) %>%
  ungroup()

# Adjust the 'Quarter' to be a string with 'Q' prefix for formatting
google_trends_data$Quarter <- paste0("Q", google_trends_data$Quarter)

# Arrange by Year and then by Quarter
google_trends_data <- google_trends_data %>%
  arrange(Year, Quarter)

# Since the data should be up to Q3 of 2023, filter out the rest
google_trends_data <- google_trends_data %>%
  filter((Year > 2011 & Year < 2023) | (Year == 2023 & Quarter != "Q4"))

# Merge Year and Quarter columns and renaming it to Year
google_trends_data <- google_trends_data %>%
  unite("Year", c("Year", "Quarter"), sep = " ")

# 'Year' is in a character format representing years,first convert it to Date to include full date information (YYYY-MM-DD)
google_trends_data <- google_trends_data %>%
  mutate(Year = case_when(
    str_detect(Year, "Q1") ~ as.Date(str_replace(Year, " Q1", "-04-01"), "%Y-%m-%d"),
    str_detect(Year, "Q2") ~ as.Date(str_replace(Year, " Q2", "-07-01"), "%Y-%m-%d"),
    str_detect(Year, "Q3") ~ as.Date(str_replace(Year, " Q3", "-10-01"), "%Y-%m-%d"),
    str_detect(Year, "Q4") ~ as.Date(str_replace(Year, " Q4", "-01-01"), "%Y-%m-%d")
  ))

#********************* Import GDP Data****************************************
#******************************************************************************

# Read the GDP excel file
file_path <- "Statement_Quarterly_Constant_01.03.2024.xlsx"

# Reading the specific sheet
data <- read_excel(file_path, range = "BE4:CY16")

# Select rows 1, 12 from the loaded data
selected_data <- data[c(1, 12), ]

# Renaming the columns of the selected_data data frame
names(selected_data) <- c("2012 Q1", "2012 Q2","2012 Q3","2012 Q4", 
                          "2013 Q1", "2013 Q2","2013 Q3","2013 Q4",
                          "2014 Q1", "2014 Q2","2014 Q3","2014 Q4",
                          "2015 Q1", "2015 Q2","2015 Q3","2015 Q4",
                          "2016 Q1", "2016 Q2","2016 Q3","2016 Q4",
                          "2017 Q1", "2017 Q2","2017 Q3","2017 Q4",
                          "2018 Q1", "2018 Q2","2018 Q3","2018 Q4",
                          "2019 Q1", "2019 Q2","2019 Q3","2019 Q4",
                          "2020 Q1", "2020 Q2","2020 Q3","2020 Q4",
                          "2021 Q1", "2021 Q2","2021 Q3","2021 Q4",
                          "2022 Q1", "2022 Q2","2022 Q3","2022 Q4",
                          "2023 Q1", "2023 Q2","2023 Q3")

# Removing the first row
selected_data <- selected_data[-1, ]

# Gather the quarter columns into two new columns: 'Year' and 'Quarterly GDP growth rate (percentage)'
gdp_data <- gather(selected_data, key = "Year", value = "Quarterly GDP growth rate (percentage)")

# The first month of each quarter to the 'Year' and converting it to a date.
gdp_data <- gdp_data %>%
  mutate(Year = case_when(
    str_detect(Year, "Q1") ~ as.Date(str_replace(Year, " Q1", "-04-01"), "%Y-%m-%d"),
    str_detect(Year, "Q2") ~ as.Date(str_replace(Year, " Q2", "-07-01"), "%Y-%m-%d"),
    str_detect(Year, "Q3") ~ as.Date(str_replace(Year, " Q3", "-10-01"), "%Y-%m-%d"),
    str_detect(Year, "Q4") ~ as.Date(str_replace(Year, " Q4", "-01-01"), "%Y-%m-%d")
  ))

# Convert the GDP growth rate to numeric if it's not already
gdp_data$`Quarterly GDP growth rate (percentage)` <- as.numeric(gdp_data$`Quarterly GDP growth rate (percentage)`)

#*********************Plot for Google Search Volume for 'download PhonePe******
#******************************************************************************

ggplot(google_trends_data, aes(x = Year, y = Average_Search_Volume)) +
  geom_line(aes(color = "Search Volume"), linewidth = 1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(values = c("Search Volume" = "blue")) +
  scale_y_continuous(breaks = seq(from = floor(min(google_trends_data$Average_Search_Volume, na.rm = TRUE)/5)*5, 
                                  to = ceiling(max(google_trends_data$Average_Search_Volume, na.rm = TRUE)/5)*5, 
                                  by = 5)) + 
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(size = 11, face = "bold", color = "#4D4D4D"),
    axis.title.y = element_text(size = 11, face = "bold", color = "#4D4D4D"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    legend.key = element_rect(fill = "white", colour = "black"),
    panel.grid.major = element_line(color = "#EAEAEA"),
    panel.grid.minor = element_blank(),
  ) +
  labs(
    x = "Year (Quarterly)",
    y = "Average Search Volume"
  )

#*********************Plot for GDP Growth Rate*********************************
#******************************************************************************

ggplot(gdp_data, aes(x = Year, y = `Quarterly GDP growth rate (percentage)`)) +
  geom_line(aes(color = "GDP Growth Rate"), linewidth = 1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(
    from = floor(min(gdp_data$`Quarterly GDP growth rate (percentage)`, na.rm = TRUE)/5)*5,
    to = ceiling(max(gdp_data$`Quarterly GDP growth rate (percentage)`, na.rm = TRUE)/5)*5,
    by = 5)) + 
  scale_color_manual(values = c("GDP Growth Rate" = "red")) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    axis.title.x = element_text(size = 11, face = "bold", color = "#333333"), 
    axis.title.y = element_text(size = 11, face = "bold", color = "#333333"),
    panel.grid.major = element_line(color = "#eaeaea"), 
    panel.grid.minor = element_blank(), 
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5), 
    legend.key = element_rect(fill = "white", colour = "black"), 
  ) +
  labs(
    x = "Year (Quarterly)", 
    y = "GDP Growth Rate (%)",
  )


#********************* Normal Correlation Test (Pearson's) *********************
#*******************************************************************************
cor.test(gdp_data$`Quarterly GDP growth rate (percentage)`,google_trends_data$Average_Search_Volume)

#*********************Plot of Google trends data histogram ********************
#******************************************************************************

ggplot(data = google_trends_data, aes(x = Average_Search_Volume)) +
  geom_histogram(aes(y = after_stat(density)), fill = "#e67e22", color = "white", binwidth = 10) +
  geom_density(aes(y = after_stat(density), color = "Density Line"), size = 1) +
  scale_color_manual(values = c("Density Line" = "red")) +
  theme_classic(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "#f0f3f4"),
    panel.grid.major.y = element_line(color = "#bdc3c7", linetype = 2),
    axis.title.x = element_text(size = 11, face = "bold"),
    axis.title.y = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(color = "#34495e"),
    axis.text.y = element_text(color = "#34495e"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key.size = unit(0.7, "lines") 
  ) +
  labs(
    x = "Search Volume",
    y = "Frequency"  
  )

#*********************Plot of GDP plot histogram**********************************
#******************************************************************************

ggplot(data = gdp_data, aes(x = `Quarterly GDP growth rate (percentage)`)) +
  geom_histogram(aes(y = after_stat(density)), fill = "#2980b9", color = "white", binwidth = 10) +
  geom_density(aes(y = after_stat(density),color = "Density Line"), adjust = 1, size = 1, alpha = 0.5) +  
  scale_color_manual(values = c("Density Line" = "red"))+
  theme_classic(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "#f0f3f4"),
    panel.grid.major.y = element_line(color = "#bdc3c7", linetype = 2),
    axis.title.x = element_text(size = 11, face = "bold"),
    axis.title.y = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(color = "#34495e"),
    axis.text.y = element_text(color = "#34495e"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key.size = unit(0.7, "lines") 
  ) +
  labs(
    x = "Quarterly GDP Growth Rate (%)", 
    y = "Frequency",
    color = "Legend"  
  )
#*********************Shapiro-Wilk Test for Normality**************************
#******************************************************************************

# Is the Google data normally distributed?
shapiro.test(google_trends_data$Average_Search_Volume)
# Is the GDP data normally distributed?
shapiro.test(gdp_data$`Quarterly GDP growth rate (percentage)`)

# Calculate the correlation again, this time with method="Kendall"
cor.test(gdp_data$`Quarterly GDP growth rate (percentage)`,google_trends_data$Average_Search_Volume, method="kendall")

#*********************Correlation scatter plot with regression line************
#******************************************************************************
# Merge the two datasets based on the Year column
merged_data <- merge(google_trends_data, gdp_data, by = "Year")

ggplot(merged_data) +
  geom_point(aes(x = `Quarterly GDP growth rate (percentage)`, y = Average_Search_Volume, color = Average_Search_Volume), 
             size = 3, alpha = 0.6) + 
  geom_smooth(aes(x = `Quarterly GDP growth rate (percentage)`, y = Average_Search_Volume, linetype = "Regression Line"), 
              method = "lm", se = FALSE, color = "darkred") +
  scale_color_gradient(low = "skyblue", high = "navy", name = "Search Volume") +
  scale_linetype_manual(values = c("Regression Line" = "solid"), name = "Legend") +
  theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "#ffffff"),
    panel.grid.major = element_line(color = "#cccccc"),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 11, face = "bold", color = "#333333"),
    axis.title.y = element_text(size = 11, face = "bold", color = "#333333"),
    axis.text = element_text(color = "#333333"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    panel.background = element_rect(fill = "ghostwhite"),
    legend.position = "right"
  ) +
  labs(
    x = "Quarterly GDP Growth Rate (%)", 
    y = "Average Search Volume",
  )  +
  geom_text(aes(label = sprintf("Kendall's tau: %.2f\nPearson's r: %.2f", -0.1702667, -0.1349876)),
            x = Inf, y = Inf, hjust = 1.05, vjust = 2, color = "#333333", size = 4, fontface = "bold") +
  guides(linetype = guide_legend(title = "Legend"), color = guide_legend(title = "Search Volume"))

#*********************ARIMA Model with AdvancedFit and BaseFit*****************
#******************************************************************************

# Convert time series data to ts object with frequency 4 (quarterly data)
google_trends_data$Average_Search_Volume <- ts(google_trends_data$Average_Search_Volume, frequency = 4)
gdp_data$`Quarterly GDP growth rate (percentage)` <- ts(gdp_data$`Quarterly GDP growth rate (percentage)`, frequency = 4)

# Fit ARIMA model for gdp_data using automatic selection of parameters
auto.arima(gdp_data$`Quarterly GDP growth rate (percentage)`)

# Fit baseline ARIMA model for Quarterly GDP growth rate without additional covariate
baseFit <- Arima(gdp_data$`Quarterly GDP growth rate (percentage)`, order = c(0,0,1),seasonal = c(0,0,1))

# Fit advanced ARIMA model for Quarterly GDP growth rate with Average_Search_Volume as covariate
advancedFit <- Arima(gdp_data$`Quarterly GDP growth rate (percentage)`, order = c(0,0,1),seasonal = c(0,0,1) ,xreg = google_trends_data$Average_Search_Volume)

# Extract residuals and transform them into absolute errors for baseline model
baseFitAbsErrs <- abs(baseFit$residuals)
advancedFitAbsErrs<- abs(advancedFit$residuals)

# Extract the residuals for both models
base_residuals <- residuals(baseFit)
advanced_residuals <- residuals(advancedFit)

# Calculate mean absolute error (MAE) for both models
base_MAE <- mean(abs(base_residuals))
advanced_MAE <- mean(abs(advanced_residuals))

# Calculate percentage improvement in MAE
improvement <- (1 - (advanced_MAE / base_MAE)) * 100

# Create a new dataframe for plotting without altering the original gdp_data
gdp_plotting_data <- gdp_data

# Add fitted values from both ARIMA models
gdp_plotting_data$Advanced_Fit <- fitted(advancedFit)
gdp_plotting_data$Base_Fit <- fitted(baseFit)

# Calculate the standard deviation of the residuals from both models
residuals_sd_advanced <- sd(residuals(advancedFit))

# Calculate prediction intervals for the advanced fit
gdp_plotting_data$upper95<- gdp_plotting_data$Advanced_Fit + (1.96 * residuals_sd_advanced)
gdp_plotting_data$lower95<- gdp_plotting_data$Advanced_Fit - (1.96 * residuals_sd_advanced)
gdp_plotting_data$upper80 <- gdp_plotting_data$Advanced_Fit + (1.28 * residuals_sd_advanced)
gdp_plotting_data$lower80<- gdp_plotting_data$Advanced_Fit - (1.28 * residuals_sd_advanced)

# Transform the dataframe into long format for ggplot2 plotting
gdp_long <- tidyr::pivot_longer(
  gdp_plotting_data,
  cols = c("Base_Fit", "Advanced_Fit"),
  names_to = "Model_Type",
  values_to = "Fitted"
)

# Specify the combined text for MAE and improvement
advanced_MAE_text <- sprintf("MAE: %.2f", advanced_MAE)
improvement_text <- sprintf("Improvement: %.2f%%", improvement)

#*********************ARIMA Plot***********************************************
#******************************************************************************

ggplot(gdp_long, aes(x = Year)) +
  geom_point(aes(y = `Quarterly GDP growth rate (percentage)`, color = "Actual Data"), size = 3, shape = 1, stroke = 1.5) +
  geom_line(aes(y = Fitted , color = "Fitted Data"), size = 1) +
  geom_ribbon(aes(ymin = lower80, ymax = upper80, fill = "80% Interval"), alpha = 0.5) +
  geom_ribbon(aes(ymin = lower95, ymax = upper95, fill = "95% Interval"), alpha = 0.5) +
  scale_color_manual(values = c("Actual Data" = "black", "Fitted Data" = "red")) +
  scale_fill_manual(values = c("80% Interval" = "#8c6bb1", "95% Interval" = "#9ebcda")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(from = floor(min(gdp_long$`Quarterly GDP growth rate (percentage)`)),
                                  to = ceiling(max(gdp_long$`Quarterly GDP growth rate (percentage)`)),
                                  by = 4)) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#f0f3f4"),
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title.x = element_text(size = 11, color = "black", face = "bold"),  
    axis.title.y = element_text(size = 11, color = "black", face = "bold"),  
    panel.grid.major = element_line(color = "#e0e0e0"),
    legend.background = element_rect(fill = "white"),
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
    axis.text.y = element_text(color = "black")
  ) +
  labs(
    x = "Year (Quarterly)", 
    y = "GDP Growth Rate (%)",
    color = "Legend", 
    fill = "Prediction Interval"
  ) +
  annotate("text", x = as.Date("2012-01-01"), y = -20, 
           label = paste(advanced_MAE_text, improvement_text, sep = "\n"), 
           hjust = 0, vjust = 0, color = "black", size = 4, 
           fontface = "bold")

#*********************Datasets for Nowcasting *********************************
#******************************************************************************

# Value for Average_Search_Volume for nowcasting the next quarter
next_quarter_search_volume <- 75  

# Forecast the next quarter GDP growth rate using the advanced ARIMA model
nowcast_gdp <- forecast(advancedFit, h = 1, xreg = c(next_quarter_search_volume))
nowcasted_value <- nowcast_gdp$mean

# Prepare historical data
historical_ts<- gdp_data %>% 
  mutate(Date = as.Date(paste0(Year, "-01-01"))) %>%
  select(Date, `Quarterly GDP growth rate (percentage)`)

#Convert time series object to numeric data, so that it can be merged
historical_ts$`Quarterly GDP growth rate (percentage)` <- as.numeric(historical_ts$`Quarterly GDP growth rate (percentage)`)

# The next quarter follows immediately after the last date in historical data
next_quarter_date <- max(historical_ts$Date) + months(3)
nowcasted_data <- data.frame(
  Date = next_quarter_date,
  `Quarterly GDP growth rate (percentage)` = nowcasted_value
)
nowcasted_data <- nowcasted_data %>%
  rename(`Quarterly GDP growth rate (percentage)` = Quarterly.GDP.growth.rate..percentage.)

nowcasted_data$`Quarterly GDP growth rate (percentage)` <- as.numeric(nowcasted_data$`Quarterly GDP growth rate (percentage)`)

# Combine historical and nowcasted data
combined_gdp_data <- rbind(historical_ts, nowcasted_data)

#Convert time series data to ts object with frequency 4 (quarterly data)
combined_gdp_data$`Quarterly GDP growth rate (percentage)` <- ts(combined_gdp_data$`Quarterly GDP growth rate (percentage)`, frequency = 4)

# Extract the fitted values from the advancedFit model
fitted_values <- fitted(advancedFit)

# Extend the fitted values vector by one, setting the last value to NA
extended_fitted_values <- c(fitted_values, NA)

# Assign the extended fitted values to the combined_gdp_data dataframe
combined_gdp_data$Fitted_ARIMA <- extended_fitted_values

#********************* Nowcasting Plot ****************************************
#******************************************************************************

ggplot(combined_gdp_data, aes(x = Date)) +
  geom_line(aes(y = Fitted_ARIMA, color = "Fitted ARIMA"), size = 1.2) +
  geom_point(aes(y = `Quarterly GDP growth rate (percentage)`, color = "Actual Data"), size = 2.5, shape = 1, alpha = 0.8, stroke = 1.5) +
  geom_point(data = combined_gdp_data[48, ], aes(y = `Quarterly GDP growth rate (percentage)`, color = "Nowcasted"), size = 4, shape = 17) +
  scale_color_manual(values = c("Fitted ARIMA" = "#D55E00", "Actual Data" = "#4F7942", "Nowcasted" = "#56B4E9"))+
  annotate("text", x = combined_gdp_data$Date [48], y = combined_gdp_data$`Quarterly GDP growth rate (percentage)`[47] - 8, label = sprintf("Nowcasted Q4 2023: %.2f%%", 3.43566), hjust = 1, vjust = 1, color = "#56B4E9", fontface = "italic", size = 3.5) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_text(size = 11, color = "black", face = "bold"),  
    axis.title.y = element_text(size = 11, color = "black", face = "bold"), 
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "#ffffff", color = NA),  
    panel.background = element_rect(fill = "ghostwhite", color = NA),
    panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5),
    plot.margin = unit(c(1, 1, 1, 1), "lines")
  ) +
  labs(
    x = "Year (Quarterly)",
    y = "GDP Growth Rate (%)",
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  guides(
    color = guide_legend(override.aes = list(shape = c(16, NA, 18)))
  )
#********************* End ****************************************************
#******************************************************************************
