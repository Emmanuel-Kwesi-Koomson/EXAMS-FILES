library(ggplot2)
library(dplyr)
library(lubridate)

data <- read.csv("C:/Users/Kumi/Desktop/covid.csv")
data$date <- as.Date(data$date, format="%m/%d/%Y")

#Cumulative Cases Over Time
cumulative_plot <- ggplot(data, aes(x = date)) +
  geom_line(aes(y = cumulative_confirmed, color = "Confirmed")) +
  geom_line(aes(y = cumulative_recovered, color = "Recovered")) +
  geom_line(aes(y = cumulative_death, color = "Death")) +
  labs(title = "Cumulative COVID-19 Cases", x = "Date", y = "Count") +
  theme_minimal()
print(cumulative_plot)

#Daily Cases Over Time
daily_plot <- ggplot(data, aes(x = date)) +
  geom_line(aes(y = confirmed, color = "Confirmed")) +
  geom_line(aes(y = recovered, color = "Recovered")) +
  geom_line(aes(y = death, color = "Death")) +
  labs(title = "Daily COVID-19 Cases", x = "Date", y = "Count") +
  theme_minimal()
print(daily_plot)

#Active Cases Over Time
active_plot <- ggplot(data, aes(x = date, y = active_cases)) +
  geom_line(color = "blue") +
  labs(title = "Active Cases Over Time", x = "Date", y = "Active Cases") +
  theme_minimal()
print(active_plot)

#Recovery & Death Rate Over Time
data <- data %>% mutate(recovery_rate = cumulative_recovered / cumulative_confirmed, 
                        death_rate = cumulative_death / cumulative_confirmed)
rate_plot <- ggplot(data, aes(x = date)) +
  geom_line(aes(y = recovery_rate, color = "Recovery Rate")) +
  geom_line(aes(y = death_rate, color = "Death Rate")) +
  labs(title = "Recovery & Death Rate Over Time", x = "Date", y = "Rate") +
  theme_minimal()
print(rate_plot)

#Case Composition (Pie Chart)
latest_data <- data[nrow(data), ]
case_data <- data.frame(
  Category = c("Active Cases", "Recovered", "Deaths"),
  Count = c(latest_data$active_cases, latest_data$cumulative_recovered, latest_data$cumulative_death)
)
pie_chart <- ggplot(case_data, aes(x = "", y = Count, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Case Composition at Latest Date") +
  theme_minimal()
print(pie_chart)
