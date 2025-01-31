library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)

data <- read.csv("C:/Users/Hp/Documents/R data/Ghana_Covid19_DailyActive.csv")
data$date <- as.Date(data$date, format="%m/%d/%Y")

ui <- fluidPage(
  titlePanel("COVID-19 Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("chart", "Select Chart:",
                  choices = c("Cumulative Cases", "Daily Cases", "Active Cases", "Recovery & Death Rate", "Case Composition"))
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$plot <- renderPlot({
    if (input$chart == "Cumulative Cases") {
      ggplot(data, aes(x = date)) +
        geom_line(aes(y = cumulative_confirmed, color = "Confirmed")) +
        geom_line(aes(y = cumulative_recovered, color = "Recovered")) +
        geom_line(aes(y = cumulative_death, color = "Death")) +
        labs(title = "Cumulative COVID-19 Cases", x = "Date", y = "Count") +
        theme_minimal()
    } else if (input$chart == "Daily Cases") {
      ggplot(data, aes(x = date)) +
        geom_line(aes(y = confirmed, color = "Confirmed")) +
        geom_line(aes(y = recovered, color = "Recovered")) +
        geom_line(aes(y = death, color = "Death")) +
        labs(title = "Daily COVID-19 Cases", x = "Date", y = "Count") +
        theme_minimal()
    } else if (input$chart == "Active Cases") {
      ggplot(data, aes(x = date, y = active_cases)) +
        geom_line(color = "blue") +
        labs(title = "Active Cases Over Time", x = "Date", y = "Active Cases") +
        theme_minimal()
    } else if (input$chart == "Recovery & Death Rate") {
      data <- data %>% mutate(recovery_rate = cumulative_recovered / cumulative_confirmed, 
                              death_rate = cumulative_death / cumulative_confirmed)
      ggplot(data, aes(x = date)) +
        geom_line(aes(y = recovery_rate, color = "Recovery Rate")) +
        geom_line(aes(y = death_rate, color = "Death Rate")) +
        labs(title = "Recovery & Death Rate Over Time", x = "Date", y = "Rate") +
        theme_minimal()
    } else if (input$chart == "Case Composition") {
      latest_data <- data[nrow(data), ]
      case_data <- data.frame(
        Category = c("Active Cases", "Recovered", "Deaths"),
        Count = c(latest_data$active_cases, latest_data$cumulative_recovered, latest_data$cumulative_death)
      )
      ggplot(case_data, aes(x = "", y = Count, fill = Category)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        labs(title = "Case Composition at Latest Date") +
        theme_minimal()
    }
  })
}

shinyApp(ui = ui, server = server)
