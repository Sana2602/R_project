library(shiny)
library(DT)
library(ggplot2)
library(kableExtra)


data <- read.csv("C:/Users/sana shaikh/Desktop/Covid_R/COVID19_line_list_data.csv", stringsAsFactors = TRUE)
data$death_dummy <- as.integer(data$death != 0)
#categorical to binary
# Data subsets for age and gender analysis
dead <- subset(data, death_dummy == 1)
alive <- subset(data, death_dummy == 0)
men <- subset(data, gender == "male")
women <- subset(data, gender == "female")

ui <- fluidPage(
  titlePanel("Interactive COVID-19 Analysis"),
  tabsetPanel(
    tabPanel("Summary", uiOutput("summary_table")), 
    tabPanel("Age Analysis", plotOutput("age_histogram")),
    tabPanel("Gender Analysis", plotOutput("gender_barplot")),
    # Add more tabs as needed
  )
)

server <- function(input, output) {
  output$summary_table <- renderUI({
    # Create a customized summary table with kableExtra
    summary_table <- kable(
      data.frame(
        Variable = colnames(data),
        Minimum = apply(data, 2, function(x) min(x, na.rm = TRUE)),
        Maximum = apply(data, 2, function(x) max(x, na.rm = TRUE)),
        Mean = apply(data, 2, function(x) mean(x, na.rm = TRUE)),
        Median = apply(data, 2, function(x) median(x, na.rm = TRUE)),
        StdDev = apply(data, 2, function(x) sd(x, na.rm = TRUE))
      ),
      format = "html", # Use HTML format
      table.attr = 'class="table table-striped table-bordered table-condensed"'
    ) %>%
      kable_styling("striped")  # Apply custom CSS styles
    
    HTML(summary_table)  # Convert to HTML and display it
  })
  
  output$age_histogram <- renderPlot({
    # Create a histogram with density plots for age analysis
    p <- ggplot(data, aes(x = age, fill = factor(death_dummy == 1))) +
      geom_histogram(bins = 30, alpha = 0.7) +
      labs(x = "Age", y = "Density") +
      theme_minimal()
    print(p)
  })
  
  output$gender_barplot <- renderPlot({
    # Create a bar chart for gender analysis
    gender_summary <- data.frame(
      Gender = c("Men", "Women"),
      Death_Rate = c(sum(men$death_dummy) / nrow(men), sum(women$death_dummy) / nrow(women))
    )
    p <- ggplot(gender_summary, aes(x = Gender, y = Death_Rate)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(x = "Gender", y = "Death Rate") +
      theme_minimal()
    print(p)
  })
  
  # Add more server logic for other tabs
}

shinyApp(ui, server)
w