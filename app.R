#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(googleAnalyticsR)
library(shinyjs)



# Load the Google Analytics credentials
ga_auth(json_file = "pageviewtest-450403-598678edc95b.json")

# Define UI for application that draws a histogram
ui <- fluidPage(
    useShinyjs(),  # Enable shinyjs
    tags$head(includeHTML("google-analytics.html")),
    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           #textOutput("page_views"),
           h3("Page Views:"),
           h3(textOutput("animated_page_views"), style = "color: black;"),
           plotOutput("distPlot"),
           plotOutput("distPlot2")
        ),
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    ga_auth(json_file = "pageviewtest-450403-598678edc95b.json")
    print("Server started")
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })

    output$distPlot2 <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2]
      bins <- seq(min(x), max(x), length.out = input$bins + 1)

      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white',
           xlab = 'Waiting time to next eruption (in mins)',
           main = 'Histogram of waiting times')
    })


    # Fetch page views from Google Analytics
    page_views <- reactive({
      #ga_auth(json_file = "client_secret_705822635407-n92t5rbfjg7sb206o0fr0odtvl7t7mve.apps.googleusercontent.com.json")
      # Fetch data
      ga_auth(json_file = "pageviewtest-450403-598678edc95b.json")
      ga_data = ga_data(
        propertyId = "477217006",  # Replace with your GA4 Property ID
        date_range = c(Sys.Date() - 30, Sys.Date()),  # Last 30 days
        metrics = "screenPageViews",
        dimensions = "date"
      )
      sum(ga_data$screenPageViews) + 100


    })

    # Fetch page views from Google Analytics
    page_views <- reactive({
      ga_auth(json_file = "pageviewtest-450403-598678edc95b.json")
      print("Fetching page views...")
      start_date = as.Date("2025-02-07")
      ga_data = ga_data(
        propertyId = "477217006",  # Replace with your GA4 Property ID]
        date_range = c(Sys.Date() - 30, Sys.Date()),  # Last 30 days
        metrics = "screenPageViews",
        dimensions = "date"
      )
      print("Page views fetched successfully")
      sum(ga_data$screenPageViews) + 100
    })


    # Render the animated page views
    output$animated_page_views <- renderText({
      ga_auth(json_file = "pageviewtest-450403-598678edc95b.json")
      print("Rendering animated page views...")
      runjs(sprintf("
      let target = %d;
      let current = 1;
      const element = document.getElementById('animated_page_views');
      const increment = Math.ceil(target / 100);  // Adjust speed here

      const interval = setInterval(() => {
        if (current >= target) {
          element.textContent = target.toLocaleString();
          clearInterval(interval);
        } else {
          current += increment;
          element.textContent = current.toLocaleString();
        }
      }, 20);  // Adjust interval for smoother/faster animation
    ", page_views()))

      # Display a placeholder initially
      "1"
    })



    # Render the page views
    output$page_views <- renderText({
      paste("Total Page Views in the Last 30 Days:", page_views(), "\n\n")
    })
}

# Run the application
shinyApp(ui = ui, server = server)
