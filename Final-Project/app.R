library(shiny)
library(tidyverse)
compare <- read_csv("~/Desktop/Gov 1005 Final Project/Final-Project/fandango/fandango_score_comparison.csv")
scrape <- read_csv("~/Desktop/Gov 1005 Final Project/Final-Project/fandango/fandango_scrape.csv")
movie_ratings <- left_join(compare, scrape, by = "FILM")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Movie Ratings on Fandango"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput(inputId = "Fandango_Stars",
                     label = "Fandango Stars:",
                     choices = c("3.0",
                                 "3.5",
                                 "4.0",
                                 "4.5",
                                 "5.0"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("fandstarPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$fandstarPlot <- renderPlot({
      
     movie_ratings <- left_join(compare, scrape, by = "FILM") %>% 
       filter(Fandango_Stars == input$Fandango_Stars) %>% 
       select(FILM) %>% 
       count(FILM)
      
      
      ggplot(data = movie_ratings, aes(x = FILM)) + 
        geom_histogram(binwidth = 0.5, stat = "count", na.rm = TRUE) + 
        labs(x = "Film",
             y = "Stars",
             title = "Film Star Ratings",
             caption = "Number of Films with Specific Star Ratings")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

