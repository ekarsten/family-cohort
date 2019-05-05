#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

dat <-
  tibble(wage = rnorm(200, 30000, 5000),
         exp = runif(200, 0, 10),
         female = runif(200, 0, 1) < .5) %>%
  mutate(wage = if_else(female, wage, wage + 10000) + 1000*exp ) %>%
  mutate(Gender = if_else(female, "Female", "Male"))

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Visualizing Model: Wage = Beta_0 + Beta_1*Experience + Beta_2*Female"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("b0",
                     "Beta_0:",
                     min = 0,
                     max = 50000,
                     value = 25000),
         sliderInput("b1",
                     "Beta_1:",
                     min = 0,
                     max = 2000,
                     value = 0),
         sliderInput("b2",
                     "Beta_2:",
                     min = -15000,
                     max = 15000,
                     value = 5000),
         checkboxInput("fit",
                       "Show Best Fit Line",
                       F),
         checkboxInput("mine",
                       "Show My Line",
                       T)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     # plot line input by user and or best fit
     if (input$fit == F & input$mine == F) {
       a <-
         dat %>%
         ggplot(aes(x = exp, y = wage, color = Gender)) +
         geom_point() +
         theme_bw() +
         labs(x = "Years of Experience",
              y = "Annual Salary") }
     
     if (input$fit == T & input$mine == F) {
     a <-
       dat %>%
       ggplot(aes(x = exp, y = wage, color = Gender)) +
       geom_point() +
       geom_smooth(method = "lm") +
       theme_bw() +
       labs(x = "Years of Experience",
            y = "Annual Salary") }
     
     if (input$fit == F & input$mine == T) {
       a <-
         dat %>%
         ggplot(aes(x = exp, y = wage, color = Gender)) +
         geom_point() +
         geom_abline(slope = input$b1, intercept = input$b0, color = "blue") +
         geom_abline(slope = input$b1, intercept = input$b0 + input$b2, color = "red") +
         theme_bw() +
         labs(x = "Years of Experience",
              y = "Annual Salary") }
     
     if (input$fit == T & input$mine == T) {
       a <-
         dat %>%
         ggplot(aes(x = exp, y = wage, color = Gender)) +
         geom_point() +
         geom_abline(slope = input$b1, intercept = input$b0, color = "blue") +
         geom_abline(slope = input$b1, intercept = input$b0 + input$b2, color = "red") +
         geom_smooth(method = "lm") +
         theme_bw() +
         labs(x = "Years of Experience",
              y = "Annual Salary") }
     return(a)
      
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

