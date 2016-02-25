#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Contour plot of investor utility"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
       numericInput("current_wealth",
                    "Current wealth",
                    min = 0,
                    value = 0),
       sliderInput("time_horizon",
                   "Time horizon",
                   min = 1,
                   max = 25,
                   step = 1,
                   value = 5),
       sliderInput("contours",
                   "Number of contours:",
                   min = 2,
                   max = 18,
                   value = 10),
       h5("Expected utility"),
       verbatimTextOutput("selection")
    ),

    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot",
                  click = "distPlot_click"
                  )
    )
  )
))
