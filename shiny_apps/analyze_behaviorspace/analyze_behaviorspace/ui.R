
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
<<<<<<< HEAD
  titlePanel("Analyze BehaviorSpace Experiments"),
=======
  titlePanel("Analyze BehaviorSpace"),
>>>>>>> 405dc8ab2147d7ac4b6012d2c25e91a91ca56cab
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv',
                         'text/comma-separated-values,text/plain',
                         '.csv')),
      tags$hr(),
      selectInput('y_var', label = "y axis", choices = c("")),
      selectInput('x_var', label = "x axis", choices = c("")),
      checkboxInput('points', 'points', value = TRUE),
      checkboxInput('lines', 'lines', value = FALSE),
      checkboxInput('group', 'group by', value = FALSE),
      tags$hr(),
      checkboxInput('summary_tab','Summary table?', FALSE),
      tags$hr(),
      h3("Rename variables"),
      selectInput('ren_from', "from", choices = c("")),
      textInput('ren_to', "to"),
      actionButton("rename", "Rename")
    ),
    mainPanel(
      plotOutput('plot'),
      tableOutput('contents')
    )
  )
))
