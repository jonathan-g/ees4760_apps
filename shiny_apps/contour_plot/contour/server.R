#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(stringr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  profit <- seq(-5000,10000, by = 100)
  risk <- seq(0, 0.1, by = 0.001)
  profit_risk <- expand.grid(profit = profit, risk = risk)
  calc_utility <- function(wealth, profit, risk, time_horizon) {
    pmax(0, (wealth + profit * time_horizon) * (1 - risk)^time_horizon)
  }

  utility <- reactive({
    current_wealth <- input$current_wealth
    time_horizon <- input$time_horizon
    contours = input$contours
    u <- profit_risk %>% mutate(utility = calc_utility(current_wealth, profit, risk, time_horizon))
    bks <- c(-10, pretty(u$utility, contours - 2))
    disc.u <- with(u, cut(utility, bks, dig.lab = 6, ordered_result = TRUE))
    disc.u <- disc.u %>% str_replace_all(c("[\\[\\(\\]]" = "", "^-[0-9]+([^0-9])" = "0\\1", "," = " - ", "0 - 0" = "0"))
    levels <- unique(disc.u)
    ranges <- levels %>% str_split(" - ") %>% lapply(as.numeric) %>%
      lapply(function(x) {if(length(x) == 1) x = c(x, x); x}) %>%
      unlist() %>% matrix(ncol = 2, byrow = TRUE) %>% as.data.frame() %>%
      {names(.) <- c('start','end'); .} %>%
      mutate(levels = levels) %>%
      arrange(start, end)
    disc.u <- ordered(disc.u, levels = ranges$levels)
    u$disc.u <- disc.u
    u
  })

  output$distPlot <- renderPlot({
    contours <- nlevels(utility()$disc.u) + 1
    # contours <- input$contours
    c2 <- contours %/% 2
    c1 <- contours - c2
    message(contours, " contours: ", c1, " and ", c2)
    p1 <- brewer.pal(max(3, c1), "YlOrBr")[1:c1]
    p2 <- brewer.pal(max(3, c2 + 1), "YlGnBu")[2:(c2+1)]
    pal <- c(rev(p2), p1, "#000000")
    ggplot(utility(), aes(x = profit, y = risk)) +
      # geom_tile(aes(fill=utility)) +
      geom_tile(aes(fill = disc.u)) +
      scale_fill_manual("Utility", values = pal,
                        guide = guide_legend(reverse=TRUE)) +
      scale_y_continuous(expand = c(0,0)) +
      scale_x_continuous(expand = c(0,0)) +
      labs(x = "Annual profit", y = "Annual risk of failure") +
      theme_bw(base_size = 20)
  })

    output$selection <- renderPrint({
    if (is.null(input$distPlot_click$x))
      return(cat("Click on plot to read out\nexpected utility"))
      profit <- input$distPlot_click$x
      risk <- input$distPlot_click$y
      u <- calc_utility(input$current_wealth, profit, risk, input$time_horizon)
    text <- paste0("Profit = ", formatC(profit, digits = 0, format = "f"),
           ", risk = ", formatC(risk,digits = 2, format = 'f', drop0trailing = FALSE),
           ",\nexpected utility = ", formatC(u, digits = 0, format = 'f', big.mark = ','))
      cat(text)
  })

})
