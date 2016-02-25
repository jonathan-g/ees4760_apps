
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(purrr)

shinyServer(function(input, output) {

  output$links <- renderText({
    list.dirs(recursive = FALSE, full.names = FALSE) %>%
      discard(~.x == '') %>%
      unlist() %>%
      map(~paste0(a(.x, href=.x), br())) %>%
      unlist() %>% HTML()
  })

})
