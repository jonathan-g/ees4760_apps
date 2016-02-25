
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(dplyr)
#library(tidyr)
library(stringr)
library(purrr)
library(ggplot2)

options(warn = 2)

shinyServer(function(input, output, session) {
  calc_vars <- function(exp_data, x_var, y_var) {
    if (nrow(exp_data) == 0) return(character(0))
    vars <- colnames(exp_data) %>%
      discard(~.x %in% c(x_var, y_var, 'run'))
    vars <- vars %>%
      map(.f = function(x) length(unique(exp_data[,x]))) %>%
      unlist() %>% set_names(nm = vars) %>% keep(~.x > 1) %>% names()
    vars
  }

  experiment <- reactiveValues(data = data.frame())

  expt_vars <- reactive({
    exp_data <- experiment$data
    if (nrow(exp_data) == 0) return(character(0))
    vars <- colnames(exp_data) %>%
      discard(~.x %in% c('run'))
    vars <- vars %>%
      map(.f = function(x) length(unique(exp_data[,x]))) %>%
      unlist() %>% set_names(nm = vars) %>% keep(~.x > 1) %>% names()
    vars
  })

  expt_yvars <- reactive({
    vars <- expt_vars() %>%
      discard(~.x == input$x_var)
    vars
  })

  expt_group_vars <- reactive({
    vars <- expt_yvars() %>%
      discard(~.x == input$y_var)
  })


  bs_data <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.

    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)

    text <- readLines(inFile$datapath)
    skip_lines <- which(str_detect(text, '^"\\[run number\\]"'))
    if (length(skip_lines) > 0) skip_lines = skip_lines[1] - 1
    d <- read.csv(text = text, header = TRUE, skip = skip_lines) %>%
      rename(run = X.run.number., tick = X.step.)
    num_vars <- d %>% map(is.numeric) %>% keep(~.x) %>% names()
    d <- d %>% select_(.dots = num_vars) %>%
      arrange(run, tick)
    names(d) <- str_replace_all(names(d), '\\.+','.')
    d
  })

  observeEvent(bs_data(),
               {
                 # message("New Behaviorspace Data")
                 data <- bs_data()
                 experiment$data <- data
                 updateSelectInput(session, "ren_from", "", selected = "")
                 updateSelectInput(session, "x_var", choices = "", selected = "")
                 updateSelectInput(session, "y_var", choices = "", selected = "")
               })

  observeEvent(experiment$data, {
    # message("Updating experimental data and choices")
    exp_data <- experiment$data
    if (! is.data.frame(exp_data)) return()
    if (nrow(exp_data) == 0) return()
    vars <- expt_vars()


    v <- vars %>% discard(~.x %in% c('run','tick'))
    # message("v = ", paste0(v, collapse = ", "))
    xv <- expt_vars()[1]

    updateSelectInput(session, "ren_from", choices = colnames(exp_data))
    updateSelectInput(session, "x_var", choices = expt_vars(), selected = xv)
    yv <- tail(expt_yvars(),1)
    updateSelectInput(session, "y_var", choices = expt_yvars(), selected = yv)
  })

  observeEvent(input$x_var, {
    yvars <- expt_yvars()
    if (length(yvars) == 0) {
      updateSelectInput(session, "y_var", choices = "", selected = "")

    } else {
      updateSelectInput(session, "y_var", choices = yvars, selected = tail(yvars,1))
    }
  })

  observeEvent(input$rename, {
    exp_data <- experiment$data
    if (nrow(exp_data) == 0) return()
    # message("Checking for rename")
    if (input$ren_from %in% colnames(exp_data))
      exp_data <- exp_data %>% rename_( .dots = setNames(list(input$ren_from), input$ren_to))
    updateSelectInput(session, "ren_from", choices = colnames(exp_data), selected = input$ren_to)
    updateTextInput(session, "ren_to", value = "")
    experiment$data <- exp_data
  })

  plot_vars <- reactive({
    x_var <- input$x_var
    y_var <- input$y_var
    exp_data <- experiment$data
    vars <- expt_group_vars()
    if (x_var == '' || y_var == '') return(character(0))
    if (is.null(exp_data) || is.null(vars)) return(character(0))
    if (nrow(exp_data) == 0) return(character(0))
    vars <- vars %>%
      discard(~.x %in% c(x_var, y_var, 'run'))
    # message("plot_vars = ", vars)
    vars
  })

  observeEvent(plot_vars(), {
    updateCheckboxInput(session, 'group',
                        label = paste("group by", plot_vars()[1]))
  })

  plot_data <- reactive({
    x_var <- input$x_var
    y_var <- input$y_var
    exp_data <- experiment$data

    # message("plot_data: Data = ", class(exp_data))
    if (nrow(exp_data) == 0) {
      # message("plot_data: empty data")
      return(data.frame())
    }
    if (! all(plot_vars() %in% names(exp_data))) {
      # message("Variable mismatch")
      return(data.frame())
    }

    # message("Checking plotting variables")
    if (! all(c(x_var, y_var) %in% names(exp_data))) {
      # message("Bad plotting variables")
      return(data.frame())
    }

    pv <- plot_vars()
    # message("Plot vars = ", paste0(pv, collapse = ', '))

    if (! 'tick' %in% c(x_var, y_var)) {
      max_tick <- max(exp_data$tick, na.rm=T)
      # message("Filtering to last tick: ", max_tick)
      exp_data <- exp_data %>% filter(tick == max_tick)
    }

    if (length(pv) >= 1) {
      if (input$group) {
        grouping <- unique(c('tick', x_var, pv)) %>%
          discard(~.x == y_var)
      } else {
      grouping <- unique(c('tick', x_var)) %>%
        discard(~.x == y_var)
    }

      message("Summarizing ", y_var, " by ", paste(grouping, collapse=", "))
      dots <- setNames(paste0(c("mean","sd"), "(", y_var, ")"),
                       c(paste0(y_var, "_mean"), paste0(y_var, "_sd")))
      message("dots = ", paste0(dots, collapse = ", "))
      message("Gropuing")
      exp_data <- exp_data %>% group_by_(.dots = grouping) %>%
        summarize_(.dots = dots) %>%
        rename_(.dots = setNames(list(paste0(y_var, "_mean")), c(y_var))) %>%
        ungroup()
      message("Ungrouped: names = ", paste0(names(exp_data), collapse = ', '))
    }
    exp_data
  })

  plot_mapping <- reactive({
    if (input$x_var == "" || input$y_var == "") return(NULL)
    if (nrow(plot_data()) == 0) return(NULL)
    # message("Mapping")
    if (length(plot_vars()) >= 1 &&  input$group) {
      mapping <- aes_string(x = input$x_var,
                            y = input$y_var,
                            colour = plot_vars()[1])
    } else {
      mapping <- aes_string(x = input$x_var,
                            y = input$y_var)
    }
    mapping
  })

  output$contents <- renderTable({
    if(input$summary_tab) {
      return(plot_data())
    } else {
      return(experiment$data)
    }
  })

  output$plot <- renderPlot({
    if (is.null(plot_mapping())) return()
    if (nrow(plot_data()) == 0) return()
    # message("Plotting")
    p <- ggplot(plot_data(), plot_mapping())
    if (input$points) p <- p + geom_point()
    if (input$lines) p <- p + geom_line()
    p + theme_bw(base_size = 20)
  })
})
