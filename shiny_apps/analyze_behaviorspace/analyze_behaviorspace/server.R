
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

options(warn = 2)

shinyServer(function(input, output, session) {
  experiment <- reactiveValues(
    data = NULL,
    ind_vars = NULL,
    dep_vars = NULL,
    mapping = NULL
  )

  tx_name <- function(var_name, mapping) {
    mapping$col[mapping$name == var_name]
  }

  tx_col <- function(var_col, mapping) {
    mapping$name[mapping$col == var_col]
  }

  expt_vars <- reactive({
    df <- experiment$data
    vars <- experiment$mapping
    if (is.null(df) || is.null(vars)) return(NULL)
    vars %>% filter(!(name %in% c('run'))) %>%
      filter( col %>%
                lapply(function(x) length(unique(df[,as.character(x)])) > 1) %>%
                unlist()
      )
  })

  expt_yvars <- reactive({
    x_var <- input$x_var
    vars <- expt_vars()
    if (is.null(vars)) return(NULL)
    vars %>% filter(col != x_var)
  })

  expt_group_vars <- reactive({
    vars <- expt_yvars()
    y_var <- input$y_var
    ind_vars <- experiment$ind_vars
    if (any(is.null(vars), is.null(y_var), is.null(ind_vars))) return(NULL)
    vars %>% filter(col != y_var & col %in% ind_vars)
  })

  expt_plot_vars <- reactive({
    y_var <- input$y_var
    ind_vars <- experiment$ind_vars
    dep_vars <- experiment$dep_vars
    vars <- expt_yvars()
    if (any(is.null(y_var), is.null(ind_vars), is.null(dep_vars), is.null(vars)))
      return(NULL)
    vars %>% filter(name != y_var & col %in% c(ind_vars, dep_vars))
  })

  classify_vars <- function(df) {
    n <- colnames(df)
    run <- which(n == 'run')
    tick <- which(n == 'tick')
    ind_vars <- character(0)
    if (tick > run + 1) {
      ind_vars <- n[(run + 1):(tick - 1)]
    }
    dep_vars <-  tail(n, -tick)
    list(ind_vars = ind_vars, dep_vars = dep_vars)
  }

  bs_data <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.

    validate(
      need(! is.null(input$file1), "Please select a .csv file from a BehaviorSpace experiment.")
    )

    inFile <- input$file1
    if (is.null(inFile)) return(NULL)

    text <- readLines(inFile$datapath)
    skip_lines <- which(str_detect(text, '^"\\[run number\\]"'))
    if (length(skip_lines) > 0) skip_lines = skip_lines[1] - 1
    d <- read.csv(text = text, header = TRUE, skip = skip_lines) %>%
      rename(run = X.run.number., tick = X.step.)
    num_vars <- d %>% map_lgl(is.numeric) %>% keep(~.x) %>% names()
    d <- d %>% select_(.dots = num_vars) %>%
      arrange(run, tick)
    names(d) <- str_replace_all(names(d), '\\.+','.')
    vars <- classify_vars(d)
    invisible(list(data = d, ind_vars = vars$ind_vars, dep_vars = vars$dep_vars,
                   mapping = data.frame(col = names(d), name = names(d),
                                        stringsAsFactors = F)))
  })

  observeEvent(bs_data(),
               {
                 # message("New Behaviorspace Data")
                 expt <- bs_data()
                 experiment$data <- expt$data
                 experiment$ind_vars <- expt$ind_vars
                 experiment$dep_vars <- expt$dep_vars
                 experiment$mapping <- expt$mapping

                 updateSelectInput(session, "ren_from", "", selected = "")
                 updateSelectInput(session, "x_var", choices = "", selected = "")
                 updateSelectInput(session, "y_var", choices = "", selected = "")
                 updateSelectInput(session, "group_var", choices = "", selected = "")
               })

  observeEvent(experiment$mapping, {
    xv <- input$x_var
    yv <- input$y_var
    gv <- input$group_var
    rv <- input$ren_from

    vars <- expt_vars() %>% {set_names(.$col, .$name)} %>% as.list()
    if (! xv %in% vars) xv <- ''
    updateSelectInput(session, "x_var", choices = vars, selected = xv)

    yvars <- expt_yvars() %>% {set_names(.$col, .$name)} %>% as.list()
    if (! yv %in% yvars) yv <- ''
    updateSelectInput(session, "y_var", choices = yvars, selected = yv)
    gvars <- expt_group_vars() %>% {set_names(.$col, .$name)} %>% as.list()
    if (! gv %in% gvars) gv <- ''
    updateSelectInput(session, "group_var", choices = gvars, selected = gv)
    rvars <- expt_vars()%>% {set_names(.$col, .$name)} %>% as.list()
    if (! rv %in% rvars) rv <- ''
    updateSelectInput(session, "ren_from", choices = rvars, selected = rv)
    # message("Updated rename variables: (", paste(names(rvars), rvars, sep = " = ", collapse = ", "), ")")
  })

  observeEvent(expt_yvars(), {
    yv <- input$y_var
    yvars <- expt_yvars() %>% {set_names(.$col, .$name)} %>% as.list()
    if (! yv %in% yvars) yv <- ''
    updateSelectInput(session, "y_var", choices = yvars, selected = yv)
  })

  observeEvent(expt_group_vars(), {
    gv <- input$y_var
    gvars <- expt_group_vars() %>% {set_names(.$col, .$name)} %>% as.list()
    if (! gv %in% gvars) gv <- ''
    updateSelectInput(session, "group_var", choices = gvars, selected = gv)
  })

  observeEvent(input$rename, {
    mapping <- experiment$mapping
    ren_from <- input$ren_from
    ren_to <- input$ren_to
    vars <- expt_vars()
    if (nrow(mapping) == 0 || is.null(vars)) return()
    validate(
      need(! (ren_to %in% filter(mapping, col != ren_from)$name),
           paste("Variable name \"", ren_to, "\" already in use."))
    )

    mapping$name[mapping$col == ren_from] <- ren_to

    rvars <- expt_vars()%>% {set_names(.$col, .$name)} %>% as.list()
    if (! ren_from  %in% rvars) ren_from <- ''
    updateSelectInput(session, "ren_from", choices = rvars, selected = ren_from)
    updateTextInput(session, "ren_to", value = "")
    experiment$mapping <- mapping
  })

  plot_data <- reactive({
    x_var <- input$x_var
    y_var <- input$y_var
    g_var <- input$group_var
    last_tick <- input$last_tick
    exp_data <- experiment$data
    mapping <- experiment$mapping

    # message("plot_data: Data = ", class(exp_data))
    if (is.null(exp_data) || is.null(mapping)) {
      # message("plot_data: empty data")
      return(NULL)
    }
    if (! all(expt_plot_vars()$col %in% names(exp_data))) {
      # message("Variable mismatch")
      return(NULL)
    }
    if (x_var == '' || y_var == '') {
      return(NULL)
    }

    # message("Checking plotting variables")
    if (! all(c(x_var, y_var) %in% names(exp_data))) {
      # message("Bad plotting variables")
      return(NULL)
    }

    pv <- expt_plot_vars()
    gv <- expt_group_vars()
    # message("Plot vars = ", paste0(pv, collapse = ', '))

    if (last_tick || (! 'tick' %in% c(x_var, y_var))) {
      max_tick <- max(exp_data$tick, na.rm=T)
      # message("Filtering to last tick: ", max_tick)
      exp_data <- exp_data %>% filter(tick == max_tick)
    }

    if (length(pv) >= 1) {
      if (g_var %in% gv) {
        grouping <- unique(c('tick', x_var, g_var))
      } else {
        grouping <- unique(c('tick', x_var))
      }
      grouping <- grouping %>% discard(~.x == y_var)

      #message("Summarizing ", tx_col(y_var, mapping), " by ",
      #        paste(map_chr(grouping, tx_col, mapping), collapse=", "))
      dots <- setNames(paste0(c("mean","sd"), "(", y_var, ")"),
                       c(paste0(y_var, "_mean"), paste0(y_var, "_sd")))
      #message("dots = ", paste0(dots, collapse = ", "))
      #message("Gropuing")
      exp_data <- exp_data %>% group_by_(.dots = grouping) %>%
        summarize_(.dots = dots) %>%
        rename_(.dots = setNames(list(paste0(y_var, "_mean")), y_var)) %>%
        ungroup()
      #message("Ungrouped: names = ", paste0(names(exp_data), collapse = ', '))
    }
    exp_data
  })

  plot_mapping <- reactive({
    x_var <- input$x_var
    y_var <- input$y_var
    g_var <- input$group_var
    err_bars <- input$error_bars
    gv <- expt_group_vars()
    mapping <- experiment$mapping
    plt_data <- plot_data()
    if (x_var == "" || y_var == "") return(NULL)
    if (is.null(mapping)) return(NULL)
    # message("Mapping")
    p_map_list = list(x = x_var, y = y_var)
    plot_legend <- NULL
    if (g_var %in% gv$col) {
      p_map_list <- c(p_map_list,  colour = paste0("ordered(", g_var,")"))
      plot_legend <- tx_col(g_var, mapping)
    }
    sd_name <- paste0(y_var, "_sd")
    if (err_bars && sd_name %in% names(plt_data)) {
      p_map_list <- c(p_map_list,
                      ymin = paste0(y_var, " - ", sd_name),
                      ymax = paste0(y_var, " + ", sd_name)
      )
    }
    p_map <- do.call(aes_string, p_map_list)
    plot_labs <- labs(x = tx_col(x_var, mapping), y = tx_col(y_var, mapping))
    rval <- list(mapping = p_map, labels = plot_labs, legend = plot_legend)
    # message("plot_mapping: rval = ", rval)
    rval
  })

  output$contents <- renderTable({
    if (is.null(experiment$data)) return(NULL)
    dots <- experiment$mapping %>% {set_names(.$col, .$name)}
    plt_data <- plot_data()
    if(input$summary_tab && ! is.null(plt_data)) {
      dots <- dots %>% keep(~.x %in% names(plt_data))
      plt_data <- plt_data %>% rename_(.dots = dots)
      return(plt_data)
    } else {
      expt_data <- experiment$data
      if (input$last_tick) {
        max_tick_ <- max(expt_data$tick)
        expt_data <- expt_data %>% filter(tick == max_tick_)
      }
      dots <- dots %>% keep(~.x %in% names(expt_data))
      expt_data <- expt_data %>% rename_(.dots = dots)
      return(expt_data)
    }
  })

  output$plot <- renderPlot({
    points <- input$points
    lines <- input$lines
    y_var <- input$y_var
    err_bars <- input$error_bars
    p_map <- plot_mapping()
    df <- plot_data()
    if (is.null(p_map) || is.null(df)) return()
    sd_name <- paste0(y_var, "_sd")
#    message("output plot: mapping = ", p_map)
    pm_mapping <- p_map$mapping
    pm_labs <- p_map$labels
    pm_legend <- p_map$legend
    # message("Plotting")
    p <- ggplot(df, pm_mapping)
    if (lines) p <- p + geom_line()
    if (err_bars && sd_name %in% names(df)) p <- p + geom_errorbar()
    if (points) p <- p + geom_point()
    if (! is.null(pm_legend)) {
      # message("adding legend ", pm_legend)
      p <- p + scale_colour_discrete(guide = guide_legend(pm_legend, reverse = TRUE))
    }
    # message("Labs = ", pm_labs)
    p <- p + pm_labs
    p + theme_bw(base_size = 20)
  })
})
