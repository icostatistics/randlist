library(shinydashboard)
library(shiny)
library(tidyverse)
library(openxlsx)



ui <- function(request){
  dashboardPage(
  dashboardHeader(title = "Random allocation list generator"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Parameters", tabName = "params"),
      radioButtons("arms", "Number of treatment arms", choices = seq(2, 6, 1), inline = TRUE),
      radioButtons("blocks", "Type of randomisation", 
                   c("Simple" = "simple",
                     "Fixed Block Size" = "fixed",
                     "Random Block Size" = "random"),
        selected="random"),
      selectInput("nsites", "Number of sites", choices = seq(1, 50, 1)),
      radioButtons("stratified", "Is the random allocation list stratified, except by site?",
        c("No" = "n", "Yes" = "y"),
        inline = TRUE
      ),
      conditionalPanel(
        condition = "input.stratified == 'y'",
        radioButtons("nstrata", "Number of stratification variables",
          choices = seq(1, 4, 1), inline = TRUE
        )
      ),
      numericInput("npat", "Number of patients within each site or site/strata combination",
        min = 2, value = 20
      ),
      radioButtons("setseed", "Set seed? If no, seed is set randomly",
        c("Yes" = "y", "No" = "n"),
        inline = TRUE
      ),
      conditionalPanel(
        condition = "input.setseed == 'y'",
        numericInput("seed", "Seed", min = 1, value = 42)
      )
    )
  ),

  dashboardBody(
  
      fluidRow(
        valueBoxOutput("seedbox", width = 12)
        
      ),
    fluidRow(
      box(width = 6, title = "Treatment arms", uiOutput("arms")),
      box(width = 6, title = "Block size(s)", uiOutput("blocks")),
      box(width = 6, title = "Download", 
          radioButtons("output_format", "Output format", 
                       c( "xlsx" = ".xlsx","csv" = ".csv"), 
                       inline = TRUE),
          radioButtons("blockinfo", "With block information?", 
                       c("Yes" = "y", "No" = "n")),
          downloadButton("gen_xls", "Download list"), 
          downloadButton("report", "Download report"), 
          downloadButton("params", "Download Parameters"), 
          downloadButton("code", "Download Code"), 
          bookmarkButton()
          )
    ),
    fluidRow(
      conditionalPanel(
        condition = "input.stratified == 'y'",
        uiOutput("strata1"),
        uiOutput("strata2")
      ),
      tableOutput("table")
    )
  )
)
}

server <- function(input, output, session) {
  output$arms <- renderUI({
    v <- list()
    armnames <- c(
      "Control", "Intervention", "Intervention 2",
      "Intervention 3", "Intervention 4", "Intervention 5"
    )
    v[[1]] <- textInput("allocvar", "Name of allocation variable",
      value = "ARM"
    )

    for (i in 1:input$arms) {
      v[[i + 1]] <- textInput(paste0("arm", i),
        paste0("Name of arm ", i),
        value = armnames[[i]]
      )
    }
    v
  })

  seed <- reactive({
    if (input$setseed == "n") {
      seed <- round(as.POSIXlt(Sys.time())$sec * 100000) %% 100000
    } else {
      seed <- input$seed
    }
    seed
  })

  output$seedbox <- renderValueBox({
    valueBox(seed(), "Seed",
      icon = icon("seedling", lib = "font-awesome"),
      color = "green"
    )
  })

  output$blocks <- renderUI({
    arms <- as.integer(input$arms)
    switch(input$blocks,
      "simple" = helpText("Simple randomization, no blocks"),
      "fixed" = sliderInput("fixedsize", "Fixed block size",
        min = arms,
        max = arms * 6,
        value = arms * 2,
        step = arms
      ),
      "random" = sliderInput("randomsize", "Random block sizes",
        min = arms,
        max = arms * 6,
        value = c(arms * 2, arms * 4),
        step = arms
      )
    )
  })

  output$strata1 <- renderUI({
    f_strata <- function(i) {
      tabPanel(
        paste0("Strata", i),
        textInput(paste0("stratname", i),
          "Name of strata variable",
          value = paste0("Stratavar", i)
        ),
        radioButtons(paste0("nstratavals", i),
          "Number of categories",
          choices = seq(2, 6, 1), inline = TRUE
        )
      )
    }
    tabs <- lapply(1:input$nstrata, f_strata)
    do.call(tabBox, tabs)
  })

  f_strata2 <- function(i) {
    tabPanel(paste0("Strata", i), {
      strvals <- tagList()
      n <- input[[paste0("nstratavals", i)]]
      if (is.null(n)) n <- 2
      for (k in 1:n) {
        strvals[[k]] <- textInput(paste0("stratval", i, k),
          paste0("Name of category", k),
          value = k
        )
      }
      strvals
    })
  }

  output$strata2 <- renderUI({
    tabs2 <- lapply(1:input$nstrata, f_strata2)
    do.call(tabBox, tabs2)
  })

  stratalist <- reactive({
    stratalist <- list()
    if (input$stratified == "y") {
      for (i in 1:input$nstrata) {
        n <- input[[paste0("nstratavals", i)]]
        if (is.null(n)) n <- 2
        vallist <- vector(mode = "character", length = n)
        for (k in 1:n) {
          if (is.null(input[[paste0("stratval", i, k)]])) {
            stratval <- "tmp"
          }
          else {
            stratval <- input[[paste0("stratval", i, k)]]
          }
          vallist[k] <- stratval
        }

        stratalist[[i]] <- vallist
      }
      stratalist <- expand.grid(stratalist)
      stratalist <- as_tibble(stratalist)
      stratanames <- lapply(
        1:input$nstrata,
        function(i) {
          input[[paste0("stratname", i)]]
        }
      )
      names(stratalist) <- stratanames
      stratalist
    }
  })

  alloclist <- reactive({
    allocvec <- vector(mode = "character", length = as.integer(input$arms))
    for (i in 1:input$arms){
      #allocvec[i] <- "test"
      if (is.null(input[[paste0("arm",i)]])){
        armval <- "tmp"
      } else {
        armval <- input[[paste0("arm",i)]]
      }
      allocvec[[i]] <- armval
    }
    #alloclist <- list()
    req(input$allocvar)
    alloclist <- enframe(allocvec, name = NULL, value = input$allocvar)
    #names(alloclist) <- c(input$allocvar)
    alloclist
    #alloclist <- as_tibble(alloclist)
  })

  blocklist <- reactive ({
    if (input$blocks == "random" & is.numeric(input$randomsize)) {
      blocksize <- seq(
        from = input$randomsize[1],
        to = input$randomsize[2],
        by = as.integer(input$arms)
      )
      blockno <- as.integer(ceiling(input$npat / mean(blocksize)))
    }
    else if (input$blocks == "fixed" & is.numeric(input$fixedsize)) {
      blocksize <- input$fixedsize
      blockno <- as.integer(ceiling(input$npat / blocksize))
    }
    else {
      blocksize <- ceiling(input$npat/as.integer(input$arms))*as.integer(input$arms)
      blockno <- 1
    }
    blocklist <- list(blocksize, blockno)
    blocklist
  })
  
  randlist <- reactive({
   
    blocksize <- blocklist()[[1]]
    blockno <- blocklist()[[2]]
  
    set.seed(seed())
   
    randlist <- stratalist() %>%
      crossing(SiteCode = 1:input$nsites) %>%
      arrange(SiteCode) %>% 
      mutate(tempstrat = 1:n()) %>%
      crossing(blockno = 1:blockno) %>%
      mutate(blockno = 1:n()) %>% 
      group_by(blockno) %>% 
      mutate(
        blocksize = ifelse(length(blocksize) > 1,
          sample(blocksize, 1),
          blocksize),
        blocksize = as.integer(blocksize)
        ) %>%
      mutate(blocksize2 = blocksize/as.integer(input$arms)) %>%
      crossing(alloclist()) %>% 
      ungroup() %>%
      select(SiteCode, blockno, blocksize, everything()) %>%
      arrange(SiteCode, blockno) %>%
      uncount(blocksize2) %>%
      group_by(blockno) %>%
      mutate(rand = runif(n())) %>%
      arrange(blockno, rand) %>%
      mutate(seq_in_block = 1:n()) %>% 
      group_by(SiteCode) %>% 
      mutate(RandNo = as.integer(SiteCode*1000 + 1:n())) %>% 
      select(-tempstrat, -rand) %>% 
      select( SiteCode, RandNo, everything())
      
    if (input$blockinfo == "y") {
      randlist
    } else {
      randlist %>% 
        select(-blockno, -blocksize, -seq_in_block) 
    }

  })

  output$table <- renderTable({
    randlist()
    # blocksize <- blocklist()[[1]]
    # blockno <- blocklist()[[2]]
    # fraction <- min(12, mean(blocksize)*blockno)/(mean(blocksize)*blockno)
    # 
    # randlist() %>% 
    #   group_by(SiteCode) %>%
    #   sample_frac(size = fraction) %>% 
    #   arrange(RandNo)
    
  })
  
  
  
  output$gen_xls <- downloadHandler(
    filename = function() {
      paste("RandList ", Sys.Date()," seed ",seed(),input$output_format, sep="")
    },
    content = function(file) {
      if(input$output_format == ".xlsx"){
      write.xlsx(randlist(), file)
      } else {
        write.csv(randlist(), file)
      }
    }
  )

 
output$report <- downloadHandler(
  # For PDF output, change this to "report.pdf"
  filename = function() {
    paste("Report RandList ", Sys.Date()," seed ",seed(),".pdf", sep="")
  },
  content = function(file) {
    # Copy the report file to a temporary directory before processing it, in
    # case we don't have write permissions to the current working dir (which
    # can happen when deployed).
    tempReport <- file.path(tempdir(), "report.Rmd")
    file.copy("report.Rmd", tempReport, overwrite = TRUE)
    
    # Set up parameters to pass to Rmd document
    params <- list(randlist = randlist(),
                   blocksize = blocklist()[[1]],
                   blockno = blocklist()[[2]],
                   seed = seed(),
                   alloclist = alloclist(),
                   stratalist = stratalist(),
                   nsites = input$nsites, 
                   blocks = input$blocks
                   )
    
    # Knit the document, passing in the `params` list, and eval it in a
    # child of the global environment (this isolates the code in the document
    # from the code in this app).
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)

output$params <- downloadHandler(
  #Download  the parameters
  filename = function() {
   "rand_params.rds"
  },
  content = function(file) {
    # Set up parameters 
    params <- list(randlist = randlist(),
                   blocksize = blocklist()[[1]],
                   blockno = blocklist()[[2]],
                   seed = seed(),
                   alloclist = alloclist(),
                   stratalist = stratalist(),
                   nsites = input$nsites, 
                   blocks = input$blocks
    )
    
    #Save the parameters
    write_rds(params, file)
    
  }
)


output$code <- downloadHandler(
  #Download  the parameters
  filename = function() {
    "rand_code.R"
  },
  content = function(file) {
    # Set up parameters 
    text <- 
    "
    library(tidyverse)
    
    params <- read_rds('rand_params.rds')
    
    set.seed(params$seed)
    arms <- nrow(params$alloclist)
    
    my_randlist <- params$stratalist %>%
      crossing(SiteCode = 1:params$nsites) %>%
      arrange(SiteCode) %>%
      mutate(tempstrat = 1:n()) %>%
      crossing(blockno = 1:params$blockno) %>%
      mutate(blockno = 1:n()) %>%
      group_by(blockno) %>%
      mutate(
        blocksize = ifelse(
          length(params$blocksize) > 1,
          sample(params$blocksize, 1),
          params$blocksize
        ),
        blocksize = as.integer(blocksize)
      ) %>%
      mutate(blocksize2 = blocksize / as.integer(arms)) %>%
      crossing(params$alloclist) %>%
      ungroup() %>%
      select(SiteCode, blockno, blocksize, everything()) %>%
      arrange(SiteCode, blockno) %>%
      uncount(blocksize2) %>%
      group_by(blockno) %>%
      mutate(rand = runif(n())) %>%
      arrange(blockno, rand) %>%
      mutate(seq_in_block = 1:n()) %>%
      group_by(SiteCode) %>%
      mutate(RandNo = as.integer(SiteCode * 1000 + 1:n())) %>%
      select(-tempstrat,-rand) %>%
      select(SiteCode, RandNo, everything())
    
      write_excel_csv2(my_randlist, file = 'randlist.csv')
      "
    
    #Save the parameters
    write_file(text, file)
    
  }
)
}

shinyApp(ui = ui, server = server, enableBookmarking = "url")


