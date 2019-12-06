shinyApp(
    ui = fluidPage(
        sliderInput("slider", "Slider", 1, 100, 50),
        radioButtons("type", 
                     "Type of randomisation?", 
                     c("Simple" = "simple",
                       "Blocked" = "blocked")),
        numericInput("n_stratvar", "Number of stratification variables", 1, min = 0, max = 5),
        conditionalPanel(condition = "input.n_stratvar != 0", uiOutput("stratvars")),
        downloadButton("report", "Generate report"),
        textOutput("test")
    ),
    server = function(input, output) {
        output$stratvars <- renderUI({
            llist <- tagList(NULL)
            for(n in 1:input$n_stratvar){
                varname <- paste0("varname", n)
                name <- paste0("Name of stratification variable ", n)
                init <- paste0("var", n)
                n_stratlev <- paste0("n_stratlev", n)
                n_stratlev_lab <- paste0("Number of levels for stratification variable", n)
                uiobj <- tagList(textInput(varname, name, init), 
                                 numericInput(n_stratlev, n_stratlev_lab, 2, min=2 , max = 6),
                                 renderText({paste0("test",input[[paste0("n_stratlev", n)]]) }))
                llist <- tagList(llist, uiobj )
            }
            llist
        })
        output$test <- renderText({paste0("test",input$n_stratlev1)})
        
        
        output$report <- downloadHandler(
            # For PDF output, change this to "report.pdf"
            filename = "report.pdf",
            content = function(file) {
                # Copy the report file to a temporary directory before processing it, in
                # case we don't have write permissions to the current working dir (which
                # can happen when deployed).
                tempReport <- file.path(tempdir(), "report.Rmd")
                file.copy("report.Rmd", tempReport, overwrite = TRUE)
                
                # Set up parameters to pass to Rmd document
                params <- list(n = input$slider)
                
                # Knit the document, passing in the `params` list, and eval it in a
                # child of the global environment (this isolates the code in the document
                # from the code in this app).
                rmarkdown::render(tempReport, output_file = file,
                                  params = params,
                                  envir = new.env(parent = globalenv())
                )
            }
        )
        
       
    }
)