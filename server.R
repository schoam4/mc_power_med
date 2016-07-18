#--- PREAMBLE (RUNS ONLY ONCE WHEN APP LOADS) --------------------------------#

# Load MASS package
require(MASS)

#--- SERVER SCRIPT -----------------------------------------------------------#

function(input, output) {
  
  # Execute model-specific power analysis code
  calc_power <- eventReactive(input$action, {
    out <- tryCatch(
      {source(paste0("./code/", input$model, ".R"), local = TRUE)$value},
      error = function(e) {return(e$message)}
    )
  })  

  # Display output or display error messages
  observeEvent(calc_power(),
    if (class(calc_power()) != "data.frame") {
      output$power <- renderText({
        paste0("<div class=\"alert alert-dismissible alert-danger\">",
               calc_power(), "</div>")})
    } else {
      output$power <- renderTable({ 
        calc_power()
      }, include.rownames=FALSE)
    }
  )

  # Render Objective Input Options
  output$obj_options <- renderUI({
    if (input$obj == "choose_n") {
      list(
        
        # Sample size input form
        withTags(
          table(
            td(nowrap = NA, label("Sample Size (N)")),
            td(numericInput(inputId = "N", label = NULL, value = 100))
          )
        )
      )
    } else {
      list(
        
        # Target power input form
        withTags(
          table(
            td(nowrap = NA, label("Target Power")),
            td(numericInput(inputId = "TarPow", label = NULL, value = 0.80,
                            width = "100%"))
          )
        ),
        
        # Minimum sample size input form
        withTags(
          table(
            td(nowrap = NA, label("Minimum N")),
            td(numericInput(inputId = "Nlow", label = NULL, value = 50))
          )
        ),
        
        # Maximum sample size input form
        withTags(
          table(
            td(nowrap = NA, label("Maximum N")),
            td(numericInput(inputId = "Nhigh", label = NULL, value = 200))
          )
        ),
        
        # Sample size steps input form
        withTags(
          table(
              td(nowrap = NA, label("Sample Size Steps")),
              td(numericInput(inputId = "Nsteps", label = NULL, value = 1))
            )
        )
      )
    }
  })
  
  # Render Image of Mediation Model 
  output$model_image <- renderImage({
    filename <- normalizePath(file.path(paste0('./images/', input$model, '.jpg'))) # Note: This can be reactive
    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste("Mediation model path diagram"))
    }, deleteFile = FALSE)

  # Render Input Options UI
  
  # NOTE 1: The '$value' is included because source() returns a logical in addition
  # to the UI output when running source in this way.
  
  # NOTE 2: If you examine the ui.R files - containing the model-specific UI
  # input code, you'll notice that text input is obtained. There doesn't seem
  # to be a straightforward way to get rid of the step selector in the
  # numericInput() widgets, so the suggested solution is to import text and
  # convert to numeric (which is done in the model-specific power analysis
  # files)
  
  observeEvent(input$input_method, {
    if (input$input_method == "correlations") {
      output$input_options <- renderUI({
        source(paste0("./code/", input$model, "_correlations_ui.R"), local = TRUE)$value  
      })
    }
    # Other input methods could go here
  })
}