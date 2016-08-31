#--- UI SCRIPT ---------------------------------------------------------------#

fluidPage(theme = 'spacelab.css', #id = "page",
  
  #--- HTML <HEAD> CODE ------------------------------------------------------#
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css_hacks.css"),
    tags$title("Monte Carlo Power Analysis for Indirect Effects")
  ),
  
  #--- HTML <BODY> CODE ------------------------------------------------------#
  
  # <header>
  withTags(
    header(
      h3("Monte Carlo Power Analysis for Indirect Effects"),
      p("Written by Alexander M. Schoemann (",
        a(href="mailto:schoemanna@ecu.edu","Contact"),
        "), Aaron J. Boulton, & Stephen D. Short")
    )
  ),
  
  # fluidRow: Main content <div>
  fluidRow(
    
    #--- LEFT COLUMN ---------------------------------------------------------#
    column(3, id = "left_column", class = "column",
      
      # Options well panel
      wellPanel(id = "options_well",
        
        # NOTE: Throughout, note that tables were used to structure many of
        # the input widgets. This allowed the labels to be put on the side
        # of the widget instead of above (no published elegant solutions to
        # this)
        
        # Model selector
        withTags(
          table(
            td(nowrap = NA, label("Model")),
            td(selectInput(inputId = "model", label = NULL,
                           choices = list("One Mediator" = "one_mediator",
                                          "Two Parallel Mediators" = "two_parallel_mediators"), 
                           selected = "one_mediator"))
          )
        ),
        
        # Objective selector
        withTags(
          table(
            td(nowrap = NA, label("Objective")),
            td(selectInput(inputId = "obj", label = NULL,
                           choices = list("Set N, Find Power" = "choose_n",
                                          "Set Power, Vary N" = "choose_power")), 
                           selected = "choose_n")
          )
        ), 
        
        # Reactive UI dependent on chosen objective
        wellPanel(id = "obj_options_well", uiOutput("obj_options")),
        
        # Number of replications input
        withTags(
          table(
            td(nowrap = NA, label("# of Replications")),
            td(numericInput(inputId = "powReps", label = NULL, value = 1000))
          )
        ),
        
        # Number of replications input form
        withTags(
          table(
            td(nowrap = NA, label("Monte Carlo Draws per Rep")),
            td(numericInput(inputId = "mcmcReps", label = NULL, value = 20000))
          )
        ),
        
        # Random seed input form
        withTags(
          table(
            td(nowrap = NA, label("Random Seed")),
            td(numericInput(inputId = "seed", label = NULL, value = 1234))        
          )
        ),
        
        # Confidence interval input form
        withTags(
          table(
            td(nowrap = NA, label("Confidence Level (%)")),
            td(numericInput(inputId = "conf", label = NULL, value = 95))
          )
        )
      )
    ),
    
    #--- MIDDLE COLUMN -------------------------------------------------------#
    column(5, id = "middle_column", class = "column",
      
      # Image section     
      imageOutput("model_image"),
      
      # Model input well panel
      wellPanel(id = "model_input_well",
        
        # Input method selector        
        withTags(
          table(id = "input_method_table",
            td(nowrap = NA, label("Input Method")),
            td(selectInput(inputId = "input_method", label = NULL,
                           choices = list("Correlations" = "correlations")
                      ),
                      selected = "correlations")
          )
        ),
        
        # Reactive UI dependent on input method chosen
        uiOutput("input_options")
      )
    ),
    
    #--- RIGHT COLUMN --------------------------------------------------------#
    column(4, id = "right_column", class = "column",
      
      # Instructions well panels     
      wellPanel(id = "instructions_well",
        wellPanel(id = "inner_instructions_well",
          tags$h4(style="text-align:center; font-size:16px;",
             "Instructions"),
          tags$p("To use this app, follow these steps:"),
          tags$p(tags$b("1. Select Model. "), "The user should first select
                 the mediation model containing the indirect effect(s) of
                 interest. Models may be selected in the drop-down menu in
                 the left-most column of the app. Note that when a different
                 mediation model is selected, the model graphic and input-value
                 sections in the middle column will be altered."),
          tags$p(tags$b("2. Select Objective. "), "Once the desired model is
                 chosen, the user should select the objective of the power
                 analysis. Two options are permitted. The user can choose to
                 estimate the statistical power for a given model and sample
                 size (\"Set N, Find Power\"), or the user may choose to
                 estimate the required sample size for a given model and 
                 desired level of power (\"Set Power, Vary N\"). Once an option
is selected, an additional set of options will appear below that require
the user's attention."),
          tags$p(style = "text-indent: 40px;", "For the ",
                 tags$b("Set N, Find Power"), "option:"),
          tags$p(style = "text-indent: 40px;", tags$b("Select Sample Size. "),
                 "The sample size (N) the user wishes to obtain a power
                 estimate for should be entered in the designated box."),
          tags$p(style = "text-indent: 40px;", "For the ",
                 tags$b("Set Power, Vary N"), "option:"),
          tags$p(style = "text-indent: 40px;", tags$b("Select Target Power. "),
                 "The level of statistical power the user wishes to determine
                 a sufficient sample size (N) for should be entered in the
                 designated box."),
          tags$p(style = "text-indent: 40px;", tags$b("Select Minimum N. "),
                 "The lowest sample size that the user wishes to compute a power 
estimate for."),
          tags$p(style = "text-indent: 40px;", tags$b("Select Maximum N. "),
                 "The largest sample size that the user wishes to compute a power 
                 estimate for."),
          tags$p(style = "text-indent: 40px;", tags$b("Select Sample Size Steps. "),
                 "The increments in sample size the user wishes to compute power 
                 estimates for that range between the selected minimum and
                 maximum sample sizes."),
          tags$p(tags$b("3. Select Number of Power Analysis Replications. "),
"The user must select the number of replications computed in the power analysis.
For each replication, a dataset is generated for the selected mediation model
and the Monte Carlo method for computing confidence intervals is used to determine
whether the indirect effect(s) of interest is significantly different from zero.
The power estimate(s) is calculated by dividing the number of replications in
which the indirect effect(s) of interest is significantly different from zero
by the total number of replications."),
tags$p(tags$b("4. Select Number of Monte Carlo Draws per Replications. "),
"For each replication of a given power analysis, the Monte Carlo method
for computing indirect effect confidence intervals requires a set number
of random draws from the distribution of regression coefficients that constitute
the indirect effect(s) of interest. For example, if the user request 5000 Monte
Carlo draws per rep for a single mediator model, the a and b 
coefficients will be sampled from their joint probability distribution 5000 times.
These draws are used to calculate a confidence interval for each indirect effect
in a given model."),
tags$p(tags$b("5. Select Random Seed. "),
       "The user may select a random seed for initiation of the random number
generator used in the application. Identical results can be obtained if the same
seed is select in conjunction with the sample analysis options and model input
values."),
tags$p(tags$b("5. Select Confidence Level. "),
       "Width of the Monte Carlo confidence intervals computed for each indirect
       effect of interest. The default option is 95%."),
tags$p(tags$b("6. Input Model Values. "),
       "To generate data for the select model, the user must enter values
       that allow the application to compute a covariance matrix for all
       variables in the model. Currently, the only input method supported is for users
       to enter the correlations between all variables in a correlation matrix as
       well as the variable standard deviations. Additional methods may become
       available in the future."),
tags$p(tags$b("6. Initiate Power Analysis. "),
       "Once all options and model input values have been specified, the user
       can press the \"Calculate Power\" button to initiate the Monte Carlo
       power analysis. If any options or input values have been entered incorrectly,
       an error message will appear below the button. If all values have been
       entered correctly, a progress bar will appear at the top of the page, indicating
       the power analysis has begun.")
        )
      ),
      
      # Ouput well panel
      wellPanel(id = "output_well",
        
        # 'Calculate Power' button
        actionButton(inputId = "action", label = "Calculate Power",
                     width = "100%", class = "btn-success"),
        
        # Power anlaysis output
        tableOutput("power"))
    )
  )
)