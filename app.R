# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# about: Shiny app to demonstrate authentication  #
# author: Thomas FILAIRE                          #
# date: 2020                                      #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# PART 1 - SETTING THE SCENE ----

# section 1.0 - load required packages ----
library(shiny)          # web app framework 
library(shinyauthr)     # shiny authentication modules
library(shinyjs)        # improve user experience
library(shinymanager)   # shiny authentication modules
library(shinythemes)    # themes for shiny
library(sodium)         # crypto library
library(tidyverse)      # data manipulation

# section 1.1 - source module ----
source("modules/module_login.R")

# PART 2 - UI PART ----
ui <- navbarPage(
  title = "R Shiny advanced tips series",
  collapsible = TRUE,
  windowTitle = "R Shiny tips - TFI",
  theme = shinytheme("readable"),        
  
  tabPanel(
    title = "Demo",
    
    useShinyjs(),  # Include shinyjs
    
    # section 2.0 - header ----
    div(
      class = "container", 
      h1(class = "page-header", 
         "Authentication layer in Shiny", 
         tags$small("- Thomas Filaire")),
      
      p(class = "lead", 
        "Learn how to secure access to your Shiny apps"),
      
      p("Note: this application is the companion of my Medium post; read 
              it to understand its underlying principles!"),
      hr()
    ),
    
    # section 2.1 - BASIC AUTHENTICATION ----
    
    # section 2.1.1 - explanations ----
    div(
      class = "container", 
      h2("First approach:",  
         tags$small("learn the basics")),
      p("Quickly build your own, fully customizable, login form. Code is 
              directly written in the core of the R Shiny application."),
      p("Give it a try:"),
      tags$ul(
        tags$li("username: user_basic_1"),
        tags$li("password: pass_basic_1")
      ),
      hr()
    ),
    div(
      class = "container", 
      
      # section 2.1.2 - login form ----
      column(
        width = 12,
        
        div(
          id = "login-basic", 
          style = "width: 500px; max-width: 100%; margin: 0 auto;",
          
          div(
            class = "well",
            h4(class = "text-center", "Please login"),
            p(class = "text-center", tags$small("First approach login form")),
            
            textInput(
              inputId     = "ti_user_name_basic", 
              label       = tagList(icon("user"), "User Name"),
              placeholder = "Enter user name"
            ),
            
            passwordInput(
              inputId     = "ti_password_basic", 
              label       = tagList(icon("unlock-alt"), 
                                    "Password"), 
              placeholder = "Enter password"
            ), 
            
            div(
              class = "text-center",
              actionButton(
                inputId = "ab_login_button_basic", 
                label = "Log in",
                class = "btn-primary"
              )
            )
          ),
        ),
        
        # section 2.1.3 - app ----    
        uiOutput(outputId = "display_content_basic")
      ),
      
    ),
    
    # section 2.2 - USING A MODULE ----
    
    # section 2.2.1 - explanations ----
    div(
      class = "container",
      hr(),
      h2("Second approach:", 
         tags$small("use a module")),
      p("Build your own module for R Shiny app authentication. Code is 
              packaged into a module, which you can reuse across many Shiny 
              apps."),
      p("Give it a try:"),
      tags$ul(
        tags$li("username: user_module_1"),
        tags$li("password: pass_module_1")
      ),
      hr()
    ), 
    
    div(
      class = "container",
      
      column(
        width = 12,
        
        # section 2.2.2 - login form ----
        login_ui(id = "module_login", title = "Please login"),
        
        # section 2.2.3 - app ----
        uiOutput(outputId = "display_content_module")
      )
    ),
    
    # section 2.3 - USING SHINYAUTHR ----
    
    # section 2.3.1 - explanations ----
    div(
      class = "container",
      hr(),
      h2("Third approach:", 
         tags$small("using shinyauthr package + encrypted password")), 
      p("Leverage an existing package dedicated to authentication. 
              Slightly less customizable but proven robustness and easy 
              implementation."),
      p("Give it a try:"),
      tags$ul(
        tags$li("username: user_authr_1"),
        tags$li("password: pass_authr_1")
      ),
      hr()
    ), 
    
    div(
      class = "container",
      
      column(
        width = 12, 
        
        # section 2.3.2 - login form ----
        shinyauthr::loginUI(
          id = "authr_login", 
          title = h4(class = "text-center", "Please login")
        ),
        
        # section 2.3.3 - app ----
        uiOutput(outputId = "display_content_authr"),
        
        verbatimTextOutput(outputId = "test")
      )
    )
  )
)

# PART 3 - SERVER PART ----
server <- function(input, output, session) {
  
  # section 3.1 - server logic for BASIC AUTHENTICATION ----
  
  # section 3.1.1 - create userbase for basic authentication ----
  user_base_basic_tbl <- tibble(
    user_name = "user_basic_1",
    password  = "pass_basic_1"
  )
  
  # section 3.1.2 - check credentials vs tibble ----
  validate_password_basic <- eventReactive(input$ab_login_button_basic, {
    
    validate <- FALSE
    
    if (input$ti_user_name_basic == user_base_basic_tbl$user_name &&
        input$ti_password_basic == user_base_basic_tbl$password) {
      validate <- TRUE
    }
  })
  
  # section 3.1.3 - hide form ----
  observeEvent(validate_password_basic(), {
    shinyjs::hide(id = "login-basic")
  })
  
  # section 3.1.4 - app ----
  output$display_content_basic <- renderUI({
    
    req(validate_password_basic())
    
    div(
      class = "bg-success",
      id = "success_basic",
      h4("Access confirmed!"),
      p("Welcome to your basically-secured application!")
    )
    
  })
  
  # section 3.2 - server logic for MODULE-BASED AUTHENTICATION ----
  
  # section 3.2.1 - create userbase for module-based authentication ----
  user_base_module_tbl <- tibble(
    user_name = "user_module_1",
    password  = "pass_module_1"
  )
  
  # section 3.2.2 - check credentials vs tibble ----
  validate_password_module <- callModule(
    module   = validate_pwd, 
    id       = "module_login", 
    data     = user_base_module_tbl, 
    user_col = user_name, 
    pwd_col  = password
  )
  
  # section 3.1.3 - app ----
  output$display_content_module <- renderUI({
    
    req(validate_password_module())
    
    div(
      class = "bg-success",
      id = "success_module",
      h4("Access confirmed!"),
      p("Welcome to your module-secured application!")
    )
    
  })
  
  # section 3.3 - server logic for SHINYAUTHR AUTHENTICATION ----
  
  # section 3.3.1 - create userbase for module-based authentication ----
  user_base_authr_tbl <- tibble(
    user_name = "user_authr_1",
    password  = password_store("pass_authr_1")
  )
  
  user_auth <- reactive({
    credentials()$user_auth
  })
  
  user_data <- reactive({
    credentials()$info
  })
  
  # section 3.3.2 - check credentials vs tibble ----
  
  # call the logout module with reactive trigger to hide/show
  logout_init <- callModule(
    module = shinyauthr::logoutServer, 
    id     = "authr_logout",
    active = reactive(user_auth)
  )
  
  credentials <- callModule(
    module   = shinyauthr::loginServer,
    id       = "authr_login",
    data     = user_base_authr_tbl, 
    user_col = user_name,
    pwd_col  = password, 
    sodium_hashed = TRUE,
    log_out  = reactive(logout_init())
  )
  
  output$test <- renderPrint({
    
    req(user_auth())
    
    user_data()
  })
  
  # section 3.3.3 - app ----
  output$display_content_authr <- renderUI({
    
    req(user_auth())
    
    div(
      class = "bg-success",
      id = "success_module",
      h4("Access confirmed!"),
      p("Welcome to your shinyauthr-secured application! 
              Notice that password is encrypted.")
    )
    
  })
}

# PART 4 - RUN APPLICATION ----
shinyApp(ui = ui, server = server)
