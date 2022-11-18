library(shiny.reglog)
library(DT)
library(RSQLite)
library(emayili)


# define custom handler function to read all data from the tables

get_table_handler <- function(self, private, message) {
  
  private$db_check_n_refresh()
  on.exit(private$db_disconnect())
  
  table <- DBI::dbReadTable(private$db_conn,
                            message$data$table_name)
  
  return(RegLogConnectorMessage(
    type = message$type, table = table,
    logcontent = paste("Got data from table:", message$data$table_name)
  ))
}

# read the gmail credentials
creds <- readRDS("creds.Rds")

ui <- fluidPage(
  title = "shiny.reglog demo",
  # for all possibilites of the 'shiny.reglog'
  shinyjs::useShinyjs(),
  # to fix DToutput and showcase mode issues 
  shinyjs::inlineCSS(readLines("style.css")),
  
  # first column with RegLog UI
  column(width = 6,
         tabsetPanel(
           type = "pills",
           tabPanel("Register", RegLog_register_UI(),
                    tags$hr(),
                    actionButton("logout",
                                 "Logout")),
           tabPanel("Login", RegLog_login_UI()),
           tabPanel("Credentials edit", RegLog_credsEdit_UI()),
           tabPanel("Password reset", RegLog_resetPass_UI()))),
  
  )

server <- function(input, output, server) {
  
  # create a temporary database
  sqlite_db <- tempfile(fileext = ".sqlite")
  
  conn <- DBI::dbConnect(RSQLite::SQLite(),
                         dbname = sqlite_db)
  
  DBI_tables_create(conn = conn, verbose = F,
                    # add some credentials for login
                    user_data = data.frame(
                      username = "shiny_reglog_testing",
                      password = "TestingRegLog!",
                      email = "statismike@gmail.com"
                    ),
                    # passwords will be hashed
                    hash_passwords = T)
  
  DBI::dbDisconnect(conn)
  
  # initialize the dbConnect
  dbConnector <- RegLogDBIConnector$new(
    driver = RSQLite::SQLite(),
    dbname = sqlite_db,
    custom_handlers = list(get_table = get_table_handler))
  
  # initialize the mailConnector - use mock connector if no emayili smtp is
  mailConnector <- RegLogEmayiliConnector$new(
    from = creds[1],
    smtp = emayili::gmail(username = creds[1],
                          password = creds[2]))
  
  # initialize main module
  RegLog <- RegLogServer$new(
    dbConnector = dbConnector,
    mailConnector = mailConnector,
    app_name = "shiny.reglog demo")
  
  # send the message to get table
  observeEvent(input$get_table, {
    dbConnector$listener(
      RegLogConnectorMessage(type = "get_table",
                             table_name = input$select_table))
  })
  
  # logout
  observeEvent(input$logout, {
    RegLog$logout()
  })
  
  # render the table contents
  output$db_lookup <- DT::renderDT(
    { req(RegLog$message()$type == "get_table")
      RegLog$message()$data$table },
    options = list(scrollX = TRUE))
  
  # get logs
  observeEvent(input$logs, {
    output$gotten_logs <- DT::renderDT(
      RegLog$get_logs(),
      options = list(scrollX = TRUE)
    )
  })
  
  output$user_data <- renderPrint(
    list(is_logged = RegLog$is_logged(),
         user_id = RegLog$user_id(),
         user_mail = RegLog$user_mail(),
         account_id = RegLog$account_id())
  )
  
  output$reglog_message <- renderPrint(
    RegLog$message()
  )
  
}

shiny::shinyApp(ui, server)