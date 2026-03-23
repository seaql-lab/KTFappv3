#' Create Connection to Kennedy Lakes Database.
#'
#' This function establishes a connection to the Kennedy Lakes database using
#' PostgreSQL. It utilizes environment variables for database credentials to
#' enhance security.
#' @export
connectKennedy <- function() {

  require(RPostgreSQL)
  require(DBI)
  
  # Database credentials and details from environment variables
  dbuser <- Sys.getenv("DB_USER", "spr") # database user
  dbpass <- Sys.getenv("DB_PASS", "spr_pass") # database password
  dbname <- "kennedylakes"
  dbhost <- "localhost"
  dbport <- 5432
  
  # Establish connection
  drv <- dbDriver("PostgreSQL")
  con <- 
  tryCatch({
    dbConnect(drv, host=dbhost, port=dbport, dbname=dbname, user=dbuser, password=dbpass)
  }, error = function(e) {
    cat("Error connecting to database: ", e$message, "\n")
    NULL
  })
  
  return(con)
}
