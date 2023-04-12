# FUNCTIONS FOR INTERACTING WITH DB

connect_to_edgar <- function(){

  source("db_login.R")

  odbc::dbConnect(
    odbc::odbc(),
    Driver   = "SQL Server",
    Server   = "139.191.9.230",
    Database = "EDGAR_v4",
    UID      = uname,
    PWD      = pwd,
    Port     = 1433
  )

}

get_emissions_data <- function(){
  # stuff
  # might be better to do join in SQL directly
}

get_country_info <- function(){
  # stuff
  # might be better to do join in SQL directly
}
