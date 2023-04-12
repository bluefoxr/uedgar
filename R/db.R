# FUNCTIONS FOR INTERACTING WITH DB

connect_to_edgar <- function(){

  odbc::dbConnect(
    odbc::odbc(),
    Driver   = "SQL Server",
    Server   = "139.191.9.230",
    Database = "EDGAR_v4",
    UID      = "edgar_beckewi",
    PWD      = "EOLO1234!",
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
