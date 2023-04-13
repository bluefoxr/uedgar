# FUNCTIONS FOR INTERACTING WITH DB
# NOTES
# DBI::dbGetQuery() - run query
# odbc::dbDisconnect(con) - disconnect

#' Establish connection with EDGAR DB
#'
#' You need to be on the network for this to work. The user ID and pwd should
#' be stored in an R file which loads these strings as variables `uname` and `pwd`.
#'
#' @return ODBC connection
#' @export
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

#' Get emissions data
#'
#' Returns a data.table with emissions data as specified. Obviously depending on the query this
#' can lead to a very large table, so use carefully.
#'
#' @param con An ODBC connection: use [connect_to_edgar()]
#' @param table_name Name of the table to query - if not the default the query might not work
#' @param substances A character vector containing any of "CO2", "N2O", "CH4". Or if `NULL`
#' returns all substances.
#' @param countries A character vector of countries to return data from. Or if `NULL` returns
#' all countries.
#' @param years Vector of years within the range 1960:2100
#'
#' @return A data.table of emissions data
#' @export
get_emissions_data <- function(con, table_name = "emi_edgar_release", substances, countries, years){

  # Checks ------------------------------------------------------------------

  stopifnot(inherits(con, "Microsoft SQL Server"),
            is.character(table_name),
            all(substances %in% c("CO2", "N2O", "CH4")),
            is.character(countries),
            years %in% 1960:2100)

  # Columns -----------------------------------------------------------------

  colnames_to_return <- c("pr_code", "Country_code_A3", "Substance", "ad_code")

  year_columns <- paste0("Y_", years)

  # combine for all col names to return
  colnames_to_return <- c(colnames_to_return, year_columns) |>
    toString()

  # Rows --------------------------------------------------------------------

  # Assemble the row part of the query - if NULL some parts are skipped and
  # this returns all rows.

  rows_to_return <- "emi_id = '29072022103026'"

  if(!is.null(substances)){
    substances <- paste0(substances, collapse = "', '")
    rows_to_return <- glue::glue(
      "{rows_to_return}",
      " AND Substance IN ('{substances}')"
    )
  }

  if(!is.null(countries)){
    countries <- paste0(countries, collapse = "', '")
    rows_to_return <- glue::glue(
      "{rows_to_return}",
      " AND Country_code_A3 IN ('{countries}')"
    )
  }

  # Run query ---------------------------------------------------------------

  SQL_query <- glue::glue("SELECT {colnames_to_return} FROM {table_name} ",
                          "WHERE {rows_to_return}")

  DBI::dbGetQuery(con, SQL_query) |>
    data.table::as.data.table()

}

#' Get country info table
#'
#' Retrieves the "Countries" table from EDGAR, which includes the development
#' group of each country (needed for uncertainty calculation).
#'
#' @param con ODBC connection to EDGAR: use [connect_to_edgar()]
#'
#' @return data.table of country info
#' @export
get_country_info <- function(con){

  DBI::dbReadTable(con, "Countries") |>
    data.table::as.data.table()
}
