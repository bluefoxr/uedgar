# FUNCTIONS FOR INTERACTING WITH DB
# NOTES
# DBI::dbGetQuery() - run query
# odbc::dbDisconnect(con) - disconnect

#' Establish connection with EDGAR DB
#'
#' You need to be on the network for this to work. You can enter your user name
#' and password directly using the `uname` and `pwd` arguments. OR use
#' `path_to_login` to point to a file where these are stored (see details).
#'
#' The `path_to_login` argument should be the path to an R file with login details for
#' EDGAR. This file can have any name, and should simply have the user name and password
#' in the following format:
#'
#' ````
#' uname <- "user_name"
#' pwd <- "password"
#' ````
#'
#' where `user_name` and `password` are replaced with your actual user name and password.
#' This file will only be accessed if `uname` and `pwd` are not specified.
#'
#' @param uname EDGAR user name
#' @param pwd EDGAR password
#' @param path_to_login File path to your login file. See details.
#'
#' @return ODBC connection
#' @export
connect_to_edgar <- function(uname = NULL, pwd = NULL, path_to_login = NULL){

  # defaults for login
  if(is.null(uname) || is.null(pwd)){
    if(is.null(path_to_login)){
      source(system.file("db_login.R", package = "uedgar"), local = TRUE)
    } else {
      source(path_to_login)
    }
  }

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
#' can lead to a very large table, so use carefully. The emissions data is returned as the lowest
#' level available in EDGAR, without aggregating to higher levels.
#'
#' @param con An ODBC connection: use [connect_to_edgar()]
#' @param table_name Name of the table to query - if not the default the query might not work
#' @param substances A character vector containing any of "CO2", "N2O", "CH4". Or if `NULL`
#' returns all substances.
#' @param countries A character vector of countries to return data from. Or if `NULL` returns
#' all countries.
#' @param years Vector of years within the range 1960:2100
#' @param sectors Character vector of sector codes to return
#' @param emi_id data set ID, as a string
#'
#' @import data.table
#'
#' @return A data.table of emissions data
#' @export
get_emissions_data <- function(con, substances = NULL, countries = NULL,
                               years = NULL, sectors = NULL, emi_id = "29072022103026",
                               table_name = "emi_edgar_release"){

  # Checks ------------------------------------------------------------------

  stopifnot(inherits(con, "Microsoft SQL Server"),
            is.character(table_name)
  )

  # Columns -----------------------------------------------------------------

  if(is.null(years)){
    years <- 1960:2100
  } else {
    stopifnot(years %in% 1960:2100)
  }

  colnames_to_return <- c("pr_code", "Country_code_A3", "Substance", "ad_code")

  year_columns <- paste0("Y_", years)

  # combine for all col names to return
  colnames_to_return <- c(colnames_to_return, year_columns) |>
    toString()

  # Rows --------------------------------------------------------------------

  # Assemble the row part of the query - if NULL some parts are skipped and
  # this returns all rows.

  # data set ID
  rows_to_return <- glue::glue("emi_id = '{emi_id}'")

  if(!is.null(substances)){
    #stopifnot(all(substances %in% c("CO2", "N2O", "CH4")))
    substances <- paste0(substances, collapse = "', '")
    rows_to_return <- glue::glue(
      "{rows_to_return}",
      " AND Substance IN ('{substances}')"
    )
  }

  if(!is.null(countries)){
    stopifnot(is.character(countries))
    countries <- paste0(countries, collapse = "', '")
    rows_to_return <- glue::glue(
      "{rows_to_return}",
      " AND Country_code_A3 IN ('{countries}')"
    )
  }

  if(!is.null(sectors)){

    stopifnot(is.character(sectors))

    rows_to_return <- glue::glue(
      "{rows_to_return}",
      " AND (ad_code LIKE '{sectors[1]}%'"
    )

    if(length(sectors) > 1){
      for (ii in 2:length(sectors)){
        rows_to_return <- glue::glue(
          "{rows_to_return}",
          " OR ad_code LIKE '{sectors[ii]}%'"
        )
      }
    }

    rows_to_return <- glue::glue("{rows_to_return})")

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

get_ISO3_in_group <- function(con, country_group){

  # check which group codes exist
  existing_groups <- DBI::dbGetQuery(con, "SELECT DISTINCT Group_code FROM Country_groups")[[1]]

  # report any missing groups and stop in that case
  missing_groups <- setdiff(country_group, existing_groups)
  if(length(missing_groups) > 0){
    stop("One or more specified groups not recognised: ", toString(missing_groups))
  }

  # return query
  DBI::dbGetQuery(
    con,
    paste0("SELECT Country_code_A3 FROM Country_groups WHERE Group_code IN ('", country_group, "')")
  )[[1]]
}

#' Get uncertainty table from DB
#'
#' Returns the uncertainty table giving uncertainty values for emissions at the
#' lowest level.
#'
#' @param con DB connection established with [connect_to_edgar()].
#'
#' @return Data frame
#' @export
get_uncertainty_table <- function(con){

  # get cols from table
  unc_table <- DBI::dbGetQuery(
    con,
    "SELECT Process, Substance, Country, Unc_emi_min_fixed, Unc_emi_max_fixed FROM unc_emi_table"
  ) |>
    data.table::as.data.table()

  # hacky: have to deal with AAAs for join and this is the easiest way I know
  # note: not too bad as it only adds extra 600 rows out of 10000
  # I extract the AAAs, duplicate them and mark each as I and D respectively
  AAAs <- unc_table[Country == "AAA", ]
  AAAs[, Country := "I"]

  unc_table <- rbind(unc_table, AAAs)
  unc_table[Country == "AAA", Country := "D"]

  unc_table
}

#' Get data set ID
#'
#' @param dataset_name Name of data set
#' @param con Connection to EDGAR
#'
#' @return ID
#' @export
get_dataset_ID <- function(dataset_name, con){

  DBI::dbGetQuery(
    con,
    paste0("SELECT ds_id FROM datasets WHERE ds_name = '", dataset_name, "'")) |>
    unlist()

}
