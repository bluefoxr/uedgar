#' Check whether substance codes are valid
#'
#' Just checks whether they are one of the substances where we have uncertainty
#' data available.
#'
#' @param substances Character vector of uncertainty codes
#'
#' @return Nothing, or an error message
check_substances <- function(substances){

  if(!is.null(substances)){
    stopifnot(is.character(substances))

    valid_substances <- c("CO2", "CH4", "N2O")

    invalid_substances <- setdiff(substances, valid_substances)

    if(length(invalid_substances) > 0){
      stop("One or more substances not valid: ", toString(invalid_substances), call. = FALSE)
    }
  }

}


#' Check whether years are valid
#'
#' Are the user-input years in the valid range?
#'
#' @param years Numerical vector of years
#'
#' @return Nothing, or an error message
check_years <- function(years){

  if(!is.null(years)){
    stopifnot(is.numeric(years))

    valid_years <- 1960:2100

    invalid_years <- setdiff(years, valid_years)

    if(length(invalid_years) > 0){
      stop("One or more years not valid: ", toString(invalid_years), call. = FALSE)
    }
  }

}

#' Check whether country codes are valid
#'
#' Check against stored ISO3 codes. This is to avoid sending incorrect queries
#' to the DB which might be harder to debug.
#'
#' Note that these codes are taken from EDGAR and stored
#' in the package to avoid having to run the query each time. Occasionally, you
#' might need to update this: do so by running `get_valid_countries()` to retrieve the vector of ISO codes, then:
#'
#' `usethis::use_data(valid_countries, overwrite = TRUE)`
#'
#' to store in the package. This will only work if you are working in the package
#' source directory.
#'
#' @param countries Character vector of country codes
#'
#' @return Nothing, or an error message
check_countries <- function(countries){

  if(!is.null(countries)){
    stopifnot(is.character(countries))

    invalid_countries <- setdiff(countries, valid_countries)

    if(length(invalid_countries) > 0){
      stop("One or more countries not valid: ", toString(invalid_countries), call. = FALSE)
    }
  }
}

#' Check whether sector codes are valid
#'
#' Check against stored top-level EDGAR sector codes. This is to avoid sending incorrect queries
#' to the DB which might be harder to debug.
#'
#' Note that these codes are taken from EDGAR and stored
#' in the package to avoid having to run the query each time. Occasionally, you
#' might need to update this: do so by running `get_valid_sectors()` to retrieve the vector of ISO codes, then:
#'
#' `usethis::use_data(valid_sectors, overwrite = TRUE)`
#'
#' to store in the package. This will only work if you are working in the package
#' source directory.
#'
#' @param sectors Character vector of sector codes
#'
#' @return Nothing, or an error message
check_sectors <- function(sectors){

  if(!is.null(sectors)){
    stopifnot(is.character(sectors))

    invalid_sectors <- setdiff(sectors, valid_sectors)

    if(length(invalid_sectors) > 0){
      stop("One or more sectors not valid: ", toString(invalid_sectors), call. = FALSE)
    }
  }
}

# returns character vector of valid top-level sector codes in EDGAR
# con is object returned by connect_to_edgar()
get_valid_sectors <- function(con){

  # you need to have opened a connection to EDGAR with connect_to_edgar()
  # before running this...
  DBI::dbGetQuery(con, "SELECT DISTINCT ad_code FROM emi_edgar_release WHERE emi_id IN ('29072022103026')") |>
    unlist() |>
    substr(1, 3) |>
    unique()
}

# get valid ISO3 country codes in EDGAR
get_valid_countries <- function(con){
  DBI::dbGetQuery(con, "SELECT DISTINCT Country_code_A3 FROM Countries") |>
    unlist() |>
    as.character()
}
