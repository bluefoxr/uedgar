# Top-level user-facing functions

get_uncertain_emissions <- function(con, substances = NULL, years = NULL, countries = NULL, sectors = NULL){


  # Input checks ------------------------------------------------------------

  check_substances(substances)
  check_years(years)
  check_countries(countries)
  check_sectors(sectors)

  # Get emissions data ------------------------------------------------------

  emi_data <- get_emissions_data(
    con,
    substances = substances,
    countries = countries,
    sectors = sectors,
    years = years
  )

  # Join on country groupings -----------------------------------------------

  # get country grouping table: only rows where country is I or D
  countries_table <- DBI::dbGetQuery(
    con,
    "SELECT Country_code_A3, dev_country FROM Countries WHERE dev_country IN ('I', 'D')"
  ) |> data.table::as.data.table()

  emi_data <- countries_table[emi_data, on = .(Country_code_A3)]

  # Join on uncertainties ---------------------------------------------------

  unc_table <- get_uncertainty_table(con)

  missing_codes <- setdiff(unique(emi_data$ad_code), unique(unc_table$Process))

  if(length(missing_codes) > 0){
    warning("Some AD codes from data set not found in uncertainty table: ", toString(missing_codes), call. = TRUE)
  }

  emi_data <- unc_table[emi_data, on = .(Country = dev_country, Process = ad_code, Substance = Substance)]

  #

}
