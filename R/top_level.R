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

  # NOTE the Unc_emi_min_fixed (and max) cols are the upper and lower uncertainty factors, for emissions,
  # as PERCENTAGES. This is already after cap at 230% and correction factor.

  # Calc emi min/max --------------------------------------------------------

  # first make long for convenience
  # note that NAs are removed at this point
  emi_data <- melt(emi_data, measure = patterns("^Y_"), value.name = "Emissions",
                   variable.name = "Year", na.rm = TRUE)
  # convert year column to integer
  emi_data[, Year := as.integer(substr(Year, 3, 6))]

  # calc min/max as new columns
  # NOTE don't allow negative emissions
  emi_data[, Emissions_Min := Emissions - (Emissions*Unc_emi_min_fixed/100)]
  emi_data[, Emissions_Min := fifelse(Emissions_Min < 0, 0, Emissions_Min)]
  emi_data[, Emissions_Max := Emissions + (Emissions*Unc_emi_max_fixed/100)]

  # Aggregate up ------------------------------------------------------------




}
