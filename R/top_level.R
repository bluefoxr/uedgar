# Top-level user-facing functions

get_uncertain_emissions <- function(con, substances = NULL, years = NULL, countries = NULL, sectors = NULL){


  # Input checks ------------------------------------------------------------

  check_substances(substances)
  check_years(years)
  check_countries(countries)
  check_sectors(sectors)

  # Get emissions data ------------------------------------------------------

  dt_emissions <- get_emissions_data(
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

  dt_emissions <- countries_table[dt_emissions, on = .(Country_code_A3)]

  # Join on uncertainties ---------------------------------------------------

  unc_table <- get_uncertainty_table(con)

  missing_codes <- setdiff(unique(dt_emissions$ad_code), unique(unc_table$Process))

  if(length(missing_codes) > 0){
    warning("Some AD codes from data set not found in uncertainty table: ", toString(missing_codes), call. = TRUE)
  }

  dt_emissions <- unc_table[dt_emissions, on = .(Country = dev_country, Process = ad_code, Substance = Substance)]

  # change names to something clearer
  setnames(dt_emissions, c("Unc_emi_min_fixed", "Unc_emi_max_fixed"), c("prc_lower", "prc_upper"))

  # NOTE the prc_lower and prc_upper cols are the upper and lower uncertainty factors, for emissions,
  # as PERCENTAGES. This is already after cap at 230% and correction factor.

  # Calc emi min/max --------------------------------------------------------

  # first make long for convenience
  # note that NAs are removed at this point
  dt_emissions <- melt(dt_emissions, measure = patterns("^Y_"), value.name = "Emissions",
                   variable.name = "Year", na.rm = TRUE)
  # convert year column to integer
  dt_emissions[, Year := as.integer(substr(Year, 3, 6))]

  # calc min/max as new columns
  # NOTE don't allow negative emissions
  # dt_emissions[, Emissions_Min := Emissions - (Emissions*prc_lower/100)]
  # dt_emissions[, Emissions_Min := fifelse(Emissions_Min < 0, 0, Emissions_Min)]
  # dt_emissions[, Emissions_Max := Emissions + (Emissions*prc_upper/100)]

  # Aggregate up ------------------------------------------------------------

  #dt_aggregated <- dt_emissions[, aggregate_substance(.SD), by = c("Country_code_A3", "Year", "Substance")]

  # split on substances
  dt_aggregated <- lapply(substances, function(substance){

    dt_emissions[Substance == substance, aggregate_substance(.SD), by = c("Country_code_A3", "Year")]

  })

  dt_aggregated <- Reduce(rbind, dt_aggregated)

}
