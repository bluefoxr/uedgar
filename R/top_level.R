# Top-level user-facing functions

#' Get aggregated uncertain emissions
#'
#' Returns a table of emissions data with confidence intervals, aggregated up to
#' a specified limit.
#'
#' @param con DB connection from [connect_to_edgar()]
#' @param substances One or more of "CO2", "N2O", "CH4"
#' @param years Vector of years
#' @param countries Vector of ISO3 country codes
#' @param sectors Vector of EDGAR sectors
#' @param agg_countries Logical: whether to aggregate all countries together
#' @param agg_sectors Logical: whether to aggregate all sectors together
#' @param agg_substance Logical: whether to aggregate substances together - NOTE
#' that this is not properly implemented since the emissions are in different
#' units at the moment.
#'
#' @return Data frame of emissions
#' @export
get_uncertain_emissions <- function(con, substances = NULL, years = NULL, countries = NULL, sectors = NULL,
                                    agg_countries = FALSE, agg_sectors = TRUE, agg_substance = FALSE){


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

  if(nrow(dt_emissions) == 0){
    warning("No data found for specified query.")
    return(NULL)
  }

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

  # Wrangling ready for aggregation ------------------------------------------

  # first make long for convenience
  # note that NAs are removed at this point
  dt_emissions <- melt(dt_emissions, measure = patterns("^Y_"), value.name = "Emissions",
                   variable.name = "Year", na.rm = TRUE)
  # convert year column to integer
  dt_emissions[, Year := as.integer(substr(Year, 3, 6))]

  # Aggregate within sectors/fuels -------------------------------------------

  # create fuel and sector cols: tried to do this lower down but ran into an error
  dt_emissions[, Sector:= shrink_process_codes(Process, 1)]
  dt_emissions[, Fuel:= extract_fuels(Process)]

  # split on substances and aggregate (because substance defines how to aggregate)
  dt_aggregated <- lapply(substances, function(substance){

    # first aggregate within substance
    agg_chunk <- dt_emissions[Substance == substance,
                              aggregate_substance(.SD),
                              by = c("Country_code_A3", "Year")]

    if (agg_countries) {

      # aggregate same sectors from different countries together as CORRELATED
      if(substance == "CO2"){
        agg_chunk <- aggregate_by_group(agg_chunk, by_group = c("Fuel", "Year") , correlated = TRUE)
      } else {
        agg_chunk <- aggregate_by_group(agg_chunk, by_group = c("Sector", "Year"), correlated = TRUE)
      }

      # here we have sector/fuel totals across all countries
      if (agg_sectors) {
        # if also aggregating sectors together, just take the uncorrelated sum of everything
        agg_chunk <-aggregate_by_group(agg_chunk, by_group = "Year", correlated = FALSE)
      }


    } else {

      if (agg_sectors) {
        # total emissions per country (all sectors/fuels added together)
        agg_chunk <- aggregate_by_group(
          agg_chunk,
          by_group = c("Country_code_A3", "Year"),
          correlated = FALSE
        )
      }

    }

    # calc min/max as new columns
    # NOTE don't allow negative emissions
    agg_chunk[, Emissions_Min := Emissions - (Emissions*prc_lower/100)]
    agg_chunk[, Emissions_Min := fifelse(Emissions_Min < 0, 0, Emissions_Min)]
    agg_chunk[, Emissions_Max := Emissions + (Emissions*prc_upper/100)]

    agg_chunk

  })

  # special case where we will have dts for different substances, some with
  # a "Fuel" column and others with a "Sector" column. Return list here.
  if (("CO2" %in% substances) && (length(substances) > 1) && !agg_sectors){
    return(dt_aggregated)
  }

  # otherwise, bind dts together
  Reduce(rbind, dt_aggregated)

}
