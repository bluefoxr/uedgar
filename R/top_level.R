# Top-level user-facing functions

#' Get aggregated uncertain emissions
#'
#' Returns a table of emissions data with confidence intervals, aggregated up to
#' a specified limit.
#'
#' Note that for `emi_id`, the default data set is `"29072022103026"`. Another
#' useful data set is `"09062022142022"`, which is CO2 emissions excluding biofuels.
#'
#' @param con DB connection from [connect_to_edgar()]
#' @param substances One or more of "CO2", "N2O", "CH4", as a character vector. OR if this is specified as a named list,
#' will map each substance to its name. For example, `list(CH4 = "GWP_100_AR5_CH4")` will retrieve "GWP_100_AR5_CH4" from
#' the database, but will use the CH4 uncertainty values in the table. This is used for example,
#' when I wanted to select substances like "GWP_100_AR5_CH4" which is CH4 in CO2 equiv. Then I map this to CH4.
#' Otherwise, although the data will be retrieved, it will not be correctly matched to the uncertainty table.
#' @param years Vector of years
#' @param countries Vector of ISO3 country codes
#' @param sectors Vector of EDGAR sectors
#' @param agg_countries Logical: whether to aggregate all countries together
#' @param agg_sectors Logical: whether to aggregate all sectors together
#' @param agg_substances Logical: whether to aggregate substances together - NOTE
#' that this is not properly implemented since the emissions are in different
#' units at the moment.
#' @param emi_id data set ID, as a string. See details
#' @param use_cache Logical: if `TRUE` tries to retrieve data from cache. If data is
#' found that matches the query or a subset of the query, it will use that, and
#' query the database for any remaining aggregate values.
#' @param country_groups Optional vector of country groups - overrides countries if specified.
#' @param use_lognormal Logical: whether to use the log-normal transformation at the end of the calculation
#'
#' @return Data frame of emissions
#' @export
get_uncertain_emissions <- function(con, substances = NULL, years = NULL, countries = NULL, country_groups = NULL,
                                    sectors = NULL, agg_countries = FALSE, agg_sectors = TRUE, agg_substances = FALSE,
                                    use_cache = TRUE, emi_id = "29072022103026", use_lognormal = TRUE){

  # Input checks ------------------------------------------------------------

  if(is.list(substances)){
    # if list, means substance mapping. Convert to character to avoid issues,
    # and name separately.
    l_substances <- substances
    substances <- as.character(substances)
  } else {
    l_substances <- NULL
  }

  #check_substances(substances)
  check_years(years)
  check_sectors(sectors)

  # Check cache -------------------------------------------------------------
  # checks in a stored df whether the emissions data is already available

  # flag which is adjusted later if anything useful found in cache
  valid_rows_in_cache <- FALSE
  # check if cache exists
  path_cache <- system.file("cache_unc.RDS", package = "uedgar")
  cache_exists <- file.exists(path_cache)

  if(use_cache && cache_exists){

    # have to convert some things to ALL
    substances_c <- if(is.null(substances) && agg_substances) "ALL" else substances
    years_c <- if(is.null(years)) 1970:2021 else years
    countries_c <- if(is.null(countries) && agg_countries) "ALL" else countries
    sectors_c <- if(is.null(sectors) && agg_sectors) "ALL" else sectors

    # load cache
    cache_unc <- readRDS(system.file("cache_unc.RDS", package = "uedgar"))

    # get valid rows of cache
    valid_rows <- (cache_unc$Substance %in% substances_c) &
      (cache_unc$Year %in% years_c) &
      (cache_unc$Sector %in% sectors_c) &
      (cache_unc$emi_id %in% emi_id) &
      (cache_unc$use_lognormal %in% use_lognormal)

    if(!is.null(country_groups)){
      valid_rows <- valid_rows & (cache_unc$Country %in% country_groups)
    } else {
      valid_rows <- valid_rows & (cache_unc$Country %in% countries_c)
    }

    if(sum(valid_rows) > 0){

      valid_rows_in_cache <-  TRUE
      dt_cache <- cache_unc[valid_rows, ]
      message("Retrieved ", nrow(dt_cache), " aggregated rows from cache")

      # see what's missing, if anything
      # the expected table
      df_combinations <- expand.grid(substances_c, years_c, countries_c,
                                     sectors_c, emi_id, use_lognormal, stringsAsFactors = FALSE)
      names(df_combinations) <- c("Substance", "Year", "Country", "Sector", "emi_id", "use_lognormal")
      missing_rows <- do.call(paste0, df_combinations) %nin% do.call(paste0, dt_cache[, names(df_combinations), with=FALSE])

      # if nothing is missing, return cached dt
      if(sum(missing_rows) == 0){
        setorder(dt_cache, cols = "Year")
        return(dt_cache)
      }

      df_combinations <- df_combinations[missing_rows, ]

      # else, get the missing stuff
      countries <- unique(df_combinations$Country)
      years <- unique(df_combinations$Year)
      substances <- unique(df_combinations$Substance)
      sectors <- unique(df_combinations$Sector)

      countries <- if(length(countries) == 1){
        if (countries == "ALL") NULL else countries
      } else countries

      sectors <- if(length(sectors) == 1){
        if (sectors == "ALL") NULL else sectors
      } else sectors

    }

  }

  # specifying a country group overrides the countries arg
  if(!is.null(country_groups)){
    countries <- get_ISO3_in_group(con, country_groups)
  }
  check_countries(countries)

  # Get emissions data ------------------------------------------------------

  dt_emissions <- get_emissions_data(
    con,
    substances = substances,
    countries = countries,
    sectors = sectors,
    years = years,
    emi_id = emi_id
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

  countries_table <- rbind(
    countries_table,
    data.frame(Country_code_A3 = c("AIR", "SEA"), dev_country = "I")
  )

  # I am going to presume that AIR and SEA are both Industrialised for the purposes of uncertainty


  dt_emissions <- countries_table[dt_emissions, on = .(Country_code_A3)]

  # Join on uncertainties ---------------------------------------------------

  unc_table <- get_uncertainty_table(con)
  # ensure all codes are upper case (a couple are not, by accident, which causes issues)
  unc_table$Process <- toupper(unc_table$Process)

  missing_codes <- setdiff(unique(dt_emissions$ad_code), unique(unc_table$Process))

  if(length(missing_codes) > 0){
    warning("Some AD codes from data set not found in uncertainty table: ", toString(missing_codes), call. = TRUE)
  }

  # map substances in uncertainty table if needed
  if(!is.null(l_substances)){
    for(ii in 1:length(l_substances)){
      unc_table$Substance[unc_table$Substance == names(l_substances)[ii]] <- l_substances[[ii]]
    }
  }

  # merge: I have allowed for duplicates here and I think it is to do with the
  # duplicates in the uncertainty table, still to be corrected
  dt_emissions <- merge(x = dt_emissions, y = unc_table,
                        by.x = c("dev_country", "ad_code", "Substance"),
                        by.y = c("Country", "Process", "Substance"),
                        all.x = TRUE, allow.cartesian = T)

  #dt_emissions <- unc_table[dt_emissions, on = .(Country = dev_country, Process = ad_code, Substance = Substance)]

  # change names to something clearer
  setnames(dt_emissions,
           c("Unc_emi_min_fixed", "Unc_emi_max_fixed", "ad_code"),
           c("prc_lower", "prc_upper", "Process"))

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
    warning("lognormal transformation not applied here even if specified - not integrated yet")
    return(dt_aggregated)
  }

  # otherwise, bind dts together
  dt_aggregated <- Reduce(rbind, dt_aggregated)

  if(use_lognormal){
    dt_aggregated <- to_lognormal(dt_aggregated)
  } else {
    dt_aggregated <- dt_aggregated
  }

  # format columns

  # Country (slightly fiddly...)
  if("Country_code_A3" %nin% names(dt_aggregated)){
    # only the case if countries have been aggregated together
    if(!is.null(country_groups)){
      dt_aggregated$Country <- country_groups
    } else if (is.null(countries)){
      dt_aggregated$Country <- "ALL"
    } else {
      dt_aggregated$Country <- paste0(countries, collapse = "_")
    }
  } else {
    names(dt_aggregated)[names(dt_aggregated) == "Country_code_A3"] <- "Country"
  }

  # Substance
  # assuming only one substance
  if("Substance" %nin% names(dt_aggregated)){
    dt_aggregated$Substance <- substance
  }

  # Sector
  if("Sector" %nin% names(dt_aggregated)){
    dt_aggregated$Sector <- "ALL"
  }

  message("Retrieved ", nrow(dt_emissions), " records from database and calculated ", nrow(dt_aggregated), " aggregated values.")

  # add IDs
  dt_aggregated$emi_id <- emi_id
  dt_aggregated$use_lognormal <- use_lognormal

  # store results in cache for next time
  if(use_cache){

    if(cache_exists){
      cache_unc <- rbind(cache_unc, dt_aggregated)
      cache_unc <- unique(cache_unc)
      saveRDS(cache_unc, system.file("cache_unc.RDS", package = "uedgar"))
      message("Saved ", nrow(dt_aggregated), " new aggregated rows in cache.")
    } else {
      saveRDS(dt_aggregated, paste0(system.file(package = "uedgar"), "/cache_unc.RDS"))
      message("Created cache and saved ", nrow(dt_aggregated), " new aggregated rows therein.")
    }


  }

  # combine with cache
  if(use_cache && valid_rows_in_cache){
    dt_aggregated <- rbind(dt_cache, dt_aggregated)
  }

  # sort by year
  setorder(dt_aggregated, cols = "Year")
  return(dt_aggregated)

}


clear_cache <- function(){

  file_name <- system.file("cache_unc.RDS", package = "uedgar")

  if (file.exists(file_name)) {
    unlink(file_name)
    message("Cache has been cleared.")
  } else{
    message("Cache file does not yet exist. Run get_uncertain_emissions() to create it.")
  }
}
