#' Aggregate a set of emissions uncertainties
#'
#' Aggregates a table of emissions uncertainties into a single value with upper
#' and lower percentages. Note that `NA`s in emissions or emissions uncertainties are excluded.
#'
#' @param dt_emissions Data table with columns Emissions, prc_lower, prc_upper, substance
#' @param correlated If `TRUE` aggregates emissions assuming full correlation.
#'
#' @return A scalar value
#' @export
aggregate_unc <- function(dt_emissions, correlated = FALSE){

  # TODO maybe move these checks higher up for speed
  stopifnot(
    is.numeric(dt_emissions$Emissions),
    is.numeric(dt_emissions$prc_lower),
    is.numeric(dt_emissions$prc_upper),
    is.logical(correlated)
  )

  sum_emi <- dt_emissions[, sum(Emissions, na.rm = TRUE)]

  substance <- dt_emissions[, unique(Substance)]

  if(correlated){
    # note this is like the weighted average
    dt_emissions[,
                 .(Substance = substance,
                   Emissions = sum_emi,
                   prc_lower = sum((prc_lower*Emissions), na.rm = TRUE)/sum_emi,
                   prc_upper = sum((prc_upper*Emissions), na.rm = TRUE)/sum_emi)]
  } else {
    dt_emissions[,
                 .(Substance = substance,
                   Emissions = sum_emi,
                   prc_lower = f_agg_correlated(Emissions, prc_lower),
                   prc_upper = f_agg_correlated(Emissions, prc_upper))]
  }
}

#' Aggregate correlated emissions
#'
#' Aggregates a vector of uncertainties and emissions using correlated formula.
#'
#' @param emi Vector of emissions values
#' @param u Vector of emissions uncertainties as percentages or fractions
#'
#' @return Percentage or fraction uncertainty of total emissions
#' @export
f_agg_correlated <- function(emi, u){
  sqrt(sum((u*emi)^2, na.rm = TRUE))/sum(emi, na.rm = TRUE)
}

#' Aggregate emissions uncertainties by substance
#'
#' Aggregates a table of emissions uncertainties depending on whether CO2 or
#' not. If CO2 aggregates by fuel. Otherwise, aggregates by sector. In both cases
#' the emissions within these groups are considered as correlated by default.
#'
#' @param dt_emissions Data table of emissions
#' @param correlate_in_fuel Whether to treat emissions within the same fuel as correlated
#' @param correlate_in_sector Whether to treat emissions within same sector as correlated
#'
#' @return A data table
#' @export
aggregate_substance <- function(dt_emissions, correlate_in_fuel = TRUE,
                                correlate_in_sector = TRUE){

  substance <- dt_emissions[, unique(Substance)]
  stopifnot(length(substance) == 1)

  if(substance == "CO2"){
    dt_emissions[, aggregate_unc(.SD, correlated = correlate_in_fuel), by = c("Fuel")]
  } else {
    dt_emissions[, aggregate_unc(.SD, correlated = correlate_in_sector), by = c("Sector")]
  }

}

#' Aggregate emissions uncertainties by groups
#'
#' Aggregates a table of emissions uncertainties based on a specified group.
#'
#' @param dt_emissions Data table of emissions
#' @param by_group Group name to use in aggregation (column name)
#' @param correlated Whether to aggregate as correlated or not
#'
#' @return A data table
#' @export
aggregate_by_group <- function(dt_emissions, by_group, correlated = TRUE){

  dt_emissions[, aggregate_unc(.SD, correlated = correlated), by = c(by_group)]

}

# SPARE CODE --------------------------------------------------------------

# Aggregate emissions uncertainties by group
#
# Aggregates a table of emissions uncertainties by a specified group: either
# Sector or Fuel. Note that the table only needs to contain the "Process" column,
# the Sector/Fuel is extracted here from that column.
#
# @param dt_emissions Data table of emissions
# @param aggregate_by Either `"Sector"` or `"Fuel"`
# @param correlated Logical: whether to consider uncertainties within groups as
# correlated or not.
#
# @return A data table
# @export
# aggregate_by_group <- function(dt_emissions, aggregate_by, correlated){
#
#   stopifnot(aggregate_by %in% c("Sector", "Fuel"))
#
#   dt_emissions[, aggregate_unc(.SD, correlated = correlated), by = c(aggregate_by)]
# }

# Aggregate a table of emissions uncertainties
#
# One-shot aggregation of emissions uncertainties using data.table.
#
# @param dt A data table of emissions
# @param col_emissions Column name where the emissions data is in
# @param col_min Column name of max uncertainty
# @param col_max Column name of min uncertainty
# @param col_group Column to group by
#
# @return A data table of aggregated uncertainties by group
# @export
# aggregate_unc_dt <- function(dt, col_emissions, col_min, col_max, col_group){
#
#   stopifnot(
#     data.table::is.data.table(dt),
#     col_emissions %in% names(dt),
#     col_min %in% names(dt),
#     col_max %in% names(dt),
#     col_group %in% names(dt)
#   )
#
#   # Hacky rename of columns due to can't figure out the correct data.table syntax...
#   names(dt)[names(dt) == col_emissions] <- "EMISSIONS"
#   names(dt)[names(dt) == col_min] <- "UNC_MIN"
#   names(dt)[names(dt) == col_max] <- "UNC_MAX"
#
#   # aggregate: this applies aggregate_unc() to the col_min and col_max columns,
#   # grouped by the col_group column
#   dt[, .(unc_min = aggregate_unc(UNC_MIN, EMISSIONS), unc_max = aggregate_unc(UNC_MAX, EMISSIONS)), by = c(col_group)]
#
# }
