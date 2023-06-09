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
                   prc_lower = f_agg_uncorrelated(Emissions, prc_lower),
                   prc_upper = f_agg_uncorrelated(Emissions, prc_upper))]
  }
}

#' Aggregate uncorrelated emissions
#'
#' Aggregates a vector of uncertainties and emissions using uncorrelated formula.
#'
#' @param emi Vector of emissions values
#' @param u Vector of emissions uncertainties as percentages or fractions
#'
#' @return Percentage or fraction uncertainty of total emissions
#' @export
f_agg_uncorrelated <- function(emi, u){
  sqrt(sum((u*emi)^2, na.rm = TRUE))/sum(abs(emi), na.rm = TRUE)
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
    if("Fuel" %nin% names(dt_emissions)){
      stop("Required column 'Fuel' not found in data (required for CO2)?")
    }
    dt_emissions[, aggregate_unc(.SD, correlated = correlate_in_fuel), by = c("Fuel")]
  } else {
    if("Sector" %nin% names(dt_emissions)){
      stop("Required column 'Sector' not found in data (required for CH4, N2O)?")
    }
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

# transform_to_lognormal <- function(dt_emissions){
#
#   # only applied where lower bound is >50%
#   dt_emissions[prc_lower > 50, ]
# }

# vectorised and should take vectors emi, u_lower and u_upper
# TO FINISH
to_lognormal <- function(dt_emissions, u_lower_thresh = 50){

  rows_to_replace <- dt_emissions$prc_lower > 50

  if(sum(rows_to_replace) == 0){
    return(dt_emissions)
  }

  # get required columns
  emi <- dt_emissions$Emissions[rows_to_replace]
  u_lower <- dt_emissions$prc_lower[rows_to_replace]
  u_upper <- dt_emissions$prc_upper[rows_to_replace]

  # CALCULATIONS

  # note emi is treated as mu
  mu_lower <- exp(log(emi) - 0.5*log(1 + (u_lower/200)^2))
  mu_upper <- exp(log(emi) - 0.5*log(1 + (u_upper/200)^2))

  sig_lower <- exp(sqrt(log(1 + (u_lower/200)^2)))
  sig_upper <- exp(sqrt(log(1 + (u_upper/200)^2)))

  # NOTE lower bound is a negative fraction (following IPCC formula), so we
  # multiply by emi and then ADD
  u_lower_ln <- (exp(log(mu_lower) - 1.96*log(sig_lower)) - emi)/emi
  u_upper_ln <- (exp(log(mu_upper) + 1.96*log(sig_upper)) - emi)/emi

  # REPLACE COLS
  dt_emissions$prc_lower[rows_to_replace] <- u_lower_ln*-100
  dt_emissions$prc_upper[rows_to_replace] <- u_upper_ln*100
  dt_emissions$Emissions_Min[rows_to_replace] <- emi + emi*u_lower_ln
  dt_emissions$Emissions_Max[rows_to_replace] <- emi + emi*u_upper_ln

  dt_emissions
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
