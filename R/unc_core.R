#' Aggregate a set of emissions uncertainties
#'
#' Note that `NA`s in emissions or emissions uncertainties are excluded.
#'
#' @param emissions A vector of emissions values
#' @param emissions_uncertainties A vector of emissions uncertainties, as e.g.
#' fractions or percentages. Must be same length as `emissions`.
#'
#' @return A scalar value
#' @export
aggregate_unc <- function(emissions_uncertainties, emissions){

  stopifnot(
    is.numeric(emissions),
    is.numeric(emissions_uncertainties),
    length(emissions) == length(emissions_uncertainties)
  )

  sqrt(sum(emissions_uncertainties^2, na.rm = TRUE))/sum(emissions, na.rm = TRUE)
}

#' Aggregate a table of emissions uncertainties
#'
#' One-shot aggregation of emissions uncertainties using data.table.
#'
#' @param dt A data table of emissions
#' @param col_emissions Column name where the emissions data is in
#' @param col_min Column name of max uncertainty
#' @param col_max Column name of min uncertainty
#' @param col_group Column to group by
#'
#' @return A data table of aggregated uncertainties by group
#' @export
aggregate_unc_dt <- function(dt, col_emissions, col_min, col_max, col_group){

  stopifnot(
    data.table::is.data.table(dt),
    col_emissions %in% names(dt),
    col_min %in% names(dt),
    col_max %in% names(dt),
    col_group %in% names(dt)
    )

  # Hacky rename of columns due to can't figure out the correct data.table syntax...
  names(dt)[names(dt) == col_emissions] <- "EMISSIONS"
  names(dt)[names(dt) == col_min] <- "UNC_MIN"
  names(dt)[names(dt) == col_max] <- "UNC_MAX"

  # aggregate: this applies aggregate_unc() to the col_min and col_max columns,  .(mpgsum = mean(sum_col), t1 = sum(mpg))
  # grouped by the col_group column
  dt[, .(unc_min = aggregate_unc(UNC_MIN, EMISSIONS), unc_max = aggregate_unc(UNC_MAX, EMISSIONS)), by = c(col_group)]

}
