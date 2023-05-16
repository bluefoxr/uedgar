# basic test of aggregation formula (uncorrelated)
test_that("agg_correlation", {

  emi <- runif(20)
  u <- runif(20)*100 # as percentage

  u_agg <- f_agg_uncorrelated(emi, u)
  u_agg2 <- sqrt(sum((emi*u)^2))/sum(abs(emi))
  expect_equal(u_agg, u_agg2)

})

# test aggregation of dt
test_that("agg_dt", {

  # fake emissions
  dt_emissions <- data.table::data.table(
    Emissions = runif(20),
    prc_lower = runif(20)*100,
    prc_upper = runif(20)*100,
    Substance = "N2O"
  )

  # aggregate uncorrelated
  dt_agg <- aggregate_unc(dt_emissions, correlated = FALSE)
  dt_agg2 <- data.table::data.table(
    Substance = "N2O",
    Emissions = sum(dt_emissions$Emissions),
    prc_lower = f_agg_uncorrelated(dt_emissions$Emissions, dt_emissions$prc_lower),
    prc_upper = f_agg_uncorrelated(dt_emissions$Emissions, dt_emissions$prc_upper)
  )
  expect_equal(dt_agg, dt_agg)

  # aggregate correlated
  dt_agg_corr <- aggregate_unc(dt_emissions, correlated = TRUE)
  dt_agg_corr2 <- data.table::data.table(
    Substance = "N2O",
    Emissions = sum(dt_emissions$Emissions),
    prc_lower = sum(dt_emissions$prc_lower * dt_emissions$Emissions)/sum(dt_emissions$Emissions),
    prc_upper = sum(dt_emissions$prc_upper * dt_emissions$Emissions)/sum(dt_emissions$Emissions)
  )
  expect_equal(dt_agg_corr, dt_agg_corr2)

})

test_that("agg_substance", {

  # fake emissions with fuel
  dt_emissions <- data.table::data.table(
    Emissions = runif(20),
    prc_lower = runif(20)*100,
    prc_upper = runif(20)*100,
    Substance = "CO2",
    Fuel = rep(c("DIE", "NAP"), 10)
  )

  dt_agg <- aggregate_substance(dt_emissions, correlate_in_fuel = TRUE)

  # since aggregate_unc() passes we can use that to test
  dt_sp <- split(dt_emissions, dt_emissions$Fuel)

  l_agg <- lapply(dt_sp, aggregate_unc, correlated = TRUE)
  dt_agg2 <- Reduce(rbind, l_agg)
  dt_agg2 <- cbind(Fuel = names(l_agg), dt_agg2)
  expect_equal(dt_agg, dt_agg2)

})


