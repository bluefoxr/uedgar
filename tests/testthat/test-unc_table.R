test_that("AAAs accounted", {

  con <- connect_to_edgar()

  unc_table <- get_uncertainty_table(con)

  # same but without subst of AAAs
  unc_table2 <- DBI::dbGetQuery(
    con,
    "SELECT Process, Substance, Country, Unc_emi_min_fixed, Unc_emi_max_fixed FROM unc_emi_table"
  ) |>
    data.table::as.data.table()

  # manually compare each row
  unc_table2 <- unc_table2[Country == "AAA", ]
  unc_table2$Check_min <- as.numeric(NA)
  unc_table2$Check_max <- as.numeric(NA)

  for (ii in 1:nrow(unc_table2)){

    rowz <- unc_table[Process == unc_table2$Process[ii] & Substance ==unc_table2$Substance[ii], ]

    unc_table2[ii, c("Check_min", "Check_max")] <- rowz[1, c("Unc_emi_min_fixed", "Unc_emi_max_fixed")]
  }

  expect_equal(unc_table2$Unc_emi_min_fixed, unc_table2$Check_min)
  expect_equal(unc_table2$Unc_emi_max_fixed, unc_table2$Check_max)

})
