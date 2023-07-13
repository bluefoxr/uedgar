# ## Aim
#
# This doc aims to compare the most recent EDGAR GHG dataset (v8.0_FT2022_booklet) with the previous one (v7.0_FT2021_GHG). More specifically, we compare at the level of:
#
# - Country
# - Booklet substance groups
# - Substances (CO2, CH4 and N2O)
#
# Each of these combinations (country/sector/substance) results in a time series, and the idea is to compare the v7 time series against the v8 one. The comparison can be both visual (plotting the two time series) and "automatic" (e.g. flagging any discrepancies above a certain threshold).
#
# ## Plan
#
# I'll use the uedgar package to access the database for each time series. Ideally we want to run the query that directly results in the time series, rather than doing any data manipulation. In short, the plan is, for each time series:
#
# 1. Run query which returns the time series for both data sets
# 2. Generate a comparison plot
# 3. Generate a couple of summary measures (e.g. % differences)
# 4. Do this over all time series, and package up in some kind of doc (TBC)?
#
# ## Implementation
#
# We will need:
#
# - A list of all countries, substances and sector groups, and the dataset IDs
# - A function that returns the two time series for any combination of the above
# - Functions for plotting
# - Functions for comparing.
#
# Begin with a list of parameters for the comparison.
con <- connect_to_edgar()


# Parameters --------------------------------------------------------------

# dataset IDs - get ID of dataset corresponding to name v8.0_FT2022_booklet
IDs <- c(
  v7 = DBI::dbGetQuery(con, "SELECT ds_id FROM datasets WHERE ds_name LIKE 'v7.0_FT2021_GHG' AND owner = 'edgar_release'") |>
    unlist(),
  v8 = DBI::dbGetQuery(con, "SELECT ds_id FROM datasets WHERE ds_name LIKE 'v8.0_FT2022_booklet'") |>
    unlist()
)

IDs_string <- paste0(IDs, collapse = "', '")

# Query ----------------------------------------------------------
# actually this doesn't need to be a function any more.

# I need to select the columns which to sum over, but there are loads. Hacky
# solution is to build a long query here in R.
emi_cols <- paste0("Y_", c(1970:2020))

emi_cols_query <- paste0("SUM(", emi_cols, ") as ", emi_cols) |>
  toString()

# this aggregates everything by dataset, country, substance, sector
q_edgar <- glue::glue(
  "SELECT emi_id, Country_code_A3 AS Country, Substance, Sector, {emi_cols_query} ",
  "FROM ER_EMISSSIONS_ext ",
  "WHERE emi_id IN ('{IDs_string}') ",
  "AND Substance IN ('CO2', 'N2O', 'CH4') ",
  "GROUP BY emi_id, Country_code_A3, Substance, Sector"
)

# run query
all_series <- DBI::dbGetQuery(con, q_edgar) |>
  data.table::as.data.table()

# now we rerun but just for CO2 for v7 dataset, EXCLUDING BIOFUELS
q_edgar <- glue::glue(
  "SELECT emi_id, Country_code_A3 AS Country, Substance, Sector, {emi_cols_query} ",
  "FROM ER_EMISSSIONS_ext ",
  "WHERE emi_id IN ('{IDs[1]}') ",
  "AND Substance IN ('CO2') ",
  "AND fossil_bio = 'fossil' ",
  "GROUP BY emi_id, Country_code_A3, Substance, Sector"
)

# run query
all_series_CO2_v7 <- DBI::dbGetQuery(con, q_edgar) |>
  data.table::as.data.table()

# put together - we basically overwrite the CO2 v7 with the new stuff
# delete rows first
all_series <- all_series[!((emi_id == IDs[1]) & (Substance == "CO2")), ]
all_series <- rbind(all_series, all_series_CO2_v7)

# make long
all_series <- melt(all_series, measure = patterns("^Y_"), value.name = "Emissions",
              variable.name = "Year", na.rm = TRUE)

# convert year column to integer
all_series[, Year := as.integer(substr(Year, 3, 6))]

# change dset ids
all_series[emi_id == IDs[1], emi_id := "v7"]
all_series[emi_id == IDs[2], emi_id := "v8"]

names(all_series)[names(all_series) == "emi_id"] <- "Dataset"

# Plotting function -------------------------------------------------------

library(ggplot2)

plot_comparison <- function(all_series, country, substance, sector){

  qdata <- all_series[
    all_series$Substance == substance &
      all_series$Country == country &
      all_series$Sector == sector
  ]

  if(is.null(qdata)) return(NULL)
  if(!all(c("v7", "v8") %in% qdata$Dataset)) return(NULL)

  ggplot(qdata, aes(x = Year, y = Emissions,
                    group = Dataset, color = Dataset, linetype = Dataset)) +
    geom_line(alpha = 0.5, linewidth = 1) +
    theme_minimal()

}

# Comparison stats --------------------------------------------------------

get_comparison <- function(qdata){

  # make wide
  wdata <- dcast(qdata, Year ~ Dataset, value.var = "Emissions")

  # diffs
  wdata[, PrcDiff := 100*abs(v8 - v7)/v7]

  data.frame(
    Mean = wdata[, mean(PrcDiff, na.rm = TRUE)],
    Median = wdata[, median(PrcDiff, na.rm = TRUE)],
    Max = wdata[, max(PrcDiff, na.rm = TRUE)]
  )
}

# Run loop ----------------------------------------------------------------

# This is going to be done in a good old-fashioned for loop.

# First, expand all combos of our parameters
df_comp <- expand.grid(
  unique(all_series$Country),
  unique(all_series$Substance),
  unique(all_series$Sector),
  stringsAsFactors = FALSE
)

names(df_comp) <- c("Country", "Substance", "Sector")
df_comp$Mean <- NA
df_comp$Median <- NA
df_comp$Max <- NA
#df_comp$series <- vector(mode = "list", length = nrow(df_comp))

# now iterate over these rows
for(ii in 1:nrow(df_comp)){

  qdata <- all_series[
    all_series$Substance == df_comp$Substance[ii] &
      all_series$Country == df_comp$Country[ii] &
      all_series$Sector == df_comp$Sector[ii]
  ]

  if(is.null(qdata)) next
  if(!all(c("v7", "v8") %in% qdata$Dataset)) next

  stats_comparison <- get_comparison(qdata)

  df_comp[ii, c("Mean", "Median", "Max")] <- stats_comparison

}

# WRITE
saveRDS(list(df_comp = df_comp, all_series = all_series), "comparison_2023-06-29.RDS")


# CHECK some selected cases with weird things...
qdata1 <- # now we rerun but just for CO2 for v7 dataset, EXCLUDING BIOFUELS
  q_edgar <-

# run query
ESP7 <- DBI::dbGetQuery(
  con,
  glue::glue(
    "SELECT emi_id, Country_code_A3 AS Country, Substance, Sector, {emi_cols_query} ",
    "FROM ER_EMISSSIONS_ext ",
    "WHERE emi_id IN ('{IDs[1]}') ",
    "AND Substance IN ('CO2') ",
    "AND Country_code_A3 = 'ESP' ",
    "AND fossil_bio = 'fossil' ",
    "AND Sector = 'Agriculture' ",
    "GROUP BY emi_id, Country_code_A3, Substance, Sector"
  )
) |>
  data.table::as.data.table()

# run query
ESP8 <- DBI::dbGetQuery(
  con,
  glue::glue(
    "SELECT emi_id, Country_code_A3 AS Country, Substance, Sector, {emi_cols_query} ",
    "FROM ER_EMISSSIONS_ext ",
    "WHERE emi_id IN ('{IDs[2]}') ",
    "AND Substance IN ('CO2') ",
    "AND Country_code_A3 = 'ESP' ",
    "AND Sector = 'Agriculture' ",
    "GROUP BY emi_id, Country_code_A3, Substance, Sector"
  )
) |>
  data.table::as.data.table()
qdata <- rbind(ESP7, ESP8)

# make long
qdata <- melt(qdata, measure = patterns("^Y_"), value.name = "Emissions",
                   variable.name = "Year", na.rm = TRUE)

# convert year column to integer
qdata[, Year := as.integer(substr(Year, 3, 6))]

# change dset ids
qdata[emi_id == IDs[1], emi_id := "v7"]
qdata[emi_id == IDs[2], emi_id := "v8"]

names(qdata)[names(qdata) == "emi_id"] <- "Dataset"

ggplot(qdata, aes(x = Year, y = Emissions,
                  group = Dataset, color = Dataset, linetype = Dataset)) +
  geom_line(alpha = 0.5, linewidth = 1) +
  theme_minimal()


# Build reports -----------------------------------------------------------
# The idea here is to generate one doc per country. Then (possibly) knit
# them all together.

# NOTE this takes quite a while and render to the inst folder. I manually copied
# the files out to another folder.

# NOTE also that I didn't bother updating this, hence why it is commented out.
# The discrepancy table, plus plotting selected cases, seem sufficient. See the
# comparison Rmd document.

# for (country in countries){
#
#   message("Rendering for country: ", country)
#
#   df_c <- df_comp[(df_comp$Country == country) & !is.na(df_comp$Mean), ]
#   if(nrow(df_c) == 0) next
#
#   cname <- countrycode::countrycode(country, "iso3c", "country.name.en")
#   if(is.na(cname)) cname <- ""
#
#   rmarkdown::render(
#     input = "inst/country_comparison_template.Rmd",
#     params = list(df_c = df_c, ISO3 = country, cname = cname),
#     output_file = country
#   )
#
# }

# From this point on I switch to an Rmd file, which I will also create in the
# dataset comparison folder, and use that as an index with some exploration.


####################################### SPARE CODE ##########################################

# This is stuff from a previous version of this code where I was directly comparing
# the two data sets with a kind of query loop. Switched to a different system
# because (a) discovered had to leave out biofuels from CO2 in v7 dataset, and this
# made the query a bit more complicated. Then, Federico made a "view" of the
# emi_edgar_release table which made the query a lot simpler. So the stuff
# here is from the old version.

# countries <- DBI::dbGetQuery(
#   con,
#   glue::glue("SELECT DISTINCT Country_code_A3 FROM emi_edgar_release WHERE emi_id IN ('{IDs_string}')")
# ) |>
#   unlist()
#
# # substances
# substances <- c("CO2", "CH4", "N2O")
#
# # sectors
# # These are sector groupings. For the moment, store like this...
# sectors <- c(
#   Power = "ad_code like 'ENE%'",
#   Buildings = "ad_code like 'RCO%'",
#   Transport = "(ad_code like 'TNR%' or ad_code like 'TRO%')",
#   OtherIndComb = "(ad_code like 'IND%' or ad_code like 'REF%' or ad_code like 'TRF.E%')",
#   Other = "(ad_code not like 'ENE%' and ad_code not like 'RCO%' and ad_code not like 'TNR%' and ad_code not like 'TRO%' and ad_code not like 'IND%' and ad_code not like 'REF%' and ad_code not like 'TRF.E%' )"
# )
#
# # biofuels
# # Biofuels need to be removed from the v7.0_FT2021_GHG dataset ONLY FOR CO2
# # get biofuels
# biofuels <- DBI::dbGetQuery(
#   con,
#   glue::glue("SELECT DISTINCT fuel_code FROM Fuels WHERE fuel_category = 'BIO'")
# ) |>
#   unlist()
# biofuels_string <- paste0(biofuels, collapse = "', '")
#
# # retrieves two time series for specified substance, country and sector query.
# # The two time series are one for each of the data set IDs
# retrieve_time_series <- function(substance, country, sector){
#
#   # The steps are:
#   # 1. Filter data to selected substance, country, sector and the two datasets
#   # 2. Sum, grouped by dset (recall years are columns)
#   # 3. Reshape, maybe do this in R
#
#   # I need to select the columns which to sum over, but there are loads. Hacky
#   # solution is to build a long query here in R. Not pretty but works.
#   table_cols <- DBI::dbListFields(con, "emi_edgar_release")
#   emi_cols <- table_cols[startsWith(table_cols, "Y_")]
#   # remove 2021 and 2022
#   emi_cols <- setdiff(emi_cols, paste0("Y_", c(1960:1970, 2021, 2022)))
#
#   emi_cols_query <- paste0("SUM(", emi_cols, ") as ", emi_cols) |>
#     toString()
#
#   if(substance == "CO2"){
#
#     # we have to remove biofuels from CO2, but only for v7 data set. Not sure how
#     # to do this in one query, so split into two
#
#     q_edgar7 <- glue::glue(
#       "SELECT emi_id, {emi_cols_query} ", # extract fuel column as last 3 characters of ad_code
#       "FROM emi_edgar_release ",
#       "WHERE Substance = '{substance}' ",
#       "AND Country_code_A3 = '{country}' ",
#       "AND {sector} ",
#       "AND emi_id = '{IDs[1]}' ",
#       "AND RIGHT(ad_code, 3) NOT IN ('{biofuels_string}') ", # exclude biofuels
#       "GROUP BY emi_id"
#     )
#
#     # run query
#     qdata7 <- DBI::dbGetQuery(con, q_edgar7) |>
#       data.table::as.data.table()
#
#     q_edgar8 <- glue::glue(
#       "SELECT emi_id, {emi_cols_query} ",
#       "FROM emi_edgar_release ",
#       "WHERE Substance = '{substance}' ",
#       "AND Country_code_A3 = '{country}' ",
#       "AND {sector} ",
#       "AND emi_id IN ('{IDs_string}') ",
#       "GROUP BY emi_id"
#     )
#
#     # run query
#     qdata8 <- DBI::dbGetQuery(con, q_edgar8) |>
#       data.table::as.data.table()
#
#     qdata <- rbind(qdata7, qdata8)
#
#   } else {
#
#     q_edgar <- glue::glue(
#       "SELECT emi_id, {emi_cols_query} ",
#       "FROM emi_edgar_release ",
#       "WHERE Substance = '{substance}' ",
#       "AND Country_code_A3 = '{country}' ",
#       "AND {sector} ",
#       "AND emi_id IN ('{IDs_string}') ",
#       "GROUP BY emi_id"
#     )
#
#     # run query
#     qdata <- DBI::dbGetQuery(con, q_edgar) |>
#       data.table::as.data.table()
#
#   }
#
#   if(nrow(qdata) == 0) return(NULL)
#
#   # make long
#   qdata <- melt(qdata, measure = patterns("^Y_"), value.name = "Emissions",
#                 variable.name = "Year", na.rm = TRUE)
#   # convert year column to integer
#   qdata[, Year := as.integer(substr(Year, 3, 6))]
#
#   # change dset ids
#   qdata[emi_id == IDs[1], emi_id := "v7"]
#   qdata[emi_id == IDs[2], emi_id := "v8"]
#
#   names(qdata)[names(qdata) == "emi_id"] <- "Dataset"
#
#   return(qdata)
#
# }
