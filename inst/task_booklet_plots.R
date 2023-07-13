# This is a script which corresponds to a particular task performed by the
# uedgar package. It is kept here as an example for how to run the same task in
# the future.

# TASK: to generate plots for the 2023 booklet. Specifically:
# - Total GHG time series for top emitters
# - Total GHG/capita time series for the same top emitters
# - Total GHG/GDPppp time series for the same top emitters

# NOTES
# This script evolved quite a bit. The uncertainty calculations are quite intensive
# especially at the global level. This is why the results were saved to RDS, to
# avoid redoing each time.
# Note that for the plots, I have disabled the uncertainty bands (geom_ribbon)
# in some places because we saved the plots with and without the uncertainty.
# A lot of the stuff here could/should be wrapped in functions to avoid
# repetition but we were in a big rush.
# Finally, would like to double check:
# - that the lognormal transformation is correctly applied only where necessary
# - Noticed that in Efisio's plots, the uncertainty seems to decrease over time
#   this seems to be due to a correction/tweak he made - to check.

# I am running uedgar in dev mode, but otherwise call:
# library(uedgar)

# these are the countries and groups we want to get time series for
countries <- c("CHN", "USA", "RUS", "IND", "BRA")
country_groups <- "EU-27"

# total emissions in CO2 equiv
substances_with_unc <- list(
  CO2 = "CO2",
  CH4 = "GWP_100_AR5_CH4",
  N2O = "GWP_100_AR5_N2O"
)

substance_no_unc <- "GWP_100_AR5_F-gases"

all_substances <- c(as.character(substances_with_unc), substance_no_unc)

# PLAN ###
# As it stands, uncertainties are at the substance level and only for CO2, CH4
# and N2O. Which means nothing for F-gases. So, the plan is:
# 1. Calculate uncertainties for CO2, CH4, N2O separately, but in CO2 equiv.
# 2. Combine uncertainties from these three to get total uncertainties as %
# 3. Make a direct query to DB to get the time series totals, including F-gases
# 4. Apply the percentages to these totals.

# 1. Calc uncertainties by substance --------------------------------------

# I comment this out to avoid redoing the calcs, which take a few mins

# # first, connect to edgar
con <- connect_to_edgar()
#
# # get the dataset ID from name
emi_id <- get_dataset_ID("v8.0_FT2022_booklet", con)

# # get country emissions
# dt_emi <- get_uncertain_emissions(
#   con,
#   substances = substances_with_unc,
#   countries = countries,
#   agg_countries = FALSE,
#   emi_id = emi_id,
#   use_cache = FALSE
# )
#
# # get country group emissions
# # get EU27 countries
EU27 <- DBI::dbGetQuery(con, "SELECT Country_code_A3 FROM V_countries_bkl WHERE Country_group = 'EU27'")[[1]]
#
# dt_emi2 <- get_uncertain_emissions(
#   con,
#   substances = substances_with_unc,
#   countries = EU27,
#   agg_countries = TRUE,
#   emi_id = emi_id,
#   use_cache = FALSE
# )
#
# dt_emi2$Country <- "EU27"
# #
# # get world emissions
# get world countries
world_countries <- DBI::dbGetQuery(con, "SELECT DISTINCT Country_code_A3 FROM V_countries_bkl")[[1]]
#
# dt_emi_world <- get_uncertain_emissions(
#   con,
#   substances = substances_with_unc,
#   countries = world_countries,
#   agg_countries = TRUE,
#   emi_id = emi_id,
#   use_cache = FALSE
# )
#
# dt_emi_world$Country <- "World"
#
# # put together
# dt_emi <- rbind(dt_emi, dt_emi2, dt_emi_world)
# #
# # # save these to avoid redoing each time
# saveRDS(dt_emi, "./inst/dt_emi_booklet_2023-07.RDS")

# load data
dt_emi <- readRDS("./inst/dt_emi_booklet_2023-07.RDS")

# 2. Aggregate substance uncertainties together ---------------------------

# next we need to combine the uncertainties by COUNTRY
# we are only interested in the percentage uncertainty cols, but add the Emissions col as it is
# expected by one of the functions below.
agg_uncerts <- dt_emi[, c("Country", "Year", "Substance", "Emissions", "prc_lower", "prc_upper")]
# hacky: to avoid messing with existing functions I make the substance column into one thing
# anyway it will be summed together...
agg_uncerts$Substance <- "CO2+N2O+CH4"

# aggregate substances together
agg_uncerts <- aggregate_by_group(agg_uncerts, c("Country", "Year"), correlated = FALSE)


# 3. Get full time series from DB -----------------------------------------

# I need to select the columns which to sum over, but there are loads. Hacky
# solution is to build a long query here in R.
emi_cols <- paste0("Y_", c(1970:2022))

emi_cols_query <- paste0("SUM(", emi_cols, ") as ", emi_cols) |>
  toString()

# this aggregates everything by dataset, country, substance, sector
q_edgar <- glue::glue(
  "SELECT Country_code_A3 AS Country, {emi_cols_query} ",
  "FROM emi_edgar_release ",
  "WHERE emi_id = '{emi_id}' ",
  "AND Country_code_A3 IN ({character_to_query(countries)}) ",
  "AND Substance IN ({character_to_query(all_substances)}) ",
  "GROUP BY Country_code_A3"
)

# run query
all_series <- DBI::dbGetQuery(con, q_edgar) |>
  data.table::as.data.table()

# now do the same but for EU27
q_edgar <- glue::glue(
  "SELECT {emi_cols_query} ",
  "FROM emi_edgar_release ",
  "WHERE emi_id = '{emi_id}' ",
  "AND Country_code_A3 IN ({character_to_query(EU27)}) ",
  "AND Substance IN ({character_to_query(all_substances)}) "
)
# run query
all_series2 <- DBI::dbGetQuery(con, q_edgar) |>
  data.table::as.data.table()

all_series2$Country <- "EU27"

# now do the same but for world
q_edgar <- glue::glue(
  "SELECT {emi_cols_query} ",
  "FROM emi_edgar_release ",
  "WHERE emi_id = '{emi_id}' ",
  "AND Country_code_A3 IN ({character_to_query(world_countries)}) ",
  "AND Substance IN ({character_to_query(all_substances)}) "
)
# run query
all_series3 <- DBI::dbGetQuery(con, q_edgar) |>
  data.table::as.data.table()

all_series3$Country <- "World"

# put together
all_series <- rbind(all_series, all_series2, all_series3)

# make long
all_series <- melt(all_series, measure = patterns("^Y_"), value.name = "Emissions",
                   variable.name = "Year", na.rm = TRUE)

# convert year column to integer
all_series[, Year := as.integer(substr(Year, 3, 6))]


# 4. Apply uncertainty percentages ----------------------------------------

# I have to merge the two tables
# Get rid of columns that are not interesting from uncertainties table
agg_uncerts <- agg_uncerts[, c("Substance", "Emissions"):=NULL]
# join
all_series2 <- agg_uncerts[all_series, on = c("Country", "Year")]

# make upper and lower bounds
all_series2[, Emissions_Min := Emissions - (Emissions*prc_lower/100)]
all_series2[, Emissions_Max := Emissions + (Emissions*prc_upper/100)]

# transform to lognormal
#all_series2 <- to_lognormal(all_series2)


# PLOTS -------------------------------------------------------------------

library(ggplot2)
library(ggthemes)

# change to gigatonnes
to_gigatonnes <- function(x) x/1e6
df_unc <- all_series2
df_unc$Emissions <- to_gigatonnes(df_unc$Emissions)
df_unc$Emissions_Min <- to_gigatonnes(df_unc$Emissions_Min)
df_unc$Emissions_Max <- to_gigatonnes(df_unc$Emissions_Max)

# set font
windowsFonts("EC Square Sans Pro" = windowsFont("EC Square Sans Pro"))

# colours
country_colours <- c(
  EU27 = "#004494",
  CHN = "#bf9b68",
  USA = "#00bfc4",
  RUS = "#f8766d",
  IND = "#FFA500",
  BRA = "#00ba38"
)

world_colour = "#000000"

# filter out World for this plot
df_unc <- df_unc[Country != "World"]

# Make the plot
# This aims to replicate the plots in previous booklets
plt <- ggplot(df_unc, aes(x = Year, y = Emissions, ymin = Emissions_Min, ymax = Emissions_Max)) +
  geom_line(aes(color = Country), linewidth = 2) +
  #geom_ribbon(alpha=0.3, aes(fill = Country)) +
  xlab(element_blank()) +
  ylab("Gt CO2eq") +
  theme_minimal() +
  #scale_color_few(labels = c("CHN" = "China", "USA" = "USA", "RUS" = "Russia", "IND" = "India", "EU-27" = "EU-27", "BRA" = "Brazil")) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        text = element_text(size = 40, family = "EC Square Sans Pro"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = '#FAFAFA', colour = "white")
  ) +
  guides(color = guide_legend(nrow = 1), fill = "none") +
  scale_x_continuous(limits=c(1970, 2022), breaks=c(seq(1970,2020,5), 2022), expand = c(0.01, 0.01)) +
  scale_fill_manual(
    values=country_colours,
    labels = c("CHN" = "China", "USA" = "USA", "RUS" = "Russia", "IND" = "India", "EU-27" = "EU-27", "BRA" = "Brazil")) +
  scale_color_manual(
    values=country_colours,
    labels = c("CHN" = "China", "USA" = "USA", "RUS" = "Russia", "IND" = "India", "EU-27" = "EU-27", "BRA" = "Brazil"))
  #ggtitle(" Emissions for top emitters")

# SAVE
png("GHG_totals_top_emitters.png", width=2000, height=1200)
plt
dev.off()


# PER CAPITA --------------------------------------------------------------

# I basically need the populations, by year, for each country plus EU27, plus world

q_edgar <- glue::glue(
  "SELECT Country_code_A3 AS Country, {emi_cols_query} ",
  "FROM emi_edgar_release ",
  "WHERE Substance = 'POP' ",
  "AND emi_id = '{emi_id}' ",
  "GROUP BY Country_code_A3"
)

pops <- DBI::dbGetQuery(con, q_edgar) |>
  data.table::as.data.table()

# calculate EU27 population
EU27_row <- colSums(pops[Country %in% EU27 ,-1]) |>
  t() |>
  as.data.frame()
EU27_row <- cbind(Country = "EU27", EU27_row)

# calculate world population
world_row <- colSums(pops[Country %in% world_countries ,-1]) |>
  t() |>
  as.data.frame()
world_row <- cbind(Country = "World", world_row)

pops <- rbind(
  pops,
  EU27_row,
  world_row
)

# make long
pops <- melt(pops, measure = patterns("^Y_"), value.name = "Population",
                   variable.name = "Year", na.rm = TRUE)

# convert year column to integer
pops[, Year := as.integer(substr(Year, 3, 6))]

df_unc <- all_series2

# join onto df_unc
df_unc <- pops[df_unc, on = c("Country", "Year")]

# calc emissions per cap, plus upper and lower prcs
df_unc[, Emissions_pc := Emissions/Population]
df_unc[, Emissions_pc_min := Emissions_pc - (Emissions_pc*prc_lower/100)]
df_unc[, Emissions_pc_max := Emissions_pc + (Emissions_pc*prc_upper/100)]

country_colours <- c(country_colours, World = world_colour)

# Make the plot
# This aims to replicate the plots in previous booklets
plt <- ggplot(df_unc, aes(x = Year, y = Emissions_pc, ymin = Emissions_pc_min, ymax = Emissions_pc_max)) +
  geom_line(aes(color = Country), linewidth = 2) +
  geom_ribbon(alpha=0.3, aes(fill = Country)) +
  xlab(element_blank()) +
  ylab("t CO2eq / cap") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        text = element_text(size = 40, family = "EC Square Sans Pro"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = '#FAFAFA', colour = "white")
  ) +
  guides(color = guide_legend(nrow = 1), fill = "none") +
  scale_x_continuous(limits=c(1970, 2022), breaks=c(seq(1970,2020,5), 2022), expand = c(0.01, 0.01)) +
  scale_fill_manual(
    values=country_colours) +
  scale_color_manual(
    values=country_colours,
    labels = c("CHN" = "China", "USA" = "USA", "RUS" = "Russia", "IND" = "India", "EU-27" = "EU-27", "BRA" = "Brazil", "World" = "World average"))

plt

# SAVE
png("GHG_pc_top_emitters.png", width=2000, height=1200)
plt
dev.off()



# PER GDP -----------------------------------------------------------------

# I basically need the populations, by year, for each country plus EU27, plus world

emi_cols <- paste0("Y_", c(1990:2022))
emi_cols_query <- paste0("SUM(", emi_cols, ") as ", emi_cols) |>
  toString()

q_edgar <- glue::glue(
  "SELECT Country_code_A3 AS Country, {emi_cols_query} ",
  "FROM emi_edgar_release ",
  "WHERE Substance = 'GPP' ",
  "AND emi_id = '{emi_id}' ",
  "GROUP BY Country_code_A3"
)

GDP <- DBI::dbGetQuery(con, q_edgar) |>
  data.table::as.data.table()

# calculate EU27 GDP
EU27_row <- colSums(GDP[Country %in% EU27 ,-1]) |>
  t() |>
  as.data.frame()
EU27_row <- cbind(Country = "EU27", EU27_row)

# calculate world population
world_row <- colSums(GDP[Country %in% world_countries ,-1]) |>
  t() |>
  as.data.frame()
world_row <- cbind(Country = "World", world_row)

GDP <- rbind(
  GDP,
  EU27_row,
  world_row
)

# make long
GDP <- melt(GDP, measure = patterns("^Y_"), value.name = "GDP",
             variable.name = "Year", na.rm = TRUE)

# convert year column to integer
GDP[, Year := as.integer(substr(Year, 3, 6))]

df_unc <- all_series2[Year >= 1990]

# join onto df_unc
df_unc <- GDP[df_unc, on = c("Country", "Year")]

# calc emissions per GDP, plus upper and lower prcs
df_unc[, Emissions_pGDP := Emissions/(GDP*1000)]
df_unc[, Emissions_pGDP_min := Emissions_pGDP - (Emissions_pGDP*prc_lower/100)]
df_unc[, Emissions_pGDP_max := Emissions_pGDP + (Emissions_pGDP*prc_upper/100)]

df_unc1 <- df_unc[Country %in% c("CHN", "IND", "RUS", "World")]
df_unc2 <- df_unc[Country %in% c("USA", "BRA", "EU27", "World")]

# This aims to replicate the plots in previous booklets
plt1 <- ggplot(df_unc1, aes(x = Year, y = Emissions_pGDP, ymin = Emissions_pGDP_min, ymax = Emissions_pGDP_max)) +
  geom_line(aes(color = Country), linewidth = 2) +
  #geom_ribbon(alpha=0.3, aes(fill = Country)) +
  xlab(element_blank()) +
  ylab("t CO2eq / k USD") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        text = element_text(size = 40, family = "EC Square Sans Pro"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = '#FAFAFA', colour = "white")
  ) +
  guides(color = guide_legend(nrow = 1), fill = "none") +
  scale_x_continuous(limits=c(1990, 2022), breaks=c(seq(1970,2020,5), 2022), expand = c(0.01, 0.01)) +
  scale_fill_manual(
    values=country_colours) +
  scale_color_manual(
    values=country_colours,
    labels = c("CHN" = "China", "USA" = "USA", "RUS" = "Russia", "IND" = "India", "EU-27" = "EU-27", "BRA" = "Brazil", "World" = "World average"))

# This aims to replicate the plots in previous booklets
plt2 <- ggplot(df_unc2, aes(x = Year, y = Emissions_pGDP, ymin = Emissions_pGDP_min, ymax = Emissions_pGDP_max)) +
  geom_line(aes(color = Country), linewidth = 2) +
  #geom_ribbon(alpha=0.3, aes(fill = Country)) +
  xlab(element_blank()) +
  ylab(element_blank()) +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        text = element_text(size = 40, family = "EC Square Sans Pro"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = '#FAFAFA', colour = "white")
  ) +
  guides(color = guide_legend(nrow = 1), fill = "none") +
  scale_x_continuous(limits=c(1990, 2022), breaks=c(seq(1970,2020,5), 2022), expand = c(0.01, 0.01)) +
  scale_fill_manual(
    values=country_colours) +
  scale_color_manual(
    values=country_colours,
    labels = c("CHN" = "China", "USA" = "USA", "RUS" = "Russia", "IND" = "India", "EU-27" = "EU-27", "BRA" = "Brazil", "World" = "World average"))

library(patchwork)
plt <- plt1 + plt2

# SAVE
png("GHG_pGDP_top_emitters.png", width=2000, height=1200)
plt
dev.off()
