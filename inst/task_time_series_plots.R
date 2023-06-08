# This is a script which corresponds to a particular task performed by the
# uedgar package. It is kept here as an example for how to run the same task in
# the future.

# TASK: to calculate total emissions for selected countries, as time series,
# with confidence intervals. Also include global totals. We need to do this for
# each of N2O and CH4, AND also CO2, where the latter should exclude biofuels

# I am running uedgar in dev mode, but otherwise call:
# library(uedgar)

# these are the countries and groups we want to get time series for
countries <- c("CHN", "USA", "RUS", "IND", "JPN")
country_groups <- "EU-27"
substances <- c("CH4", "N2O", "CO2")

# first, connect to edgar
con <- connect_to_edgar()

# create df for results
# The idea is that this will cache common results
columns = c("Country", "Year", "Substance", "Sector",
            "Emissions", "prc_lower", "prc_upper", "Emissions_Min", "Emissions_Max")
cache_unc = data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(cache_unc) = columns

for(substance in substances){

  if(substance == "CO2"){
    emi_id <- "09062022142022"
  } else {
    emi_id <- "29072022103026"
  }

  for(country in countries){

    # ADD COUNTRIES

    glue::glue("Calculating {country} for {substance}....") |>
      message()
    df_unc <- get_uncertain_emissions(con, substances = substance, countries = country,
                                      agg_countries = FALSE, emi_id = emi_id)
    names(df_unc)[names(df_unc) == "Country_code_A3"] <- "Country"
    df_unc$Substance <- substance

    if("Sector" %nin% names(df_unc)){
      df_unc$Sector <- "ALL"
    }

    # add to df
    cache_unc <- rbind(cache_unc, df_unc)

  }

  # ADD COUNTRY GROUPS

  glue::glue("Calculating {country_groups} for {substance}....") |>
    message()
  df_unc <- get_uncertain_emissions(con, substances = substance, country_groups = country_groups,
                                    agg_countries = TRUE, emi_id = emi_id)
  df_unc$Country <- country_groups
  df_unc$Substance <- substance

  if("Sector" %nin% names(df_unc)){
    df_unc$Sector <- "ALL"
  }

  # add to df
  cache_unc <- rbind(cache_unc, df_unc)

}

# PLOTS -------------------------------------------------------------------

library(ggplot2)
library(ggthemes)

# change to gigatonnes
to_gigatonnes <- function(x) x/1e6
df_unc <- cache_unc
df_unc$Emissions <- to_gigatonnes(df_unc$Emissions)
df_unc$Emissions_Min <- to_gigatonnes(df_unc$Emissions_Min)
df_unc$Emissions_Max <- to_gigatonnes(df_unc$Emissions_Max)

plot_time_series_group <- function(substance, df_unc){

  # quick test
  df_plot <- df_unc[df_unc$Substance == substance, ]

  # Make the plot
  plt <- ggplot(df_plot, aes(x = Year, y = Emissions, ymin = Emissions_Min, ymax = Emissions_Max, fill = Country)) +
    geom_line() +
    geom_ribbon(alpha=0.5) +
    xlab(element_blank()) +
    ylab("Emissions (Gt)") +
    theme_few() +
    scale_fill_few(labels = c("CHN" = "China", "USA" = "USA", "RUS" = "Russia", "IND" = "India", "JPN" = "Japan", "EU-27" = "EU-27")) +
    theme(legend.position = "top", legend.title = element_blank(), text = element_text(size = 20)) +
    guides(fill = guide_legend(nrow = 1)) +
    ggtitle(paste0(substance, " Emissions for top emitters"))

  plt

}


