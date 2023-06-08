# This is a script which corresponds to a particular task performed by the
# uedgar package. It is kept here as an example for how to run the same task in
# the future.

# TASK: to We need to do this for
# each of N2O and CH4, AND also CO2, where the latter should exclude biofuels

# I am running uedgar in dev mode, but otherwise call:
# library(uedgar)

# these are the countries and groups we want to get time series for
countries <- c("CHN", "BRA", "IND", "IDN", "NGA")
country_groups <- "EU-27"
substances <- c("CH4")

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
                                      agg_countries = FALSE, emi_id = emi_id, years = 2021, use_cache = F)
    # names(df_unc)[names(df_unc) == "Country_code_A3"] <- "Country"
    # df_unc$Substance <- substance
    #
    # if("Sector" %nin% names(df_unc)){
    #   df_unc$Sector <- "ALL"
    # }

    # add to df
    cache_unc <- rbind(cache_unc, df_unc)

  }

  # ADD COUNTRY GROUPS

  glue::glue("Calculating {country_groups} for {substance}....") |>
    message()
  df_unc <- get_uncertain_emissions(con, substances = substance, country_groups = country_groups,
                                    agg_countries = TRUE, emi_id = emi_id, years = 2021, use_cache = F)
  # df_unc$Country <- country_groups
  # df_unc$Substance <- substance
  #
  # if("Sector" %nin% names(df_unc)){
  #   df_unc$Sector <- "ALL"
  # }

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

plot_2021 <- function(substance, df_unc){

  # quick test
  df_plot <- df_unc[df_unc$Substance == substance, ]

  ggplot(df_plot) +
    geom_bar( aes(x=reorder(Country, -Emissions), y=Emissions), stat="identity", fill="skyblue", alpha=0.5) +
    geom_errorbar( aes(x=reorder(Country, -Emissions), ymin=Emissions_Min, ymax=Emissions_Max), width=0.2, colour="orange", alpha=0.9, size=0.9) +
    scale_x_discrete(labels=c("CHN" = "China", "USA" = "USA", "RUS" = "Russia",
                              "IND" = "India", "JPN" = "Japan", "EU-27" = "EU-27",
                              "BRA" = "Brazil", "MEX" = "Mexico", "IDN" = "Indonesia",
                              "NGA" = "Nigeria")) +
    xlab(element_blank()) +
    ylab("Emissions (Gt)") +
    ggtitle(paste0(substance)) +
    theme(
      plot.title = element_text(hjust=0.95, vjust=0.6, margin = margin(t=40,b=-30)),
      text = element_text(size = 20)
    )

}


