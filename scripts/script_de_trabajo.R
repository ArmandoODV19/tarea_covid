# script de trabajo

# llamando archivo de trabajo COVID-19

covid <- read_csv('raw_data/owid-covid-data.csv')

# convirtiendo archivo a .rds para eficazar el trabajo

saveRDS(covid, "clean_data/covid_dataset.rds")

# llamando archivo de trabajo covid_dataset.rds

covid <- readRDS("clean_data/covid_dataset.rds")

# visualizar dataset

glimpse(covid)
summary(covid)


# evaluar la relacion del dgp y los casos por millon
# tomar fecha maxima 2023-01-01
# gdp_per_capita vs total_cases_per_million

gdp_vs_cases <- covid %>%
  filter(date == "2023-01-01")







# prevalencia dm y muertes por covid
