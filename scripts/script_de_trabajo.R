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
