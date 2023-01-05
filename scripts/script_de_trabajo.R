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

########################################

# evaluar la relacion del dgp y los casos por millon de muertes y casos
# tomar fecha maxima 2023-01-01
# 1. gdp_per_capita vs total_cases_per_million
# 2. gdp_per_capita vs total_deaths_per_million

gdp_vs_cases <- covid %>%
  filter(date == "2023-01-01")


ggplot(gdp_vs_cases, aes(x = total_cases_per_million, y = gdp_per_capita)) +
  geom_point(size = 4, color = "forestgreen")+
  geom_smooth(method = 'lm')+
  theme_bw()

ggplot(gdp_vs_cases, aes(x = total_cases_per_million, y = gdp_per_capita,
                         color = continent)) +
  geom_point(size = 4)+
  geom_smooth(method = 'lm', na.rm = TRUE, fullrange= TRUE,
              aes(group=1),colour="black")+
  theme_bw()

ggplot(gdp_vs_cases, aes(x = total_deaths_per_million, y = gdp_per_capita)) +
  geom_point(size = 4, color = "forestgreen")+
  geom_smooth(method = 'lm')+
  theme_bw()

# obtener media, mediana, sd de casos por covid por millon de habitantes

ggplot(gdp_vs_cases, aes(x = total_cases_per_million)) +
  geom_histogram(bins = 15)

mean(gdp_vs_cases$total_cases_per_million, na.rm = TRUE)
sd(gdp_vs_cases$total_cases_per_million, na.rm = TRUE)
median(gdp_vs_cases$total_cases_per_million, na.rm = TRUE)

# obtener media, mediana, sd de muertes por covid por millon de habitantes

ggplot(gdp_vs_cases, aes(x = total_deaths_per_million)) +
  geom_histogram(bins = 15)

mean(gdp_vs_cases$total_deaths_per_million, na.rm = TRUE)
sd(gdp_vs_cases$total_deaths_per_million, na.rm = TRUE)
median(gdp_vs_cases$total_deaths_per_million, na.rm = TRUE)


########################################

# esperanza de vida y muertes por covid

ggplot(gdp_vs_cases, aes(x = total_deaths_per_million, y = life_expectancy)) +
  geom_point(size = 4, color = "forestgreen")+
  geom_smooth(method = 'lm')+
  theme_bw()


#ggplot(gdp_vs_cases, aes(x = total_cases_per_million, y = diabetes_prevalence)) +
#  geom_point(size = 4, color = "forestgreen")+
#  geom_smooth(method = 'lm')+
#  theme_bw()


########################################

# casos vs muertes

ggplot(gdp_vs_cases, aes(x = total_cases_per_million, y = total_deaths_per_million)) +
  geom_point(size = 4, color = "forestgreen")+
  geom_smooth(method = 'lm')+
  theme_bw()


# obtener media, mediana, sd de gdp

ggplot(gdp_vs_cases, aes(x = gdp_per_capita)) +
  geom_histogram(bins = 15)

mean(gdp_vs_cases$gdp_per_capita, na.rm = TRUE)
sd(gdp_vs_cases$gdp_per_capita, na.rm = TRUE)
median(gdp_vs_cases$gdp_per_capita, na.rm = TRUE)

# al final se reportaran gdp vs casos
# y casos vs muertes


