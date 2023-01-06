### script borrador


# llamando archivo de trabajo COVID-19

covid <- read_csv('raw_data/owid-covid-data.csv')

# convirtiendo archivo a .rds para eficazar el trabajo

saveRDS(covid, "clean_data/covid_dataset.rds")

# llamando archivo de trabajo covid_dataset.rds

covid_raw <- readRDS("clean_data/covid_dataset.rds")

# visualizar dataset

glimpse(covid_raw)
summary(covid_raw)

# evaluar la relacion del dgp y los casos por millon de muertes y casos
# tomar fecha maxima 2023-01-01
# 1. gdp_per_capita vs total_cases_per_million

covid_limpio <- covid_raw %>%
  filter(date == "2023-01-01",
         continent != "NA")

# cambiando nombre a los continentes
covid_limpio$continent <- plyr::revalue(covid_limpio$continent,
                                       c("Africa" = "África",
                                         "Asia" = "Asia",
                                         "Europe" = "Europa",
                                         "North America" = "Norteamérica",
                                         "Oceania" = "Oceania",
                                         "South America" = "Sudamérica"))

ggplot(covid_limpio, aes(x = total_cases_per_million, y = gdp_per_capita,
                         color = continent)) +
  geom_point(size = 7, alpha = 0.7)+
  scale_color_manual(values = c('dodgerblue', 'forestgreen', 'salmon',
                                'wheat', 'lightsteelblue', 'chocolate')) +
  geom_smooth(method = 'lm', na.rm = TRUE, fullrange= TRUE,
              aes(group=1),colour="azure4")+
  xlab("Total de casos por millon de habitantes") +
  ylab("Ingreso bruto por habitante") +
  ggtitle('Total de casos por millon de habitantes e ingreso bruto por habitante') +
  labs(caption = element_text("my caption"),
       color = 'Continente') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0))

# obtener media, mediana, sd de casos por covid por millon de habitantes

ggplot(covid_limpio, aes(x = total_cases_per_million)) +
  geom_histogram(bins = 15)

mean(covid_limpio$total_cases_per_million, na.rm = TRUE)
sd(covid_limpio$total_cases_per_million, na.rm = TRUE)
median(covid_limpio$total_cases_per_million, na.rm = TRUE)

########################################

# evaluar la relacion de muertes por millon y los casos por millon

ggplot(covid_limpio, aes(x = total_cases_per_million, y = total_deaths_per_million,
                         color = continent)) +
  geom_point(size = 7, alpha = 0.7) +
  scale_color_manual(values = c('dodgerblue', 'forestgreen', 'salmon',
                                'wheat', 'lightsteelblue', 'chocolate')) +
  geom_smooth(method = 'lm', na.rm = TRUE, fullrange= TRUE,
              aes(group=1),colour="azure4")+
  xlab("Casos totales por millón") +
  ylab("Muertes totales por millón") +
  ggtitle("Casos por millón y muertes por millón") +
  labs(caption = "my caption",
       color = 'Continente') +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0))

# obtener media, mediana, sd de muertes por covid por millon de habitantes

ggplot(covid_limpio, aes(x = total_deaths_per_million)) +
  geom_histogram(bins = 15)

mean(covid_limpio$total_deaths_per_million, na.rm = TRUE)
sd(covid_limpio$total_deaths_per_million, na.rm = TRUE)
median(covid_limpio$total_deaths_per_million, na.rm = TRUE)


