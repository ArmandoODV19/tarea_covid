# Cargando librerias de trabajo

library(readr)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(scales)

# cargando archivo de trabajo COVID-19

covid <- read_csv('raw_data/owid-covid-data.csv')

# convirtiendo archivo a .rds para disminuir el uso de memoria

saveRDS(covid, "clean_data/covid_dataset.rds")

# cargando archivo de trabajo covid_dataset.rds

covid_raw <- readRDS("clean_data/covid_dataset.rds")

# visualizar dataset

glimpse(covid_raw)
summary(covid_raw)

# evaluar la relacion del dgp y los casos por millon de muertes y casos
# tomar fecha maxima 2023-01-01 y eliminar valores nulos

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



ggplot(covid_limpio, aes(y = total_cases_per_million, x = gdp_per_capita,
                         color = continent)) +
  geom_point(size = 7, alpha = 0.7)+
  scale_color_manual(values = c('dodgerblue', 'forestgreen', 'salmon',
                                'wheat', 'lightsteelblue', 'chocolate')) +
  geom_smooth(method = 'lm', na.rm = TRUE, fullrange= TRUE,
              aes(group=1),colour="azure4")+
  xlab("Producto interno bruto per cápita [Dólares, 2023]") +
  ylab("Casos totales por millón de habitantes") +
  labs(caption = element_text("Figura 1.Relación de producto interno bruto per cápita y total de casos positivos por millón de habitantes. Cada punto representa a los países
que conforman la base de datos. El color detalla 6 regiones geográficas en las cuales se agrupa la información. En la parte superior derecha
se muestra el coeficiente de determinación (r2 = 0.35) y la función que explica el modelo lineal."),
color = 'Continente') +
  scale_x_continuous(labels = label_comma()) +
  scale_y_continuous(labels = label_comma()) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0)) +
  stat_regline_equation(label.y = 750000, show.legend = NULL, label.x = 100000,
                        aes(group = 1, label = ..eq.label..)) +
  stat_regline_equation(label.y = 700000, show.legend = NULL, label.x = 100000,
                        aes(group = 1, label = ..rr.label..))
# guardando la imagen

ggsave(path = "imagenes/cases_vs_gdp.png", scale = 1, width = 976, height = 448,
       units = "px", dpi = 300)

dev.off()

# obtener media, mediana, sd de casos por covid por millon de habitantes

ggplot(covid_limpio, aes(x = total_cases_per_million)) +
  geom_histogram(bins = 15) +
  theme_bw()

mean(covid_limpio$total_cases_per_million, na.rm = TRUE)
sd(covid_limpio$total_cases_per_million, na.rm = TRUE)
median(covid_limpio$total_cases_per_million, na.rm = TRUE)

# obtener media, mediana, sd de gdp_per_capita

ggplot(covid_limpio, aes(x = gdp_per_capita)) +
  geom_histogram(bins = 15, fill = 'dodgerblue', alpha = 0.7) +
  theme_bw() +
  xlab("Producto Interno Bruto per cápita") +
  ylab("Frecuencia")

mean(covid_limpio$gdp_per_capita, na.rm = TRUE)
sd(covid_limpio$gdp_per_capita, na.rm = TRUE)
median(covid_limpio$gdp_per_capita, na.rm = TRUE)

########################################

# evaluar la relacion de muertes por millon y los casos por millon

ggplot(covid_limpio, aes(x = total_cases_per_million, y = total_deaths_per_million,
                         color = continent)) +
  geom_point(size = 7, alpha = 0.7) +
  scale_color_manual(values = c('dodgerblue', 'forestgreen', 'salmon',
                                'wheat', 'lightsteelblue', 'chocolate')) +
  geom_smooth(method = 'lm', na.rm = TRUE, fullrange= TRUE,
              aes(group=1),colour="azure4")+
  xlab("Casos totales por millón de habitantes") +
  ylab("Muertes totales por millón de habitantes") +
  labs(caption = "Figura 2. Relación del total de casos positivos por millón de habitantes y el total de muertes por millón de habitantes. Cada punto representa a
los países que conforman la base de datos. El color detalla 6 regiones geográficas en las cuales se agrupa la información. En la parte superior
derecha se muestra el coeficiente de determinación (r2 = 0.23) y la función que explica el modelo lineal.",
color = 'Continente') +
  scale_x_continuous(labels = label_comma()) +
  scale_y_continuous(labels = label_comma()) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0)) +
  stat_regline_equation(label.y = 6000, show.legend = NULL, label.x = 600000,
                        aes(group = 1, label = ..eq.label..)) +
  stat_regline_equation(label.y = 5700, show.legend = NULL, label.x = 600000,
                        aes(group = 1, label = ..rr.label..))

# guardando imagen

ggsave(path = "imagenes/cases_vs_deaths.png", scale = 1, width = 976, height = 448,
       units = "px", dpi = 300)

dev.off()

# obtener media, mediana, sd de muertes por covid por millon de habitantes

ggplot(covid_limpio, aes(x = total_deaths_per_million)) +
  geom_histogram(bins = 15)

mean(covid_limpio$total_deaths_per_million, na.rm = TRUE)
sd(covid_limpio$total_deaths_per_million, na.rm = TRUE)
median(covid_limpio$total_deaths_per_million, na.rm = TRUE)


