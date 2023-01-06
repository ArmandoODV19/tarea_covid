eq <- function(x,y) {
  m <- lm(y ~ x)
  as.character(
    as.expression(
      substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                 list(a = format(coef(m)[1], digits = 4),
                      b = format(coef(m)[2], digits = 4),
                      r2 = format(summary(m)$r.squared, digits = 3)))
    )
  )
}


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
        plot.caption = element_text(hjust = 0)) +
  geom_text(x = 2, y = 300, label = eq(covid_limpio$total_cases_per_million,
                                       covid_limpio$total_deaths_per_million),
            parse = TRUE)

### segundo intento

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
        plot.caption = element_text(hjust = 0)) +
  stat_regline_equation(label.y = 6000, show.legend = NULL, label.x = 600000,
                        aes(group = 1, label = ..eq.label..)) +
  stat_regline_equation(label.y = 5700, show.legend = NULL, label.x = 600000,
                        aes(group = 1, label = ..rr.label..))
