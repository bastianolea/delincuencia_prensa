library(ggplot2)
library(patchwork)
library(scales)
library(tidyr)
library(ggview)


# gráficos ----
m_movil <- 7*3

## temas ----

ancho <- .3
ancho_reg <- .6
color <- list(delitos = "#D96621" |> col_lighter(5),
              noticias = "#179992")
color_reg <- list(delitos = "#D96621" |> col_lighter(35) |> col_saturate(30),
                  noticias = "#179992" |> col_lighter(35) |> col_saturate(-5))
  
theme_set(
  theme_linedraw() +
    theme(text = element_text(family = "Rubik"),
          axis.title = element_text(size = 9),
          plot.title = element_text(face = "bold", size = 14),
          plot.subtitle = element_text(size = 10),
          plot.caption = element_text(color = "grey70", size = 8),
          panel.border = element_rect(color = "grey60"),
          axis.ticks = element_line(color = "grey50"),
          panel.grid = element_line(color = "grey50"),
          panel.background = element_rect(fill = "gray98"),
          plot.margin = unit(c(10,5,5,5), "pt")
    )
)


## noticias ----

### gráfico noticias homicidios ----
# noticias_homicidios_conteo <- noticias_homicidios |> 
#   group_by(fecha) |> 
#   summarize(n = n()) |> 
#   complete(fecha = seq.Date(min(noticias_homicidios$fecha), 
#                             max(noticias_homicidios$fecha), by="days"),
#            fill = list(n = 0)) |> 
#   arrange(fecha) |> 
#   mutate(n2 = slide_dbl(n, .before = 14, mean))

noticias_homicidios_porcentaje <- noticias_homicidios |>
  group_by(fecha) |>
  summarize(n = n()) |>
  complete(fecha = seq.Date(min(noticias_homicidios$fecha),
                            max(noticias_homicidios$fecha), by="days"),
           fill = list(n = 0)) |>
  left_join(noticias_totales) |> 
  arrange(fecha) |>
  mutate(n2 = slide_dbl(n, .before = m_movil, mean),
         total2 = slide_dbl(total, .before = m_movil, mean)) |> 
  group_by(fecha) |>
  mutate(p = n2/total2)

g_noticias_homicidios <- noticias_homicidios_porcentaje |> 
  ggplot() +
  aes(fecha, p) +
  geom_smooth(method = "lm", se = T, 
              color = color_reg$noticias, alpha = 0.1, lwd = ancho_reg) +
  geom_line(lwd = ancho-0.1, color = color$noticias) +
  labs(title = "Noticias sobre homicidios",
       subtitle = "Detección de conceptos en prensa escrita digital",
       y = "% noticias diarias sobre homicidios",
       caption = "Fuente: web scraping de prensa digital") +
  scale_y_continuous(limits = c(0, NA), labels = label_percent(accuracy = 0.1, drop0trailing = T)) +
  theme(plot.margin = unit(c(0,10,0,20), "pt"))

# g_noticias_homicidios


### gráfico noticias delincuencia ----
# noticias_delincuencia_conteo <- noticias_delincuencia |> 
#   group_by(fecha) |> 
#   summarize(n = n()) |> 
#   complete(fecha = seq.Date(min(noticias_delincuencia$fecha), max(noticias_delincuencia$fecha), by="days"),
#            fill = list(n = 0)) |> 
#   arrange(fecha) |> 
#   mutate(n2 = slide_dbl(n, .before = 14, mean))

noticias_delincuencia_porcentaje <- noticias_delincuencia |>
  group_by(fecha) |>
  summarize(n = n()) |>
  complete(fecha = seq.Date(min(noticias_delincuencia$fecha), max(noticias_delincuencia$fecha), by="days"),
           fill = list(n = 0)) |>
  left_join(noticias_totales) |> 
  arrange(fecha) |>
  mutate(n2 = slide_dbl(n, .before = m_movil, mean),
         total2 = slide_dbl(total, .before = m_movil, mean)) |> 
  group_by(fecha) |>
  mutate(p = n2/total2)


g_noticias_delincuencia <- noticias_delincuencia_porcentaje |> 
  ggplot() +
  aes(fecha, p) +
  geom_smooth(method = "lm", se = T, color = color_reg$noticias, alpha = 0.1, lwd = ancho_reg) +
  geom_line(lwd = ancho-0.1, color = color$noticias) +
  labs(title = "Noticias sobre delincuencia",
       subtitle = "Detección de conceptos en prensa escrita digital",
       y = "% noticias diarias sobre delincuencia",
       caption = "Fuente: web scraping de prensa digital") +
  scale_y_continuous(limits = c(0, NA), labels = label_percent(accuracy = 0.1, drop0trailing = T)) +
  theme(plot.margin = unit(c(0,10,0,20), "pt"))

# g_noticias_delincuencia


## casos ----
### gráfico casos homicidios ----
g_casos_homicidios <- homicidios_consumados_n |> 
  ggplot() +
  aes(fecha, n) +
  geom_smooth(method = "lm", se = T, color = color_reg$delitos, fill = color_reg$delitos, alpha = 0.15, lwd = ancho_reg) +
  geom_line(lwd = ancho, color = color$delitos) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(c(0.05, 0.15)),) +
  labs(y = "víctimas de homicidios",
       title = "Casos de homicidios",
       subtitle = "Homicidios consumados con validación interinstitucional",
       caption = "Fuente: Observatorio de Homicidios, Ministerio de Seguridad Pública")

# g_casos_homicidios


###  gráfico casos delincuencia ----

g_casos_delincuencia <- delincuencia_chile_n |> 
  ggplot() +
  aes(fecha, n) +
  geom_smooth(method = "lm", se = T, color = color_reg$delitos, fill = color_reg$delitos, alpha = 0.15, lwd = .7) +
  geom_line(lwd = ancho, color = color$delitos) +
  scale_y_continuous(limits = c(0, NA), 
                     expand = expansion(c(0.05, 0.15)),
                     labels = label_comma(big.mark = ".")) +
  labs(y = "casos policiales mensuales",
       title = "Casos de delincuencia",
       subtitle = "Casos policiales de mayor connotación social",
       caption = "Fuente: Centro de Estudios y Análisis del Delito")

# g_casos_delincuencia


## unir gráficos ----
# g_noticias_delincuencia
# g_casos_delincuencia
# 
# g_noticias_homicidios
# g_casos_homicidios


## delincuencia
gs_delincuencia <- g_casos_delincuencia + g_noticias_delincuencia &
  scale_x_date(date_breaks = "years", date_labels = "%Y") &
  labs(x = NULL)

## homicidios
gs_homicidios <- g_casos_homicidios + g_noticias_homicidios &
  scale_x_date(date_breaks = "years", date_labels = "%Y") &
  labs(x = NULL) &
  canvas(8, 4)

library(ggtext)
library(glue)

titulo_conjunto <- plot_annotation(title = glue("Comparación entre <span style='color:{color$delitos};'>casos de delincuencia</span> en Chile<br>versus la <span style='color:{color$noticias};'>cobertura comunicacional</span> de la delincuencia"),
                                   theme = theme(plot.title = element_markdown(size = 18, face = "plain", margin = margin(b = 10))))

fuente_conjunta <- plot_annotation(caption = "Análisis de datos y visualización desarrollada por Bastián Olea Herrera",
                                   theme = theme(plot.caption = element_text(size = 8, face = "italic")))

# cuadrícula con los 4 gráficos
gs_delincuencia / gs_homicidios &
  scale_x_date(date_breaks = "years", date_labels = "%Y") &
  labs(x = NULL) &
  titulo_conjunto &
  fuente_conjunta &
  canvas(8.5, 8)

# guardar
save_ggplot(plot = last_plot(),
            file = "grafico_delincuencia_prensa.png")
