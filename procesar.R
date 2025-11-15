library(dplyr)
library(stringr)
library(purrr)
library(arrow)
library(lubridate)
library(slider)
library(furrr)
library(future)
library(beepr)

plan(multisession, workers = 6)


# cargar datos ----
# datos alojados en un disco duro externo, no son públicos
datos_prensa <- read_parquet("~/R/prensa/datos/prensa_datos.parquet")
resumen <- read_parquet("~/R/prensa/datos/prensa_llm_resumen.parquet")


## preparar datos de prensa ----
datos_prensa_filt <- datos_prensa |> 
  filter(año >= 2018)

datos_prensa_filt_split <- datos_prensa_filt |> 
  mutate(grupos = (row_number()-1) %/% (n()/6)) |> # n grupos de igual cantidad de filas
  group_split(grupos)


# detección de temas ----
palabras_delincuencia = c("homicid",
                          "asesin",
                          "hurt", "robo\\b",
                          "asalto|asalta",
                          "lanzazo",
                          "arma.*fuego", "pistola",
                          "secuestr",
                          "violaci|violad",
                          "delito", "delincuen", "delict", 
                          "crimen|crimin",
                          "narco", "droga",
                          "saqueo|saquea",
                          "portonazo", "turba")

palabras_homicidios = c("homicid",
                        "asesin", 
                        "balead",
                        "(arma|bala|cuchill|punzante|acribill).*(fallec|muere|muert|murió)", 
                        "(fallec|muere|muert|murió).*(arma|bala|cuchill|punzante|acribill)")

# # pruebas
# c("fallece por impacto de bala",
#   "fue acribillado y horas después muere",
#   "asesinado con un machete",
#   "le di covid y murió",
#   "fallece en un choque") |> 
# str_detect(str_flatten(palabras_homicidios, collapse = "|"))


## buscar palabras ----
datos_detect <- future_map(datos_prensa_filt_split, \(datos_parte) {
  # datos_parte <- datos_prensa_filt_split[[3]]
  datos_parte |> 
    # slice(1:100) |> 
    # detectar tema a partir de palabras presentes 
    mutate(tema_homicidios = str_detect(cuerpo_limpio, str_flatten(palabras_homicidios, collapse = "|")),
           tema_delincuencia = str_detect(cuerpo_limpio, str_flatten(palabras_delincuencia, collapse = "|"))) |>
    # cantidad de palabras presentes
    mutate(n_term = ifelse(tema_homicidios | tema_delincuencia,
                           str_count(cuerpo, str_flatten(c(palabras_delincuencia, palabras_homicidios), collapse = "|")),
                           NA))
}); beep()

## revisar noticias clasificadas
# entrega una cantidad de noticias al azar con sus resúmenes
# luego se puede contrastar con su texto completo

# # obtener resúmenes de noticias
# datos_detect |> 
#   list_rbind() |> 
#   filter(tema_homicidios) |>
#   filter(n_term >= 3) |> 
#   left_join(resumen, join_by(id)) |> 
#   filter(!is.na(resumen)) |> 
#   slice_sample(n = 8) |> 
#   select(resumen)
# 
# # revisar texto de noticias
# datos_prensa |> 
#   filter(id == "00ed98e3fb5776d55f6602790cd804b1") |> 
#   pull(cuerpo) |> 
#   str_extract_all(str_flatten(palabras_delincuencia, collapse = "|"))


# bases de conteo ----
noticias_delincuencia <- datos_detect |> 
  list_rbind() |> 
  filter(tema_delincuencia) |> 
  filter(n_term >= 3) |> 
  select(fecha, titulo, año, id) |> 
  left_join(resumen |> select(id, resumen), join_by(id))

noticias_homicidios <- datos_detect |> 
  list_rbind() |> 
  filter(tema_homicidios) |>
  filter(n_term > 3) |> 
  select(fecha, titulo, año, id) |> 
  left_join(resumen |> select(id, resumen), join_by(id))

# total de noticias por fecha
noticias_totales <- datos_detect |> 
  list_rbind() |> 
  group_by(fecha) |> 
  summarize(total = n())

# para calcular porcentajes
noticias_totales



# datos de casos ----

### datos homicidios ----
# https://prevenciondehomicidios.cl/#informes

homicidios_consumados <- readxl::read_xlsx("datos/Base-VHC_2018_PS2025.xlsx") |> 
  janitor::clean_names()

homicidios_consumados_n <- homicidios_consumados |> 
  mutate(fecha = dmy(paste(15, mes2, id_ano))) |> 
  group_by(fecha) |> 
  summarize(n = n())

homicidios_consumados_n |> pull(fecha) |> max()


### datos delincuencia ----

# # descargar
# # https://github.com/bastianolea/delincuencia_chile/blob/main/datos_procesados/cead_delincuencia_chile.parquet
# download.file("https://github.com/bastianolea/delincuencia_chile/raw/main/datos_procesados/cead_delincuencia_chile.parquet",
#               destfile = "datos/cead_delincuencia_chile.parquet")

delincuencia_chile <- arrow::read_parquet("datos/cead_delincuencia_chile.parquet")

delincuencia_chile |> pull(fecha) |> max()

delincuencia_chile_n <- delincuencia_chile |> 
  # delitos de mayor connotación social
  filter(delito %in% c(
    "Homicidios",
    "Violencia intrafamiliar",
    "Robos con violencia o intimidación",
    "Robo violento de vehículo motorizado",
    "Robo en lugar habitado",
    "Robos en lugar no habitado",
    "Robo por sorpresa",
    "Robo frustrado",
    "Hurtos",
    "Robo de vehículo motorizado",
    "Otros robos con fuerza en las cosas",
    "Lesiones graves o gravísimas",
    "Lesiones menos graves",
    "Lesiones leves",
    "Homicidios",
    "Femicidios",
    "Violaciones")) |>
  group_by(fecha) |> 
  summarize(n = sum(delito_n))



