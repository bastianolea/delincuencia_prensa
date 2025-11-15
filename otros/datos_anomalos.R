# revisión de datos por la punta que se ve en marzo de 2023

noticias_homicidios |> 
  filter(year(fecha) == 2023) |> 
  filter(month(fecha) >= 3) |>
  filter(month(fecha) <= 4) |>
  ggplot() +
  aes(x = fecha) +
  geom_histogram()

noticias_homicidios |> 
  filter(year(fecha) == 2023) |> 
  filter(month(fecha) == 3) |> 
  filter(day(fecha) > 26) |> 
  filter(day(fecha) < 30) |> 
  select(resumen) |> 
  print(n=Inf)

# fue por el asesinato de carabineros