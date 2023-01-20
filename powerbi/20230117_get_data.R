library(tidyverse)
library(lubridate)

types <- 'cccccccccccccccccccccccccccccccccc'
setwd("C:/Users/Usuario/Desktop/Power BI/23. PowerBI Bilbao/data")



files <- list.files(path = "C:/Users/Usuario/Desktop/Power BI/23. PowerBI Bilbao/data",
                    pattern = "*.csv")
df <- read_csv2 (files, col_types = types)


df %>% glimpse()
getwd()

###############################################################################3

df <- df %>% 
  select(year_month, colectivo_reducido, edad, edad_index, sexo, procedencia,
         municipio, municipio_nombre, comarca_nombre, contributiva, rgi, subsidio,
         total)

df <- df %>% 
  separate(year_month, into = c("año", "mes"), sep ="-")

df <- df %>% 
  mutate(
    fecha = ymd(paste(año, mes, "01"))
  )

df <- df %>%
  mutate(
    año = parse_factor(año),
    mes = parse_factor(mes),
    colectivo_reducido = parse_factor(colectivo_reducido),
    edad = parse_factor(edad),
    edad_index = parse_factor(edad_index),
    sexo = parse_factor(sexo),
    procedencia = parse_factor(procedencia),
    municipio_nombre = parse_factor(municipio_nombre),
    comarca_nombre = parse_factor(comarca_nombre),
    municipio = parse_integer(municipio),
    contributiva = parse_integer(contributiva),
    rgi = parse_integer(rgi),
    subsidio = parse_integer(subsidio),
    total = parse_integer(total),
    ayuda = ifelse( contributiva == 1 | rgi == 1 | subsidio == 1, 1,0)
  )


df <- df %>% 
  rename(demandantes = total)

df <- df %>% 
  group_by(fecha, colectivo_reducido, edad, edad_index, sexo, 
           procedencia,municipio,municipio_nombre,ayuda)%>%
  summarise(demandantes=sum(demandantes))

df <- df %>% 
  mutate(
    riesgo = ifelse(ayuda == 1 & colectivo_reducido == 'Paro registrado', 1 , 0)
  )

df <- df %>% 
  rename(EUSTAT = municipio)
