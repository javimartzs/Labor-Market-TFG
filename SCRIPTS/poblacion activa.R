library(tidyverse)
library(pxR)
library(knitr)
library(TTR)
library(kableExtra)

#ACTIVOS EN VALOR ABSOLUTO POR GRUPOS DE EDAD 04-07-10-13-16-19 ----
activos <- read.px("DATOS/INE:AIREF/Activos.px") %>% as.tibble()

activos <- activos %>% separate(Periodo, into = c("año", "trimestre"), sep = 4) %>% 
  filter(Sexo == "Ambos sexos", trimestre == "T4", año %in% c("2004","2007", "2010", "2013", "2016", "2019" )) %>%
  arrange(año) %>% 
  pivot_wider(names_from = Edad, values_from = value)

activos <- activos %>%
  mutate("16 - 19" = apply(activos[ , 6], 1, sum),
         "20 - 24" = apply(activos[ , 7], 1, sum),
         "25 - 29" = apply(activos[ , 8], 1, sum),
         "30 - 39" = apply(activos[ , 9:10], 1, sum),
         "40 - 49" = apply(activos[ , 11:12], 1, sum),
         "50 - 59" = apply(activos[ , 13:14], 1, sum),
         "60 o mas" = apply(activos[ , 15:17], 1, sum)) %>% 
  select(1:5,18:24)
activos$Unidad <- NULL
activos$Sexo <- NULL

activos <- activos %>% pivot_longer(cols = 3:10, names_to = "edad", values_to = "value")
activos <- activos %>% pivot_wider(names_from = año, values_from = value)
activos$trimestre <- NULL
activos <- activos%>% arrange(edad) %>% remove_rownames %>% column_to_rownames(var="edad")





#ACTIVOS EN VALOR ABSOLUTO POR SEXO 04-07-10-13-16-19 ----
activos.sex <- read.px("DATOS/INE:AIREF/Activos.px") %>% as.tibble()

activos.sex <- activos.sex %>% separate(Periodo, into = c("año", "trimestre"), sep = 4) %>% 
  filter(Sexo != "Ambos sexos", Edad == "Total", trimestre == "T4", año %in% c("2004","2007", "2010", "2013", "2016", "2019" )) %>%
  arrange(año) %>% 
  pivot_wider(names_from = año, values_from = value)

activos.sex$trimestre <- NULL
activos.sex$Unidad <- NULL
activos.sex$Edad <- NULL

activos.sex <- activos.sex%>% remove_rownames %>% column_to_rownames(var="Sexo")
#ACTIVOS POR GENERO EN TASAS DE ACTIVIDAD ----
tasa.sex <- read.px("DATOS/INE:AIREF/tasa.actividad.px") %>% as.tibble()

tasa.sex <- tasa.sex %>% separate(Periodo, into = c("año", "trimestre"), sep = 4) %>% 
  filter(Edad == "Total", trimestre == "T4", año %in% c("2004","2007", "2010", "2013", "2016", "2019")) %>%
  mutate(Sexo = case_when(Sexo == "Ambos sexos" ~ "Total",
                          Sexo == "Hombres" ~ "Hombres",
                          Sexo == "Mujeres" ~ "Mujeres")) %>% 
  arrange(año) %>% 
  pivot_wider(names_from = año, values_from = value)

tasa.sex$trimestre <- NULL
tasa.sex$Edad <- NULL

tasa.sex <- tasa.sex%>% remove_rownames %>% column_to_rownames(var="Sexo")