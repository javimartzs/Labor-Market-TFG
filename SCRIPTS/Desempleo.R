library(tidyverse)
library(pxR) 
library(knitr)
library(TTR)
library(kableExtra)

#DESEMPLEADOS EN VALOR ABSOLUTO POR GRUPOS DE EDAD 04-07-10-13-16-19 ----
paro <- read.px("DATOS/INE:AIREF/desempleo.px") %>% as.tibble()

paro <- paro %>% separate(Periodo, into = c("año", "trimestre"), sep = 4) %>% 
  filter(Sexo == "Ambos sexos", trimestre == "T4", año %in% c("2004","2007", "2010", "2013", "2016", "2019" )) %>%
  arrange(año) %>% 
  pivot_wider(names_from = Edad, values_from = value)

paro <- paro %>%
  mutate("16 - 19" = apply(paro[ , 6], 1, sum),
         "20 - 24" = apply(paro[ , 7], 1, sum),
         "25 - 29" = apply(paro[ , 8], 1, sum),
         "30 - 39" = apply(paro[ , 9:10], 1, sum),
         "40 - 49" = apply(paro[ , 11:12], 1, sum),
         "50 - 59" = apply(paro[ , 13:14], 1, sum),
         "60 o mas" = apply(paro[ , 15:17], 1, sum)) %>% 
  select(1:5,18:24)
paro$Unidad <- NULL
paro$Sexo <- NULL

paro <- paro %>% pivot_longer(cols = 3:10, names_to = "edad", values_to = "value")
paro <- paro %>% pivot_wider(names_from = año, values_from = value)
paro$trimestre <- NULL
paro <- paro%>% arrange(edad) %>% remove_rownames %>% column_to_rownames(var="edad")

#DESEMPLEADOS EN VALOR ABSOLUTO POR SEXO 04-07-10-13-16-19 ----
paro.sex <- read.px("DATOS/INE:AIREF/desempleo.px") %>% as.tibble()

paro.sex <- paro.sex %>% separate(Periodo, into = c("año", "trimestre"), sep = 4) %>% 
  filter(Sexo != "Ambos sexos", Edad == "Total", trimestre == "T4", año %in% c("2004","2007", "2010", "2013", "2016", "2019" )) %>%
  arrange(año) %>% 
  pivot_wider(names_from = año, values_from = value)

paro.sex$trimestre <- NULL
paro.sex$Unidad <- NULL
paro.sex$Edad <- NULL

paro.sex <- paro.sex%>% remove_rownames %>% column_to_rownames(var="Sexo")


#TASA DESEMPLEO POR GRUPOS DE EDAD ----
tasa.paro <- read.px("DATOS/INE:AIREF/Tasa de paro.px") %>% as.tibble()
tasa.paro <- tasa.paro %>% separate(Periodo, into = c("año", "trimestre"), sep = 4) %>% 
  filter(trimestre == "T4", año %in% c("2004","2007", "2010", "2013", "2016", "2019" )) %>% 
  arrange(año)

tasa.paro <- tasa.paro %>% pivot_wider(names_from = Edad, values_from = value)
tasa.paro <- tasa.paro %>%
  mutate("16 a 19 años" = round(apply(tasa.paro[ , 5], 1, mean),2),
         "20 a 24 años" = round(apply(tasa.paro[ , 6], 1, mean),2),
         "25 a 29 años" = round(apply(tasa.paro[ , 7], 1, mean),2),
         "30 a 39 años" = round(apply(tasa.paro[ , 8:9], 1, mean),2),
         "40 a 49 años" = round(apply(tasa.paro[ , 10:11], 1, mean),2),
         "50 a 59 años" = round(apply(tasa.paro[ , 12:13], 1, mean),2),
         "60 o mas" = round(apply(tasa.paro[ , 14:15], 1, mean),2)) %>% 
  select(1:4,17:23) %>% 
  pivot_longer(cols = 4:11, names_to = "Edad por Sexo", values_to = "value")
tasa.paro <- tasa.paro %>% pivot_wider(names_from = año, values_from = value)
tasa.paro$trimestre <- NULL
tasa.paro <- tasa.paro %>% arrange(`Edad por Sexo`)
tasa.paro <- tasa.paro %>% arrange(Sexo)
