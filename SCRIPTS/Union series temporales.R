library(tidyverse)
library(pxR)
library(stargazer)
#TASAS DE ACTIVIDAD DESDE 1987 A 2021 ----
#CARGAMOS DATOS DESDE EL 1987 A 1995 Y LA SERIE DE 1996 A 2001
act.87_95 <- read.px("DATOS/actividad.px/02002.px") %>% as.tibble() 
act.96_01 <- read.px("DATOS/actividad.px/02002-2.px") %>% as.tibble() 

#JUNTAMOS AMBAS SERIES CON FULL_JOIN
act.87_01 <- act.87_95 %>% full_join(act.96_01)

#SEPARAMOS LAS LETRAS DE LOS TRIMESTRES DE LOS AÑOS
act.87_01 <- act.87_01 %>% separate(Periodo, into = c("año", "trimestre"), sep = 4)

#CON LA FUNCION CASE_WHEN RECODIFICAMOS LOS TRIMESTRES
act.87_01 <- act.87_01 %>% mutate(trimestre = case_when(
  trimestre == "TI" ~ "-03-01",
  trimestre == "TII" ~ "-06-01",
  trimestre == "TIII" ~ "-09-01",
  trimestre == "TIV" ~ "-12-01")) %>% 
  
  #REUNIMOS LOS TRIMESTRES CON SUS AÑOS Y ORDENAMOS POR AÑO
  unite(periodo,c(1:2),  sep = "", remove = TRUE) %>% arrange(periodo)

#RECODIFICAMOS LOS NOMBRES DE LOS SEXOS 
act.87_01 <- act.87_01 %>% mutate(Sexo = case_when(
  Sexo == "total" ~ "Ambos sexos",
  Sexo == "varones" ~ "Hombres",
  Sexo == "mujeres" ~ "Mujeres"))
names(act.87_01)[names(act.87_01) == "Grupos.de.edad..años."] <- "Edad"

#RECODIFICAMOS LOS NOMBRES DE LOS GRUPOS DE EDAD
act.87_01 <- act.87_01 %>% mutate(Edad = case_when(
  Edad == "total" ~ "Total", 
  Edad == "de 16 a 19" ~ "De 16 a 19 años", 
  Edad == "de 20 a 24" ~ "De 20 a 24 años", 
  Edad == "de 25 a 29" ~ "De 25 a 29 años", 
  Edad == "de 30 a 34" ~ "De 30 a 34 años", 
  Edad == "de 35 a 39" ~ "De 35 a 39 años", 
  Edad == "de 40 a 44" ~ "De 40 a 44 años", 
  Edad == "de 45 a 49" ~ "De 45 a 49 años", 
  Edad == "de 50 a 54" ~ "De 50 a 54 años", 
  Edad == "de 55 a 59" ~ "De 55 a 59 años", 
  Edad == "de 60 a 64" ~ "De 60 a 64 años", 
  Edad == "de 65 a 69" ~ "De 65 a 69 años", 
  Edad == "de 70 y más" ~ "70 y más años"))

#CARGAMOS LOS DATOS DEL 2002 AL 2021
act.02_21 <- read.px("DATOS/actividad.px/4050.px") %>% as.tibble()
act.02_21 <- act.02_21 %>% separate(Periodo, into = c("año", "trimestre"), sep = 4)
act.02_21 <- act.02_21 %>% mutate(trimestre = case_when(
  trimestre == "T1" ~ "-03-01",
  trimestre == "T2" ~ "-06-01",
  trimestre == "T3" ~ "-09-01",
  trimestre == "T4" ~ "-12-01")) %>% 
  unite(periodo,c(1:2),  sep = "", remove = TRUE) %>% arrange(periodo)

#UNIMOS TODA LA SERIE TEMPORAL
actividad <- act.87_01 %>% full_join(act.02_21)

save(actividad, file = "actividad.RData")
rm(list = ls())


#TASAS DE PARO DESDE 1987 A 2021 ----
#CARGAMOS DATOS DESDE EL 1987 A 1995 Y LA SERIE DE 1996 A 2001
paro.87_95 <- read.px("DATOS/Paro.px/04002.px") %>% as.tibble() 
paro.96_01 <- read.px("DATOS/Paro.px//04002-2.px") %>% as.tibble() 

#JUNTAMOS AMBAS SERIES CON FULL_JOIN
paro.87_01 <- paro.87_95 %>% full_join(paro.96_01)

#SEPARAMOS LAS LETRAS DE LOS TRIMESTRES DE LOS AÑOS
paro.87_01 <- paro.87_01 %>% separate(Periodo, into = c("año", "trimestre"), sep = 4)

#CON LA FUNCION CASE_WHEN RECODIFICAMOS LOS TRIMESTRES
paro.87_01 <- paro.87_01 %>% mutate(trimestre = case_when(
  trimestre == "TI" ~ "-03-01",
  trimestre == "TII" ~ "-06-01",
  trimestre == "TIII" ~ "-09-01",
  trimestre == "TIV" ~ "-12-01")) %>% 
  
  #REUNIMOS LOS TRIMESTRES CON SUS AÑOS Y ORDENAMOS POR AÑO
  unite(periodo,c(1:2),  sep = "", remove = TRUE) %>% arrange(periodo)

#RECODIFICAMOS LOS NOMBRES DE LOS SEXOS 
paro.87_01 <- paro.87_01 %>% mutate(Sexo = case_when(
  Sexo == "ambos sexos" ~ "Ambos sexos",
  Sexo == "varones" ~ "Hombres",
  Sexo == "mujeres" ~ "Mujeres"))
names(paro.87_01)[names(paro.87_01) == "Grupos.de.edad...años."] <- "Edad"

#RECODIFICAMOS LOS NOMBRES DE LOS GRUPOS DE EDAD
paro.87_01 <- paro.87_01 %>% mutate(Edad = case_when(
  Edad == "total" ~ "Total", 
  Edad == "de 16 a 19" ~ "De 16 a 19 años", 
  Edad == "de 20 a 24" ~ "De 20 a 24 años", 
  Edad == "de 25 a 29" ~ "De 25 a 29 años", 
  Edad == "de 30 a 34" ~ "De 30 a 34 años", 
  Edad == "de 35 a 39" ~ "De 35 a 39 años", 
  Edad == "de 40 a 44" ~ "De 40 a 44 años", 
  Edad == "de 45 a 49" ~ "De 45 a 49 años", 
  Edad == "de 50 a 54" ~ "De 50 a 54 años", 
  Edad == "de 55 a 59" ~ "De 55 a 59 años", 
  Edad == "de 60 a 64" ~ "De 60 a 64 años", 
  Edad == "de 65 a 69" ~ "De 65 a 69 años", 
  Edad == "de 70 y más" ~ "70 y más años"))

#CARGAMOS LOS DATOS DEL 2002 AL 2021
paro.02_21 <- read.px("DATOS/Paro.px/4086.px") %>% as.tibble()
paro.02_21 <- paro.02_21 %>% separate(Periodo, into = c("año", "trimestre"), sep = 4)
paro.02_21 <- paro.02_21 %>% mutate(trimestre = case_when(
  trimestre == "T1" ~ "-03-01",
  trimestre == "T2" ~ "-06-01",
  trimestre == "T3" ~ "-09-01",
  trimestre == "T4" ~ "-12-01")) %>% 
  unite(periodo,c(1:2),  sep = "", remove = TRUE) %>% arrange(periodo)

#UNIMOS TODA LA SERIE TEMPORAL
paro <- paro.87_01 %>% full_join(paro.02_21)
save(paro, file = "Desempleo.RData")

rm(list = ls())




