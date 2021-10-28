library(tidyverse)
library(pxR)
library(knitr)
library(TTR)
library(kableExtra)

#OCUPADOS EN VALOR ABSOLUTO POR GRUPOS DE EDAD 04-07-10-13-16-19 ----
ocupados <- read.px("DATOS/INE:AIREF/ocupados.px") %>% as.tibble()

ocupados <- ocupados %>% separate(Periodo, into = c("año", "trimestre"), sep = 4) %>% 
  filter(Sexo == "Ambos sexos", trimestre == "T4", año %in% c("2004","2007", "2010", "2013", "2016", "2019" )) %>%
  arrange(año) %>% 
  pivot_wider(names_from = Edad, values_from = value)

ocupados <- ocupados %>%
  mutate("16 - 19" = apply(ocupados[ , 6], 1, sum),
         "20 - 24" = apply(ocupados[ , 7], 1, sum),
         "25 - 29" = apply(ocupados[ , 8], 1, sum),
         "30 - 39" = apply(ocupados[ , 9:10], 1, sum),
         "40 - 49" = apply(ocupados[ , 11:12], 1, sum),
         "50 - 59" = apply(ocupados[ , 13:14], 1, sum),
         "60 o mas" = apply(ocupados[ , 15:17], 1, sum)) %>% 
  select(1:5,18:24)
ocupados$Unidad <- NULL
ocupados$Sexo <- NULL

ocupados <- ocupados %>% pivot_longer(cols = 3:10, names_to = "edad", values_to = "value")
ocupados <- ocupados %>% pivot_wider(names_from = año, values_from = value)
ocupados$trimestre <- NULL
ocupados <- ocupados%>% arrange(edad) %>% remove_rownames %>% column_to_rownames(var="edad")

#OCUPADOS EN VALOR ABSOLUTO POR SEXO 04-07-10-13-16-19 ----
ocupados.sex <- read.px("DATOS/INE:AIREF/ocupados.px") %>% as.tibble()

ocupados.sex <- ocupados.sex %>% separate(Periodo, into = c("año", "trimestre"), sep = 4) %>% 
  filter(Sexo != "Ambos sexos", Edad == "Total", trimestre == "T4", año %in% c("2004","2007", "2010", "2013", "2016", "2019" )) %>%
  arrange(año) %>% 
  pivot_wider(names_from = año, values_from = value)

ocupados.sex$trimestre <- NULL
ocupados.sex$Unidad <- NULL
ocupados.sex$Edad <- NULL

ocupados.sex <- ocupados.sex%>% remove_rownames %>% column_to_rownames(var="Sexo")

#OCUPADOS: CALCULO DE TASA DE TEMPORALIDAD POR SEXO 04-07-10-13-16-19 ----
temp <- read.px("DATOS/INE:AIREF/tipo contrato y jornada.px") %>% as.tibble()

temp <- temp %>% separate(Periodo, into = c("año", "trimestre"), sep = 4) %>% 
  filter(Tipo.de.jornada == "Total", trimestre == "T4", año %in% c("2004","2007", "2010", "2013", "2016", "2019"), 
         Tipo.de.contrato.o.relación.laboral %in% c("Total asalariados", "Asalariados con contrato temporal")) %>%
  mutate(Sexo = case_when(Sexo == "Ambos sexos" ~ "Total",
                          Sexo == "Hombres" ~ "Hombres",
                          Sexo == "Mujeres" ~ "Mujeres")) %>% 
  arrange(año)

temp$trimestre <- NULL
temp$Tipo.de.jornada <- NULL

temp <- temp %>% pivot_wider(names_from = Tipo.de.contrato.o.relación.laboral, values_from = value)
  
temp <- temp %>% 
  mutate(tasa.temporalidad = round((temp$`Asalariados con contrato temporal`/`Total asalariados`)*100, 2)) %>% 
  pivot_longer(cols = 3:5, names_to = "tipo.contrato", values_to = "value")
  
temp <- temp %>% pivot_wider(names_from = año, values_from = value) 
temp <- temp %>% filter(tipo.contrato == "tasa.temporalidad") %>% arrange(Sexo) %>% remove_rownames %>% column_to_rownames(var="Sexo")
temp$tipo.contrato <- NULL

#OCUPADOS: CALCULO DE TASA DE PARCALIDAD POR SEXO 04-07-10-13-16-19 ----
parc <- read.px("DATOS/INE:AIREF/tipo contrato y jornada.px") %>% as.tibble()

parc <- parc %>% separate(Periodo, into = c("año", "trimestre"), sep = 4) %>% 
  filter(Tipo.de.jornada %in% c("Jornada a tiempo parcial", "Total"), trimestre == "T4", año %in% c("2004","2007", "2010", "2013", "2016", "2019"), 
         Tipo.de.contrato.o.relación.laboral == "Total") %>%
  mutate(Sexo = case_when(Sexo == "Ambos sexos" ~ "Total",
                          Sexo == "Hombres" ~ "Hombres",
                          Sexo == "Mujeres" ~ "Mujeres")) %>% 
  arrange(año)
parc$trimestre <- NULL
parc$Tipo.de.contrato.o.relación.laboral <- NULL

parc <- parc %>% pivot_wider(names_from = Tipo.de.jornada, values_from = value)

parc <- parc %>% 
  mutate(tasa.parcialidad = round((parc$`Jornada a tiempo parcial`/Total)*100, 2)) %>% 
  pivot_longer(cols = 3:5, names_to = "tipo.jornada", values_to = "value")

parc <- parc %>% pivot_wider(names_from = año, values_from = value) 
parc <- parc %>% filter(tipo.jornada == "tasa.parcialidad") %>% arrange(Sexo) %>% remove_rownames %>% column_to_rownames(var="Sexo")
parc$tipo.jornada <- NULL

#OCUPADOS: VARIACION INTERANUAL 2019 Y 2020 POR RAMAS DE ACTIVIDAD ----
sector <- read.px("DATOS/INE:AIREF/ocupados x sectores.px") %>% as.tibble()

sector <- sector %>% separate(Periodo, into = c("año", "trimestre"), sep = 4) %>% 
  unite(año, c(1:2),  sep = "", remove = TRUE) %>% 
  arrange(año)

sector$Edad <- NULL
sector$Sexo <- NULL

sector <- sector %>% pivot_wider(names_from = Sector.económico, values_from = value) %>% 
  mutate(Agricultura = round(ROC(Agricultura, n = 4, type = "discrete")*100, 2),
         Industria = round(ROC(Industria, n = 4, type = "discrete")*100, 2),
         Construcción = round(ROC(Construcción, n = 4, type = "discrete")*100, 2),
         Servicios = round(ROC(Servicios, n = 4, type = "discrete")*100, 2),
         Total = round(ROC(Total, n = 4, type = "discrete")*100, 2))


sector <- sector %>% pivot_longer(cols = 2:6, names_to = "sector", values_to = "value")
sector <- na.omit(sector)
sector <- sector %>% pivot_wider(names_from = año, values_from = value) %>% arrange(sector) %>%  remove_rownames %>% column_to_rownames(var="sector")
