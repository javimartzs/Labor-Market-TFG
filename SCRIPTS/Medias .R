library(tidyverse)
library(pxR)
library(TTR)
library(kableExtra)


#TABLA DE MEDIDAS DE POSICIÓN SERIE TEMPORAL DE DESEMPLEO ----
paro <- read.px("DATOS/Paro.px/4086.px") %>% as.tibble() %>% 
  filter(Edad != "Total")

paro <- paro %>% pivot_wider(names_from = Edad, values_from = value)
paro <- paro %>%
  mutate("De 16 a 29 años" = round(apply(paro[ , 3:5], 1, mean),2),
         "De 30 a 64 años" = round(apply(paro[ , 6:12], 1, mean),2)) %>% 
  select(1:2,15:16)
paro <- paro %>% pivot_longer(cols = 3:4, names_to = "edad", values_to = "value") 
paro <- paro %>% arrange(desc(Periodo))

paro <- paro %>% pivot_wider(names_from = Periodo, values_from = value) 
paro$"2021T1" <- NULL

paro <- paro %>%
  mutate("2002 - 2007" = round(apply(paro[ , 3:26], 1, mean),2),
         "2008 - 2013" = round(apply(paro[ , 27:50], 1, mean),2),
         "2014 - 2019" = round(apply(paro[ , 51:74], 1, mean),2),
         "2020"        = round(apply(paro[ , 75:78], 1, mean),2)) %>% 
  select(1:2,79:82)


kable(paro[1:6, 2:6], booktabs = TRUE,format = "html",
        caption = "Tasa de paro por grandes periodos", align = "c",
  col.names = c("", "2002-2007", "2008-2013", "2014-2019", "2020")) %>% 
  kable_styling(latex_options = c("striped", "condensed","hold_position"),
                position = "center",full_width = F) %>% 
  pack_rows("Ambos sexos", 1, 2) %>%
  pack_rows("Hombres", 3, 4) %>%
  pack_rows("Mujeres", 5, 6) %>%
  footnote(general = "Encuesta de Población Activa (EPA)", general_title = "Source:",
           number = c("Cifras en porcentaje", "Medias de los trimestres"),
           footnote_as_chunk = T)


#TABLA DE MEDIDAS DE POSICION SERIE TEMPORAL DE OCUPADOS ----
ocu <- read.px("DATOS/ocupados totales.px") %>% as.tibble() %>% 
  filter(Unidad == "Valor absoluto", Edad == "Total") %>% 
  arrange(desc(Periodo))

ocu$Unidad <- NULL
ocu$Edad <- NULL

ocu <- ocu %>% pivot_wider(names_from = Periodo, values_from = value)
ocu <- ocu %>%
  mutate("2002 - 2007" = round(apply(ocu[ , 2:25], 1, mean),2),
         "2008 - 2013" = round(apply(ocu[ , 26:49], 1, mean),2),
         "2014 - 2019" = round(apply(ocu[ , 5:73], 1, mean),2),
         "2020"        = round(apply(ocu[ , 74:77], 1, mean),2)) %>% select(1, 79:82)

ocu %>% kable(booktabs = TRUE,format = "html",
      caption = "Ocupados medios por periodos", align = "c",
      col.names = c("", "2002-2007", "2008-2013", "2014-2019", "2020")) %>% 
  kable_styling(latex_options = c("striped", "condensed","hold_position"),
                position = "center",full_width = F) %>% 
  footnote(general = "Encuesta de Población Activa (EPA)", general_title = "Source:",
           number = c("Cifras en porcentaje", "Medias de los trimestres"),
           footnote_as_chunk = T)

#TABLA DE MEDIDAS DE LOS ACTIVOS - OCUPADOS E INACTIVOS ----
activos <- read.px("DATOS/Activos totales.px") %>% as.tibble()
ocupados <- read.px("DATOS/ocupados totales.px") %>% as.tibble()
inactivos <- read.px("DATOS/Inactivos.px") %>% as.tibble()

names(activos)[names(activos) == "value"] <- "Activos"
names(ocupados)[names(ocupados) == "value"] <- "Ocupados"
names(inactivos)[names(inactivos) == "value"] <- "Inactivos"

todo <- activos %>% left_join(inactivos)
todo <- todo %>% left_join(ocupados)
rm(activos, ocupados, inactivos)

todo <- todo %>% filter(Unidad == "Valor absoluto", Sexo == "Ambos sexos", Edad == "Total") %>% arrange(desc(Periodo))
todo$Unidad <- NULL
todo$Sexo <- NULL
todo$Edad <- NULL


todo <- todo %>% separate(Periodo, into = c("año", "trimestre"), sep = 4) %>% filter(trimestre == "T4") %>% 
  filter(año %in% c("2002","2008","2014","2019","2020"))

todo <- todo %>% unite(Periodo,c(1:2),  sep = "", remove = TRUE) 

todo <- todo %>% 
  mutate(Activos = round(ROC(Activos, n = 1, type = "discrete")*100, digits = 2)) %>% 
  mutate(Inactivos = round(ROC(Inactivos, n = 1, type = "discrete")*100, digits = 2)) %>% 
  mutate(Ocupados = round(ROC(Ocupados, n = 1, type = "discrete")*100, digits = 2))


todo <- todo %>% pivot_longer(cols = 2:4, names_to = "variables", values_to = "value")
todo <- todo %>% pivot_wider(names_from = Periodo, values_from = value)
todo$`2002T4` <- NULL

todo %>% kable(booktabs = TRUE,format = "latex", align = "c",
              col.names = c("", "2002-2007", "2008-2013", "2014-2019", "2020")) %>% 
  kable_styling(latex_options = c("striped", "condensed","hold_position"),
                position = "center",full_width = T) %>% 
  column_spec(1, "2.5cm") %>% column_spec(2:5, "2cm") %>%
  footnote(general = "Encuesta de Población Activa (EPA)", general_title = "Source:",
           number = c("Cifras en porcentaje", "Medias trimestrales"),
           footnote_as_chunk = T) %>% as_image(file = "variaciones.png")





