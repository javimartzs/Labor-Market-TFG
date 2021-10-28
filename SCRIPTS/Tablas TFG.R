library(tidyverse)
library(pxR)
library(knitr)
library(kableExtra)

#ACTIVOS EN VALOR ABSOLUTO POR GRUPOS DE EDAD 04-07-10-13-16-19 ----
activos <- read.px("DATOS/Activos totales.px") %>% as.tibble()

activos <- activos %>% separate(Periodo, into = c("año", "trimestre"), sep = 4) %>% 
  filter(Sexo == "Ambos sexos", Unidad == "Valor absoluto",trimestre == "T4", año %in% c("2004","2007", "2010", "2013", "2016", "2019" )) %>%
  arrange(año) %>% 
  pivot_wider(names_from = Edad, values_from = value)

activos <- activos %>%
  mutate("16 a 19 años" = apply(activos[ , 6], 1, sum),
         "20 a 24 años" = apply(activos[ , 7], 1, sum),
         "25 a 29 años" = apply(activos[ , 8], 1, sum),
         "30 a 39 años" = apply(activos[ , 9:10], 1, sum),
         "40 a 49 años" = apply(activos[ , 11:12], 1, sum),
         "50 a 59 años" = apply(activos[ , 13:14], 1, sum),
         "60 o mas" = apply(activos[ , 15:17], 1, sum)) %>% 
  select(1:5,18:24)
activos$Unidad <- NULL
activos$Sexo <- NULL
activos$trimestre <- NULL

activos <- activos %>% pivot_longer(cols = 2:9, names_to = "edad", values_to = "value")
activos <- activos %>% pivot_wider(names_from = año, values_from = value)
activos <- activos%>% arrange(edad) %>% remove_rownames %>% column_to_rownames(var="edad")

#CREAMOS LA TABLA CON EL PAQUETE KNITRY LA EDITAMOS CON KABLEEXTRA 
activos %>%
  kable(booktabs = TRUE,format = "latex", align = "c") %>%
  kable_styling(latex_options = c("striped", "condensed","hold_position"),
                position = "center",full_width = T) %>% 
  footnote(general = "Encuesta de Población Activa (EPA)", general_title = "Source:",
           number = c("Cifras en miles de personas", "Los datos hacen referencia al T4 de cada año"),
           footnote_as_chunk = T) %>% 
  column_spec(1, "2.5cm") %>% column_spec(2:7, "1.5cm") %>% 
  row_spec(7, hline_after = T) %>% as_image(file = "poblacion activa.png")
