library(tidyverse)
library(pxR)
library(kableExtra)
library(knitr)

actividad <- read_csv("DATOS/eurostat/activity rates.csv",  na = ":", col_types = cols(Value = col_number()))
names(actividad)[names(actividad) == "Value"] <- "actividad"

empleo <- read_csv("DATOS/eurostat/employment rates.csv",na = ":", col_types = cols(Value = col_number()))
names(empleo)[names(empleo) == "Value"] <- "empleo"

temporalidad <- read_csv("DATOS/eurostat/temporal rates.csv",na = ":", col_types = cols(Value = col_number()))
names(temporalidad)[names(temporalidad) == "Value"] <- "temporalidad"

desempleo <- read_csv("DATOS/eurostat/unemployment rates.csv",na = ":", col_types = cols(Value = col_number()))
names(desempleo)[names(desempleo) == "Value"] <- "desempleo"


eurostat <- actividad %>% left_join(empleo)
eurostat <- eurostat %>% left_join(temporalidad)
eurostat <- eurostat %>% left_join(desempleo)
eurostat$GEO <- as.character(eurostat$GEO)

rm(actividad, empleo, temporalidad, desempleo)

eurostat <- eurostat %>% filter(TIME != "2020")
eurostat$GEO[eurostat$GEO == "Germany (until 1990 former territory of the FRG)"] <- "Germany"
eurostat$SEX <- NULL
eurostat$CITIZEN <- NULL
eurostat$AGE <- NULL
eurostat$UNIT <- NULL

eurostat <- eurostat %>% pivot_longer(cols = 3:6, names_to = "variables", values_to = "value")
eurostat <- eurostat %>% pivot_wider(names_from = TIME, values_from = value)


eurostat <- eurostat %>% mutate("Media" = round(apply(eurostat[ , 3:20], 1, mean),2), na.rm,
                                "Desviación tipica" = round(apply(eurostat[ , 3:20], 1, sd),2)) %>% 
  select(1:2, 21:22) %>% 
  pivot_longer(cols = 3:4, names_to = "Medidas", values_to = "value") %>% 
  pivot_wider(names_from = variables, values_from = value)


eurostat <- eurostat %>% arrange(desc(Medidas))

eurostat <- eurostat %>% pivot_longer(cols = 3:6, names_to = "variable", values_to = "value")
eurostat <- eurostat %>% pivot_wider(names_from = GEO, values_from=value)

eurostat <- eurostat %>% select(Medidas,variable,Spain,
                                 Germany,France,Italy,Portugal,Finland,`United Kingdom`)

eurostat <- eurostat %>% mutate("Media sin España" = round(apply(eurostat[ ,4:9], 1, mean), digits = 2))

eurostat <- eurostat %>% pivot_longer(cols = 3:10, names_to="geo", values_to="value")
eurostat <- eurostat %>% pivot_wider(names_from = variable, values_from=value)

eurostat$geo[eurostat$geo == "Spain"] <- "España"
eurostat$geo[eurostat$geo == "Germany"] <- "Alemania"
eurostat$geo[eurostat$geo == "France"] <- "Francia"
eurostat$geo[eurostat$geo == "Italy"] <- "Italia"
eurostat$geo[eurostat$geo == "Finland"] <- "Finlandia"
eurostat$geo[eurostat$geo == "United Kingdom"] <- "Reino Unido"


eurostat1 <- eurostat %>% pivot_longer(cols = 3:6, names_to = "variable", values_to = "value")
eurostat1 <- eurostat1 %>% pivot_wider(names_from = Medidas, values_from = value)
eurostat1 <- eurostat1 %>% mutate("CV" = round(`Desviación tipica`/Media, digits = 3)) %>% 
  select(1:2, 5) %>% pivot_wider(names_from = variable, values_from = CV)


kable(eurostat1, booktabs = TRUE, format = "latex", align = "c",
      col.names = c("", "Actividad", "Empleo", "Temporalidad", "Desempleo")) %>% 
  row_spec(7, hline_after = T) %>% 
  footnote(general = "Eurostat", general_title = "Source:",
           number = c("Cifras en porcentaje"),
           footnote_as_chunk = T) %>% 
  as_image(file = "coeficiente variacion.png", width = )





kable(eurostat[,2:6], booktabs = TRUE, format = "latex", align = "c",
      col.names = c("", "Actividad", "Empleo", "Temporalidad", "Desempleo")) %>% 
  pack_rows("Media", 1, 8) %>%
  pack_rows("Desviación típica", 9, 16) %>%
  footnote(general = "Eurostat", general_title = "Source:",
           number = c("Cifras en porcentaje"),
           footnote_as_chunk = T) %>% 
  row_spec(7, hline_after = T) %>%
  row_spec(8, hline_after = T) %>% 
  row_spec(15, hline_after = T) %>% 
  as_image(file = "peperoni.png", width = )


