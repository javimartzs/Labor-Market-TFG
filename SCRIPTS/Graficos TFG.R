library(tidyverse)
library(TTR)
library(pxR)
library(viridis)
library(viridisLite)
library(gameofthrones)
library(patchwork)
library(kableExtra)
library(colorspace)

#OCUPADOS Y PIB ----
sector <- read.px("DATOS/Ocupados totales.px") %>% as.tibble() %>% 
  filter(Edad == "Total", Unidad == "Valor absoluto", Sexo == "Ambos sexos")
pib <- read.px("DATOS/Evolucion pib.px") %>% as.tibble()

names(sector)[names(sector) == "value"] <- "sector"
names(pib)[names(pib) == "value"] <- "pib"

total <- sector %>% left_join(pib)

total$Unidad <- NULL
total$Sexo <- NULL
total$Edad <- NULL
total$Niveles.y.tasas <- NULL
total$Agregados.macroeconómicos <- NULL
total$Tipo.de.dato <- NULL

total <- total %>% separate(Periodo, into = c("año", "trimestre"), sep = 4)
total <- total %>% mutate(trimestre = case_when(
    trimestre == "T1" ~ "-03-01",
    trimestre == "T2" ~ "-06-01",
    trimestre == "T3" ~ "-09-01",
    trimestre == "T4" ~ "-12-01")) %>% 
  unite(periodo,c(1:2),  sep = "", remove = TRUE) %>% arrange(periodo)

total <- total %>% mutate("Ocupados" = ROC(sector, n = 4, type = "discrete")*100)

reg1 <- lm(pib~Ocupados, data = total)
summary(reg1)

total <- total %>% pivot_longer(cols = 3:4, names_to = "variables", values_to = "value")

total <- na.omit(total)
total$periodo <- as.Date.character(total$periodo)

ggplot(total) + 
  geom_point(aes(x = periodo, y = value, color = variables), size = 2) + 
  geom_line(aes(x = periodo, y = value, color = variables), size = 1.2)+
  geom_hline(yintercept = 0, linetype = 5, size = 0.5)+
  annotate("text", x=as.Date("2016-09-01"), y= -15, label = "R2 = 0.7357", size =  10)+
  scale_color_manual(values = c("#006cb2", "black"), name = "", labels = c("Ocupados", "PIB"))+
  labs(x = "",
       y = "%", title = " ",
       caption = "Elaboración propia a partir de los datos del INE")+
  scale_y_continuous(breaks = seq(-25,10, by = 5))+
  scale_x_date(date_breaks = "1 year",
               date_labels = c("%y"),
               limits = as.Date(c('2003-01-01','2021-01-01')))+
  theme_classic()+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 15),
        axis.text = element_text(size = 15, color = "black"),
        axis.title = element_text(size = 15),
        plot.caption = element_text(size = 15, hjust=-0.1))


#OCUPADOS-ERTES Y PIB ----
  
pib <- read.px("DATOS/Evolucion pib.px") %>% as.tibble()
names(pib)[names(pib) == "value"] <- "pib"
pib <- pib %>% separate(Periodo, into = c("año", "trimestre"), sep = 4) %>% 
  mutate(trimestre = case_when(
    trimestre == "T1" ~ "-03-01",
    trimestre == "T2" ~ "-06-01",
    trimestre == "T3" ~ "-09-01",
    trimestre == "T4" ~ "-12-01")) %>% 
  unite(periodo,c(1:2),  sep = "", remove = TRUE) %>% arrange(periodo)

ertes <- read.csv(url("https://raw.githubusercontent.com/jescuderoma/filas-y-columnas/main/2021-04-12_erte-afiliados-sectores/data/tidy/erte_sectores_tidy.csv"))

ertes_final <- ertes %>% select(c(fecha, cnae_cod, trabajadores_erte_ultimo_dia))

ertes_final <- ertes_final %>% pivot_wider(names_from = cnae_cod, values_from = trabajadores_erte_ultimo_dia)
ertes_final <- ertes_final %>% mutate("Total" = apply(ertes_final[ , 2:89], 1, sum))
ertes_final <- ertes_final %>% pivot_longer(cols = 2:90, names_to = "cnae_cod", values_to = "trabajadores_erte_ultimo_dia")

ertes_total <- ertes_final %>% filter(cnae_cod == "Total", fecha %in% c("2020-06-01", "2020-09-01", "2020-12-01", "2021-03-01"))
rm(ertes, ertes_final)
names(ertes_total)[names(ertes_total) == "fecha"] <- "periodo"

ocupados <- read.px("DATOS/ocupados totales.px") %>% as.tibble()
ocupados <- ocupados %>% filter(Unidad == "Valor absoluto", Sexo == "Ambos sexos", Edad == "Total") %>% 
  separate(Periodo, into = c("año", "trimestre"), sep = 4) %>% 
  mutate(trimestre = case_when(
    trimestre == "T1" ~ "-03-01",
    trimestre == "T2" ~ "-06-01",
    trimestre == "T3" ~ "-09-01",
    trimestre == "T4" ~ "-12-01")) %>% 
  unite(periodo,c(1:2),  sep = "", remove = TRUE) %>% arrange(periodo)

ocu_ertes <- ocupados %>% left_join(ertes_total)

ocu_ertes$Unidad <- NULL
ocu_ertes$Sexo <- NULL
ocu_ertes$Edad <- NULL
ocu_ertes$cnae_cod <- NULL
ocu_ertes$trabajadores_erte_ultimo_dia[is.na(ocu_ertes$trabajadores_erte_ultimo_dia)] <-0

ocu_ertes <- ocu_ertes %>% mutate(trabajadores_erte_ultimo_dia = trabajadores_erte_ultimo_dia/1000) %>% 
  mutate("Ocupados-ertes" = (value - trabajadores_erte_ultimo_dia))

ocu_ertes$trabajadores_erte_ultimo_dia <- NULL
names(ocu_ertes)[names(ocu_ertes) == "value"] <- "Ocupados"

ocu_ertes <- ocu_ertes %>% mutate(Ocupados = ROC(Ocupados, n = 4, type = "discrete")*100,
                                  `Ocupados-ertes` = ROC(`Ocupados-ertes`, n = 4, type = "discrete")*100)

rm(ertes_total, ocupados)

pib_ertes <- pib %>% left_join(ocu_ertes)
pib_ertes <- na.omit(pib_ertes)
pib_ertes$Niveles.y.tasas <- NULL
pib_ertes$Agregados.macroeconómicos <- NULL
pib_ertes$Tipo.de.dato <- NULL
pib_ertes$Ocupados <- NULL

reg2 <- lm(pib~`Ocupados-ertes`, data = pib_ertes)
summary(reg2)

pib_ertes <- pib_ertes %>% pivot_longer(cols = 2:3, names_to = "variable", values_to = "value")
pib_ertes$periodo <- as.Date.character(pib_ertes$periodo)
rm(ocu_ertes, pib)


ggplot(pib_ertes) + 
  geom_point(aes(x = periodo, y = value, color = variable), size = 2) + 
  geom_line(aes(x = periodo, y = value, color = variable), size = 1.2)+
  geom_hline(yintercept = 0, linetype = 5, size = 0.5)+
  annotate("text", x=as.Date("2016-09-01"), y= -15, label = "R2 = 0.8867", size =  10)+
  labs(x="", y="%", title = " ")+
  scale_color_manual(values = c("red", "black"), name = "", labels = c("Ocupados-Ertes", "PIB"))+
  scale_y_continuous(breaks = seq(-25,10, by = 5))+
  scale_x_date(date_breaks = "1 year",
               date_labels = c("%y"),
               limits = as.Date(c('2003-01-01','2021-01-01')))+
  theme_classic()+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 15),
        axis.text = element_text(size = 15, color = "black"),
        axis.title = element_text(size = 15),
        plot.caption = element_text(size = 12, hjust=0))



#SERE TEMPORALIDAD POR GENERO ----
temp <- read.px("DATOS/tipo contrato y jornada.px") %>% as.tibble() %>% 
  filter(Tipo.de.jornada == "Total",
        Tipo.de.contrato.o.relación.laboral %in% c("Total asalariados", "Asalariados con contrato temporal")) %>% 
  separate(Periodo, into = c("año", "trimestre"), sep = 4) %>% 
  mutate(trimestre = case_when(
         trimestre == "T1" ~ "-03-01",
         trimestre == "T2" ~ "-06-01",
         trimestre == "T3" ~ "-09-01",
         trimestre == "T4" ~ "-12-01")) %>% 
  unite(periodo,c(1:2),  sep = "", remove = TRUE) %>% arrange(periodo)

temp <- temp %>% pivot_wider(names_from = Tipo.de.contrato.o.relación.laboral, values_from = value)

temp <- temp %>% group_by(Sexo) %>% 
  mutate(tasa.temporalidad = round((`Asalariados con contrato temporal`/`Total asalariados`)*100, 2)) 

temp$periodo <- as.Date.character(temp$periodo)

ggplot(temp) + geom_smooth(aes(periodo, tasa.temporalidad))+
  geom_point(aes(periodo, tasa.temporalidad, color = Sexo), size = 1.5)+
  geom_line(aes(periodo, tasa.temporalidad, color = Sexo), size = 1)+
  scale_color_got_d(option = "Daenerys", name = "")+
  labs(x = "",
       y = "",
       title = " ",
       caption = "Elaboración propia a partir de los datos del INE")+
  scale_x_date(date_breaks = "1 year",
              date_labels = c("%y"),
              limits = as.Date(c('2002-01-01','2021-01-01')))+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 15),
        axis.text = element_text(size = 15, color = "black"),
        axis.title = element_text(size = 15),
        plot.caption = element_text(size = 12, hjust=-0.1))

#JOVENES Y ADULTOS CORRELACION ----
load("DATOS/Desempleo.RData")
paro <- paro %>% filter(Sexo == "Ambos sexos", Edad != "Total")

paro <- paro %>% pivot_wider(names_from = Edad, values_from = value)
paro <- paro %>%
  mutate("De 16 a 29 años" = round(apply(paro[ , 3:5], 1, mean),2),
          "De 30 a 64 años" = round(apply(paro[ , 6:12], 1, mean),2)) %>% 
  select(1:2,15:16)


ggplot(paro) + geom_point(aes(x = `De 30 a 64 años`, y = `De 16 a 29 años`), size = 4, shape = 18)+
  geom_smooth(method = "lm", aes(x = `De 30 a 64 años`, y = `De 16 a 29 años`), se = F)+
  labs(title = " ",
       caption = "Elaboración propia a partir de los datos del INE")+
  annotate("text", x= 17.5, y= 21, label = "5.38723+2.23208x", size =  8)+
  annotate("text", x= 17.5, y= 18, label = "R^2 = 0.9283", size = 8)+
  theme_classic()+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 15),
        axis.text = element_text(size = 15, color = "black"),
        axis.title = element_text(size = 15),
        plot.caption = element_text(size = 12, hjust=-0.1))

#SERIE TEMPORAL DESEMPLEO GRUPOS DE EDAD ----

load("DATOS/Desempleo.RData")
paro <- paro %>% filter(Sexo == "Ambos sexos", Edad != "Total")

paro <- paro %>% pivot_wider(names_from = Edad, values_from = value)

paro <- paro %>%
  mutate("De 16 a 29 años" = round(apply(paro[ , 3:5], 1, mean),2),
         "De 30 a 64 años" = round(apply(paro[ , 6:12], 1, mean),2)) %>% 
  select(1:2,15:16)

paro <- paro %>% pivot_longer(cols = 3:4, names_to = "edad", values_to = "value")
paro$periodo <- as.Date.character(paro$periodo)

ggplot(paro) + geom_point(aes(periodo, value, color = edad))+
  geom_line(aes(periodo, value, color = edad), size = 1.2)+
  labs(x = "",
       y = "%",
       title = " ",
       caption = "Elaboración propia a partir de los datos del INE")+
  scale_x_date(date_breaks = "1 year",
               date_labels = c("%y"),
               limits = as.Date(c("2001-12-01", "2021-01-01")))+
  scale_colour_manual(values = c("dodgerblue4", "firebrick4"), name = "")+
  theme_classic()+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 15),
        axis.text = element_text(size = 15, color = "black"),
        axis.title = element_text(size = 15),
        plot.caption = element_text(size = 12, hjust=-0.1),
        panel.grid.major = element_line(colour = "grey"))


#SERIE TEMPORAL TASAS DE ACTIVIDAD GRANDES GRUPOS DE EDAD ----
load("DATOS/actividad.RData")
actividad <- actividad %>% filter(Sexo == "Ambos sexos", Edad != "Total")
actividad <- actividad %>% pivot_wider(names_from = Edad, values_from = value)

actividad <- actividad %>%
  mutate("De 16 a 29 años" = round(apply(actividad[ , 3:5], 1, mean),2),
         "De 30 a 64 años" = round(apply(actividad[ , 6:12], 1, mean),2)) %>% 
  select(1:2,15:16)

actividad <- actividad %>% pivot_longer(cols = 3:4, names_to = "edad", values_to = "value")
actividad$periodo <- as.Date.character(actividad$periodo)

ggplot(actividad) + geom_point(aes(periodo, value, color = edad), size = 1.5)+
  geom_line(aes(periodo, value, color = edad), size = 1.2)+
  geom_smooth(aes(periodo, value, color = edad))+
  labs(x = "",
       y = "%",
       title = " ", 
       caption = "Elaboración propia a partir de los datos del INE")+
  scale_x_date(date_breaks = "1 year",
               date_labels = c("%y"),
               limits = as.Date(c("2002-01-01", "2021-01-01")))+
  scale_colour_manual(values = c("dodgerblue1", "maroon4"), name = "")+
  theme_classic()+
  theme(panel.grid.major = element_line(color = "grey", size = 0.1),
        legend.position = "bottom",
        legend.text = element_text(size = 15),
        axis.text = element_text(size = 15, color = "black"),
        axis.title = element_text(size = 15),
        plot.caption = element_text(size = 12, hjust=-0.1))
        
#VARIACION ANUAL DE OCUPADOS POR RAMAS DE ACTIVIDAD ----
ocupados <- read.px("DATOS/ocupados rama actividad.px") %>% as.tibble() %>% 
  filter(Sexo == "Ambos sexos", Edad == "Total", Rama.de.actividad.CNAE.2009 != "Total", Periodo %in% c("2019T4", "2020T4")) %>% 
  separate(Periodo, into = c("año", "trimestre"), sep = 4) %>% 
  separate(Rama.de.actividad.CNAE.2009, into = c("CNAE", "actividad"), sep = 1) %>% 
  mutate(año = case_when(
    año == "2019" ~ "año19",
    año == "2020" ~ "año20"))
ocupados <- ocupados %>% unite(periodo, c(1:2), sep = "", remove = TRUE)

diferenciaT4 <- ocupados %>% filter(periodo %in% c("año19T4", "año20T4")) %>%
  pivot_wider(id_cols = CNAE, names_from = periodo, values_from = value) %>% 
  mutate(dif = año20T4 - año19T4,
         brecha = ifelse(dif <=0, 1, 0))

ggplot(diferenciaT4)+
  geom_col(aes(x=dif, y = reorder(CNAE, dif), fill = factor(brecha)))+
  scale_x_continuous(limits = c(-400, 100))+
  scale_fill_discrete_diverging()+
  guides(fill= FALSE)+
  labs(y = NULL,
       x = "Miles de ocupados",
       title = " ", y=" ",
       caption = "Elaboración propia a partir de los datos del INE")+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 15),
        axis.text = element_text(size = 15, color = "black"),
        axis.title = element_text(size = 15),
        plot.caption = element_text(size = 12, hjust=0))


##TABLA DE ANEXO

ocupados <- ocupados %>% filter(periodo == "año20T4")

ocupados$periodo <- NULL
ocupados$Edad <- NULL
ocupados$Sexo <- NULL
ocupados$value <- NULL

kable(ocupados, booktabs = TRUE, format = "html", align = "c",
      col.names = c("CNAE-2009", "Rama de Actividad")) %>% 
  footnote(general = "INE", general_title = "Source:", footnote_as_chunk = T)



#ACTIVOS - OCUPADOS - INACTIVOS ----
activos <- read.px("DATOS/Activos totales.px") %>% as.tibble()
ocupados <- read.px("DATOS/ocupados totales.px") %>% as.tibble()
inactivos <- read.px("DATOS/Inactivos.px") %>% as.tibble()

names(activos)[names(activos) == "value"] <- "activos"
names(ocupados)[names(ocupados) == "value"] <- "ocupados"
names(inactivos)[names(inactivos) == "value"] <- "inactivos"

todo <- activos %>% left_join(inactivos)
todo <- todo %>% left_join(ocupados)

todo <- todo %>% filter(Unidad == "Valor absoluto", Sexo == "Ambos sexos", Edad == "Total") %>% 
  separate(Periodo, into = c("año", "trimestre"), sep = 4) %>% 
  mutate(trimestre = case_when(
    trimestre == "T1" ~ "-03-01",
    trimestre == "T2" ~ "-06-01",
    trimestre == "T3" ~ "-09-01",
    trimestre == "T4" ~ "-12-01")) %>% 
  unite(periodo,c(1:2),  sep = "", remove = TRUE) %>% arrange(periodo)

todo <- todo %>% mutate(parados = (activos - ocupados)) %>% 
  mutate(tasa.paro = round(((parados/activos)*100),2))

todo <- todo %>% pivot_longer(cols = 5:7, names_to = "variable", values_to = "value")

todo$periodo <- as.Date.character(todo$periodo, format = c("%Y-%m-%d"))  

ggplot(todo) + geom_segment(aes(x = as.Date("2007-06-01"), xend = as.Date("2007-06-01"),
                               y = 22350, yend = 20590), size = .5)+
  geom_segment(aes(x = as.Date("2013-03-01"), xend = as.Date("2013-03-01"),
                                  y = 23300, yend = 17040), size = .5)+
  geom_segment(aes(x = as.Date("2020-12-01"), xend = as.Date("2020-12-01"),
                   y = 23060, yend = 19350), size = .5)+
  geom_line(aes(periodo, value, color = variable), size = 1.5)+
  geom_point(aes(periodo, value, color = variable), size = 2.5)+
  geom_smooth(aes(periodo, value, color = variable))+
  annotate("text", label = "7,9%", x = as.Date("2007-12-31"), y = 21350, size = 6, colour = "black")+
  annotate("text", label = "26,9%", x = as.Date("2013-12-01"), y = 20500, size = 6, colour = "black")+
  annotate("text", label = "16,1%", x = as.Date("2020-03-15"), y = 21350, size = 6, colour = "black")+
  scale_color_manual(values = c("#80a198","firebrick3","#6697c9"), name = "", labels = c("Activos", "Inactivos", "Ocupados"))+
  scale_x_date(date_breaks = "1 year",
               date_labels = c("%y"),
               limits = as.Date(c('2002-01-01','2021-01-02')))+
  labs(x = "",
       y = "",
       caption = "Elaboración propia a partir de los datos del INE")+ 
  theme_classic()+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 15),
        axis.text = element_text(size = 15, color = "black"),
        axis.title = element_text(size = 15),
        plot.caption = element_text(size = 12, hjust=-0.1),
        panel.grid.major = element_line())
  
#RESTAR ERTES A LOS OCUPADOS COMO SI NO ESTUVIERAN INCLUIDOS EN ESTOS ----

ertes <- read.csv(url("https://raw.githubusercontent.com/jescuderoma/filas-y-columnas/main/2021-04-12_erte-afiliados-sectores/data/tidy/erte_sectores_tidy.csv"))

ertes_final <- ertes %>% select(c(fecha, cnae_cod, trabajadores_erte_ultimo_dia))

ertes_final <- ertes_final %>% pivot_wider(names_from = cnae_cod, values_from = trabajadores_erte_ultimo_dia)
ertes_final <- ertes_final %>% mutate("Total" = apply(ertes_final[ , 2:89], 1, sum))
ertes_final <- ertes_final %>% pivot_longer(cols = 2:90, names_to = "cnae_cod", values_to = "trabajadores_erte_ultimo_dia")

ertes_total <- ertes_final %>% filter(cnae_cod == "Total", fecha %in% c("2020-06-01", "2020-09-01", "2020-12-01", "2021-03-01"))
rm(ertes, ertes_final)
names(ertes_total)[names(ertes_total) == "fecha"] <- "periodo"

ocupados <- read.px("DATOS/ocupados totales.px") %>% as.tibble()
ocupados <- ocupados %>% filter(Unidad == "Valor absoluto", Sexo == "Ambos sexos", Edad == "Total") %>% 
  separate(Periodo, into = c("año", "trimestre"), sep = 4) %>% 
  mutate(trimestre = case_when(
    trimestre == "T1" ~ "-03-01",
    trimestre == "T2" ~ "-06-01",
    trimestre == "T3" ~ "-09-01",
    trimestre == "T4" ~ "-12-01")) %>% 
  unite(periodo,c(1:2),  sep = "", remove = TRUE) %>% arrange(periodo)

ocu_ertes <- ocupados %>% left_join(ertes_total)

ocu_ertes$Unidad <- NULL
ocu_ertes$Sexo <- NULL
ocu_ertes$Edad <- NULL
ocu_ertes$cnae_cod <- NULL
ocu_ertes$trabajadores_erte_ultimo_dia[is.na(ocu_ertes$trabajadores_erte_ultimo_dia)] <-0

ocu_ertes <- ocu_ertes %>% mutate(trabajadores_erte_ultimo_dia = trabajadores_erte_ultimo_dia/1000) %>% 
  mutate("ocupados-ertes" = (value - trabajadores_erte_ultimo_dia))
ocu_ertes$trabajadores_erte_ultimo_dia <- NULL

ocu_ertes <- ocu_ertes %>% pivot_longer(cols = 2:3, names_to = "tipos", values_to = "value")

ocu_ertes$periodo <- as.Date.character(ocu_ertes$periodo)

ggplot(ocu_ertes) + geom_line(aes(periodo, value, color = tipos), size = 1.5) + 
  scale_color_manual(values = c("#df2726", "#006cb2"), name = "", labels = c("Ocupados-Ertes", "Ocupados"))+
  scale_x_date(date_breaks = "1 year",
             date_labels = c("%y"),
             limits = as.Date(c('2003-01-01','2021-01-01')))+
  labs(x = "",
       y = "Miles de personas",
       caption = "Elaboración propia a partir de los datos del INE")+
  theme_minimal() + 
  theme(legend.position = "bottom",
        legend.text = element_text(size = 15),
        axis.text = element_text(size = 15, color = "black"),
        axis.title = element_text(size = 15),
        plot.caption = element_text(size = 12, hjust=-0.1),
        axis.line = element_line(color = "black"))


#VARIACION ANUAL DE TEMPORALES E INDEFINIDOS ----

temp <- read.px("DATOS/tipo contrato y jornada.px") %>% as.tibble() %>% 
  filter(Tipo.de.jornada == "Total", Sexo == "Ambos sexos",
         Tipo.de.contrato.o.relación.laboral %in% c("Asalariados con contrato indefinido", "Asalariados con contrato temporal")) %>% 
  separate(Periodo, into = c("año", "trimestre"), sep = 4) %>% 
  mutate(trimestre = case_when(
    trimestre == "T1" ~ "-03-01",
    trimestre == "T2" ~ "-06-01",
    trimestre == "T3" ~ "-09-01",
    trimestre == "T4" ~ "-12-01")) %>% 
  unite(periodo,c(1:2),  sep = "", remove = TRUE) %>% arrange(periodo)

temp <- temp %>% pivot_wider(names_from = Tipo.de.contrato.o.relación.laboral, values_from = value)

temp <- temp %>% mutate("Indefinidos" = ROC(`Asalariados con contrato indefinido`, n = 4, type = "discrete")*100,
                        "Temporales" = ROC(`Asalariados con contrato temporal`, n = 4, type = "discrete")*100)

temp <- temp %>% select(1:3, 6:7) %>% pivot_longer(cols = 4:5, names_to = "contrato", values_to = "value")

temp$periodo <- as.Date.character(temp$periodo)

ggplot(temp) + 
  geom_hline(yintercept = 0, linetype = 5, color = "black")+
  geom_line(aes(periodo, value, color = contrato), size = 1.2)+
  scale_color_manual(values = c("#006cb2", "#df2726"), name = "")+
  scale_x_date(date_breaks = "1 year",
               date_labels = c("%y"),
               limits = as.Date(c("2003-01-01", "2021-01-02")))+
  labs(x = "", y = "%", title = " ",
       caption = "Elaboración propia a partir de los datos del INE")+
  theme_classic()+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 15),
        axis.text = element_text(size = 15, color = "black"),
        axis.title = element_text(size = 15),
        plot.caption = element_text(size = 12, hjust=-0.1))


#VARIACIÓN ANUAL DE EMPLEADOS PUBLICOS Y PRIVADOS ----

pub <- read.px("DATOS/PUBLICO VS PRIVADO.PX") %>% as.tibble() %>% 
  filter(Edad == "Total", Tipo.de.sector != "Total", Sexo == "Ambos sexos")

pub <- pub %>% separate(Periodo, into = c("año", "trimestre"), sep = 4) %>% 
  mutate(trimestre = case_when(
    trimestre == "T1" ~ "-03-01",
    trimestre == "T2" ~ "-06-01",
    trimestre == "T3" ~ "-09-01",
    trimestre == "T4" ~ "-12-01")) %>% 
  unite(periodo,c(1:2),  sep = "", remove = TRUE) %>% arrange(periodo)

pub <- pub %>% pivot_wider(names_from = Tipo.de.sector, values_from = value)
pub <- pub %>% mutate("Público" = ROC(`Empleo público`, n = 4, type = "discrete")*100,
                      "Privado" = ROC(`Empleo privado`, n = 4, type = "discrete")*100)

pub <- pub %>% pivot_longer(cols = 6:7, names_to = "Empleo", values_to = "value")
pub <- pub %>% select(1:3, 6:7)
pub$periodo <- as.Date.character(pub$periodo)

ggplot(pub) + 
  geom_point(aes(periodo, value, color = Empleo), size = 2)+
  geom_line(aes(periodo, value, color = Empleo), size = 1.2)+
  geom_hline(yintercept = 0, linetype = 5, size = 0.2)+
  labs(x="", y="%", title = " ", 
       caption = "Elaboración propia a partir de los datos del INE")+
  scale_color_manual(values = c("#df2726", "#006cb2"), name = "")+
  scale_x_date(date_breaks = "1 year",
               date_labels = c("%y"),
               limits = as.Date(c('2003-01-01','2021-01-01')))+
  theme_classic()+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 15),
        axis.text = element_text(size = 15, color = "black"),
        axis.title = element_text(size = 15),
        plot.caption = element_text(size = 12, hjust=-0.1))


