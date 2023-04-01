# Lectura de las bases de datos por separado
library(magrittr)
library(tidyverse)
library(plotly)
library(tm)
library(janitor)
#library(ts)
library(forecast)
library(TSstudio)
library(TSA)
library(tseries)
library(tsibble)
library(cluster)
library(radiant)

# Depuración de la base de datos
# load(file = "BDs_Finales.RData")
# 
# # Simplificación de los nombres de las bases de datos
# 
# inscritos <- base_inscritos_2000_2021
# inscritos$inscritos %<>% as.numeric()
# v <- inscritos$nivel_de_formacion %>%  unique()
# v <- v[c(4,5,6,7,12,13,17,19,23,29)]
# inscritos %<>% filter(nivel_de_formacion %in% v)
# admitidos <- base_admitidos_2000_2021
# admitidos$admitidos %<>% as.numeric()
# admitidos$nivel_de_formacion %>% unique()
# v <- admitidos$nivel_de_formacion %>% unique()
# v <- v[c(1,5,6,7,12,13,17,20,24,25,30)]
# admitidos %<>% filter(nivel_de_formacion %in% v)
# 
# ins_admi <- merge(inscritos, admitidos, all = TRUE)
# 
# insNNadmi <- ins_admi %>% na.omit() %>% filter(inscritos != 0 & admitidos != 0)
# graduados <- base_graduados_2000_2021
# graduados$graduados %<>% as.numeric()
# admi_grad <- merge(admitidos, graduados, all = T)
# admiNNgrad <- admi_grad %>% na.omit()
# m3 <- merge(inscritos, admitidos) %>% merge(graduados)
# Se guardan los datos
# save(admitidos, inscritos, graduados, file = "BD_Separadas.RData")
# save(ins_admi, insNNadmi, admi_grad, admiNNgrad, m3, file = "BD_unidas.RData")
# rm(list = ls())

load(file= "BD_Separadas.RData")
load(file = "BD_unidas.RData")

code_unal <- c(1101,1102,1103,1104,1124,1125,9933)

ins <- inscritos %>% select(ano, codigo_de_la_institucion,
                     sector_ies, metodologia,
                     area_de_conocimiento, codigo_snies_del_programa,
                     sexo, semestre,
                     departamento_de_domicilio_de_la_ies,
                     departamento_de_oferta_del_programa,
                     inscritos)

ins$codigo_de_la_institucion %<>% as.numeric()
 
ins$comparacacion <- ifelse(ins$codigo_de_la_institucion %in%
                              code_unal, "UNAL", "otras IES")

admi <- admitidos %>% select(ano, codigo_de_la_institucion,
                             sector_ies, metodologia,
                             area_de_conocimiento, codigo_snies_del_programa,
                             sexo, semestre,
                             departamento_de_domicilio_de_la_ies,
                             departamento_de_oferta_del_programa,
                             admitidos)

admi$codigo_de_la_institucion %<>% as.numeric()

admi$comparacacion <- ifelse(admi$codigo_de_la_institucion %in%
                              code_unal, "UNAL", "otras IES")

grad <- graduados %>% select(ano, codigo_de_la_institucion,
                             sector_ies, metodologia,
                             area_de_conocimiento,
                             codigo_snies_del_programa,
                             sexo, semestre,
                             departamento_de_domicilio_de_la_ies,
                             departamento_de_oferta_del_programa,
                             graduados)

grad$codigo_de_la_institucion %<>% as.numeric()

grad$comparacacion <- ifelse(grad$codigo_de_la_institucion %in%
                              code_unal, "UNAL", "otras IES")

rm(admitidos,graduados,inscritos)



# ---- Renombre Variables ----

names(grad)[11] <- "demanda_potencial"
names(ins)[11] <- "demanda_real"

## Limpieza de los datos:

ins$departamento_de_domicilio_de_la_ies %<>% toupper() %>% 
  chartr("ÁÉÍÓÚ", "AEIOU", .) %>%
  removePunctuation() %>% 
  str_replace_all(., "[[:cntrl:]]", " ") %>% 
  str_to_lower() %>% 
  str_replace_all(., "-", " ") %>%
  removeWords(., words  = stopwords("spanish")) %>% 
  stripWhitespace(.)

ins$departamento_de_domicilio_de_la_ies %<>%
  str_replace(.," guajira", "guajira") %>%
  str_replace(.,"archipielago san andres providencia santa catalina",
              "san andres providencia") %>% 
  str_replace(.,"archipielag", "san andres providencia") %>%
  str_replace(.,"cundinamarca", "cundinamarc") %>% 
  str_replace(.,"cundinamarc", "cundinamarca") %>%
  str_replace(., "valle cauca", "valle c") %>%
  str_replace(., "valle c", "valle cauca") %>%
  str_replace(., "norte santander", "norte sa") %>%
  str_replace(., "norte sa", "norte santander")

ins$departamento_de_oferta_del_programa %<>% toupper() %>% 
  chartr("ÁÉÍÓÚ", "AEIOU", .) %>%
  removePunctuation() %>% 
  str_replace_all(., "[[:cntrl:]]", " ") %>% 
  str_to_lower() %>% 
  str_replace_all(., "-", " ") %>%
  removeWords(., words  = stopwords("spanish")) %>% 
  stripWhitespace(.)

ins$departamento_de_oferta_del_programa %<>%
  str_replace(.," guajira", "guajira") %>%
  str_replace(.,"archipielago san andres providencia santa catalina",
              "san andres providencia") %>% 
  str_replace(.,"archipielago","san andres providencia") %>% 
  str_replace(., "narinio", "nariño") %>%
  str_replace(., "narino", "nariño") %>%
  str_replace(., "norte santander", "norte santan") %>% 
  str_replace(., "norte santan", "norte santander") %>%
  str_replace(., "san andres providencia ", "san andres providencia") %>%
  str_replace(., " identificado", "no identificado")

ins$sexo %<>% tolower() %>% str_replace(., "hombre", "masculino") %>%
  str_replace(., "mujer", "femenino")

ins$area_de_conocimiento %<>% toupper() %>% 
  chartr("ÁÉÍÓÚ", "AEIOU", .) %>%
  removePunctuation() %>% 
  str_replace_all(., "[[:cntrl:]]", " ") %>% 
  str_to_lower() %>% 
  str_replace_all(., "-", " ") %>%
  removeWords(., words  = stopwords("spanish")) %>% 
  stripWhitespace(.) %>% str_replace(., " clasificar", "sin clasificar")

ins$metodologia %<>% tolower() %>%
  str_replace(.,"^(?!(presencial|presencial-virtual|sin metodologia definida)$).*$",
              "no presencial")

ins$sector_ies %<>% tolower()

ins$ano %<>% parse_number() %>% factor(., levels = 2000:2021)

# ---- Admitidos ----

admi$departamento_de_oferta_del_programa %<>% toupper() %>% 
  chartr("ÁÉÍÓÚ", "AEIOU", .) %>%
  removePunctuation() %>% 
  str_replace_all(., "[[:cntrl:]]", " ") %>% 
  str_to_lower() %>% 
  str_replace_all(., "-", " ") %>%
  removeWords(., words  = stopwords("spanish")) %>% 
  stripWhitespace(.)

admi$departamento_de_oferta_del_programa %<>%
  str_replace(.," guajira", "guajira") %>%
  str_replace(.,"archipielago san andres providencia santa catalina",
              "san andres providencia") %>% 
  str_replace(., "narinio", "nariño") %>%
  str_replace(., "narino", "nariño") %>%
  str_replace(., "san andres providencia", "san andres provi") %>%
  str_replace(., "san andres provi", "san andres providencia") %>%
  str_replace(., " identificado", "no identificado") %>%
  str_replace(., " informacion", "no identificado") %>%
  str_replace_na(.) %>% str_replace(., "NA", "no identificado")

admi$departamento_de_domicilio_de_la_ies %<>% toupper() %>% 
  chartr("ÁÉÍÓÚ", "AEIOU", .) %>%
  removePunctuation() %>% 
  str_replace_all(., "[[:cntrl:]]", " ") %>% 
  str_to_lower() %>% 
  str_replace_all(., "-", " ") %>%
  removeWords(., words  = stopwords("spanish")) %>% 
  stripWhitespace(.)

admi$departamento_de_domicilio_de_la_ies %<>%
  str_replace(.," guajira", "guajira") %>%
  str_replace(.,"archipielago san andres providencia santa catalina",
              "san andres providencia") %>% 
  str_replace(., "narinio", "nariño") %>%
  str_replace(., "narino", "nariño") %>%
  str_replace(., "san andres providencia", "san andres provi") %>%
  str_replace(., "san andres provi", "san andres providencia")

admi$sexo %<>% tolower() %>% str_replace(., "hombre", "masculino") %>%
  str_replace(., "mujer", "femenino") %>% str_replace(., "sin información",
                                                      "no informa")
admi$area_de_conocimiento %<>% toupper() %>% 
  chartr("ÁÉÍÓÚ", "AEIOU", .) %>%
  removePunctuation() %>% 
  str_replace_all(., "[[:cntrl:]]", " ") %>% 
  str_to_lower() %>% 
  str_replace_all(., "-", " ") %>%
  removeWords(., words  = stopwords("spanish")) %>% 
  stripWhitespace(.) %>% str_replace(., " clasificar", "sin clasificar")

admi$metodologia %<>% tolower() %>%
  str_replace(.,"^(?!(presencial|presencial-virtual|sin metodologia definida)$).*$",
              "no presencial")

admi$sector_ies %<>% tolower()

admi$ano %<>% parse_number() %>% factor(., levels = 2000:2021)

# ---- Graduados ----

grad$departamento_de_oferta_del_programa %<>% toupper() %>% 
  chartr("ÁÉÍÓÚ", "AEIOU", .) %>%
  removePunctuation() %>% 
  str_replace_all(., "[[:cntrl:]]", " ") %>% 
  str_to_lower() %>% 
  str_replace_all(., "-", " ") %>%
  removeWords(., words  = stopwords("spanish")) %>% 
  stripWhitespace(.) %>%
  str_replace(.," guajira", "guajira") %>%
  str_replace(.,"archipielago san andres providencia santa catalina",
              "san andres providencia") %>% 
  str_replace(.,"archipielago sa", "san andres providencia")

grad$departamento_de_domicilio_de_la_ies %<>% toupper() %>% 
  chartr("ÁÉÍÓÚ", "AEIOU", .) %>%
  removePunctuation() %>% 
  str_replace_all(., "[[:cntrl:]]", " ") %>% 
  str_to_lower() %>% 
  str_replace_all(., "-", " ") %>%
  removeWords(., words  = stopwords("spanish")) %>% 
  stripWhitespace(.) %>%
  str_replace(.," guajira", "guajira") %>%
  str_replace(.,"archipielago san andres providencia santa catalina",
              "san andres providencia") %>% 
  str_replace(.,"archipielago sa", "san andres providencia")


grad$sexo %<>% tolower() %>% str_replace(., "hombre", "masculino") %>%
  str_replace(., "mujer", "femenino")


grad$area_de_conocimiento %<>% toupper() %>% 
  chartr("ÁÉÍÓÚ", "AEIOU", .) %>%
  removePunctuation() %>% 
  str_replace_all(., "[[:cntrl:]]", " ") %>% 
  str_to_lower() %>% 
  str_replace_all(., "-", " ") %>%
  removeWords(., words  = stopwords("spanish")) %>% 
  stripWhitespace(.) %>% str_replace(., " clasificar", "sin clasificar")


grad$metodologia %<>% tolower() %>%
  str_replace(.,"^(?!(presencial|presencial-virtual|sin metodologia definida)$).*$",
              "no presencial")

grad$sector_ies %<>% tolower()

grad$ano %<>% parse_number() %>% factor(., levels = 2000:2021)

# ---- Sub-sets ----

sum.ins <- ins %>% select(sector_ies, comparacacion,
                          metodologia, area_de_conocimiento,
                          semestre, ano, sexo, demanda_real) %>%
  group_by(sector_ies, comparacacion,metodologia,
           area_de_conocimiento, semestre, ano, sexo) %>% 
  summarise(across(everything(), sum),
            .groups = 'drop')


sum.admi <- admi %>% select(sector_ies, comparacacion,
                            metodologia, area_de_conocimiento,
                          semestre, ano, sexo, admitidos) %>%
  group_by(sector_ies, comparacacion,metodologia,
           area_de_conocimiento, semestre, ano, sexo) %>% 
  summarise(across(everything(), sum),
            .groups = 'drop')

sum.grad <- grad %>% select(sector_ies, comparacacion,metodologia, area_de_conocimiento,
                          semestre, ano, sexo, demanda_potencial) %>%
  group_by(sector_ies, comparacacion,metodologia,
           area_de_conocimiento, semestre, ano, sexo) %>% 
  summarise(across(everything(), sum),
            .groups = 'drop')


sum.tot <- merge(sum.ins, sum.admi) %>% merge(sum.grad)



# ---- Tasa de Absorción ----

y <- sum.tot %>% select(ano, semestre, demanda_real, admitidos) %>%
  group_by(ano,semestre) %>%
  summarise(across(everything(), sum), .groups = 'drop') %>% select(demanda_real) %>%
  pull()
x <- sum.tot %>% select(ano, semestre, demanda_real, admitidos) %>%
  group_by(ano,semestre) %>%
  summarise(across(everything(), sum), .groups = 'drop') %>% select(admitidos) %>%
  pull()
t <- vector()

for (i in 1:length(x)) {
  if (i != 1) {
    a <- i -1
    if (y[a] > y[i] & x[i] > y[i]) {
      t[i] <- x[i]/(y[a]-y[i])
    } else{
      t[i] <- x[i]/y[i]
    }
  }else{
    t[i] <- x[i]/y[i]
  }
}

tasa.abs.anio <- data.frame(anio_semestre = as.vector(t(outer(2007:2021, 1:2, paste, sep = '_'))),
           tasa_absorcion = t*10)


tasa.abs.area <- sum.tot %>% select(area_de_conocimiento, demanda_real, admitidos) %>%
  filter(area_de_conocimiento != "sin clasificar") %>% 
  group_by(area_de_conocimiento) %>%
  summarise(across(everything(), sum), .groups = 'drop') %>%
  mutate(tasa_absorcion = admitidos/demanda_real*10)


tasa.abs.sector <- sum.tot %>% select(sector_ies, demanda_real, admitidos) %>%
  group_by(sector_ies) %>%
  summarise(across(everything(), sum), .groups = 'drop') %>%
  mutate(tasa_absorcion = admitidos/demanda_real*10)

tasa.abs.meto <- sum.tot %>% select(metodologia, demanda_real, admitidos) %>%
  filter(metodologia != "presencial-virtual") %>% 
  group_by(metodologia) %>%
  summarise(across(everything(), sum), .groups = 'drop') %>%
  mutate(tasa_absorcion = admitidos/demanda_real*10)

tasa.abs.sexo <- sum.tot %>% select(sexo, demanda_real, admitidos) %>%
  group_by(sexo) %>%
  summarise(across(everything(), sum), .groups = 'drop') %>%
  mutate(tasa_absorcion = admitidos/demanda_real*10)

# ---- Tasa de Cobertura ----

y <- sum.tot %>% select(ano, semestre, demanda_potencial, admitidos) %>%
  group_by(ano,semestre) %>%
  summarise(across(everything(), sum), .groups = 'drop') %>% select(demanda_potencial) %>%
  pull()
x <- sum.tot %>% select(ano, semestre, demanda_potencial, admitidos) %>%
  group_by(ano,semestre) %>%
  summarise(across(everything(), sum), .groups = 'drop') %>% select(admitidos) %>%
  pull()
t <- vector()

for (i in 1:length(x)) {
  if (i != 1) {
    a <- i -1
    if (y[a] > y[i] & x[i] > y[i]) {
      t[i] <- x[i]/(y[a]-y[i])
    } else{
      t[i] <- x[i]/y[i]
    }
  }else{
    t[i] <- x[i]/y[i]
  }
}

tasa.cob.anio <- data.frame(anio_semestre = as.vector(t(outer(2007:2021, 1:2, paste, sep = '_'))),
                            tasa_absorcion = t)


tasa.cob.area <- sum.tot %>% select(area_de_conocimiento, demanda_potencial, admitidos) %>%
  filter(area_de_conocimiento != "sin clasificar") %>% 
  group_by(area_de_conocimiento) %>%
  summarise(across(everything(), sum), .groups = 'drop') %>%
  mutate(tasa_absorcion = demanda_potencial/admitidos*10)


tasa.cob.sector <- sum.tot %>% select(sector_ies, demanda_potencial, admitidos) %>%
  group_by(sector_ies) %>%
  summarise(across(everything(), sum), .groups = 'drop') %>%
  mutate(tasa_absorcion = demanda_potencial/admitidos*10)

tasa.cob.meto <- sum.tot %>% select(metodologia, demanda_potencial, admitidos) %>%
  filter(metodologia != "presencial-virtual") %>% 
  group_by(metodologia) %>%
  summarise(across(everything(), sum), .groups = 'drop') %>%
  mutate(tasa_absorcion = demanda_potencial/admitidos*10)

tasa.cob.sexo <- sum.tot %>% select(sexo, demanda_potencial, admitidos) %>%
  group_by(sexo) %>%
  summarise(across(everything(), sum), .groups = 'drop') %>%
  mutate(tasa_absorcion = demanda_potencial/admitidos*10)

# ---- IPG ----

# ipg = no. de mujeres/total por genero

t <- admi %>% filter(area_de_conocimiento != "sin clasificar" &
                  sexo != "no informa") %>%
  select(comparacacion,area_de_conocimiento, admitidos) %>%
  group_by(comparacacion,area_de_conocimiento) %>%
  summarise(across(everything(), sum), .groups = 'drop') %>%
  select(admitidos) %>% pull()

ipg <- admi %>% filter(area_de_conocimiento != "sin clasificar" &
                  sexo == "femenino") %>%
  select(comparacacion,area_de_conocimiento, admitidos) %>%
  group_by(comparacacion,area_de_conocimiento) %>%
  summarise(across(everything(), sum), .groups = 'drop') %>%
  mutate(IPG = paste0(round(admitidos/t*100,1), "%"))
ipg

# ---- Serie Temporal ----


ts.admi <- admi %>% select(ano, semestre, admitidos) %>%
  group_by(ano, semestre) %>%
  summarise(across(everything(), sum), .groups = 'drop') %>%
  select(admitidos) %>% pull() %>% ts(.,start = c(2007,1),
                                      frequency = 2)

ts.ins <- ins %>% select(ano, semestre, demanda_real) %>%
  group_by(ano, semestre) %>%
  summarise(across(everything(), sum), .groups = 'drop') %>%
  select(demanda_real) %>% pull() %>% ts(.,start = c(2000,1),
                                      frequency = 2)

ts.grad <- grad %>% select(ano, semestre, demanda_potencial) %>%
  group_by(ano, semestre) %>%
  summarise(across(everything(), sum), .groups = 'drop') %>%
  select(demanda_potencial) %>% pull() %>% ts(.,start = c(2001,1),
                                         frequency = 2)

plot(ts.ins,type = "l", lwd = 2, col = "red", xlab = "Año",
     ylab = "# de Personas")

lines(ts.admi, type = "l", col = "blue", lwd = 2)
lines(ts.grad, type = "l", col = "green", lwd = 2)
grid()

legend("topleft", c("Demanda real", "Admitidos", "Demanda potencial"),
       lty = 1, col = c("red", "blue", "green"))

# Nueva escala y limites diferentes ---------------------------------------

plot(ts.ins/1000,type = "l", lwd = 2, col = "red", xlab = "Año",
     ylab = "# de Personas x1000", xlim = c(2007,2021), ylim = c(0, 1000))

lines(ts.admi/1000, type = "l", col = "blue", lwd = 2)
lines(ts.grad/1000, type = "l", col = "green", lwd = 2)
grid()

legend("topleft", c("Demanda real", "Admitidos", "Demanda potencial"),
       bty = 'n', lty = 1, col = c("red", "blue", "green"))

# ---- Cluster mixto ----

sum.tot %<>% filter(area_de_conocimiento != "sin clasificar")

sum.tot$sector_ies %<>% factor()

sum.tot$metodologia %<>% factor()

sum.tot$area_de_conocimiento %<>% factor()

sum.tot$semestre %<>% factor()

sum.tot$sexo %<>% factor()

names(sum.tot)[2] <- "comparacion"

sum.tot$comparacion %<>% factor(., levels = c("UNAL", "otras IES"))

# calculate distance
d_dist <- daisy(sum.tot, metric = "gower")

# hierarchical clustering
hc <- hclust(d_dist, method = "complete")

# dendrogram 
plot(hc, labels=FALSE)
par(lwd=3, mar=c(0,0,0,0))
rect.hclust(hc, k=8, border = rainbow(8))

# choose k, number of clusters 
cluster<-cutree(hc, k=8)

# add cluster to original data 
sum.tot<-cbind(sum.tot,clust = as.factor(cluster))

sum.tot %>% count(sexo,clust) %>%
  ggplot(aes(x = sexo, y = n, fill = sexo))+
  geom_col(position = "dodge")+
  facet_wrap(~clust, scales = "free_y")+
  scale_fill_manual(values = c("slateblue", "orangered"))


sum.tot %>% filter(clust %in% c("4", "3", "5")) %>%
  count(comparacion, area_de_conocimiento, sexo, clust)

radiant()


