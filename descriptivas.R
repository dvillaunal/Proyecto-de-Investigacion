library(tidyverse)

load("base_total.RData")

copy <- base_total[, -c(24, 25, 2, 7, 9, 11, 18, 2)]

## valores lógicos si tienen na 
aux <- complete.cases(copy)

## base sin na
copy_sin_na <- copy[aux, ]

names(copy_sin_na)

#copy_sin_na$principal_o_seccional %>% unique()

copy_sin_na$caracter_ies  <- str_replace_all(copy_sin_na$caracter_ies,
                                             c("UNIVERSIDAD"  = "Universidad", 
                                               "INSTITUCION UNIVERSITARIA/ESCUELA TECNOLOGICA"="Institución Universitaria/Escuela Tecnológica",
                                               "INSTITUCIÓN UNIVERSITARIA/ESCUELA TECNOLÓGICA" = "Institución Universitaria/Escuela Tecnológica",
                                               "INSTITUCION TECNOLOGICA" = "Institución Tecnológica",
                                               "INSTITUCIÓN TECNOLÓGICA" = "Institución Tecnológica",
                                               "INSTITUCION TECNICA PROFESIONAL" = "Institución Técnica Profesional",
                                               "INSTITUCIÓN TÉCNICA PROFESIONAL" = "Institución Técnica Profesional"))


#copy_sin_na$nivel_de_formacion %>% unique()

copy_sin_na$nivel_de_formacion <-  str_replace_all(copy_sin_na$nivel_de_formacion,
                                                   c("Universitaria" = "Universitario",
                                                   "Tecnológica" = "Tecnólogo",
                                                   "Tecnológico"  = "Tecnólogo"))

#names(copy_sin_na)

#copy_sin_na$metodologia %>% unique()

copy_sin_na$metodologia <-  str_replace_all(copy_sin_na$metodologia,
                                                   c("A distancia" = "Distancia",
                                                     "Distancia (virtual)" = "Distancia_virtual",
                                                     "Distancia (tradicional)"  = "Distancia"
                                                     ))



copy_sin_na$departamento_de_oferta_del_programa <- tolower(copy_sin_na$departamento_de_oferta_del_programa)

copy_sin_na$departamento_de_oferta_del_programa <- str_replace_all(copy_sin_na$departamento_de_oferta_del_programa, 
                                  c("á" = "a", "é" = "e", "í" = "i", "ó" = "o", "ú" = "u"))

#copy_sin_na$departamento_de_oferta_del_programa %>% unique()


copy_sin_na$departamento_de_oferta_del_programa <-  str_replace_all(copy_sin_na$departamento_de_oferta_del_programa,
                                            c("bogota d.c" = "bogota",
                                              "bogota d.c." = "bogota",
                                              "bogota, d.c." = "bogota",
                                              "bogota." = "bogota"
                                              ))

#names(copy_sin_na)

#copy_sin_na$sexo %>% unique()


serie <- base_total %>% filter(!is.na(admitidos) | !is.na(graduados)) %>%
  dplyr::select(c("ano", "inscritos", "graduados"))

