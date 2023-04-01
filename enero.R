library(tidyverse)
library(magrittr)

load("base_total.RData")
load('base_total0.RData')

#### Nuevas Bases de datos ####

## Ingresados-Admitidos:

copy <- base_total0[, -c(24, 25, 2, 7, 9, 11, 18, 2, 26)]

### valores lógicos si tienen na 
aux <- complete.cases(copy)

### base sin na
ins_admi <- copy[aux, ]

ins_admi$inscritos %<>% as.numeric()
ins_admi$admitidos %<>% as.numeric()

## Admitidos-Graduados:

copy <- base_total0[, -c(24, 25, 2, 7, 9, 11, 18, 2, 22)]

### valores lógicos si tienen na 
aux <- complete.cases(copy)

### base sin na
admi_gra <- copy[aux, ]

admi_gra$admitidos %<>% as.numeric()
admi_gra$graduados %<>% as.numeric()

#### Nueva Variable ####

aux <- ins_admi$admitidos/ins_admi$inscritos

ins_admi <- cbind(ins_admi, aux)

# ---- 

aux <- admi_gra$graduados/admi_gra$admitidos

admi_gra <- cbind(admi_gra, aux)

rm(copy, aux)

save(ins_admi, admi_gra, file= "nuevasBD.RData")

rm(list = ls())
