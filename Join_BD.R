#load("pro_inv1.RData")

library(tidyverse)
library(magrittr)
library(janitor)


## para url1
names(url1) <- as.matrix(url1[9, ])
url1 = url1[10:nrow(url1), ]


## para url2

names(url2) <- as.matrix(url2[9, ])
url2 = url2[10:nrow(url2), ]


## para url3

names(url3) <- as.matrix(url3[9, ])
url3 = url3[10:nrow(url3), ]

## para url4

names(url4) <- as.matrix(url4[9, ])
url4 = url4[10:nrow(url4), ]


## para url5

names(url5) <- as.matrix(url5[8, ])
url5 = url5[9:nrow(url5), ]


## para url6

names(url6) <- as.matrix(url6[8, ])
url6 = url6[9:nrow(url6), ]



## para url7

names(url7) <- as.matrix(url7[8, ])
url7 = url7[9:nrow(url7), ]



## para url8

names(url8) <- as.matrix(url8[8, ])
url8 = url8[9:nrow(url8), ]


## para url9

names(url9) <- as.matrix(url9[8, ])
url9 <- url9[9:nrow(url9), ]



## para url10

names(url10) <- as.matrix(url10[8, ])
url10 <- url10[9:nrow(url10), ]


## para url11

names(url11) <- as.matrix(url11[6, ])
url11 <- url11[7:nrow(url11), ]


## para url12

names(url12) <- as.matrix(url12[7, ])
url12 <- url12[8:nrow(url12), ]



## para url13

names(url13) <- as.matrix(url13[7, ])
url13 <- url13[8:nrow(url13), ]


## para url14

names(url14) <- as.matrix(url14[8, ])
url14 <- url14[9:nrow(url14), ]


## para url15

names(url15) <- as.matrix(url15[7, ])
url15 <- url15[8:nrow(url15), ]


## para url16

names(url16) <- as.matrix(url16[7, ])
url16 <- url16[8:nrow(url16), ]




## para url17

# names(url17) <- as.matrix(url17[7, ])
# url17 <- url17[8:nrow(url17), ]
# 


## para url18

#names(url18) <- as.matrix(url16[7, ])
#url16 <- url16[8:nrow(url16), ]



## para url19

names(url19) <- as.matrix(url19[7, ])
url19 <- url19[8:nrow(url19), ]



## para url20

names(url20) <- as.matrix(url20[6, ])
url20 <- url20[7:nrow(url20), ]



## para url21

names(url21) <- as.matrix(url21[6, ])
url21 <- url21[7:nrow(url21), ]




## para url22

names(url22) <- as.matrix(url22[6, ])
url22 <- url22[7:nrow(url22), ]


## para url23

names(url23) <- as.matrix(url23[6, ])
url23 <- url23[7:nrow(url23), ]



## para url25

names(url25) <- as.matrix(url25[6, ])
url25 <- url25[7:nrow(url25), ]


## para url26

names(url26) <- as.matrix(url26[6, ])
url26 <- url26[7:nrow(url26), ]


## para url27

names(url27) <- as.matrix(url27[6, ])
url27 <- url27[7:nrow(url27), ]


## para url28

names(url28) <- as.matrix(url28[6, ])
url28 <- url28[7:nrow(url28), ]



## para url29

names(url29) <- as.matrix(url29[8, ])
url29 <- url29[9:nrow(url29), ]


## para url31

names(url31) <- as.matrix(url31[6, ])
url31 <- url31[7:nrow(url31), ]



## para url32

names(url32) <- as.matrix(url32[6, ])
url32 <- url32[7:nrow(url32), ]



## para url33

names(url33) <- as.matrix(url33[6, ])
url33 <- url33[7:nrow(url33), ]



## para url34

names(url34) <- as.matrix(url34[6, ])
url34 <- url34[7:nrow(url34), ]



## para url35

names(url35) <- as.matrix(url35[6, ])
url35 <- url35[7:nrow(url35), ]



## para url37

names(url37) <- as.matrix(url37[6, ])
url37 <- url37[7:nrow(url37), ]


## para url38

names(url38) <- as.matrix(url38[6, ])
url38 <- url38[7:nrow(url38), ]


## para url39

names(url39) <- as.matrix(url39[6, ])
url39 <- url39[7:nrow(url39), ]


## para url40

names(url40) <- as.matrix(url40[6, ])
url40 <- url40[7:nrow(url40), ]



## para url41

names(url41) <- as.matrix(url41[7, ])
url41 <- url41[8:nrow(url41), ]


#### UNION PARA LAS BASES DE DATOS CON 33 COLUMNAS 

# nombres_bases <- list(url1, url2, url3, url4, url5, url6, url7,
#                    url8, url9, url10, url11, url12, url13, url14,
#                    url15, url16, url17, url18, url19, url20, url21,
#                    url22, url23, url24, url25, url26, url27, url28,
#                    url29, url30, url31, url32, url33, url34, url35,
#                    url36, url37, url38, url39, url40, url41)
# 
# lapply(nombres_bases, clean_names)
# 
# 
# 
# difere_cantida_vari <- c(33, 41, 39, 34, 104, 62, 106, 41)



# for (i in nombres_bases){
#   i <- clean_names(i)
#  }


# merge_aux <- function(...) merge (..., all=TRUE)
# datos_4<-Reduce(merge_aux, list(url11, url19, url20, url21, url22, url23, url25, url26,
#                                 url27, url28, url29, url31, url32, url33, url34, url35,
#                                 url37, url38, url39))

url1 %<>% clean_names()
url2 %<>% clean_names()
url3 %<>% clean_names()
url4 %<>% clean_names()
url5 %<>% clean_names()
url6 %<>% clean_names()
url7 %<>% clean_names()
url8 %<>% clean_names()
url9 %<>% clean_names()
url10 %<>% clean_names()
url11 %<>% clean_names()
url12 %<>% clean_names()
url13 %<>% clean_names()
url14 %<>% clean_names()
url15 %<>% clean_names()
url16 %<>% clean_names()
url17 %<>% clean_names()
url18 %<>% clean_names()
url19 %<>% clean_names()
url20 %<>% clean_names()
url21 %<>% clean_names()
url22 %<>% clean_names()
url23 %<>% clean_names()
url24 %<>% clean_names()
url25 %<>% clean_names()
url26 %<>% clean_names()
url27 %<>% clean_names()
url28 %<>% clean_names()
url29 %<>% clean_names()
url30 %<>% clean_names()
url31 %<>% clean_names()
url32 %<>% clean_names()
url33 %<>% clean_names()
url34 %<>% clean_names()
url35 %<>% clean_names()
url36 %<>% clean_names()
url37 %<>% clean_names()
url38 %<>% clean_names()
url39 %<>% clean_names()
url40 %<>% clean_names()
url41 %<>% clean_names()


# base_33col <- rbind(url11, url19, url20, url21, url22, url23, url25, url26,
#                     url27, url28, url29, url31, url32, url33, url34, url35,
#                     url37, url38, url39, url40)
# 

################# notas

# url17 posible base de graduados menor a 2013
# url18 Identificar a que corresponde esa base datos (encabezado) # matriculados hasta el 2013
# url19 matriculados 2014 
# url20 matriculados 2015
# url30 identificar de que trata (problemas de encabezado)
# url36 identificar de que trata (problemas de encabezado)
# url41 revisar si faltan datos, asociada al año 2018 de los inscritos (no se sabe si realmente es)
# url24 indentificar de que trata.



#################

# b2021 <- c("url3", "url5", "url1", "url2", "url4")
# 
# b2020 <- c( "url9", "url10", "url8",  "url7","url6")
# 
# b2019 <- c("url14", "url16", "url13", "url12", "url15") ##  posibles problemas url13 url14
# 
# b2018 <- c("url41", "url35", "url23", "url29", "url11") # problemas url41
# 
# b2017 <- c("url40", "url34", "url22", "url28", "Falta_graduados_2017, pero esta en url17")
# 
# b2016 <- c("url39", "url33",  "url21", "url27", "Falta_graduados_2016, pero esta en url17")
# 
# b2015 <- c("url38", "url32", "url20", "url26", "Falta_graduados_2015, pero esta en url17")
# 
# b2014 <- c("url37", "url31", "url19", "url25", "Falta_graduados_2014, pero esta en url17")

dimensiones_iscritos <- c(dim(url37)[2], dim(url38)[2], dim(url39)[2], dim(url40)[2],
                          dim(url41)[2], dim(url9)[2],dim(url3)[2], dim(url14)[2]) 


variables_interes <- c(2,3,6,7,8,9,10,13,14,16,18,20,21,23,24,30,31,35,37,38,39)


names(url3)[26:39]
names(url39)[25:33]

variables_dif <- c("id_cine_campo_amplio", "desc_cine_campo_amplio", "id_cine_campo_especifico",
                   "desc_cine_campo_especifico", "id_cine_codigo_detallado" ,
                   "desc_cine_codigo_detallado") 


library(tidyverse)
url3_f <- url3 %>% dplyr::select(!variables_dif)
url9_f <- url9 %>% dplyr::select(!variables_dif)
url14_f <- url14 %>% dplyr::select(!variables_dif)

#lista <- c("url37", "url38", "url39", "url40", "url41")


names(url37) = names(url3_f)
names(url38) = names(url3_f)
names(url39) = names(url3_f)
names(url40) = names(url3_f)
names(url41) = names(url3_f)
names(url9_f) = names(url3_f)
names(url14_f) = names(url3_f)

base_inscritos_2014_2021 <- rbind(url37, url38, url39, url40, url41, url14_f,
                                  url9_f, url3_f)




ins <- c(2,3,6,7,8,9,10,13,14,16,18,20,21,23,24,35,37,38,39)

nom.ins <- names(url3[,ins]) %>% tolower()

base_inscritos_2014_2021_f <- base_inscritos_2014_2021 %>% dplyr::select(nom.ins)



############ unión admitidos 


dimensiones_abmin <- c(dim(url31)[2], dim(url32)[2], dim(url33)[2], dim(url34)[2],
                       dim(url35)[2], dim(url5)[2],dim(url10)[2], dim(url16)[2]) 






dimensiones_abmin

url5_f <- url5 %>% dplyr::select(!variables_dif)
url10_f <- url10 %>% dplyr::select(!variables_dif)
url16_f <- url16 %>% dplyr::select(!variables_dif)

dim(url5_f)[2]
dim(url10_f)[2]
dim(url16_f)[2]




names(url31) = names(url5_f)
names(url32) = names(url5_f)
names(url33) = names(url5_f)
names(url34) = names(url5_f)
names(url35) = names(url5_f)
names(url10_f) = names(url5_f)
names(url16_f) = names(url5_f)




base_abmitidos_2014_2021 <- rbind(url31, url32, url33, url34, url35, url5_f, url10_f, url16_f)

############################## Union matriculados ############

dimensiones_matri <- c(dim(url19)[2], dim(url20)[2], dim(url21)[2], dim(url22)[2],
                       dim(url23)[2], dim(url1)[2],dim(url8)[2], dim(url13)[2]) 

dimensiones_matri

# variables adicionales que presentaba url1() matriculados 2021

url1<- url1 %>% dplyr::select(!c(ies_acreditada, programa_acreditado))

names(url1) == names(url8)

url1_f <- url1 %>% dplyr::select(!variables_dif)
url8_f <- url8 %>% dplyr::select(!variables_dif)
url13_f <- url13 %>% dplyr::select(!variables_dif)



names(url19) = names(url1_f)
names(url20) = names(url1_f)
names(url21) = names(url1_f)
names(url22) = names(url1_f)
names(url23) = names(url1_f)
names(url8_f) = names(url1_f)
names(url13_f) = names(url1_f)




base_matriculados_2014_2021 <- rbind(url19, url20, url21, url22, url23, url1_f, url8_f, url13_f)


################### union matriculados primer curso 


dimensiones_matri_primer_c <- c(dim(url25)[2], dim(url26)[2], dim(url27)[2], dim(url28)[2],
                                dim(url29)[2], dim(url2)[2],dim(url7)[2], dim(url12)[2]) 

dimensiones_matri_primer_c

url2<- url2 %>% dplyr::select(!c(ies_acreditada, programa_acreditado))


names(url7)

url2_f <- url2 %>% dplyr::select(!variables_dif)
url7_f <- url7 %>% dplyr::select(!variables_dif)
url12_f <- url12 %>% dplyr::select(!variables_dif)


url25 <- url25 %>% dplyr::select(!genero)
url26 <- url26 %>% dplyr::select(!genero)
url27 <- url27 %>% dplyr::select(!sexo)
url28 <- url28 %>% dplyr::select(!sexo)
url29 <- url29 %>% dplyr::select(!sexo)
url2_f <- url2_f %>% dplyr::select(!sexo)
url7_f <- url7_f %>% dplyr::select(!sexo)




names(url25) = names(url2_f)
names(url26) = names(url2_f)
names(url27) = names(url2_f)
names(url28) = names(url2_f)
names(url29) = names(url2_f)
names(url7_f) = names(url2_f)
names(url12_f) = names(url2_f)




base_matriculados_pri_curs_2014_2021 <- rbind(url25, url26, url27, url28,
                                              url29, url2_f, url7_f, url12_f)


########################## Unión Graduados


dimensiones_graduados <- c(dim(url17)[2], dim(url11)[2], dim(url15)[2], dim(url4)[2],
                           dim(url6)[2]) 

dimensiones_graduados

names(url4)
url4 <- url4 %>% dplyr::select(!c(ies_acreditada, programa_acreditado))
url17 <- url17%>% dplyr::select(!x1)

names(url6)

url6_f <- url6 %>% dplyr::select(!c("id_cine_campo_amplio_desc", "cine_campo_amplio","id_cine_campo_especifico",
                                    "desc_cine_campo_especifico", "id_cine_codigo_detallado",
                                    "desc_cine_codigo_detallado"))

url4_f <- url4 %>% dplyr::select(!variables_dif)
url15_f <- url15 %>% dplyr::select(!variables_dif)

names(url17) = names(url6_f)
names(url11) = names(url6_f)
names(url15_f) = names(url6_f)
names(url4_f) = names(url6_f)


base_graduados_2000_2021 <- rbind(url17, url11, url15_f, url6_f, url4_f)


#-----------------------------------------------------

columnas <- c()

for (i in 1:41) {
  t <- paste0("dim(url",i,")[2]")
  columnas[i] <- eval(parse(text=t))
}

dic0 <- data.frame(titulo,urls)

dic0 <- cbind(dic0,columnas)

dic0[grepl("inscritos", tolower(titulo)),]

titulo

url9 %>% names()

dim(url9)

i.ins <- dic0[grepl("inscritos", tolower(titulo)),]$url %>% parse_number()

ins <- c(2,3,6,7,8,9,10,13,14,16,18,20,21,23,24,35,37,38,39)

nom.ins <- names(url9[,ins]) %>% tolower()
nom.ins

#-----------------------------------------------------


#### Resumen ####

# base_inscritos_2014_2021 & base_inscritos_2014_2021_f = Inscritos.
# base_abmitidos_2014_2021 = Admitidos.
# base_matriculados_2014_2021 = Matriculados.
# base_matriculados_pri_curs_2014_2021 = Matriculados en primer curso.

# Ahora se organizan las bases de datos eliminado las variables omitidas
# (documento en google drive)


base2_inscritos_2014_2021 <- base_inscritos_2014_2021 %>%
  filter(id_nivel_academico == "1") %>% 
  dplyr::select(nom.ins[-c(5,16)])


base2_abmitidos_2014_2021 <- base_abmitidos_2014_2021 %>%
  filter(id_nivel_academico == "1") %>%
  dplyr::select(c(2,3,6,7,9,10,13,14,
                  18,20,21,23,24,29,31,32,33)) 


base2_matriculados_2014_2021 <- base_matriculados_2014_2021 %>%
  filter(id_nivel_academico == "1") %>%
  dplyr::select(c(2,3,6,7,9,10,13,14,
                  18,20,21,23,24,29,31,32,33))


base2_matriculados_pri_curs_2014_2021 <- base_matriculados_pri_curs_2014_2021 %>%
  filter(id_nivel_academico == "1") %>%
  dplyr::select(c(2,3,6,7,9,10,13,14,
                  18,20,21,23,24,29,30,31,32))


base2_graduados_2000_2021 <- base_graduados_2000_2021 %>%
  filter(id_nivel_academico == "1") %>%
  dplyr::select(c(2,3,6,7,9,10,13,14,
                  18,20,21,23,24,29,31,32,33))



b1 <- base2_inscritos_2014_2021 %>% names()
b2 <- base2_abmitidos_2014_2021 %>%  names()
b3 <- base2_matriculados_2014_2021 %>% names()
b4 <- base2_matriculados_pri_curs_2014_2021 %>% names()
b5 <- base2_graduados_2000_2021 %>%  names()

matrix(c(b1,b2,b3,b4,b5), ncol = 5, nrow = 17,
       dimnames = list(c(), c("INSCRITOS","ADMITIDOS",
                              "MATRICULADOS", "MATRI-1ER CURSO", "GRADUADOS")))



new.nombs <- c("code_ies", "nom_ies", "sector", "caracter",
               "code_dep", "nom_dep", "code_prog", "prog_aca",
               "nivel_form", "metodo", "area", "nucleo",
               "nbc", "sexo", "ano", "sem", "conteo")

b1 <- base2_inscritos_2014_2021
b2 <- base2_abmitidos_2014_2021
b3 <- base2_matriculados_2014_2021
b4 <- base2_matriculados_pri_curs_2014_2021
b5 <- base2_graduados_2000_2021

# Renombramiento
names(b1) <- new.nombs
names(b2) <- new.nombs
names(b3) <- new.nombs
names(b4) <- new.nombs
names(b5) <- new.nombs

# exportación:
# library("writexl")
# 
# write_xlsx(b1, "base_inscritos_2014_2021.xlsx")
# write_xlsx(b2, "base_abmitidos_2014_2021.xlsx")
# write_xlsx(b3, "base_matriculados_2014_2021.xlsx")
# write_xlsx(b4, "base_matriculados_pri_curs_2014_2021.xlsx")
# write_xlsx(b5, "base_graduados_2000_2021.xlsx")

# rm(b1,b2,b3,b4,b5)

#### --------------------------------- Parte 2 -------------------------- ####


d <- url36$nivel_de_formacion %>% unique()

############## url18 

columnas <- names(url18)

patron <- "(total_.)"

t = grep(patron, columnas)

url18_1 <- url18[, -c(t, 1)] %>% filter(nivel_de_formacion %in% d[-c(2:4)])


#####################33 url24

columnas1 <- names(url24)

patron1 <- "(total_.)"

t1 = grep(patron1, columnas1)

url24_1 <- url24[, -c(t1, 1)] %>% filter(nivel_de_formacion %in% d[-c(2:4)])


#####################33 url30

columnas2 <- names(url30)

patron2 <- "(total_.)"

t2 = grep(patron2, columnas2)

url30_1 <- url30[, -c(t2, 1)] %>% filter(nivel_de_formacion %in% d[-c(2:4)])

#####################33 url36

columnas3 <- names(url36)

patron3 <- "(total_.)"

t3 = grep(patron3, columnas3)

url36_1 <- url36[, -c(t3, 1)] %>% filter(nivel_de_formacion %in% d[-c(2:4)])

#### Pivoteo de las BD* ####

names(url18_1)[20:75] 


url36_1 %>% names()


#### "Estudiantes inscritos hasta 2013"

matriculados_2k_2013 <- url18_1 %>%
  pivot_longer(
    cols = names(url18_1)[20:ncol(url18_1)],
    names_to = "aux",
    #names_prefix = "hombre_",
    values_to = "matriculados",
    values_drop_na = TRUE
  )

matriculados_1c_2k_2013 <- url24_1 %>%
  pivot_longer(
    cols = names(url24_1)[20:ncol(url24_1)],
    names_to = "aux",
    #names_prefix = "hombre_",
    values_to = "matriculados",
    values_drop_na = TRUE
  )


admitidos_2007_2013 <- url30_1 %>%
  pivot_longer(
    cols = names(url30_1)[20:ncol(url30_1)],
    names_to = "aux",
    #names_prefix = "hombre_",
    values_to = "admitidos",
    values_drop_na = TRUE
  )


inscritos_2k_2013 <- url36_1 %>%
  pivot_longer(
    cols = names(url36_1)[22:77],
    names_to = "aux",
    #names_prefix = "hombre_",
    values_to = "inscritos",
    values_drop_na = TRUE
  )

inscritos_2k_2013 %<>% filter(nivel_academico == "PREGRADO")

no <-c ("matriculados_2k_2013","matriculados_1c_2k_2013",
        "admitidos_2007_2013", "inscritos_2k_2013")
new <- paste0("url", c(18,24,30,36),"_2")

c1 <- c(rep(19,3), 21)
c2 <- c(rep(21,3), 23)

for (i in 1:4) {
  p0 <- parse(text = paste0(no[i],
                            "[,",c1[i]+1,"]")) %>%
    eval() %>% pull()
  
  p1 <- grepl("hombre",p0)*1
  
  sexo <- p1 %>% factor(.,levels = c(0,1),
                        labels = c("mujer","hombre"))
  
  ano <- p0 %>% parse_number()
  
  semestre <- p0 %>% str_sub(.,start = 12) %>%
    parse_number()
  
  a <- paste0("cbind(",no[i],"[,1:",c1[i],
              "], sexo,ano,semestre,"
              ,no[i],"[,",c2[i],"])") %>% parse(text=.) %>%
    eval()
  
  assign(new[i], a)
  
}

#save(list = ls(),file = "plan_decem.RData")

#rm(list = ls())

#### --------------------------------- Parte 3 -------------------------- ####


######################## union de las bases desde el 2000 hasta el 2021 


#### ADMITIDOS 2000 a 2021 final sin filtros


dim(url30_2)[2]
dim(url36_2)[2]
dim(url24_2)[2]
dim(url18_2)[2]



macht_variables <- c("codigo_de_la_institucion", "institucion_de_educacion_superior_ies", "principal_o_seccional",
  "sector_ies",  "caracter_ies", "codigo_del_departamento_ies", "departamento_de_domicilio_de_la_ies",
  "codigo_del_municipio_ies", "municipio_de_domicilio_de_la_ies", "codigo_snies_del_programa",
  "programa_academico", "nivel_de_formacion", "metodologia", "area_de_conocimiento",
  "nucleo_basico_del_conocimiento_nbc", "departamento_de_oferta_del_programa",
  "codigo_del_municipio_programa", "municipio_de_oferta_del_programa",
  "sexo", "ano", "semestre",  "admitidos")

colnames(url30_2)[13] <- "metodologia"

auxli <- base_abmitidos_2014_2021[, macht_variables]

url30_2 <- url30_2[, macht_variables]

names(auxli) == names(url30_2)

base_admitidos_2000_2021 <- rbind(auxli, url30_2)


#################### para inscritos desde el 2000 hasta el 2021 
macht_variables1 <- c("codigo_de_la_institucion", "institucion_de_educacion_superior_ies", "principal_o_seccional",
                      
                      "sector_ies",  "caracter_ies", "codigo_del_departamento_ies", "departamento_de_domicilio_de_la_ies",
                     "codigo_del_municipio_ies", "municipio_de_domicilio_de_la_ies", "codigo_snies_del_programa",
                     "programa_academico", "nivel_de_formacion", "metodologia", "area_de_conocimiento",
                     "nucleo_basico_del_conocimiento_nbc", "departamento_de_oferta_del_programa",
                     "codigo_del_municipio_programa", "municipio_de_oferta_del_programa",
                     "sexo", "ano", "semestre",  "inscritos")


names(url36_2)

names(base_inscritos_2014_2021)
colnames(url36_2)[15] <- "metodologia"
colnames(base_inscritos_2014_2021)[6] <- "sector_ies"

auxili1 <- base_inscritos_2014_2021[, macht_variables1]

url36_2 <- url36_2[, macht_variables1]

names(auxili1) == names(url36_2)

base_inscritos_2000_2021 <- rbind(auxili1, url36_2)


#################### para matriculados desde el 2000 hasta el 2021 

macht_variables2 <- c("codigo_de_la_institucion", "institucion_de_educacion_superior_ies", "principal_o_seccional",
                      "sector_ies",  "caracter_ies", "codigo_del_departamento_ies", "departamento_de_domicilio_de_la_ies",
                      "codigo_del_municipio_ies", "municipio_de_domicilio_de_la_ies", "codigo_snies_del_programa",
                      "programa_academico", "nivel_de_formacion", "metodologia", "area_de_conocimiento",
                      "nucleo_basico_del_conocimiento_nbc", "departamento_de_oferta_del_programa",
                      "codigo_del_municipio_programa", "municipio_de_oferta_del_programa",
                      "sexo", "ano", "semestre",  "matriculados")


names(url24_2)

names(base_matriculados_2014_2021)

colnames(url24_2)[13] <- "metodologia"

auxili2 <- base_matriculados_2014_2021[, macht_variables2]

url24_2 <- url24_2[, macht_variables2]

names(auxili2) == names(url24_2)

base_matriculados_2000_2021 <- rbind(auxili2, url24_2)

#################### para matriculados 1er curso desde el 2000 hasta el 2021 
macht_variables3 <- c("codigo_de_la_institucion", "institucion_de_educacion_superior_ies", "principal_o_seccional",
                      "sector_ies",  "caracter_ies", "codigo_del_departamento_ies", "departamento_de_domicilio_de_la_ies",
                      "codigo_del_municipio_ies", "municipio_de_domicilio_de_la_ies", "codigo_snies_del_programa",
                      "programa_academico", "nivel_de_formacion", "metodologia", "area_de_conocimiento",
                      "nucleo_basico_del_conocimiento_nbc", "departamento_de_oferta_del_programa",
                      "codigo_del_municipio_programa", "municipio_de_oferta_del_programa",
                      "id_sexo", "ano", "semestre",  "primer_curso")


names(url18_2)

colnames(url18_2)[13] <- "metodologia"

auxili2 <- base_matriculados_pri_curs_2014_2021[, macht_variables3]

url18_2 <- url18_2[, macht_variables2]

table(names(auxili2) == names(url18_2))

names(auxili2)[19] <- c("sexo")

names(url18_2)[22] <- "primer_curso"

base_matriculados_pri_curs_2000_2021 <- rbind(auxili2, url18_2)

#### Filtrado de las BD ####

base_graduados_2000_2021 %<>% filter(nivel_academico == "PREGRADO")

base_graduados_2000_2021 <- base_graduados_2000_2021[,c(macht_variables[-22],
                                                        "graduados")]

# save(base_admitidos_2000_2021,
#      base_graduados_2000_2021,
#      base_inscritos_2000_2021,
#      base_matriculados_2000_2021,
#      base_matriculados_pri_curs_2000_2021,
#      file = "BDs_Finales.RData")
# 
# rm(list = ls())
# load("BDs_Finales.RData")

merge_aux <- function(...) merge (..., all=TRUE)

base_total<-Reduce(merge_aux, list(base_inscritos_2000_2021,
                                   base_admitidos_2000_2021,
                                   base_matriculados_2000_2021,
                                   base_matriculados_pri_curs_2000_2021,
                                   base_graduados_2000_2021))


base_total$caracter_ies  <- str_replace_all(base_total$caracter_ies,
                                             c("UNIVERSIDAD"  = "Universidad", 
                                               "INSTITUCION UNIVERSITARIA/ESCUELA TECNOLOGICA"="Institución Universitaria/Escuela Tecnológica",
                                               "INSTITUCIÓN UNIVERSITARIA/ESCUELA TECNOLÓGICA" = "Institución Universitaria/Escuela Tecnológica",
                                               "INSTITUCION TECNOLOGICA" = "Institución Tecnológica",
                                               "INSTITUCIÓN TECNOLÓGICA" = "Institución Tecnológica",
                                               "INSTITUCION TECNICA PROFESIONAL" = "Institución Técnica Profesional",
                                               "INSTITUCIÓN TÉCNICA PROFESIONAL" = "Institución Técnica Profesional"))



base_total$nivel_de_formacion <-  str_replace_all(base_total$nivel_de_formacion,
                                                   c("Universitaria" = "Universitario",
                                                     "Tecnológica" = "Tecnólogo",
                                                     "Tecnológico"  = "Tecnólogo"))

base_total$metodologia <-  str_replace_all(base_total$metodologia,
                                            c("A distancia" = "Distancia",
                                              "Distancia (virtual)" = "Distancia_virtual",
                                              "Distancia (tradicional)"  = "Distancia"
                                            ))



base_total$departamento_de_oferta_del_programa <- tolower(base_total$departamento_de_oferta_del_programa)

base_total$departamento_de_oferta_del_programa <- str_replace_all(base_total$departamento_de_oferta_del_programa, 
                                                                   c("á" = "a", "é" = "e", "í" = "i", "ó" = "o", "ú" = "u"))


base_total$departamento_de_oferta_del_programa <-  str_replace_all(base_total$departamento_de_oferta_del_programa,
                                                                    c("bogota d.c" = "bogota",
                                                                      "bogota d.c." = "bogota",
                                                                      "bogota, d.c." = "bogota",
                                                                      "bogota." = "bogota"
                                                                    ))

base_total0 <- base_total
base_total <- base_total0[, -c(2, 7, 9, 11, 18, 2)]
base_total$inscritos %<>% as.numeric()
base_total$admitidos %<>% as.numeric()
base_total$graduados %<>% as.numeric()
#save(base_total,file="base_total.RData")
#save(base_total0,file="base_total0.RData")
# library("writexl")



