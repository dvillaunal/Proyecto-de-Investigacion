#### Cargando los paquetes ####
# install.packages('httr')
# install.packages('XML')
# install.packages("scrapeR")
# install.packages("RCurl")
#install.packages("readxlsb")
library(rvest)
library(tidyverse)
library(magrittr)
library(httr)
library(XML)
library(scrapeR)
library(RCurl)
library(openxlsx)
library(readxlsb)
library(readxl)
#---


# Cargando la página web en R.

url<-"https://snies.mineducacion.gov.co/portal/ESTADISTICAS/Bases-consolidadas/"

# making http request
resource<-httr::GET(url)


# parsing data to html format
parse<-htmlParse(resource)

#---

nodeset <- c()

for (i in 1:184) {
  nodeset[i] <- getNodeSet(parse,"//a")[[i]] %>% as(.,'character')
}


  
estu <- nodeset[str_detect(nodeset,
                           regex("title=\"Ir a Estudiantes", dotall = T))]

href <- str_extract(estu, regex("(?<=href=[\"\']).*?(?=[\"\'])"))

links <- paste0("https://snies.mineducacion.gov.co/1778/",href %>% unique())

titulo <- str_extract(estu, regex("(?<=title=[\"\']).*?(?=[\"\'])")) %>%
  unique()


titulo %<>% substring(., first = 6) 


urls <- paste0("url",1:length(links))

path <- "C:/Users/danie/OneDrive - Universidad Nacional de Colombia/Documentos (OneDrive-UNAL)/pro_inv/Proyecto-de-Investigacion/"

ext <- href %>% unique() %>% substring(., first = 24) 

options(download.file.method="curl", download.file.extra="-k -L")

for (i in 1:length(urls)) {
  download.file(links[i],paste0(path,urls[i],ext[i]),mode = "wb")
}

# Lectura de todas las bases de datos:

for (i in 1:length(urls)) {
  assign(urls[i], read_excel(paste0(urls[i],".xlsx")))
}


# Lectura del total de dimensiones:

ldim <- c()

for (i in 1:41) {
  t <- paste0("dim(url",i,")[1]")
  ldim[i] <- eval(parse(text=t))
}

sum(ldim)

dim(url1)[1]


# Save your entire workspace


save.image(file = "pro_inv1.RData")


# Verificación de las bases de datos:
for (i in 1:41) {
  t <- paste0("dim(url",i,")")
  print(paste("url", i)) 
  print(eval(parse(text=t)))
  print("------------")
  t <- paste0("dim(na.omit(url",i,"))")
  print(eval(parse(text=t)))
  print("===============")
}

dic0 <- data.frame(titulo,urls)




