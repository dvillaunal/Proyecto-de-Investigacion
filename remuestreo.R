library(devtools)
#install_github("DFJL/SamplingUtil")
library(SamplingUtil)
#install.packages("pps")
#install.packages("survey")
library(pps)
library(survey)
library(readxl)
library(tidyverse)
library(janitor)
library(magrittr)

en <- read_excel("Encuesta 23 y 24 de octubre  (respuestas).xlsx")

set.seed(123)
# f <- sample(1:160,160,replace = F)
# c <- sample(1:15,15,replace = F)


d <- matrix(ncol = 15,nrow = 160)

for (j in 1:160){
  i <- sample(1:15,1)
  d[j,i] <- en[sample(1:160,1),i] %>% pull()
}


m <- d %>% as.data.frame() %>% na.exclude()


names(m) <- names(en)

ifelse(as.numeric(en$`Marca temporal`) == "1666518121.207",1,0)


en[159,] %>% as.matrix()


