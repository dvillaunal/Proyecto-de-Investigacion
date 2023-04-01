require(tidyverse)
require(tseries)


tasa_abs <- tasa.abs.anio$tasa_absorcion %>%
  ts(., start = c(2007,1), frequency = 2)

for (i in 1:length(tasa_abs)) {
  if(tasa_abs[i]*10 > 100){
    tasa_abs[i] <- tasa_abs[i]/10
  }else{
    tasa_abs[i] <- tasa_abs[i]*10
  }
}

tasa_cob <- tasa.cob.anio$tasa_absorcion %>%
  ts(., start = c(2007,1), frequency = 2)

for (i in 1:length(tasa_cob)) {
  if(tasa_cob[i]*10 > 100){
    tasa_cob[i] <- tasa_cob[i]/10
  }else{
    tasa_cob[i] <- tasa_cob[i]*10
  }
}

plot(tasa_abs,type = "o", lwd = 2, col = "red", xlab = "Año, Semestre",
     ylab = "Porcentaje (%)", ylim = c(0,100))

lines(tasa_cob, type = "o", col = "blue", lwd = 2)
grid()

legend("top", c("Tasa de Absorción", "Tasa de Cobertura"),
       bty = "n", lty = 1, lwd = 2, col = c("red", "blue"))


data.frame(year = tasa.abs.anio$anio_semestre,
           tasa_abs, tasa_cob) %>%
  pivot_longer(cols = c("tasa_abs", "tasa_cob"),
               names_to = "tasas")
                        
                        
                        
                        