# ---- Regresión MNL -----
result <- mnl(
  sum.tot, 
  rvar = "area_de_conocimiento", 
  evar = c(
    "sector_ies", "comparacion", "metodologia", "semestre", 
    "ano", "sexo", "demanda_real", "admitidos", "demanda_potencial"
  ), 
  lev = "ingenieria arquitectura urbanismo afines", 
  check = c("no_int", "stepwise-backward")
)
summary(result, sum_check = "confint")
