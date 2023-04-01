serie <- base_total %>%
  filter(!is.na(admitidos) | !is.na(graduados)) %>%
  dplyr::select(c("ano", "admitidos", "graduados"))

serie1 <- serie %>% group_by(ano) %>%
  summarise(.,admitidos = sum(admitidos, na.rm= T),
            graduados = sum(graduados,  na.rm= T)) %>%
  pivot_longer(cols = c("admitidos", "graduados"),
               names_to = "Estado_Estudiante",
               values_to = "total")


#save(serie, serie1, file="series.RData")
