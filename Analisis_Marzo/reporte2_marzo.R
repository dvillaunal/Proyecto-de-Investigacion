library(tidyverse)
library(magrittr)
library(reshape2)

sum.tot %<>% as_tibble()

sum.tot %>% head()
num_vars <- sum.tot %>% select(-c(11,12)) %>% select_if(is.numeric)
correlation_matrix <- cor(num_vars)

melted_matrix <- melt(correlation_matrix)
ggplot(melted_matrix, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", midpoint = 0.85, limit = c(0.70,1),
                       name="Correlation\nCoefficient") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1))
pca <- prcomp(num_vars, center = TRUE, scale. = TRUE)
summary(pca)

plot(pca)

variance <- pca$sdev^2/sum(pca$sdev^2)
df <- data.frame(PC = paste0("PC", 1:ncol(pca$x)), Variance = variance)
ggplot(df, aes(x=PC, y=Variance)) + 
  geom_bar(stat="identity", fill="steelblue") +
  ggtitle("Porcentaje de varianza explicada por cada componente principal") +
  xlab("Componente Principal") +
  ylab("Porcentaje de varianza explicada")



# Bases de datos a compartir  ---------------------------------------------
require(writexl)

sum.tot0 %>% dplyr::select(c(1:11)) %>%
  write_xlsx(.,"BD_agrupada.xlsx")

sum.admi %>% write_xlsx(., "BD_AdmitidosAgrupada.xlsx")
sum.grad %>% write_xlsx(., "BD_GraduadosAgrupada.xlsx")
sum.ins %>% write_xlsx(., "BD_InscritosAgrupada.xlsx")

insNNadmi %>% write_xlsx(., "BDmerge_InscritosAdmitidos.xlsx")
admiNNgrad %>% write_xlsx(., "BDmerge_AdmitidosGraduados.xlsx")


