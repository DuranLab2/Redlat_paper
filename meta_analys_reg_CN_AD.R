library(metafor)
library(meta)
library(readxl)
library(grid)

# Leer los datos desde el archivo Excel
data <- read_excel("Meta_cn_x_an.xlsx")

#data <- data[data$Comparison == 'CN X AD', ]

convert_auc_to_or <- function(auc) {
  # Verificar que el AUC esté en el rango válido
  if (auc <= 0.5 || auc >= 1) {
    stop("El AUC debe estar entre 0.5 y 1 (exclusivo).")
  }
  
  d <- sqrt(2) * qnorm(auc)
  
  log_OR <- (d * pi) / sqrt(3)
  
  OR <- exp(log_OR)
  
  return(OR)
}

data$OR <- sapply(data$Mean_AUC, convert_auc_to_or)

transform_sem_to_logor <- function(sem_auc, auc) {
  # Calcular Cohen's d
  d <- sqrt(2) * qnorm(auc)
  
  # Calcular el error estándar del log Odds Ratio
  sem_logor <- (sem_auc * pi) / sqrt(3)
  
  return(sem_logor)
}

# Aplicar la transformación del SEM para cada fila
data$SE <- mapply(transform_sem_to_logor, data$SEM_AUC, data$Mean_AUC)


data$log_OR <- log(data$OR);


print(data[, c("Country", "OR", "SE")])



# Meta-análisis con el paquete 'metafor'
meta_result <-  metagen(TE = log_OR, seTE = SE, studlab = Country, data = data, sm = "OR", method.tau = "REML")


summary(meta_result)

pdf("forest_plot_CN_AD.pdf", width = 20, height = 25) 

forest(meta_result, 
       colgap = unit(30, "mm"), # Ajuste del espacio entre columnas
       cex = 0.75,       
       layout = "BMJ", xlim = c(0.5, 100)) 

dev.off()
