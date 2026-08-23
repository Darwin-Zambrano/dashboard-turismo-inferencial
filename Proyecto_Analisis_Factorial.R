# Proyecto: Análisis Factorial Sector Turismo - Modelo Espacio Temporal
# Autor Responsable: Darwin Arturo Zambrano Palacios

# 1. Instalar y cargar librerías necesarias
if(!require(psych)) install.packages("psych")
if(!require(MASS)) install.packages("MASS")
library(psych)
library(MASS)

# 2. Semilla de reproducibilidad
set.seed(2026)

# 3. Definir el Modelo Espacio-Temporal
balnearios <- c("Crucita", "San Clemente", "Puerto Cayo", "Puerto López", "Pedernales")
anios <- c(2025, 2026)

# 4. Generar la muestra poblacional base (N = 1000)
medias_esperadas <- c(4.2, 4.0, 4.3, 4.1, 4.0, 1.8, 2.1, 1.5, 1.9, 1.7)
cov_matrix <- matrix(0.15, nrow = 10, ncol = 10) 
cov_matrix[1:5, 1:5] <- 0.75 
cov_matrix[6:10, 6:10] <- 0.80 
diag(cov_matrix) <- 1 

datos_simulados <- mvrnorm(n = 1000, mu = medias_esperadas, Sigma = cov_matrix)

# Ajustar a escala Likert (1 a 5)
datos_simulados <- round(datos_simulados)
datos_simulados[datos_simulados < 1] <- 1
datos_simulados[datos_simulados > 5] <- 5

df_turismo <- as.data.frame(datos_simulados)
colnames(df_turismo) <- c("V1_SER", "V2_SER", "V3_SER", "V4_SER", "V5_SER",
                          "V6_INF", "V7_INF", "V8_INF", "V9_INF", "V10_INF")

# 5. Inyectar la dimensión Espacio-Temporal
df_turismo$Balneario <- sample(balnearios, 1000, replace = TRUE)
df_turismo$Anio <- sample(anios, 1000, replace = TRUE)

# Impresión de todos los resultados

cat("\n1. Prueba KMO\n")
print(KMO(df_turismo[, 1:10]))

cat("\n2. Prueba de Esfericidad de Bartlett\n")
# cortest.bartlett contrasta si la matriz de correlación es una matriz identidad
print(cortest.bartlett(cor(df_turismo[, 1:10]), n = 1000))

cat("\n3. Cargas Factoriales (Rotación Varimax)\n")
modelo_fa <- fa(df_turismo[, 1:10], nfactors = 2, rotate = "varimax", fm = "ml")
print(modelo_fa$loadings, cutoff = 0.0)

cat("\n4. Promedios Generales de las 1000 Encuestas\n")
print(colMeans(df_turismo[, 1:10]))

cat("\n5. Resultado Espacio-Temporal: Promedio de Infraestructura\n")
df_turismo$Promedio_INF <- rowMeans(df_turismo[, 6:10])
resultado_espacial <- aggregate(Promedio_INF ~ Balneario + Anio, data = df_turismo, FUN = mean)
print(resultado_espacial)
