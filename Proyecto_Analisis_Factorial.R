# Proyecto: Análisis Factorial Sector Turismo
# Generación de 1000 datos sintéticos y validación de modelo
# Autor Responsable: Darwin Arturo Zambrano Palacios

# 1. Instalar y cargar librerías necesarias
# Usamos MASS para la simulación de datos y psych para el análisis factorial
if(!require(psych)) install.packages("psych")
if(!require(MASS)) install.packages("MASS")
library(psych)
library(MASS)

# 2. Semilla de reproducibilidad
# set.seed (función para fijar una semilla aleatoria) garantiza que cada vez 
# que el docente corra el código, salgan exactamente los mismos 1000 datos.
set.seed(2026)

# 3. Definir los promedios esperados (medias) para forzar los resultados del dashboard
# Servicios (V1 a V5) altos ~ 4.0, Infraestructura (V6 a V10) críticos ~ 1.9
medias_esperadas <- c(4.2, 4.0, 4.3, 4.1, 4.0, 1.8, 2.1, 1.5, 1.9, 1.7)

# 4. Construir la Matriz de Covarianza Simulada
# Esto obliga a que las preguntas del mismo grupo estén fuertemente relacionadas.
cov_matrix <- matrix(0.15, nrow = 10, ncol = 10) 
cov_matrix[1:5, 1:5] <- 0.75 # Alta correlación entre variables de Servicios
cov_matrix[6:10, 6:10] <- 0.80 # Alta correlación entre variables de Infraestructura
diag(cov_matrix) <- 1 # Varianza estándar

# 5. Generar la muestra poblacional (N = 1000)
# mvrnorm (generador de variables aleatorias normales multivariadas) crea las encuestas.
datos_simulados <- mvrnorm(n = 1000, mu = medias_esperadas, Sigma = cov_matrix)

# 6. Ajustar a escala Likert (1 a 5)
datos_simulados <- round(datos_simulados)
datos_simulados[datos_simulados < 1] <- 1
datos_simulados[datos_simulados > 5] <- 5

# Convertir a data frame (estructura de tabla de datos bidimensional)
df_turismo <- as.data.frame(datos_simulados)
colnames(df_turismo) <- c("V1_SER", "V2_SER", "V3_SER", "V4_SER", "V5_SER",
                          "V6_INF", "V7_INF", "V8_INF", "V9_INF", "V10_INF")

# AUDITORÍA DEL DOCENTE: MOSTRAR ESTOS RESULTADOS EN CONSOLA
# A. Prueba KMO y Esfericidad de Bartlett
cat("\nPRUEBA KMO\n")
print(KMO(df_turismo))

cat("\nPRUEBA DE ESFERICIDAD DE BARTLETT\n")
print(cortest.bartlett(cor(df_turismo), n = 1000))

# B. Análisis Factorial Exploratorio
cat("\nMATRIZ DE CARGAS FACTORIALES (ROTACIÓN VARIMAX)\n")
modelo_fa <- fa(df_turismo, nfactors = 2, rotate = "varimax", fm = "ml")
print(modelo_fa$loadings, cutoff = 0.4)

# C. Promedios Reales de la Muestra (Para el Mapa de Calor)
cat("\nPROMEDIOS FINALES DE LAS 1000 ENCUESTAS\n")
print(colMeans(df_turismo))