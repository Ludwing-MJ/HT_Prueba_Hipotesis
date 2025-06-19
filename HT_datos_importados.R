# Instalación y carga de paquetes necesarios
## Para leer archivos Excel
if(!require(readxl)) install.packages("readxl")
## Para hacer las pruebas de hipotesis
if (!require(BSDA)) install.packages("BSDA")

# Importar datos ejercicio 2
ejercicio2 <- read_excel("HT.xlsx", sheet = 1)

# Importar datos ejercicio 3
ejercicio3 <- read_excel("HT.xlsx", sheet = 2)

# Importar datos ejercicio 4
ejercicio4 <- read_excel("HT.xlsx", sheet = 3)

