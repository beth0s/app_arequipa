# scripts/crear_archivos_fuente.R
# Crear archivos CSV con datos de INEI y RENIEC

library(tidyverse)

# ====================================================
# 1. DATOS DE PROYECCIONES INEI 2026 (COMPLETOS)
# ====================================================

proyecciones_inei <- data.frame(
  ubigeo = c("040101", "040102", "040103", "040104", "040105", "040106",
             "040107", "040108", "040109", "040110", "040111", "040112",
             "040113", "040114", "040115", "040116", "040117", "040118",
             "040119", "040120", "040121", "040122", "040123", "040124",
             "040125", "040126", "040127", "040128", "040129"),
  distrito = c("Arequipa", "Alto Selva Alegre", "Cayma", "Cerro Colorado",
               "Characato", "Chiguata", "Jacobo Hunter", "La Joya",
               "Mariano Melgar", "Miraflores", "Mollebaya", "Paucarpata",
               "Pocsi", "Polobaya", "Quequeña", "Sabandia", "Sachaca",
               "San Juan de Siguas", "San Juan de Tarucani", "Santa Isabel de Siguas",
               "Santa Rita de Siguas", "Socabaya", "Tiabaya", "Uchumayo",
               "Vitor", "Yanahuara", "Yarabamba", "Yura", "José Luis Bustamante y Rivero"),
  poblacion_2026 = c(54201, 97900, 118443, 265275, 20610, 3433, 56602, 47244,
                     71157, 70819, 9201, 140086, 416, 647, 9066, 5113, 33198,
                     461, 1100, 548, 9524, 94168, 18164, 19748, 5736, 28023,
                     1851, 50632, 85711),
  stringsAsFactors = FALSE
)

# ====================================================
# 2. DATOS DE ELECTORES RENIEC 2026 (ESTIMADOS POR DISTRITO)
# ====================================================

electores_reniec <- data.frame(
  distrito = c("Alto Selva Alegre", "Arequipa", "Cayma", "Cerro Colorado",
               "José Luis Bustamante y Rivero", "Mariano Melgar", "La Joya",
               "Miraflores", "Paucarpata", "Sachaca", "Socabaya", "Yanahuara", "Yura"),
  electores = c(67075, 69346, 75875, 138275, 63042, 50090, 25414, 49194, 108915, 24997, 52301, 28286, 24238),
  stringsAsFactors = FALSE
)

# ====================================================
# 3. GUARDAR ARCHIVO COMPLETO DE POBLACIÓN (TODOS LOS DISTRITOS)
# ====================================================

write.csv(proyecciones_inei, "data/poblacion_distritos_completo.csv", row.names = FALSE)

# ====================================================
# 4. CREAR ARCHIVO PARA LOS 13 DISTRITOS DE LA ENCUESTA
# ====================================================

# Definir los 13 distritos de tu encuesta
distritos_encuesta <- c("Alto Selva Alegre", "Arequipa", "Cayma", "Cerro Colorado",
                        "José Luis Bustamante y Rivero", "Mariano Melgar", "La Joya",
                        "Miraflores", "Paucarpata", "Sachaca", "Socabaya", 
                        "Yanahuara", "Yura")

# Filtrar proyecciones para estos distritos
poblacion_encuesta <- proyecciones_inei %>%
  filter(distrito %in% distritos_encuesta) %>%
  left_join(electores_reniec, by = "distrito") %>%
  mutate(
    porcentaje_electoral = round(electores / sum(electores) * 100, 1)
  ) %>%
  arrange(distrito)

# Corregir nombre de distrito "Arequipa" a "Arequipa Cercado" para tu app
poblacion_encuesta <- poblacion_encuesta %>%
  mutate(distrito = ifelse(distrito == "Arequipa", "Arequipa Cercado", distrito))

# ====================================================
# 5. GUARDAR ARCHIVOS
# ====================================================

# Archivo para población electoral (ponderación)
poblacion_encuesta %>%
  select(distrito, electores, porcentaje_electoral) %>%
  write.csv("data/poblacion_electoral.csv", row.names = FALSE)

# Archivo completo con población y electores
poblacion_encuesta %>%
  select(distrito, ubigeo, poblacion_2026, electores, porcentaje_electoral) %>%
  write.csv("data/poblacion_distritos.csv", row.names = FALSE)

# ====================================================
# 6. VERIFICACIÓN
# ====================================================

cat("========================================\n")
cat("ARCHIVOS CREADOS CORRECTAMENTE\n")
cat("========================================\n\n")

cat("poblacion_electoral.csv:\n")
print(poblacion_encuesta %>% select(distrito, electores, porcentaje_electoral))

cat("\nSuma de porcentajes:", sum(poblacion_encuesta$porcentaje_electoral), "%\n")
cat("Total de electores:", sum(poblacion_encuesta$electores), "\n")
cat("Total de población 2026:", sum(poblacion_encuesta$poblacion_2026), "\n")

cat("\n========================================\n")
cat("Archivos guardados en data/\n")
cat("- data/poblacion_distritos.csv\n")
cat("- data/poblacion_electoral.csv\n")
cat("- data/poblacion_distritos_completo.csv\n")
cat("========================================\n")

