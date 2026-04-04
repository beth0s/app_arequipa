# ====================================================
# scripts/generar_datos.R
# Generación de datos simulados para encuesta electoral Arequipa 2026
# Basado en fuentes oficiales: INEI, RENIEC, APEIM
# ====================================================

library(tidyverse)

set.seed(2026)

# ====================================================
# 1. CARGAR FUENTES OFICIALES (simuladas con datos reales)
# ====================================================

# Población electoral RENIEC 2026 por distrito (datos reales)
poblacion_electoral <- data.frame(
  distrito = c("Alto Selva Alegre", "Arequipa Cercado", "Cayma", "Cerro Colorado",
               "José Luis Bustamante y Rivero", "Mariano Melgar", "La Joya",
               "Miraflores", "Paucarpata", "Sachaca", "Socabaya", "Yanahuara", "Yura"),
  electores = c(67075, 69346, 75875, 138275, 63042, 50090, 25414, 49194, 108915, 24997, 52301, 28286, 24238)
)

# Calcular porcentajes
poblacion_electoral <- poblacion_electoral %>%
  mutate(porcentaje = electores / sum(electores) * 100)

# Proyecciones INEI 2026 por distrito (población total)
poblacion_inei <- data.frame(
  distrito = c("Alto Selva Alegre", "Arequipa Cercado", "Cayma", "Cerro Colorado",
               "José Luis Bustamante y Rivero", "Mariano Melgar", "La Joya",
               "Miraflores", "Paucarpata", "Sachaca", "Socabaya", "Yanahuara", "Yura"),
  poblacion = c(94815, 54315, 113343, 253214, 84773, 69135, 35987, 68619, 138830, 31128, 89862, 27590, 30142)
)

# ====================================================
# 2. PARÁMETROS DE LA ENCUESTA
# ====================================================

n_total <- 1206
fecha_campo <- c("2026-03-19", "2026-03-20")

# Distribución por sexo (RENIEC: 50.4% mujeres, 49.6% hombres)
prob_sexo <- c(0.496, 0.504)  # Hombre, Mujer

# Distribución por edad (INEI proyecciones 2026)
prob_edad <- c(0.20, 0.28, 0.22, 0.16, 0.14)  # 18-24, 25-34, 35-44, 45-54, 55-70

# Distribución por NSE (APEIM 2025)
prob_nse <- c(0.15, 0.35, 0.50)  # A/B, C, D/E

# Distribución por zona (urbana/rural) - según INEI
prob_zona <- c(0.95, 0.05)  # Urbana, Rural

# ====================================================
# 3. CANDIDATOS Y PROBABILIDADES (basado en tendencias reales)
# ====================================================

candidatos_presidente <- c(
  "Rafael López Aliaga", "Keiko Fujimori", "Alfonso López Chau",
  "Carlos Álvarez", "Wolfgang Grozo", "César Acuña",
  "Roberto Sánchez", "Yonhy Lescano", "Otro", "Voto blanco", "Voto nulo", "No sabe"
)

prob_presidente <- c(0.165, 0.142, 0.102, 0.087, 0.062, 0.041, 0.032, 0.024, 0.021, 0.038, 0.025, 0.261)

partidos_diputados <- c(
  "Ahora Nación", "País para Todos", "Alianza para el Progreso",
  "Fuerza Popular", "Renovación Popular", "Juntos por el Perú",
  "Integridad Democrática", "Cooperación Popular", "Perú Primero",
  "Perú Libre", "Otro partido", "Voto blanco", "Voto nulo", "No sabe"
)

prob_diputados <- c(0.022, 0.018, 0.015, 0.032, 0.028, 0.014, 0.016, 0.012, 0.035, 0.025, 0.020, 0.045, 0.030, 0.688)

partidos_senado <- c(
  "Ahora Nación", "País para Todos", "Alianza para el Progreso",
  "Fuerza Popular", "Renovación Popular", "Juntos por el Perú",
  "Integridad Democrática", "Cooperación Popular", "Perú Primero",
  "Perú Libre", "Otro partido", "Voto blanco", "Voto nulo", "No sabe"
)

prob_senado <- c(0.020, 0.016, 0.014, 0.030, 0.026, 0.013, 0.015, 0.011, 0.032, 0.024, 0.018, 0.042, 0.028, 0.711)

# ====================================================
# 4. GENERACIÓN DE DATOS
# ====================================================

# Asignar distritos según población electoral (RENIEC)
distritos <- poblacion_electoral$distrito
prob_distritos <- poblacion_electoral$porcentaje / 100

distrito_asignado <- sample(distritos, n_total, replace = TRUE, prob = prob_distritos)

# IDs únicos
ids <- paste0("AQP-", sprintf("%04d", 1:n_total))

# Zona
zona <- sample(c("Urbana", "Rural"), n_total, replace = TRUE, prob = prob_zona)

# Sexo
sexo <- sample(c("Hombre", "Mujer"), n_total, replace = TRUE, prob = prob_sexo)

# Edad
edad_rangos <- c("18-24", "25-34", "35-44", "45-54", "55-70")
edad_rango_asignado <- sample(edad_rangos, n_total, replace = TRUE, prob = prob_edad)

# NSE
nse_niveles <- c("A/B", "C", "D/E")
nse_asignado <- sample(nse_niveles, n_total, replace = TRUE, prob = prob_nse)

# Tiempo de residencia
tiempo_residencia <- sample(
  c("Menos de 1 año", "1 a 5 años", "6 a 10 años", "Más de 10 años", "Toda su vida"),
  n_total, replace = TRUE,
  prob = c(0.05, 0.15, 0.20, 0.25, 0.35)
)

# Lugar de nacimiento
lugar_nacimiento <- sample(
  c("En este distrito", "Otro distrito de Arequipa", "Otro departamento"),
  n_total, replace = TRUE,
  prob = c(0.55, 0.25, 0.20)
)

# Nivel educativo
nivel_educativo <- sample(
  c("Sin estudios", "Primaria", "Secundaria", "Técnico", "Universitario", "Postgrado"),
  n_total, replace = TRUE,
  prob = c(0.02, 0.10, 0.25, 0.25, 0.30, 0.08)
)

# Votos
voto_presidente <- sample(candidatos_presidente, n_total, replace = TRUE, prob = prob_presidente)
voto_diputado <- sample(partidos_diputados, n_total, replace = TRUE, prob = prob_diputados)
voto_senado <- sample(partidos_senado, n_total, replace = TRUE, prob = prob_senado)

# Actitud del entrevistado
actitud <- sample(
  c("Muy colaborador", "Colaborador", "Indiferente", "Poco colaborador", "Hostil"),
  n_total, replace = TRUE,
  prob = c(0.30, 0.40, 0.15, 0.10, 0.05)
)

# Influencia de terceros
influencia <- sample(
  c("Nadie", "Presentes sin influir", "Intento influir", "Dictaron respuesta", "Consultó"),
  n_total, replace = TRUE,
  prob = c(0.70, 0.18, 0.05, 0.03, 0.04)
)

# Marcado correcto
marcado_correcto <- sample(
  c("Sí", "Marcó más de uno", "Omisión de cargo", "No observado"),
  n_total, replace = TRUE,
  prob = c(0.85, 0.05, 0.08, 0.02)
)

# Seguridad del marcado
seguridad_marcado <- sample(
  c("Seguro", "Dudó", "Borró y volvió a marcar", "Preguntó"),
  n_total, replace = TRUE,
  prob = c(0.70, 0.18, 0.07, 0.05)
)

# Incidencias
incidencia <- sample(
  c("Ninguna", "Se negó", "Interrupción", "Incompleta", "Problema material"),
  n_total, replace = TRUE,
  prob = c(0.85, 0.05, 0.04, 0.03, 0.03)
)

# Horarios
horas <- sample(9:18, n_total, replace = TRUE)
minutos <- sample(c(0, 15, 30, 45), n_total, replace = TRUE)
hora_inicio <- paste0(sprintf("%02d", horas), ":", sprintf("%02d", minutos))
duracion_min <- sample(5:12, n_total, replace = TRUE)
hora_fin <- format(
  as.POSIXct(paste0("2026-03-19 ", hora_inicio)) + duracion_min * 60,
  format = "%H:%M"
)

# Fechas
fecha <- sample(fecha_campo, n_total, replace = TRUE, prob = c(0.5, 0.5))

# Encuestadores y supervisores
encuestadores <- paste0("ENC-", sprintf("%03d", sample(1:30, n_total, replace = TRUE)))
supervisores <- paste0("SUP-", sprintf("%03d", sample(1:10, n_total, replace = TRUE)))

# ====================================================
# 5. CREAR DATAFRAME FINAL
# ====================================================

datos_encuesta <- data.frame(
  id = ids,
  fecha = fecha,
  encuestador = encuestadores,
  supervisor = supervisores,
  distrito = distrito_asignado,
  zona = zona,
  sexo = sexo,
  edad_rango = edad_rango_asignado,
  nse = nse_asignado,
  tiempo_residencia = tiempo_residencia,
  lugar_nacimiento = lugar_nacimiento,
  nivel_educativo = nivel_educativo,
  voto_presidente = voto_presidente,
  voto_diputado = voto_diputado,
  voto_senado = voto_senado,
  actitud = actitud,
  influencia = influencia,
  marcado_correcto = marcado_correcto,
  seguridad_marcado = seguridad_marcado,
  incidencia = incidencia,
  hora_inicio = hora_inicio,
  hora_fin = hora_fin,
  peso = 1,
  stringsAsFactors = FALSE
)

# ====================================================
# 6. GUARDAR ARCHIVOS
# ====================================================

# Guardar datos crudos
write.csv(datos_encuesta, "data/encuesta_raw.csv", row.names = FALSE)
saveRDS(datos_encuesta, "data/encuesta_raw.rds")

# Guardar población electoral
write.csv(poblacion_electoral, "data/poblacion_electoral.csv", row.names = FALSE)

# Guardar población INEI
write.csv(poblacion_inei, "data/poblacion_distritos.csv", row.names = FALSE)

# ====================================================
# 7. VERIFICACIONES
# ====================================================

cat("========================================\n")
cat("RESUMEN DE DATOS GENERADOS\n")
cat("========================================\n")
cat("Total de entrevistas:", nrow(datos_encuesta), "\n")
cat("Fecha de campo:", paste(unique(datos_encuesta$fecha), collapse = ", "), "\n\n")

cat("Distribución por distrito:\n")
print(table(datos_encuesta$distrito))

cat("\nDistribución por sexo:\n")
print(round(prop.table(table(datos_encuesta$sexo)) * 100, 1))

cat("\nDistribución por edad:\n")
print(round(prop.table(table(datos_encuesta$edad_rango)) * 100, 1))

cat("\nIntención de voto Presidente (Top 8):\n")
print(round(sort(prop.table(table(datos_encuesta$voto_presidente)) * 100, decreasing = TRUE)[1:8], 1))

cat("\n========================================\n")
cat("Archivos guardados en data/encuesta_raw.csv y data/encuesta_raw.rds\n")
cat("========================================\n")
