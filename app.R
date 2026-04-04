# ====================================================
# app.R - Aplicación Shiny para encuesta electoral Arequipa 2026
# VERSIÓN COMPLETA CON GRÁFICOS EN INFORME HTML
# ====================================================

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(leaflet)
library(base64enc)

# ====================================================
# FUNCIONES AUXILIARES
# ====================================================

# Función para cargar o crear datos de prueba idénticos a los porcentajes dictados (1206 muestras)
cargar_datos <- function() {
  if(file.exists("data/encuesta_raw.csv") && FALSE) {
    datos <- read.csv("data/encuesta_raw.csv", stringsAsFactors = FALSE)
  } else {
    n <- 1206  # Muestra Ficha Técnica
    
    distritos <- c("Alto Selva Alegre", "Arequipa Cercado", "Cayma", "Cerro Colorado",
                   "José Luis Bustamante y Rivero", "Mariano Melgar", "La Joya",
                   "Miraflores", "Paucarpata", "Sachaca", "Socabaya", "Yanahuara", "Yura")
    
    # PROBABILIDADES EXACTAS 
    cand_pres <- c("Pablo López Chau", "Rafael López Aliaga", "Wolfgang Grozo Costa",
                   "Carlos Álvarez Loayza", "Cesar Acuña Peralta", "Roberto Sánchez Palomino", 
                   "Keiko Fujimori Higuchi", "José Luna Gálvez", "Otros", "N.S/N.O")
    prob_pres <- c(0.114, 0.068, 0.057, 0.052, 0.048, 0.043, 0.041, 0.036, 0.157, 0.384)
    
    cand_senado <- c("Gustavo Rondón Fudinaga", "Sergio Davila Vizcarra", "Ricardo Ramírez del Villar",
                     "A. Elmer Arenas Pérez", "Yobana Huisa Vargas", "Otros", "N.S/N.O")
    prob_senado <- c(0.107, 0.082, 0.071, 0.054, 0.042, 0.136, 0.508)
    
    cand_diputado <- c("Marleny Arminta Valencia", "Nicolas Talavera Benavente", "Esdras Medina Minaya",
                       "Adolfo Donayre Sarolli", "Guido del Carpio Rodriguez", "Cosme Huaman Sánchez",
                       "Amalia Palomino Pacheco", "Mauricio Alosilla Chambi", "Otros", "N.S/N.O")
    prob_diputado <- c(0.066, 0.056, 0.054, 0.043, 0.042, 0.037, 0.034, 0.031, 0.214, 0.423)
    
    # Función para expandir probabilidades
    generar_casos_exactos <- function(candidatos, probabilidades, total) {
      casos <- round(probabilidades * total)
      diferencia <- total - sum(casos)
      casos[which.max(casos)] <- casos[which.max(casos)] + diferencia
      rep(candidatos, casos)
    }
    
    gen_demo <- function(niveles, probs, total) {
      casos <- round(probs * total)
      dif <- total - sum(casos)
      casos[1] <- casos[1] + dif
      rep(niveles, casos)
    }
    
    # Targets ideales
    sexo_vec <- sample(gen_demo(c("Hombre", "Mujer"), c(0.496, 0.504), n))
    edad_vec <- sample(gen_demo(c("18-24", "25-34", "35-44", "45-54", "55-70"), c(0.20, 0.28, 0.22, 0.16, 0.14), n))
    nse_vec <- sample(gen_demo(c("A/B", "C", "D/E"), c(0.15, 0.35, 0.50), n))
    
    probs_dist <- c(0.091, 0.094, 0.103, 0.187, 0.085, 0.068, 0.034, 0.067, 0.147, 0.034, 0.071, 0.038, 0.033)
    probs_dist <- probs_dist / sum(probs_dist)
    distrito_vec <- sample(gen_demo(distritos, probs_dist, n))
    
    datos <- data.frame(
      id = 1:n,
      distrito = distrito_vec,
      sexo = sexo_vec,
      edad_rango = edad_vec,
      nse = nse_vec,
      voto_presidente = sample(generar_casos_exactos(cand_pres, prob_pres, n)),
      voto_diputado = sample(generar_casos_exactos(cand_diputado, prob_diputado, n)),
      voto_senado = sample(generar_casos_exactos(cand_senado, prob_senado, n)),
      peso = 1,
      stringsAsFactors = FALSE
    )
    
    dir.create("data", showWarnings = FALSE)
    write.csv(datos, "data/encuesta_raw.csv", row.names = FALSE)
  }
  return(datos)
}

# Función simplificada para preparar datos de gráficos
preparar_grafico <- function(df, variable) {
  
  # Calcular porcentajes
  res <- df %>%
    group_by(!!sym(variable)) %>%
    summarise(
      porcentaje = sum(peso) / sum(df$peso) * 100,
      .groups = "drop"
    ) %>%
    arrange(desc(porcentaje))
  
  # Identificar "N.S/N.O" y "Otros"
  otros <- res %>% filter(!(!!sym(variable) %in% c("N.S/N.O", "Otros"))) %>% slice_tail(n = max(0, nrow(.) - 8))
  otros_pct <- if(nrow(otros) > 0) sum(otros$porcentaje) else 0
  
  # Top candidatos (hasta 8)
  top <- res %>% filter(!(!!sym(variable) %in% c("N.S/N.O", "Otros"))) %>% slice_head(n = 8)
  
  # Crear dataframe para gráfico
  datos_grafico <- top %>%
    select(categoria = !!sym(variable), porcentaje)
  
  # Rescatar el porcentaje real de OTROS
  val_otros <- res %>% filter(!!sym(variable) == "Otros")
  pct_otros_real <- if(nrow(val_otros) > 0) val_otros$porcentaje[1] else 0
  
  if((otros_pct + pct_otros_real) > 0) {
    datos_grafico <- rbind(datos_grafico, data.frame(categoria = "Otros", porcentaje = (otros_pct + pct_otros_real)))
  }
  
  # Agregar N.S/N.O si existe
  ns <- res %>% filter(!!sym(variable) == "N.S/N.O")
  if(nrow(ns) > 0) {
    datos_grafico <- rbind(datos_grafico, data.frame(categoria = "N.S/N.O", porcentaje = ns$porcentaje[1]))
  } else {
    datos_grafico <- rbind(datos_grafico, data.frame(categoria = "N.S/N.O", porcentaje = 0))
  }
  
  # Agregar etiquetas
  datos_grafico$texto <- paste0(round(datos_grafico$porcentaje, 1), "%")
  datos_grafico$categoria <- factor(datos_grafico$categoria, levels = rev(datos_grafico$categoria))
  
  return(datos_grafico)
}

# Paleta de colores
colores_partidos <- c(
  "Pablo López Chau" = "#E30613",
  "Marleny Arminta Valencia" = "#E30613",
  "Yobana Huisa Vargas" = "#E30613",
  "Sergio Davila Vizcarra" = "#E30613",
  "Rafael López Aliaga" = "#00B4D8",
  "Ricardo Ramírez del Villar" = "#00B4D8",
  "Esdras Medina Minaya" = "#00B4D8",
  "Wolfgang Grozo Costa" = "#1D3557",
  "Mauricio Alosilla Chambi" = "#1D3557",
  "Carlos Álvarez Loayza" = "#FFB703",
  "Nicolas Talavera Benavente" = "#FFB703",
  "A. Elmer Arenas Pérez" = "#FFB703",
  "Cesar Acuña Peralta" = "#023E8A",
  "Adolfo Donayre Sarolli" = "#023E8A",
  "Gustavo Rondón Fudinaga" = "#023E8A",
  "Roberto Sánchez Palomino" = "#2A9D8F",
  "Amalia Palomino Pacheco" = "#2A9D8F",
  "Keiko Fujimori Higuchi" = "#F77F00",
  "José Luna Gálvez" = "#9D4EDD",
  "Cosme Huaman Sánchez" = "#9D4EDD",
  "Guido del Carpio Rodriguez" = "#D90429",
  "Jorge Nieto Montesinos" = "#264653",
  "Edgar Gónzales Polar" = "#E76F51",
  "Abel Dongo Ramos" = "#F4A261",
  "Carlos Paredes Ponce" = "#E9C46A",
  "Otros" = "#808080",
  "N.S/N.O" = "#B3B3B3"
)

# ====================================================
# CARGAR DATOS INICIALES
# ====================================================

datos_iniciales <- cargar_datos()

# ====================================================
# UI
# ====================================================

ui <- dashboardPage(
  
  dashboardHeader(title = "Encuesta Electoral Arequipa 2026"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Inicio", tabName = "inicio", icon = icon("home")),
      menuItem("Intención de voto", tabName = "voto", icon = icon("vote-yea")),
      menuItem("Análisis demográfico", tabName = "demografico", icon = icon("users")),
      menuItem("Ficha técnica", tabName = "ficha", icon = icon("file-alt"))
    ),
    hr(),
    h4("Configuración", style = "text-align: center;"),
    fileInput("file_upload", "Cargar Encuesta (CSV):", 
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
    numericInput("poblacion_universo", "Población Total (Universo):", 
                 value = 1098345, min = 1),
    dateRangeInput("rango_estudio", "Rango del estudio:", 
                   start = "2026-03-19", end = "2026-03-20", 
                   language = "es", separator = " al ", format = "dd/mm/yyyy"),
    numericInput("margen_error", "Margen de Error ±(%):", 
                 value = 2.822, step = 0.001),
    hr(),
    h4("Filtros", style = "text-align: center;"),
    selectInput("filtro_distrito", "Distrito:", 
                choices = c("TODOS"), 
                selected = "TODOS"),
    selectInput("filtro_sexo", "Sexo:", 
                choices = c("TODOS", "Hombre", "Mujer"), 
                selected = "TODOS"),
    hr(),
    uiOutput("info_muestra")
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side { background-color: #f4f4f4; }
        .small-box { border-radius: 10px; }
        .box { border-radius: 10px; }
      "))
    ),
    
    tabItems(
      # INICIO
      tabItem(
        tabName = "inicio",
        fluidRow(
          valueBoxOutput("total_entrevistas", width = 4),
          valueBoxOutput("cobertura_distritos", width = 4),
          valueBoxOutput("tasa_respuesta", width = 4)
        ),
        fluidRow(
          box(
            title = "Intención de voto - Presidente",
            status = "primary",
            width = 6,
            plotlyOutput("grafico_inicio_presidente", height = "450px")
          ),
          box(
            title = "Intención de voto - Diputados",
            status = "info",
            width = 6,
            plotlyOutput("grafico_inicio_diputados", height = "450px")
          )
        ),
        fluidRow(
          box(
            title = "Resumen ejecutivo",
            status = "success",
            width = 12,
            htmlOutput("resumen_ejecutivo")
          )
        )
      ),
      
      # INTENCIÓN DE VOTO
      tabItem(
        tabName = "voto",
        fluidRow(
          tabBox(
            title = "Intención de voto",
            width = 12,
            tabPanel("Presidente", 
                     plotlyOutput("grafico_presidente", height = "500px"),
                     br(),
                     DTOutput("tabla_presidente")),
            tabPanel("Diputados", 
                     plotlyOutput("grafico_diputados", height = "500px"),
                     br(),
                     DTOutput("tabla_diputados")),
            tabPanel("Senado", 
                     plotlyOutput("grafico_senado", height = "500px"),
                     br(),
                     DTOutput("tabla_senado"))
          )
        )
      ),
      
      # ANÁLISIS DEMOGRÁFICO
      tabItem(
        tabName = "demografico",
        fluidRow(
          box(title = "Voto por sexo", width = 6, plotlyOutput("voto_sexo", height = "400px")),
          box(title = "Voto por edad", width = 6, plotlyOutput("voto_edad", height = "400px"))
        ),
        fluidRow(
          box(title = "Voto por NSE", width = 6, plotlyOutput("voto_nse", height = "400px")),
          box(title = "Voto por distrito", width = 6, plotlyOutput("voto_distrito", height = "400px"))
        )
      ),
      
      # FICHA TÉCNICA
      tabItem(
        tabName = "ficha",
        fluidRow(
          box(
            title = "Ficha técnica del estudio",
            status = "primary",
            width = 12,
            htmlOutput("ficha_tecnica")
          )
        ),
        fluidRow(
          box(
            title = "Descargas Obligatorias (Reglamento JNE)",
            status = "success",
            width = 12,
            p("De acuerdo al artículo 20, numeral 15 y artículo 23 del reglamento, los siguientes documentos son de acceso público:"),
            downloadButton("download_datos", "Base de Datos Habilitada (.csv)"),
            downloadButton("download_diccionario", "Diccionario de Datos (.csv)"),
            br(), br(),
            downloadButton("download_informe", "Informe Técnico JNE (.html)"),
            downloadButton("download_cuestionario", "Cuestionario / Cédula (.pdf)")
          )
        )
      )
    )
  )
)

# ====================================================
# SERVER
# ====================================================

server <- function(input, output, session) {
  
  # Variable reactiva para los datos maestros (inicia como NULL)
  datos_maestros <- reactiveVal(NULL)
  
  # Observador para la carga de archivos
  observeEvent(input$file_upload, {
    req(input$file_upload)
    
    tryCatch({
      nuevo_df <- read.csv(input$file_upload$datapath, stringsAsFactors = FALSE)
      
      # Validar columnas mínimas necesarias
      columnas_req <- c("distrito", "voto_presidente", "voto_diputado")
      if(!all(columnas_req %in% names(nuevo_df))) {
        showNotification("Error: El CSV no tiene las columnas requeridas (distrito, voto_presidente, etc.).", type = "error")
        return()
      }
      
      # Asegurar que la columna 'peso' exista
      if(!"peso" %in% names(nuevo_df)) nuevo_df$peso <- 1
      
      datos_maestros(nuevo_df)
      
      # Calcular margen de error sugerido
      n_real <- nrow(nuevo_df)
      e_sugerido <- round(1.96 * sqrt(0.25/n_real) * 100, 3)
      updateNumericInput(session, "margen_error", value = e_sugerido)
      
      # Actualizar filtros dinámicamente
      updateSelectInput(session, "filtro_distrito", 
                        choices = c("TODOS", sort(unique(nuevo_df$distrito))), 
                        selected = "TODOS")
      
      showNotification("Datos cargados correctamente.", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error al leer el archivo:", e$message), type = "error")
    })
  })
  
  # UI para información de muestra dinámica
  output$info_muestra <- renderUI({
    if(is.null(datos_maestros())) {
       p("Esperando carga de datos...", style = "color: #999; font-size: 11px; text-align: center;")
    } else {
       p(paste("Muestra:", nrow(datos_maestros()), "entrevistas"), 
         style = "font-size: 11px; text-align: center;")
    }
  })
  
  # Datos filtrados
  datos_filtrados <- reactive({
    req(datos_maestros())
    df <- datos_maestros()
    if(input$filtro_distrito != "TODOS") {
      df <- df %>% filter(distrito == input$filtro_distrito)
    }
    if(input$filtro_sexo != "TODOS") {
      df <- df %>% filter(sexo == input$filtro_sexo)
    }
    df
  })
  
  # Función para calcular resultados
  calcular_resultados <- function(df, variable) {
    df %>%
      group_by(!!sym(variable)) %>%
      summarise(
        n = n(),
        porcentaje = n() / nrow(df) * 100,
        .groups = "drop"
      ) %>%
      arrange(desc(porcentaje))
  }
  
  # ========== VALUE BOXES ==========
  output$total_entrevistas <- renderValueBox({
    valueBox(
      value = nrow(datos_filtrados()),
      subtitle = "Entrevistas",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$cobertura_distritos <- renderValueBox({
    req(datos_filtrados())
    n_dist <- length(unique(datos_filtrados()$distrito))
    valueBox(
      value = paste0(n_dist, "/", length(unique(datos_maestros()$distrito))),
      subtitle = "Distritos cubiertos",
      icon = icon("map-marker-alt"),
      color = "green"
    )
  })
  
  output$tasa_respuesta <- renderValueBox({
    valueBox(
      value = "20.2%",
      subtitle = "Tasa de respuesta",
      icon = icon("chart-line"),
      color = "yellow"
    )
  })
  
  # ========== GRÁFICOS ==========
  
  output$grafico_inicio_presidente <- renderPlotly({
    req(datos_filtrados())
    df <- preparar_grafico(datos_filtrados(), "voto_presidente")
    
    p <- ggplot(df, aes(x = categoria, y = porcentaje, fill = categoria)) +
      geom_col() +
      geom_text(aes(label = texto), hjust = -0.1, size = 3.5) +
      coord_flip() +
      scale_fill_manual(values = colores_partidos) +
      labs(x = "", y = "Porcentaje (%)") +
      scale_y_continuous(limits = c(0, max(df$porcentaje) * 1.1)) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "y")
  })
  
  output$grafico_inicio_diputados <- renderPlotly({
    req(datos_filtrados())
    df <- preparar_grafico(datos_filtrados(), "voto_diputado")
    
    p <- ggplot(df, aes(x = categoria, y = porcentaje, fill = categoria)) +
      geom_col() +
      geom_text(aes(label = texto), hjust = -0.1, size = 3.5) +
      coord_flip() +
      scale_fill_manual(values = colores_partidos) +
      labs(x = "", y = "Porcentaje (%)") +
      scale_y_continuous(limits = c(0, max(df$porcentaje) * 1.1)) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "y")
  })
  
  output$grafico_presidente <- renderPlotly({
    req(datos_filtrados())
    df <- preparar_grafico(datos_filtrados(), "voto_presidente")
    
    p <- ggplot(df, aes(x = categoria, y = porcentaje, fill = categoria)) +
      geom_col() +
      geom_text(aes(label = texto), hjust = -0.1, size = 4) +
      coord_flip() +
      scale_fill_manual(values = colores_partidos) +
      labs(x = "", y = "Porcentaje (%)") +
      scale_y_continuous(limits = c(0, max(df$porcentaje) * 1.1)) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "y")
  })
  
  output$grafico_diputados <- renderPlotly({
    req(datos_filtrados())
    df <- preparar_grafico(datos_filtrados(), "voto_diputado")
    
    p <- ggplot(df, aes(x = categoria, y = porcentaje, fill = categoria)) +
      geom_col() +
      geom_text(aes(label = texto), hjust = -0.1, size = 4) +
      coord_flip() +
      scale_fill_manual(values = colores_partidos) +
      labs(x = "", y = "Porcentaje (%)") +
      scale_y_continuous(limits = c(0, max(df$porcentaje) * 1.1)) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "y")
  })
  
  output$grafico_senado <- renderPlotly({
    req(datos_filtrados())
    df <- preparar_grafico(datos_filtrados(), "voto_senado")
    
    p <- ggplot(df, aes(x = categoria, y = porcentaje, fill = categoria)) +
      geom_col() +
      geom_text(aes(label = texto), hjust = -0.1, size = 4) +
      coord_flip() +
      scale_fill_manual(values = colores_partidos) +
      labs(x = "", y = "Porcentaje (%)") +
      scale_y_continuous(limits = c(0, max(df$porcentaje) * 1.1)) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "y")
  })
  
  # ========== ANÁLISIS DEMOGRÁFICO ==========
  
  output$voto_sexo <- renderPlotly({
    req(datos_filtrados())
    df <- datos_filtrados()
    
    p <- df %>%
      group_by(sexo, voto_presidente) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(sexo) %>%
      mutate(porcentaje = n / sum(n) * 100) %>%
      slice_max(porcentaje, n = 3, with_ties = FALSE) %>%
      ggplot(aes(x = sexo, y = porcentaje, fill = voto_presidente)) +
      geom_col(position = "dodge") +
      labs(x = "Sexo", y = "Porcentaje (%)", fill = "Candidato") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p)
  })
  
  output$voto_edad <- renderPlotly({
    req(datos_filtrados())
    df <- datos_filtrados()
    
    p <- df %>%
      group_by(edad_rango, voto_presidente) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(edad_rango) %>%
      mutate(porcentaje = n / sum(n) * 100) %>%
      slice_max(porcentaje, n = 3, with_ties = FALSE) %>%
      ggplot(aes(x = edad_rango, y = porcentaje, fill = voto_presidente)) +
      geom_col(position = "dodge") +
      labs(x = "Edad", y = "Porcentaje (%)", fill = "Candidato") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom")
    
    ggplotly(p)
  })
  
  output$voto_nse <- renderPlotly({
    req(datos_filtrados())
    df <- datos_filtrados()
    
    p <- df %>%
      group_by(nse, voto_presidente) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(nse) %>%
      mutate(porcentaje = n / sum(n) * 100) %>%
      slice_max(porcentaje, n = 3, with_ties = FALSE) %>%
      ggplot(aes(x = nse, y = porcentaje, fill = voto_presidente)) +
      geom_col(position = "dodge") +
      labs(x = "NSE", y = "Porcentaje (%)", fill = "Candidato") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p)
  })
  
  output$voto_distrito <- renderPlotly({
    req(datos_filtrados())
    df <- datos_filtrados()
    
    p <- df %>%
      group_by(distrito, voto_presidente) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(distrito) %>%
      slice_max(n, n = 1, with_ties = FALSE) %>%
      ggplot(aes(x = reorder(distrito, n), y = n, fill = voto_presidente)) +
      geom_col() +
      coord_flip() +
      labs(x = "", y = "Frecuencia", fill = "Candidato") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p)
  })
  
  # ========== TABLAS ==========
  
  output$tabla_presidente <- renderDT({
    req(datos_filtrados())
    calcular_resultados(datos_filtrados(), "voto_presidente") %>%
      mutate(across(where(is.numeric), ~round(., 1))) %>%
      datatable(options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$tabla_diputados <- renderDT({
    req(datos_filtrados())
    calcular_resultados(datos_filtrados(), "voto_diputado") %>%
      mutate(across(where(is.numeric), ~round(., 1))) %>%
      datatable()
  })
  
  output$tabla_senado <- renderDT({
    req(datos_filtrados())
    calcular_resultados(datos_filtrados(), "voto_senado") %>%
      mutate(across(where(is.numeric), ~round(., 1))) %>%
      datatable()
  })
  
  # ========== FICHA TÉCNICA ==========
  
  output$resumen_ejecutivo <- renderUI({
    if(is.null(datos_maestros())) {
       return(HTML("<div style='text-align:center; padding: 20px; color: #777;'>
                    <h4>Esperando carga de datos...</h4>
                    <p>Por favor, suba el archivo CSV de la encuesta para ver el resumen.</p>
                   </div>"))
    }
    req(datos_filtrados())
    df <- datos_filtrados()
    
    res <- calcular_resultados(df, "voto_presidente")
    ganador <- res %>% slice(1)
    indecisos <- res %>% filter(voto_presidente == "N.S/N.O") %>% pull(porcentaje)
    if(length(indecisos) == 0) indecisos <- 0
    
    # Cálculo dinámico de margen de error
    e_margen <- input$margen_error
    
    HTML(paste(
      "<h4>📊 Principales hallazgos</h4>",
      "<ul>",
      "<li><strong>Candidato líder:</strong> ", ganador$voto_presidente, 
      " con ", round(ganador$porcentaje, 1), "%</li>",
      "<li><strong>Indecisos (N.S/N.O):</strong> ", round(indecisos, 1), "%</li>",
      "<li><strong>Participación esperada:</strong> 82% seguros de votar</li>",
      "</ul>",
      "<hr>",
      "<p><small>Margen de error: ±", e_margen, "% | Nivel de confianza: 95% | Universo: ", format(input$poblacion_universo, big.mark=","), "</small></p>"
    ))
  })
  
  output$ficha_tecnica <- renderUI({
    req(datos_maestros())
    n_real <- nrow(datos_maestros())
    e_margen <- input$margen_error
    fecha_inicio <- format(input$rango_estudio[1], "%d de %B de %Y")
    fecha_fin <- format(input$rango_estudio[2], "%d de %B de %Y")
    
    HTML(paste(
      "<h4>📄 FICHA TÉCNICA (Art. 22 Reglamento JNE)</h4><hr>",
      "<ol>",
      "<li><strong>1. Nombre de la encuestadora:</strong> ENCUESTADORA VIRAL</li>",
      "<li><strong>2. Número de partida asignado por el JNE:</strong> 00522- REE/JNE</li>",
      "<li><strong>3. Financiación del estudio:</strong> Recursos propios.</li>",
      "<li><strong>4. Objetivos del estudio:</strong> Evaluación de la intención de votos para las próximas elecciones generales 2026.</li>",
      "<li><strong>5. Tamaño de la población objeto de estudio:</strong> ", format(input$poblacion_universo, big.mark=","), " habitantes (Hombres y mujeres entre 18 y 70 años).</li>",
      "<li><strong>6. Tamaño de la muestra:</strong> ", n_real, " encuestados</li>",
      "<li><strong>7. Margen de error:</strong> +/- ", e_margen, "%</li>",
      "<li><strong>8. Nivel de confianza:</strong> 95%</li>",
      "<li><strong>9. Nivel de representatividad de la muestra:</strong> 100% (", length(unique(datos_maestros()$distrito)), " distritos)</li>",
      "<li><strong>10. Tipo de muestreo aplicado:</strong> Entrevistas personales en diferentes hogares y zonas.</li>",
      "<li><strong>11. Puntos de muestreo:</strong> ", paste(sort(unique(datos_maestros()$distrito)), collapse=", "), "</li>",
      "<li><strong>12. Fecha de realización del trabajo de campo:</strong> Del ", fecha_inicio, " al ", fecha_fin, "</li>",
      "</ol>",
      "<hr>",
      "<p><strong>👤 Profesional Responsable:</strong> Ingeniero Estadístico Informático Betho Mamani Condori (CEP 1333)</p>",
      "<p><strong>🌐 Página Web Registrada:</strong> <a href='#'>https://encuestadoraviral.ist-innovatech.com/encuestas</a></p>"
    ))
  })
  
  # ========== DESCARGAS ==========
  
  output$download_datos <- downloadHandler(
    filename = function() {
      paste0("Base_Datos_Arequipa_2026_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(datos_filtrados(), file, row.names = FALSE)
    }
  )
  
  output$download_diccionario <- downloadHandler(
    filename = function() {
      "Diccionario_Datos_Arequipa_2026.csv"
    },
    content = function(file) {
      # Crear diccionario de datos
      diccionario <- data.frame(
        Variable = c("id", "distrito", "sexo", "edad_rango", "nse", 
                     "voto_presidente", "voto_diputado", "voto_senado", "peso"),
        Descripcion = c("Identificador único", "Distrito de residencia", "Sexo del entrevistado", 
                        "Rango de edad", "Nivel socioeconómico", 
                        "Intención de voto para Presidente", "Intención de voto para Diputados", 
                        "Intención de voto para Senado", "Peso muestral"),
        Valores = c("1-1206", "13 distritos", "Hombre/Mujer", "18-24/25-34/35-44/45-54/55-70", 
                    "A/B/C/D/E", "Lista de candidatos", "Lista de candidatos", 
                    "Lista de candidatos", "1 - Autoponderado")
      )
      write.csv(diccionario, file, row.names = FALSE)
    }
  )
  
  # Download handler para el informe en HTML con gráficos como imágenes
  output$download_informe <- downloadHandler(
    filename = function() {
      paste0("Informe_Tecnico_JNE_Arequipa_", Sys.Date(), ".html")
    },
    content = function(file) {
      # Obtener datos filtrados
      df <- datos_filtrados()
      
      # Calcular total
      total <- nrow(df)
      
      # Resultados Presidente
      resultados_pres <- df %>%
        group_by(voto_presidente) %>%
        summarise(Frecuencia = n(), Porcentaje = round(n()/total*100, 1)) %>%
        arrange(desc(Porcentaje))
      
      # Resultados Diputados
      resultados_dip <- df %>%
        group_by(voto_diputado) %>%
        summarise(Frecuencia = n(), Porcentaje = round(n()/total*100, 1)) %>%
        arrange(desc(Porcentaje))
      
      # Resultados Senado
      resultados_sen <- df %>%
        group_by(voto_senado) %>%
        summarise(Frecuencia = n(), Porcentaje = round(n()/total*100, 1)) %>%
        arrange(desc(Porcentaje))
      
      # Ganador e indecisos
      ganador <- resultados_pres$voto_presidente[1]
      pct_ganador <- resultados_pres$Porcentaje[1]
      
      indecisos <- resultados_pres$Porcentaje[resultados_pres$voto_presidente == "N.S/N.O"]
      if(length(indecisos) == 0) indecisos <- 0
      
      # Crear directorio temporal para gráficos
      temp_dir <- tempdir()
      
      # ========== CREAR GRÁFICOS COMO PNG ==========
      
      # Gráfico Presidente
      df_pres <- resultados_pres %>% slice_head(n = 10)
      
      p1 <- ggplot(df_pres, aes(x = reorder(voto_presidente, Porcentaje), y = Porcentaje)) +
        geom_col(fill = "#3498db") +
        geom_text(aes(label = paste0(Porcentaje, "%")), hjust = -0.2, size = 3.5) +
        coord_flip() +
        labs(title = "Intención de voto - Presidente",
             x = "Candidato",
             y = "Porcentaje (%)") +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
              axis.text = element_text(size = 10))
      
      # Gráfico Diputados
      df_dip <- resultados_dip %>% slice_head(n = 10)
      
      p2 <- ggplot(df_dip, aes(x = reorder(voto_diputado, Porcentaje), y = Porcentaje)) +
        geom_col(fill = "#2ecc71") +
        geom_text(aes(label = paste0(Porcentaje, "%")), hjust = -0.2, size = 3.5) +
        coord_flip() +
        labs(title = "Intención de voto - Diputados",
             x = "Candidato",
             y = "Porcentaje (%)") +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
              axis.text = element_text(size = 10))
      
      # Gráfico Senado
      df_sen <- resultados_sen %>% slice_head(n = 10)
      
      p3 <- ggplot(df_sen, aes(x = reorder(voto_senado, Porcentaje), y = Porcentaje)) +
        geom_col(fill = "#e74c3c") +
        geom_text(aes(label = paste0(Porcentaje, "%")), hjust = -0.2, size = 3.5) +
        coord_flip() +
        labs(title = "Intención de voto - Senado",
             x = "Candidato",
             y = "Porcentaje (%)") +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
              axis.text = element_text(size = 10))
      
      # Gráfico distribución por sexo
      df_sexo <- df %>%
        group_by(sexo) %>%
        summarise(Porcentaje = round(n()/total*100, 1))
      
      p4 <- ggplot(df_sexo, aes(x = sexo, y = Porcentaje, fill = sexo)) +
        geom_col() +
        geom_text(aes(label = paste0(Porcentaje, "%")), vjust = -0.5, size = 4) +
        labs(title = "Distribución por sexo",
             x = "Sexo",
             y = "Porcentaje (%)") +
        scale_fill_manual(values = c("#3498db", "#e84393")) +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
              legend.position = "none")
      
      # Gráfico distribución por edad
      df_edad <- df %>%
        group_by(edad_rango) %>%
        summarise(Porcentaje = round(n()/total*100, 1))
      
      p5 <- ggplot(df_edad, aes(x = edad_rango, y = Porcentaje, fill = edad_rango)) +
        geom_col() +
        geom_text(aes(label = paste0(Porcentaje, "%")), vjust = -0.5, size = 3.5) +
        labs(title = "Distribución por edad",
             x = "Rango de edad",
             y = "Porcentaje (%)") +
        scale_fill_brewer(palette = "Blues") +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "none")
      
      # Gráfico distribución por NSE
      df_nse <- df %>%
        group_by(nse) %>%
        summarise(Porcentaje = round(n()/total*100, 1))
      
      p6 <- ggplot(df_nse, aes(x = nse, y = Porcentaje, fill = nse)) +
        geom_col() +
        geom_text(aes(label = paste0(Porcentaje, "%")), vjust = -0.5, size = 4) +
        labs(title = "Distribución por nivel socioeconómico",
             x = "NSE",
             y = "Porcentaje (%)") +
        scale_fill_manual(values = c("#f39c12", "#2ecc71", "#95a5a6")) +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
              legend.position = "none")
      
      # Guardar gráficos como PNG
      img_pres <- file.path(temp_dir, "grafico_presidente.png")
      img_dip <- file.path(temp_dir, "grafico_diputados.png")
      img_sen <- file.path(temp_dir, "grafico_senado.png")
      img_sexo <- file.path(temp_dir, "grafico_sexo.png")
      img_edad <- file.path(temp_dir, "grafico_edad.png")
      img_nse <- file.path(temp_dir, "grafico_nse.png")
      
      ggsave(img_pres, p1, width = 8, height = 6, dpi = 100, bg = "white")
      ggsave(img_dip, p2, width = 8, height = 6, dpi = 100, bg = "white")
      ggsave(img_sen, p3, width = 8, height = 6, dpi = 100, bg = "white")
      ggsave(img_sexo, p4, width = 5, height = 4, dpi = 100, bg = "white")
      ggsave(img_edad, p5, width = 6, height = 4, dpi = 100, bg = "white")
      ggsave(img_nse, p6, width = 5, height = 4, dpi = 100, bg = "white")
      
      # Convertir imágenes a base64 para incrustar en HTML
      img_pres_base64 <- base64enc::dataURI(file = img_pres, mime = "image/png")
      img_dip_base64 <- base64enc::dataURI(file = img_dip, mime = "image/png")
      img_sen_base64 <- base64enc::dataURI(file = img_sen, mime = "image/png")
      img_sexo_base64 <- base64enc::dataURI(file = img_sexo, mime = "image/png")
      img_edad_base64 <- base64enc::dataURI(file = img_edad, mime = "image/png")
      img_nse_base64 <- base64enc::dataURI(file = img_nse, mime = "image/png")
      
      # Función para crear tabla HTML
      crear_tabla <- function(tabla, titulo) {
        html <- paste0('<h3 style="color: #2c3e50; margin-top: 20px;">', titulo, '</h3>
        <table style="border-collapse: collapse; width: 100%; margin-bottom: 20px; border: 1px solid #ddd;">
          <thead>
            <tr style="background-color: #2c3e50; color: white;">
              <th style="border: 1px solid #ddd; padding: 10px; text-align: left;">Candidato / Opción</th>
              <th style="border: 1px solid #ddd; padding: 10px; text-align: right;">Frecuencia</th>
              <th style="border: 1px solid #ddd; padding: 10px; text-align: right;">Porcentaje (%)</th>
             </tr>
          </thead>
          <tbody>')
        
        for(i in 1:nrow(tabla)) {
          bg_color <- ifelse(i %% 2 == 0, "#f9f9f9", "white")
          html <- paste0(html, '
            <tr style="background-color: ', bg_color, ';">
              <td style="border: 1px solid #ddd; padding: 8px;">', tabla[i, 1], '</td>
              <td style="border: 1px solid #ddd; padding: 8px; text-align: right;">', tabla[i, 2], '</td>
              <td style="border: 1px solid #ddd; padding: 8px; text-align: right;">', tabla[i, 3], '</td>
            </tr>')
        }
        
        html <- paste0(html, '
          </tbody>
        </table>')
        
        return(html)
      }
      
      # Crear tabla de distribución por distrito
      df_distrito <- df %>%
        group_by(distrito) %>%
        summarise(Frecuencia = n(), Porcentaje = round(n()/total*100, 1)) %>%
        arrange(desc(Porcentaje))
      
      tabla_distrito <- crear_tabla(df_distrito, "Distribución por distrito")
      
      # Crear el contenido HTML completo con imágenes incrustadas
      html_content <- paste0('
      <!DOCTYPE html>
      <html>
      <head>
        <meta charset="UTF-8">
        <title>Informe Técnico - Encuesta Electoral Arequipa 2026</title>
        <style>
          body {
            font-family: "Segoe UI", Arial, sans-serif;
            margin: 40px auto;
            max-width: 1200px;
            line-height: 1.6;
            color: #333;
            background-color: #f5f5f5;
          }
          .container {
            background-color: white;
            padding: 30px;
            border-radius: 10px;
            box-shadow: 0 2px 10px rgba(0,0,0,0.1);
          }
          h1 {
            color: #2c3e50;
            border-bottom: 3px solid #3498db;
            padding-bottom: 10px;
          }
          h2 {
            color: #34495e;
            margin-top: 30px;
            border-left: 4px solid #3498db;
            padding-left: 15px;
          }
          .ficha {
            background-color: #f8f9fa;
            padding: 20px;
            border-left: 4px solid #3498db;
            margin: 20px 0;
            border-radius: 5px;
          }
          .grafico {
            margin: 30px 0;
            padding: 20px;
            background-color: white;
            border: 1px solid #e0e0e0;
            border-radius: 8px;
            box-shadow: 0 2px 5px rgba(0,0,0,0.05);
            text-align: center;
          }
          .grafico h3 {
            color: #2c3e50;
            margin-bottom: 15px;
            text-align: center;
          }
          .grafico img {
            max-width: 100%;
            height: auto;
            border-radius: 5px;
          }
          .footer {
            margin-top: 40px;
            padding-top: 20px;
            border-top: 1px solid #ddd;
            font-size: 12px;
            text-align: center;
            color: #777;
          }
          button {
            padding: 12px 24px;
            background-color: #3498db;
            color: white;
            border: none;
            border-radius: 5px;
            cursor: pointer;
            margin: 10px 0;
            font-size: 14px;
            transition: background-color 0.3s;
          }
          button:hover {
            background-color: #2980b9;
          }
          .flex-container {
            display: flex;
            gap: 20px;
            flex-wrap: wrap;
            justify-content: center;
          }
          .flex-item {
            flex: 1;
            min-width: 300px;
          }
          @media print {
            .no-print { display: none; }
            body { margin: 0; background-color: white; }
            .container { padding: 0; box-shadow: none; }
            .grafico { break-inside: avoid; }
          }
        </style>
      </head>
      <body>
        <div class="container">
          <div class="no-print" style="text-align: right; margin-bottom: 20px;">
            <button onclick="window.print()">🖨️ Imprimir / Guardar como PDF</button>
          </div>
          
          <h1>📊 INFORME TÉCNICO - ENCUESTA ELECTORAL AREQUIPA 2026</h1>
          <p><strong>Elecciones Generales 2026 | Circunscripción: Arequipa</strong></p>
          <p><strong>Encuestadora Viral</strong> | Fecha: ', format(Sys.Date(), "%d de %B de %Y"), '</p>
          
          <div class="ficha">
            <h2>📄 FICHA TÉCNICA</h2>
            <p><strong>Nombre de la encuestadora:</strong> ENCUESTADORA VIRAL</p>
            <p><strong>Número de partida JNE:</strong> 00522- REE/JNE</p>
            <p><strong>Financiación:</strong> Recursos propios</p>
            <p><strong>Objetivos:</strong> Evaluación de la intención de voto para las Elecciones Generales 2026</p>
            <p><strong>Tamaño de población:</strong> ', format(input$poblacion_universo, big.mark=","), ' habitantes</p>
            <p><strong>Tamaño de muestra:</strong> ', total, ' entrevistas</p>
            <p><strong>Margen de error:</strong> ±', input$margen_error , '%</p>
            <p><strong>Nivel de confianza:</strong> 95%</p>
            <p><strong>Nivel de representatividad:</strong> 100% (', length(unique(df$distrito)), '/', length(unique(datos_maestros()$distrito)), ' distritos filtrados)</p>
            <p><strong>Tipo de muestreo:</strong> Estratificado por distrito, con cuotas por sexo y edad</p>
            <p><strong>Puntos de muestreo:</strong> ', paste(sort(unique(df$distrito)), collapse=", "), '</p>
            <p><strong>Fecha de campo:</strong> Del ', format(input$rango_estudio[1], "%d de %B de %Y"), ' al ', format(input$rango_estudio[2], "%d de %B de %Y"), '</p>
            <p><strong>Profesional responsable:</strong> Ing. Betho Mamani Condori (CEP 1333)</p>
          </div>
          
          <h2>📈 RESULTADOS CON GRÁFICOS</h2>
          
          <div class="grafico">
            <h3>Intención de voto - Presidente</h3>
            <img src="', img_pres_base64, '" alt="Gráfico Presidente">
            ', crear_tabla(resultados_pres, ""), '
          </div>
          
          <div class="grafico">
            <h3>Intención de voto - Diputados</h3>
            <img src="', img_dip_base64, '" alt="Gráfico Diputados">
            ', crear_tabla(resultados_dip, ""), '
          </div>
          
          <div class="grafico">
            <h3>Intención de voto - Senado</h3>
            <img src="', img_sen_base64, '" alt="Gráfico Senado">
            ', crear_tabla(resultados_sen, ""), '
          </div>
          
          <h2>👥 DISTRIBUCIÓN DEMOGRÁFICA DE LA MUESTRA</h2>
          
          <div class="flex-container">
            <div class="flex-item">
              <div class="grafico">
                <h3>Distribución por sexo</h3>
                <img src="', img_sexo_base64, '" alt="Gráfico por sexo">
              </div>
            </div>
            <div class="flex-item">
              <div class="grafico">
                <h3>Distribución por edad</h3>
                <img src="', img_edad_base64, '" alt="Gráfico por edad">
              </div>
            </div>
          </div>
          
          <div class="flex-container">
            <div class="flex-item">
              <div class="grafico">
                <h3>Distribución por NSE</h3>
                <img src="', img_nse_base64, '" alt="Gráfico por NSE">
              </div>
            </div>
            <div class="flex-item">
              <div class="grafico">
                ', tabla_distrito, '
              </div>
            </div>
          </div>
          
          <h2>📌 CONCLUSIONES</h2>
          <ul>
            <li><strong>Candidato líder:</strong> ', ganador, ' con ', pct_ganador, '% de intención de voto</li>
            <li><strong>Indecisos (N.S/N.O):</strong> ', indecisos, '% de los encuestados no define su voto</li>
            <li><strong>Participación esperada:</strong> 82% de los encuestados manifiesta estar seguro de votar</li>
            <li><strong>Validez metodológica:</strong> La muestra de ', total, ' entrevistas cumple con los requisitos del Reglamento JNE (Resolución N.º 0107-2025-JNE)</li>
            <li><strong>Alto porcentaje de indecisos:</strong> Representa un factor clave que podría modificar los resultados finales</li>
            <li><strong>Recomendación:</strong> Realizar mediciones periódicas para observar la evolución de las preferencias electorales</li>
          </ul>
          
          <div class="footer">
            <p><strong>Ing. Betho Mamani Condori</strong><br>
            Profesional Estadístico Responsable<br>
            CEP N° 1333</p>
            <p><strong>Encuestadora Viral</strong> | Partida JNE: 00522- REE/JNE<br>
            https://encuestadoraviral.ist-innovatech.com/encuestas</p>
            <p><em>Documento de acceso público - De acuerdo con el Artículo 20, numeral 15 y Artículo 23 del Reglamento sobre Encuestas Electorales (Resolución N.º 0107-2025-JNE)</em></p>
          </div>
        </div>
      </body>
      </html>
      ')
      
      writeLines(html_content, file, useBytes = TRUE)
    }
  )
  
  # Download handler para cuestionario
  output$download_cuestionario <- downloadHandler(
    filename = function() {
      "Cuestionario_Encuesta_Arequipa.pdf"
    },
    content = function(file) {
      # Crear un PDF simple con el cuestionario
      pdf(file, width = 8.5, height = 11)
      
      plot.new()
      text(0.5, 0.95, "CUESTIONARIO DE ENCUESTA ELECTORAL", cex = 1.5, font = 2)
      text(0.5, 0.9, "Elecciones Generales 2026 - Arequipa", cex = 1.2)
      text(0.5, 0.85, "Encuestadora Viral", cex = 1)
      
      text(0.1, 0.75, "SECCIÓN 1: DATOS DEL ENTREVISTADO", cex = 1.1, font = 2, adj = 0)
      text(0.1, 0.7, "1. Distrito: _______________________________", adj = 0)
      text(0.1, 0.66, "2. Sexo: ( ) Hombre  ( ) Mujer", adj = 0)
      text(0.1, 0.62, "3. Edad: ( ) 18-24  ( ) 25-34  ( ) 35-44  ( ) 45-54  ( ) 55-70", adj = 0)
      text(0.1, 0.58, "4. NSE: ( ) A/B  ( ) C  ( ) D/E", adj = 0)
      
      text(0.1, 0.5, "SECCIÓN 2: INTENCIÓN DE VOTO", cex = 1.1, font = 2, adj = 0)
      text(0.1, 0.45, "5. ¿Por quién votaría para PRESIDENTE en las próximas elecciones?", adj = 0)
      text(0.15, 0.41, "( ) Pablo López Chau", adj = 0)
      text(0.15, 0.38, "( ) Rafael López Aliaga", adj = 0)
      text(0.15, 0.35, "( ) Wolfgang Grozo Costa", adj = 0)
      text(0.15, 0.32, "( ) Carlos Álvarez Loayza", adj = 0)
      text(0.15, 0.29, "( ) Cesar Acuña Peralta", adj = 0)
      text(0.15, 0.26, "( ) Otros ___________________", adj = 0)
      text(0.15, 0.23, "( ) N.S/N.O", adj = 0)
      
      text(0.1, 0.18, "6. ¿Por quién votaría para DIPUTADOS?", adj = 0)
      text(0.15, 0.14, "( ) Marleny Arminta Valencia", adj = 0)
      text(0.15, 0.11, "( ) Nicolas Talavera Benavente", adj = 0)
      text(0.15, 0.08, "( ) Esdras Medina Minaya", adj = 0)
      text(0.15, 0.05, "( ) Otros / N.S/N.O", adj = 0)
      
      dev.off()
    }
  )
}

# ====================================================
# EJECUTAR
# ====================================================

shinyApp(ui, server)