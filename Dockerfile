FROM rocker/shiny:latest

# Instalar dependencias del sistema necesarias para R y paquetes web
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libcairo2-dev \
    libicu-dev \
    pandoc \
    && rm -rf /var/lib/apt/lists/*

# Instalar paquetes de R usando binarios pre-compilados (MUCHO MÁS RÁPIDO)
RUN R -e "options(repos = c(CRAN = 'https://packagemanager.posit.co/cran/__linux__/bookworm/latest')); install.packages(c('shiny', 'shinydashboard', 'tidyverse', 'plotly', 'DT', 'leaflet', 'base64enc'))"

# Limpiar archivos temporales
RUN rm -rf /tmp/downloaded_packages/

# Copiar la aplicación al directorio de trabajo
COPY . /app
WORKDIR /app

# Asegurar permisos para la carpeta de datos
RUN mkdir -p /app/data && chown -R shiny:shiny /app

# Render usa una variable de entorno $PORT. Si no existe, usamos 3838 por defecto.
ENV PORT=3838

# Iniciar la aplicación directamente sin shiny-server para mayor estabilidad en la nube
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = as.numeric(Sys.getenv('PORT')))"]

