FROM rocker/shiny:latest

# Instalar dependencias del sistema necesarias para R y paquetes web
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libcairo2-dev \
    && rm -rf /var/lib/apt/lists/*

# Instalar paquetes de R usando binarios pre-compilados (MUCHO MÁS RÁPIDO)
RUN R -e "options(repos = c(CRAN = 'https://packagemanager.posit.co/cran/__linux__/bookworm/latest')); install.packages(c('shiny', 'shinydashboard', 'tidyverse', 'plotly', 'DT', 'leaflet', 'base64enc'))"

# Limpiar archivos temporales
RUN rm -rf /tmp/downloaded_packages/

# Copiar la aplicación al directorio del servidor Shiny
COPY . /srv/shiny-server/

# Asegurar permisos para la carpeta de datos (para que la app pueda escribir reportes/CSV temporales)
RUN mkdir -p /srv/shiny-server/data && \
    chown -R shiny:shiny /srv/shiny-server/

# Exponer el puerto estándar de Shiny Server
EXPOSE 3838

# Iniciar Shiny Server
CMD ["/usr/bin/shiny-server"]

