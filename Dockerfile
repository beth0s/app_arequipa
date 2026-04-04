FROM rocker/shiny:latest

# Instalar dependencias del sistema necesarias para R y paquetes web
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Instalar paquetes de R de forma eficiente
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'tidyverse', 'plotly', 'DT', 'leaflet', 'base64enc'), repos='https://cran.rstudio.com/')"

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

