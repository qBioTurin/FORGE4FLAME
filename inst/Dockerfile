# Immagine base di Shiny Server
FROM rocker/shiny:latest

# Dipendenze di sistema necessarie
RUN apt-get update && apt-get install -y \
    libfftw3-dev git

# Pacchetti R necessari
RUN R -e "install.packages(c('remotes', 'shiny', 'shinyWidgets', 'shinydashboard', 'shinythemes', 'shinybusy', 'glue', 'readr', 'zip', 'sortable', 'stringr', 'ggplot2', 'tidyr', 'DT'))"

# Installa BiocManager e EBImage
RUN R -e "install.packages('BiocManager', repos='http://cran.rstudio.com/')"
RUN R -e "BiocManager::install('EBImage', force=TRUE)"

# Installa devtools e EBImageExtra
RUN R -e "install.packages('devtools')"
RUN R -e "devtools::install_github('ornelles/EBImageExtra', force=TRUE)"
RUN git config --global url."https://github.com/".insteadOf "git@github.com:" && \
    R -e "devtools::install_github('qBioTurin/FORGE4FLAME')"

# Copia gli script dell'applicazione nel container
RUN mkdir -p /srv/shiny-server/F4F
RUN cp -r /usr/local/lib/R/site-library/F4F/* /srv/shiny-server/F4F/

# Copia il file app.R nel container
#COPY app.R /srv/shiny-server/

# Imposta i permessi corretti per Shiny Server
RUN chown -R shiny:shiny /srv/shiny-server

# Espone la porta per Shiny Server
EXPOSE 3838

# Comando per eseguire automaticamente l'applicazione Shiny
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/app.R', host = '0.0.0.0', port = 3838)"]
