
FROM rocker/shiny:latest


RUN apt-get update && apt-get install -y \
    libfftw3-dev \
    libmagick++-dev \
    git \
    docker.io


RUN R -e "install.packages(c('devtools', 'shinyWidgets', 'shinydashboard', 'shinythemes', 'shinybusy', 'glue', 'readr', 'zip', 'sortable', 'stringr', 'ggplot2', 'tidyr', 'DT'))"


RUN R -e "install.packages('BiocManager', repos='http://cran.rstudio.com/')"
RUN R -e "BiocManager::install('EBImage', force=TRUE)"
RUN R -e "devtools::install_github('ornelles/EBImageExtra', dependencies = TRUE, force = TRUE)"
RUN R -e "devtools::install_github('https://github.com/qBioTurin/FORGE4FLAME', force=TRUE)"

WORKDIR /usr/local/lib/R/site-library/FORGE4FLAME/

RUN rm -fr FLAMEGPU-FORGE4FLAME

# Espone la porta per Shiny Server
EXPOSE 3838

# Comando per avviare direttamente l'app Shiny 
CMD ["R", "-e", "library(shiny); library(FORGE4FLAME); FORGE4FLAME::FORGE4FLAME.run(FromDocker = TRUE)"]
