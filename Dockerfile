# Immagine base di R con RStudio
FROM rocker/rstudio:latest
# Dipendenze di sistema necessarie
# Aggiorna e installa le dipendenze di sistema necessarie
RUN apt-get update && apt-get install -y \
    libfftw3-dev \
    libmagick++-dev \
    git \
    sudo

# Installa devtools e pacchetti R necessari
RUN R -e "install.packages(c('devtools', 'remotes', 'shinyWidgets', 'shinydashboard', 'shinythemes', 'shinybusy', 'glue', 'readr', 'zip', 'sortable', 'stringr', 'ggplot2', 'tidyr', 'DT'))"

# Installa BiocManager e EBImage
RUN R -e "install.packages('BiocManager', repos='http://cran.rstudio.com/')"
RUN R -e "BiocManager::install('EBImage', force=TRUE)"

# Clona e installa EBImageExtra dal repository GitHub
RUN git clone https://github.com/ornelles/EBImageExtra.git /usr/local/src/EBImageExtra
RUN R -e "devtools::install('/usr/local/src/EBImageExtra', dependencies = TRUE, force = TRUE)"

# Clona il repository FORGE4FLAME
RUN git clone https://francescosiv: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@github.com/qBioTurin/FORGE4FLAME.git /usr/local/src/FORGE4FLAME

# Installa il pacchetto R dal repository clonato
RUN R -e "devtools::install('/usr/local/src/FORGE4FLAME', dependencies = TRUE, force = TRUE)"

# Copia gli script nel container
COPY . /home/rstudio/
# Imposta la directory di lavoro
WORKDIR /home/rstudio/