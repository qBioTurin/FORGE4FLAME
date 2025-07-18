FROM rocker/shiny:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    gdal-bin \
    libfftw3-dev \
    libmagick++-dev \
    git \
    docker.io

# Install necessary R packages
RUN R -e "install.packages(c('remotes', 'shinyWidgets', 'shinydashboard', 'shinythemes', 'shinybusy', 'glue', 'readr', 'zip', 'sortable', 'stringr', 'ggplot2', 'tidyr', 'DT', 'purrr'))"

# Install Bioconductor and dependencies
RUN R -e "install.packages('BiocManager', repos='http://cran.rstudio.com/')"
RUN R -e "BiocManager::install('EBImage', force=TRUE)"

# Install missing dependencies explicitly
RUN R -e "install.packages(c('devtools', 'pkgbuild', 'pkgload', 'rcmdcheck'), repos='http://cran.rstudio.com/')"

# Install sf library
RUN R -e "remotes::install_github('r-spatial/sf')"

# Install GitHub packages
RUN R -e "remotes::install_github('ornelles/EBImageExtra', dependencies = TRUE, force = TRUE)"
RUN R -e "remotes::install_github('qBioTurin/FORGE4FLAME', force=TRUE)"

# Set working directory
WORKDIR /usr/local/lib/R/site-library/FORGE4FLAME/

# Cleanup
RUN rm -fr FLAMEGPU-FORGE4FLAME

# Expose Shiny Server port
EXPOSE 3838

# Start Shiny app
CMD ["R", "-e", "FORGE4FLAME::FORGE4FLAME.run(FromDocker = TRUE)"]
