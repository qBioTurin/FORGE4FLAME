FROM rocker/r-ver:4.2.1

ENV S6_VERSION=v2.1.0.2
ENV RSTUDIO_VERSION=daily
ENV DEFAULT_USER=rstudio
ENV PANDOC_VERSION=default
ENV PATH=/usr/lib/rstudio-server/bin:$PATH

RUN /rocker_scripts/install_rstudio.sh
RUN /rocker_scripts/install_pandoc.sh

# system libraries
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libz-dev \
    libpng-dev\
    libtiff5-dev\
    libfftw3-dev
  

# install R packages required 
RUN R -e 'install.packages(c(\
  "shinydashboard", \ 
  "shinyjs", \ 
  "jsonlite", \ 
  "dplyr", \ 
  "shinythemes", \ 
  "colourpicker", \ 
  "glue", \ 
  "readr", \ 
  "zip", \ 
  "sortable", \ 
  "shinyalert", \ 
  "shinybusy", \ 
  "shinyBS", \ 
  "stringr", \ 
  "ggplot2", \ 
  "tidyr", \ 
  "remotes", \
  "BiocManager" \
            ), \
            repos="https://packagemanager.rstudio.com/cran/__linux__/focal/2022-08-01"\
          )'

RUN R -e 'BiocManager::install("EBImage", ask = FALSE)'
RUN R -e 'remotes::install_github("ornelles/EBImageExtra")'
RUN R -e 'remotes::install_github("qBioTurin/F4F")'

# date > marker
# COPY marker /dev/null

# copy the app directory into the image
# COPY ./Shiny/ ./app/
# COPY ./Docker/ ./docker/

# Create data directory
# RUN mkdir ./home/data; chmod 777 ./home/data
# COPY ./Data/ ./home/data/

# expose port
EXPOSE 3838

# Define default command.
# CMD ["bash"]
# run app on container start
CMD ["R", "-e", "F4F::F4F.run(TRUE)')"]

