# Get shiny server and R from the rocker project
FROM rocker/shiny:4.4 as base

# Remove example apps
RUN rm -rf /srv/shiny-server/*

# Install system libraries
RUN apt-get update && apt-get install -y \
  software-properties-common

# Install R package dependencies
RUN apt update && apt install -y \
  git \
  libcurl4-gnutls-dev \
  libssl-dev \
  libgdal-dev \
  libgeos-dev \
  libproj-dev \
  libudunits2-dev \
  build-essential \
  gfortran \
  libglpk-dev \
  libxml2-dev

# Install CRAN packages
RUN Rscript -e 'install.packages(c(\
  "shiny",\
  "shinydashboard",\
  "shinyIncubator",\
  "shinymanager",\
  "here",\
  "janitor",\
  "readxl",\
  "writexl",\
  "plyr",\
  "dplyr",\
  "readr",\
  "tibble",\
  "readr",\
  "sf",\
  "ggplot2",\
  "foreign",\
  "vegan",\
  "terra",\
  "leaflet",\
  "remotes",\
  "rhandsontable",\
  "googlesheets4",\
  "Matrix",\
  "tidyr",\
  "leaflet",\
  "zip",\
  "glue",\
  "DT",\
  "argparser")\
  )'

# Install remote packages
RUN Rscript -e 'remotes::install_github(c(\
  "rstudio/shiny-incubator")\
  )'

RUN R -e 'install.packages("prioritizr", repos = "https://cran.rstudio.com/")'

ADD gurobi11.0.3_linux64.tar.gz /opt

RUN R -e 'install.packages("/opt/gurobi1103/linux64/R/gurobi_11.0-3_R_4.4.0.tar.gz", repos = NULL)'

# May be needed to get Gurobi working
RUN echo "/opt/gurobi1103/linux64/lib" > /etc/ld.so.conf.d/gurobi.conf

RUN ldconfig

COPY . /srv/shiny-server/

RUN chown -R shiny:shiny /srv/shiny-server/

RUN chmod 755 /srv/shiny-server/

RUN chown shiny:shiny /var/lib/shiny-server

USER shiny 

EXPOSE 3838

WORKDIR /srv/shiny-server

ARG iso3
ARG location
ARG language=en
ARG googlesheet
ARG sheetname
ARG blm=0
ARG pa_lock=true
ARG restore_lock=false
ARG urban_green=false

RUN Rscript pre_global.R -i $iso3 -n "$location" -l $language -g $googlesheet -s $sheetname -b $blm -p $pa_lock -r $restore_lock -u $urban_green

CMD ["/usr/bin/shiny-server"]
