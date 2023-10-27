FROM analythium/shinyproxy-demo:latest

# packages
USER root
RUN install2.r -r http://cran.rstudio.com/ remotes

# install tidyverse packages
RUN install2.r -r http://cran.rstudio.com/ \
  dplyr \
  tidyr \
  purrr \
  forcats \
  stringr \
  ggplot2

# install data readring/writing packages
RUN install2.r -r http://cran.rstudio.com/ \
  readr \
  readxl \
  gargle \
  googledrive \ 
  googlesheets4

# install UI packages
RUN install2.r -r http://cran.rstudio.com/ \
  shiny \
  shinydashboard \
  shinyjs \
  shinytoastr \
  shinycssloaders \
  rlog \
  prompter \
  DT  

# provide an argument to be set from built to restart build from here
ARG refresh=unknown

# app
RUN rm -rf /home/app/*
ARG app
COPY $app /home/app
LABEL app="$app"
USER app
