# start with the app-basetidy
FROM kopflab/apps:basetidy

# packages
USER root
RUN install2.r -r http://cran.rstudio.com/ remotes

# install additional data readring/writing packages
RUN install2.r -r http://cran.rstudio.com/ \
  gargle \
  googledrive \
  googlesheets4

# rebuild-* restarts build from here
ARG refresh=unknown

# copy app
ARG app
COPY $app /home/app
LABEL app="$app"

# run app
CMD ["R", "-q", "-e", "shiny::runApp('/home/app')"]
