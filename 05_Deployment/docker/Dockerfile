FROM rocker/shiny:4.0.5

RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev


RUN R -e 'install.packages(c(\
              "shiny", \
              "shinydashboard", \
              "tidyr" ,"dplyr"))'
          
# copy the app directory into the image
COPY ./shiny-app/* /srv/shiny-server/


# run app
CMD ["/usr/bin/shiny-server"]
