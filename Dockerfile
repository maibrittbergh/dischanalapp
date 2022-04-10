# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:latest

# system libraries of general use
## install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \

   
     libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libgdal-dev \
    libproj-dev \
    libgeos-dev \
    libudunits2-dev \
    netcdf-bin \
    libmysqlclient-dev
    
 

  






## update system libraries \
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean






# install renv & restore packages

RUN Rscript -e "install.packages('devtools', repos='http://cran.rstudio.com/')"

RUN Rscript -e  "devtools::install_github('maibrittbergh/dischanalyst')"
RUN Rscript -e  "devtools::install_github('rstudio/httpuv')"



RUN Rscript -e "install.packages(pkgs=c('shiny', 'shinythemes', 'gridExtra', 'scico', 'dichromat','RColorBrewer', 'base64enc', 'crosstalk', 'htmltools', 'leaflet.providers', 'magrittr','markdown','png', 'sp', 'viridis' , 'readxl', 'sf', 'dplyr', 'ggplot2', 'readr',  'zyp', 'Kendall', 'zoo', 'readr',  'viridisLite', 'RColorBrewer',  'rgdal', 'DT', 'shinycssloaders', 'shinyWidgets', 'fontawesome', 'shinyjs', 'tidyverse', 'viridis', 'tigris', 'raster'),repos='http://cran.rstudio.com/')"

RUN Rscript -e  "devtools::install_github('rstudio/leaflet')"
RUN Rscript -e  "devtools::install_github('bhaskarvk/leaflet.extras')"


RUN mkdir /root/app
COPY dockapp2 /root/shiny_save




# expose port
EXPOSE 3838

# run app on container start
CMD ["R", "-e", "shiny::runApp('/root/shiny_save',host = '0.0.0.0', port = 3838)"]