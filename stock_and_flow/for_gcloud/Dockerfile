FROM rocker/geospatial

RUN apt-get update && . /etc/environment \
  && wget sourceforge.net/projects/mcmc-jags/files/JAGS/4.x/Source/JAGS-4.3.0.tar.gz  -O jags.tar.gz \
  && tar -xf jags.tar.gz \
  && cd JAGS* && ./configure && make -j4 && make install

## httr authentication uses this port
EXPOSE 1410
ENV HTTR_LOCALHOST 0.0.0.0

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
     git \
 && install2.r --error \
    cowplot \
    data.table \
    devtools \
    doParallel \
    getPass \
    ggfortify \
    ggplot2 \
    lubridate \
    raster \
    readr \
    rgl \
    rjags \ 
    rmarkdown \
    sparseMVN \
    spatstat \
    spdep \
    tictoc \
    TMB \
    zoo 

ARG INLA_REPO='stable'

RUN Rscript -e "install.packages('INLA', repos=c('https://cloud.r-project.org/', INLA='https://inla.r-inla-download.org/R/$INLA_REPO'), dep=TRUE)" && \
    rm -rf /tmp/*

RUN git clone https://github.com/timcdlucas/INLAutils.git
RUN R CMD build INLAutils
RUN Rscript -e "install.packages('INLAutils', repos = NULL, type = 'source')"
RUN Rscript -e "devtools::install_github('malaria-atlas-project/malariaAtlas', ref = 'dev')"