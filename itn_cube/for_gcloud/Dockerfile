FROM rocker/verse:3.6.3-ubuntu18.04

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    lbzip2 \
    libfftw3-dev \
    libgdal-dev \
    libgeos-dev \
    libgsl0-dev \
    libgl1-mesa-dev \
    libglu1-mesa-dev \
    libhdf4-alt-dev \
    libhdf5-dev \
    libjq-dev \
    liblwgeom-dev \
    libpq-dev \
    libproj-dev \
    libprotobuf-dev \
    libnetcdf-dev \
    libsqlite3-dev \
    libssl-dev \
    libudunits2-dev \
    netcdf-bin \
    postgis \
    protobuf-compiler \
    sqlite3 \
    tk-dev \
    unixodbc-dev \
    git \
  && install2.r --error \
    RColorBrewer \
    classInt \
    gstat \
    mapdata \
    maptools \
    proj4 \
    raster \
    rgdal \
    rgeos \
    rlas \
    sf \
    sp \
    spacetime \
    spatstat \
    geoR \
    geosphere \
    cowplot \
    data.table \
    devtools \
    doParallel \
    getPass \
    ggfortify \
    lubridate \
    readr \
    rgl \
    rmarkdown \
    sparseMVN \
    spatstat \
    spdep \
    tictoc \
    TMB \
    zoo \
    VGAM \
    png \
    rasterVis \
    cvTools \
    boot \
    stringr \
    dismo \
    gbm \
    ggplot2 \
    gridExtra \
    latticeExtra \
    pryr \
    survey \
    VGAM \
    zoo \
    remotes
    
ARG INLA_REPO='testing'

RUN Rscript -e "install.packages('INLA', repos=c('https://cloud.r-project.org/', INLA='https://inla.r-inla-download.org/R/$INLA_REPO'), dep=TRUE)" && \
    rm -rf /tmp/*
