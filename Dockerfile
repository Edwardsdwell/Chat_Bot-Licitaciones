FROM rocker/r-ver:4.3.3
WORKDIR /app
ENV R_LIBS_USER=/usr/local/lib/R/site-library
RUN apt-get update && apt-get install -y --no-install-recommends libcurl4-openssl-dev libssl-dev libxml2-dev libuv1-dev zlib1g-dev \
    && rm -rf /var/lib/apt/lists/* \
    && mkdir -p /usr/local/lib/R/site-library \
    && Rscript -e "lib <- '/usr/local/lib/R/site-library'; .libPaths(unique(c(lib, .libPaths()))); pkgs <- c('httpuv','jsonlite','later','httr','curl','xml2','digest'); install.packages(pkgs, lib=lib, repos='https://cloud.r-project.org'); faltan <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly=TRUE)]; if (length(faltan)) stop('No se instalaron paquetes: ', paste(faltan, collapse=', '))"
COPY . /app
EXPOSE 8000
ENV LANG=C.UTF-8
ENV LC_ALL=C.UTF-8
ENV LA_PALA_HOST=0.0.0.0
ENV LA_PALA_OPEN_BROWSER=0
ENV LA_PALA_DATA_DIR=/tmp/data
ENV LA_PALA_LOG_DIR=/tmp/logs
CMD ["Rscript", "start.R"]
