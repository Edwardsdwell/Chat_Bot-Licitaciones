FROM rocker/r-ver:4.3.3
WORKDIR /app
RUN apt-get update && apt-get install -y --no-install-recommends libcurl4-openssl-dev libssl-dev libxml2-dev \
    && rm -rf /var/lib/apt/lists/* \
    && R -e "install.packages(c('httpuv','jsonlite','later','httr','curl','xml2','digest'), repos='https://cloud.r-project.org')"
COPY . /app
EXPOSE 8000
ENV LA_PALA_HOST=0.0.0.0
ENV LA_PALA_OPEN_BROWSER=0
ENV LA_PALA_DATA_DIR=/var/data/data
ENV LA_PALA_LOG_DIR=/var/data/logs
CMD ["Rscript", "start.R"]
