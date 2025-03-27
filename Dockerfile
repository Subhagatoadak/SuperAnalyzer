FROM rocker/shiny:latest

# Install system dependencies for rstan and other packages
RUN apt-get update && apt-get install -y \
    build-essential \
    libssl-dev \
    libcurl4-gnutls-dev \
    libxml2-dev \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Install required R packages
RUN R -e "install.packages(c('shiny', 'bs4Dash', 'shinyjs', 'DT', 'sortable', 'httr', 'jsonlite', 'rpivotTable', 'moments', 'rstan', 'bayesplot', 'ggplot2'), repos='http://cran.rstudio.com/')"

# Copy the Shiny app into the container
COPY app/ /srv/shiny-server/app/

# Expose the Shiny port
EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
