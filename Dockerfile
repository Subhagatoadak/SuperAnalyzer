FROM rocker/shiny:latest

# Install system dependencies for rstan and other packages
RUN apt-get update && apt-get install -y \
    build-essential \
    libssl-dev \
    libcurl4-gnutls-dev \
    libxml2-dev \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Option 1: Copy your .Renviron file if available.
# Make sure your .Renviron file contains the line:
# OPENAI_API_KEY=your_openai_api_key_here
# Uncomment the next line if you want to include it in the image.
# COPY .Renviron /root/.Renviron

# Option 2: Alternatively, pass the OPENAI_API_KEY as an environment variable at runtime:
# For example: docker run -p 3838:3838 -e OPENAI_API_KEY=your_key superanalyzer

# Install required R packages
RUN R -e "install.packages(c('shiny', 'bs4Dash', 'shinyjs', 'DT', 'sortable', 'httr', 'jsonlite', 'rpivotTable', 'moments', 'rstan', 'bayesplot', 'ggplot2'), repos='http://cran.rstudio.com/')"

# Copy the Shiny app into the container
COPY app/ /srv/shiny-server/app/

# Expose the Shiny port
EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
