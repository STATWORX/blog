FROM quantumobject/docker-shiny

# Install dependencies
RUN apt-get update && apt-get install -y \
    libmysqlclient-dev \
    libpq-dev \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libxml2-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libpng-dev \
    xtail \
    wget

# Install R packages

RUN R -e "install.packages(c('dashboard','datasets'),repos = 'http://cran.us.r-project.org')"

# The test app
COPY testapp /srv/shiny-server/testapp

# Index html
COPY index_addins /srv/shiny-server/index_addins
COPY index.html /srv/shiny-server/index.html

# Make port available
EXPOSE 3838