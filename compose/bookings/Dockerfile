FROM openanalytics/r-base:3.6.1

RUN mkdir -p root/bookings_api/code
RUN mkdir -p root/bookings_api/data

COPY code /root/bookings_api/code
#COPY data /root/bookings_api/data

COPY renv.lock /root/bookings_api/renv.lock
COPY renv /root/bookings_api/renv

WORKDIR /root/bookings_api 

RUN apt-get update -qq && apt-get install -y \
	git-core \
  	libcurl4-gnutls-dev \
	libxml2-dev \
	libpq-dev \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.0.0 \
    libpng-dev \
    xtail \
    wget

RUN R -e "install.packages('renv')"
RUN R -e "library('renv'); renv::consent(provided=TRUE)"
RUN R -e "renv::restore(confirm=FALSE)"
RUN R -e "install.packages('PKI')"
    
EXPOSE 6060

ENTRYPOINT ["R", "-e", "source('code/run.R')"]
