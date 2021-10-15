FROM openanalytics/r-base

LABEL maintainer "Samuel Talle <samueltalle275@gmail.com>"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev 
    

RUN apt-get update -qq && apt-get -y --no-install-recommends install \
  libxml2-dev \
  libcairo2-dev \
  libsqlite3-dev \
  libmariadbd-dev \
  libmariadbclient-dev \
  libpq-dev \
  libcurl4-openssl-dev \
  && install2.r --error \
    --deps TRUE \
    tidyverse \
    dplyr \
    #devtools \
    #formatR \
    #remotes \
    #selectr \
    #caTools \
    #	BiocManager \
  && rm -rf /tmp/downloaded_packages


# RODBC
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    unixodbc-dev \
    unixodbc \
    unixodbc-dev \
    r-cran-rodbc \
    apt-transport-https \
    libssl-dev \
    libsasl2-dev \
    openssl \
    curl \
    unixodbc \
    gnupg \
    libodbc1 \
    odbcinst1debian2 \
    freetds-dev \
    freetds-bin \
    tdsodbc \
    libc6 libc6-dev libc6-dbg

RUN apt-get update \
   && apt-get install --reinstall build-essential -y

RUN apt-get update && apt-get install -y locales && rm -rf /var/lib/apt/lists/* \
 && locale-gen "en_US.UTF-8"
ENV LANG=en_US.UTF-8 \
    LANGUAGE=en_US:en \
    LC_ALL=en_US.UTF-8

#get msodbcsql17 and install it
RUN curl https://packages.microsoft.com/keys/microsoft.asc | apt-key add -
RUN curl https://packages.microsoft.com/config/ubuntu/18.04/prod.list > /etc/apt/sources.list.d/mssql-release.list
RUN apt-get update -y
RUN ACCEPT_EULA=Y apt-get install -y msodbcsql17 

#rename SQL Driver title in odbcinst file
RUN sed -i 's/ODBC Driver 17 for SQL Server/SQL Server/' etc/odbcinst.ini


# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'shinythemes', 'devtools', 'tinytex','shinyWidgets','shinycssloaders','shinyBS','shinyjs','shinyAce','flexdashboard'), repos='https://cran.rstudio.com/')"

# install dependencies of the app
RUN R -e "install.packages(c('ggplot2','RMariaDB','DBI','pool','RODBC','scales','lattice','tidyverse','dplyr','gridExtra','DT','waterfalls','RODBCext','tcltk','xtable','plotly','fs','mailR','tinytex','V8','htmlTable','formattable','readxl','xlsx','janitor','sqldf','knitr','kableExtra','rpivotTable','pivottabler','excelR', 'rmarkdown'), repos='https://cran.rstudio.com/')"
RUN R -e "install.packages('RODBCext', repos='https://mran.microsoft.com/snapshot/2019-02-01/')"
RUN R -e "install.packages('RODBC', repos='https://mran.microsoft.com/snapshot/2019-02-01/')"
#RUN R -e "devtools::install_github('tidyverse/googlesheets4')"
RUN Rscript -e "install.packages('tidyverse', dependencies=TRUE)"
#RUN R -e "library(tidyverse)"

# copy the app to the image
RUN mkdir /root/bi
COPY bi /root/bi

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 8080

CMD ["R", "-e", "shiny::runApp('/root/bi', host='0.0.0.0', port=8080)"]