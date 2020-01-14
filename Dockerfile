# Install R version 3.5
FROM r-base:3.5.0

# Install Ubuntu packages
RUN apt-get update && apt-get install -y \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev/unstable \
    libxt-dev \
    libssl-dev

# Add shiny user
RUN groupadd  shiny \
&& useradd --gid shiny --shell /bin/bash --create-home shiny

# Download and install ShinyServer (latest version)
RUN wget --no-verbose https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb

# Install R packages that are required
# TODO: add further package if you need!
RUN R -e "install.packages(c('shiny', 'shinyjs', 'DT', 'dashboardthemes', 'formattable', 'shinythemes', 'shinydashboard', 'shinyWidgets', 'googleAuthR', 'googleID', 'httr', 'gargle', 'stringi', 'sass'), repos='http://cran.rstudio.com/')"

# R Firebase
RUN R -e "install.packages('remotes')"
RUN R -e "remotes::install_github('Kohze/fireData')"

# Copy configuration files into the Docker image
COPY shiny-server.conf  /etc/shiny-server/shiny-server.conf
COPY /app /srv/shiny-server/

# Make the ShinyApp available at port 80
EXPOSE 5555

# Copy further configuration files into the Docker image
COPY shiny-server.sh /usr/bin/shiny-server.sh
# Make executable
RUN ["chmod", "+x", "/usr/bin/shiny-server.sh"]
# allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server
# run server
CMD ["/usr/bin/shiny-server.sh"]
