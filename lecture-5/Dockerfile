FROM dragonflyscience/dragonverse-22.04:2023-07-05

RUN Rscript -e "install.packages('ISLR', repos = 'https://cloud.r-project.org/')"
RUN Rscript -e "install.packages('tree', repos = 'https://cloud.r-project.org/')"
RUN Rscript -e "install.packages('randomForest', repos = 'https://cloud.r-project.org/')"
RUN Rscript -e "install.packages('gbm', repos = 'https://cloud.r-project.org/')"
RUN Rscript -e "remotes::install_github('gbm-developers/gbm3')"
