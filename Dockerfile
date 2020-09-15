FROM rocker/r-base

USER root

RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y git

# Give back control
USER root

# Cleaning
RUN apt-get clean
