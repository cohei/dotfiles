FROM ubuntu:20.04

RUN apt-get update -qq && apt-get install --no-install-recommends --yes \
        ca-certificates \
        curl \
        git

ENV USER docker
RUN useradd --create-home $USER
USER $USER
WORKDIR /home/$USER
