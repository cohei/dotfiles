FROM ubuntu:18.10

RUN apt-get update -qq && apt-get install --yes curl git

ENV USER docker
RUN useradd --create-home $USER
USER $USER
WORKDIR /home/$USER

COPY --chown=docker:docker . .dotfiles
RUN .dotfiles/install.sh
