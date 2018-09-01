FROM ubuntu:18.10

RUN apt-get update -qq && apt-get install --yes curl git

ENV USER docker

RUN useradd --create-home $USER

ENV HOME /home/$USER

COPY . $HOME/.dotfiles
RUN chown -R docker $HOME/.dotfiles

USER $USER
WORKDIR $HOME

RUN .dotfiles/install.sh
