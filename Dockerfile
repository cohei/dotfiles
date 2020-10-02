FROM cohei/ubuntu-with-user

USER root
RUN apt-get update -qq && apt-get install --no-install-recommends --yes \
        ca-certificates \
        curl \
        git
USER docker

RUN bash -c 'set -o pipefail && curl -L https://dotfiles.cohei.me | bash'
