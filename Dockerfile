FROM silex/emacs:latest

RUN apt-get update
RUN apt-get install -y git make bsdmainutils

WORKDIR /home/docker/tmacs

# If we don't do this, then the directory gets created in the
# container filesystem with root ownership :/
RUN mkdir -p "$HOME/.emacs.d/straight/repos"

CMD bash