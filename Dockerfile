FROM silex/emacs:latest

RUN apt-get update
RUN apt-get install -y git make bsdmainutils

CMD bash