FROM debian:bookworm

RUN apt-get -y update && apt-get install -y racket
WORKDIR /usr/rkt/src
ADD src/ .
