FROM ubuntu:latest
ARG USERID=6666

USER root
WORKDIR /root
RUN apt-get update
RUN apt-get install -y curl make build-essential libgmp-dev

RUN addgroup --gid $USERID haskell
RUN adduser --disabled-password --gecos "" --force-badname --uid $USERID --gid $USERID haskell
RUN mkdir /appdata
RUN chown haskell: /appdata

USER haskell
WORKDIR /home/haskell
RUN mkdir -p ~/.local/bin

# Setup linux stack
RUN curl -L https://get.haskellstack.org/stable/linux-x86_64.tar.gz | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'

USER haskell
ENV HOME /home/haskell
ENV PATH="${HOME}/.local/bin:${PATH}"
RUN stack setup 8.6.3
WORKDIR /appdata
ENTRYPOINT [ "stack" ]