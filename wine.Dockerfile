FROM ubuntu:18.04
ARG USERID=6666

USER root
WORKDIR /root
RUN dpkg --add-architecture i386
RUN apt-get update
RUN apt-get install -y wine64 wine32 curl unzip xz-utils

RUN addgroup --gid $USERID haskell
RUN adduser --disabled-password --gecos "" --force-badname --uid $USERID --gid $USERID haskell
RUN mkdir /appdata
RUN chown haskell: /appdata

USER haskell
WORKDIR /home/haskell
RUN mkdir -p ~/.local/bin

# Setup wine stack
RUN curl -o stack.zip -L 'https://get.haskellstack.org/stable/windows-x86_64.zip'
RUN unzip stack.zip -d win_stack
RUN mv win_stack/stack.exe ~/.local/bin/win_stack.exe
RUN rm stack.zip
RUN rm -rf win_stack

# Set local GHC
RUN mkdir -p ~/.wine/drive_c/ghc
RUN curl -L https://downloads.haskell.org/~ghc/8.6.3/ghc-8.6.3-x86_64-unknown-mingw32.tar.xz | tar xJ -C ~/.wine/drive_c/ghc
RUN echo '#!/bin/sh' > ~/.local/bin/stack.exe
RUN echo 'WINEPATH=C:\\\\ghc\\\\ghc-8.6.3\\\\bin wine /home/haskell/.local/bin/win_stack.exe --system-ghc --no-install-ghc "$@"' >> ~/.local/bin/stack.exe
RUN chmod +x ~/.local/bin/stack.exe
RUN mkdir -p ~/.wine/drive_c/users
RUN chmod -R o+w "$HOME/.wine/drive_c/users/"

USER haskell
ENV HOME /home/haskell
ENV PATH="${HOME}/.local/bin:${PATH}"
WORKDIR /appdata
ENTRYPOINT [ "stack.exe" ]