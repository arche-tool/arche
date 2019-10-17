FROM ubuntu:latest

USER root
WORKDIR /root
RUN dpkg --add-architecture i386
RUN apt-get update
RUN apt-get install -y wine64 wine32 curl unzip xz-utils

RUN adduser haskell
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
RUN curl -L https://downloads.haskell.org/~ghc/7.10.3/ghc-7.10.3-x86_64-unknown-mingw32.tar.xz | tar xJ -C ~/.wine/drive_c/ghc
RUN echo 'WINEPATH=C:\\\\ghc\\\\ghc-7.10.3\\\\bin wine /home/haskell/.local/bin/win_stack.exe --system-ghc --no-install-ghc "$@"' > ~/.local/bin/stack.exe
RUN chmod +x ~/.local/bin/stack.exe
RUN mkdir -p ~/.wine/drive_c/users
RUN chmod -R o+w "$HOME/.wine/drive_c/users/"

USER haskell
ENV HOME /home/haskell
RUN stack.exe path
WORKDIR /appdata