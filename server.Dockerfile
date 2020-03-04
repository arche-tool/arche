FROM ubuntu:18.04

RUN apt-get update && apt-get install -y ca-certificates build-essential libgmp-dev zlib1g-dev language-pack-en-base sysbench

# Copy local code to the container image.
USER root
ARG BUILD_NAME
WORKDIR /usr/src/app
COPY benchmark/data /usr/src/app/benchmark/data
COPY .aws-keys /root/.aws-keys
COPY .output/${BUILD_NAME}/arche-server /usr/src/app/arche-server

# Service must listen to $PORT environment variable.
# This default value facilitates local development.
ENV PORT 8080

ENV PATH="/usr/src/app:${PATH}"
ENV LANG en_US.utf8
# Run the web service on container startup.
CMD ["arche-server"]