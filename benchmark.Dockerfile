FROM ubuntu:16.04

ARG BUILD_NAME

RUN apt-get update && apt-get install -y ca-certificates build-essential libgmp-dev zlib1g-dev language-pack-en-base sysbench

# Copy local code to the container image.
USER root
WORKDIR /usr/src/app
COPY benchmark/data /usr/src/app/benchmark/data
COPY .output/${BUILD_NAME}/arche-bench /usr/src/app/arche-bench

ENV PATH="/usr/src/app:${PATH}"
ENV LANG en_US.utf8
# Run the web service on container startup.
CMD ["arche-bench"]