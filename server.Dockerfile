FROM ubuntu:latest

RUN apt-get update && apt-get install -y ca-certificates libgmp10 libc6

# Copy local code to the container image.
USER root
ARG BUILD_NAME
WORKDIR /usr/arche
COPY .credentials.json /root/.config/gcloud/application_default_credentials.json
COPY .output/${BUILD_NAME}/arche-server /usr/arche/arche-server
COPY app/build /usr/arche/app/build
COPY config-server.json /usr/arche/config-server.json

# Service must listen to $PORT environment variable.
# This default value facilitates local development.
ENV PORT 8080

ENV PATH="/usr/arche:${PATH}"
# Run the web service on container startup.
CMD ["arche-server", "/usr/arche/config-server.json"]