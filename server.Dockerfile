FROM ubuntu:latest

RUN apt-get update && apt-get install -y ca-certificates libgmp10 libc6

# Copy local code to the container image.
USER root
ARG BUILD_NAME
ARG GCLOUD_SERVICE_KEY
ARG OAUTH_CLIENT_ID
WORKDIR /usr/arche

# Set GCP credentials
RUN mkdir -p /root/.config/gcloud/
ENV GCLOUD_SERVICE_KEY=${GCLOUD_SERVICE_KEY}
RUN echo $(echo "$GCLOUD_SERVICE_KEY" | base64 -d) > /root/.config/gcloud/application_default_credentials.json

# Copy server bin
COPY .output/${BUILD_NAME}/arche-server /usr/arche/arche-server

# Service must listen to $PORT environment variable.
ENV PORT 8080
ENV PATH="/usr/arche:${PATH}"

# Run the web service on container startup. Needs external arg to set RunMode.
CMD ["arche-server", "--port ${PORT}" "--oauth-client-id ${OAUTH_CLIENT_ID}", "full-api"]