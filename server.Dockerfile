FROM ubuntu:latest

RUN apt-get update && apt-get install -y ca-certificates libgmp10 libc6

# Copy local code to the container image.
USER root
ARG BUILD_NAME
ARG GCLOUD_SERVICE_KEY
ARG SIGNER_SERVICE_KEY
ARG OAUTH_CLIENT_ID
WORKDIR /usr/arche

ENV GCLOUD_SERVICE_KEY=${GCLOUD_SERVICE_KEY}
ENV SIGNER_SERVICE_KEY=${SIGNER_SERVICE_KEY}
ENV OAUTH_CLIENT_ID=${OAUTH_CLIENT_ID}

# Set GCP credentials
RUN mkdir -p /root/.config/gcloud/
RUN echo $(echo "$GCLOUD_SERVICE_KEY" | base64 -d) > /root/.config/gcloud/application_default_credentials.json

# Copy server bin
COPY .output/${BUILD_NAME}/arche-server /usr/arche/arche-server

# Service must listen to $PORT environment variable.
ENV PORT 8080
ENV PATH="/usr/arche:${PATH}"

RUN echo '#!/bin/bash' > run.sh
RUN echo "/usr/arche/arche-server --port=$PORT --oauth-client-id=$OAUTH_CLIENT_ID --signer-credentials=$SIGNER_SERVICE_KEY \"\$@\" " >> run.sh
RUN chmod +x run.sh

# Run the web service on container startup.
#ENTRYPOINT bash
ENTRYPOINT ["run.sh"]
CMD ["full-api"]