FROM fpco/stack-build:lts-15.15 as build

COPY . /opt/build/

WORKDIR /opt/build

RUN stack build --system-ghc

RUN mv "$(stack path --local-install-root --system-ghc)/bin" /opt/build/bin

# -------------------------------------------------------------------------------------------
# Base image for stack build so compiled artifact from previous
# stage should run
FROM ubuntu:18.04 as app
RUN mkdir -p /opt/app
WORKDIR /opt/app

RUN apt-get update
RUN apt-get install git -y
RUN apt-get upgrade -y
RUN apt-get autoremove -y

COPY --from=build /opt/build/bin .

RUN mkdir -p /opt/app/config
COPY ./resources/config-sample.yaml /opt/app/config/config.yaml

CMD [ "/opt/app/github-webhook-listener-exe", "-c", "/opt/app/config/config.yaml" ]
