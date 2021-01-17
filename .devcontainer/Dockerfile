# See here for image contents: https://github.com/microsoft/vscode-dev-containers/tree/v0.154.2/containers/ubuntu/.devcontainer/base.Dockerfile

# [Choice] Ubuntu version: bionic, focal
ARG VARIANT="focal"
FROM mcr.microsoft.com/vscode/devcontainers/base:0-${VARIANT}

# [Optional] Uncomment this section to install additional OS packages.
RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install --no-install-recommends \
    build-essential \
    less \
    vim \
    software-properties-common \
    && apt-get update
    && add-apt-repository -y ppa:hvr/ghc \
    && apt-get update \
    && apt-get install -y cabal-install ghc \
    && curl -sSL https://get.haskellstack.org/ | sh
