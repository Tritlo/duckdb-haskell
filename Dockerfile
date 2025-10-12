# syntax=docker/dockerfile:1.7-labs
# ^ we want the --parent flag for COPY

FROM ubuntu:noble as build

SHELL ["/bin/bash", "-o", "pipefail", "-c"]

# Args
ARG USER_NAME=haskeller
ARG GHC_VERSION=9.6.6
ARG CABAL_VERSION=3.16.0.0
ARG UID=1001
ARG GID=1001

ENV DEBIAN_FRONTEND=noninteractive \
    TZ=Europe/Stockholm \
    LANG=C.UTF-8 \
    LC_ALL=C.UTF-8 \
    USER_NAME=${USER_NAME}\
    UID=${UID} \
    GID=${GID}


# Install dependencies
# We ignore the "pin versions" warning here, as we are not aiming for long-term
# stability.
# hadolint ignore=DL3008
RUN --mount=type=cache,id=apt-cache,target=/var/cache/apt \
    --mount=type=cache,id=apt-libs,target=/var/lib/apt \
    ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && \
    echo $TZ > /etc/timezone && \
    apt-get update && \
    apt-get install -y --no-install-recommends \
      sudo \
      git \
      curl \
      ca-certificates \
      locales \
      build-essential \
      libffi-dev \
      libgmp-dev \
      libncurses-dev \
      unzip \
      && \
    apt-get autoremove -y && \
    apt-get clean -y && \
    sed -i 's/^# *en_US.UTF-8/en_US.UTF-8/' /etc/locale.gen && locale-gen && \
    rm -rf /var/lib/apt/lists/*

# user
RUN groupadd -g "$GID" -o "$USER_NAME" && \
    useradd -l -m -u "$UID" -g "$GID" -G sudo -o -s /bin/bash -d /home/$USER_NAME "$USER_NAME" && \
    echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers

WORKDIR /tmp
RUN curl -L -o /tmp/libduckdb.zip https://github.com/duckdb/duckdb/releases/download/v1.4.1/libduckdb-linux-amd64.zip && \
    unzip libduckdb.zip && \
    mv libduckdb.so /usr/lib/libduckdb.so && \
    mv duckdb.h /usr/include/ && \
    ldconfig && \
    rm libduckdb.zip

# Switch to the new user
USER ${UID}:${GID}
WORKDIR /home/$USER_NAME

# toolchain env
ENV GHCUP_INSTALL_BASE_PREFIX=/home/$USER_NAME \
    HOME=/home/$USER_NAME \
    PATH=/home/$USER_NAME/.cabal/bin:/home/$USER_NAME/.ghcup/bin:$PATH \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_NO_UPGRADE=1 \
    BOOTSTRAP_HASKELL_MINIMAL=1 \
    BOOTSTRAP_HASKELL_INSTALL=0

# ghcup + toolchain
RUN curl -fsSL https://get-ghcup.haskell.org -o /tmp/get-ghcup.sh && \
    chmod +x /tmp/get-ghcup.sh && \
    /tmp/get-ghcup.sh && \
    ghcup install ghc "$GHC_VERSION" && \
    ghcup set ghc "$GHC_VERSION" && \
    ghcup install cabal "$CABAL_VERSION" && \
    cabal --version && ghc --version


# We copy only the cabal files, since these won't change usually. This lets us avoid
# rebuilding the dependencies all the time.
COPY --parents --chown=${UID}:${GID} duckdb-*/*.cabal /app/
COPY --chown=${UID}:${GID} cabal.project /app/cabal.project

WORKDIR /app
# Using the cabal files, we can build the dependencies
RUN cabal update && \
    cabal build all --only-dependencies --project-file=cabal.project --project-dir=/app

COPY --link --parents --chown=${UID}:${GID}  duckdb-* /app/



WORKDIR /app
# Build all the packages
RUN cabal build all --project-file=cabal.project --project-dir=/app





# Test the packages
RUN cabal test all --project-file=cabal.project --project-dir=/app --test-show-details=streaming

# Generate Haddocks for all packages
RUN cabal haddock all --project-file=cabal.project --project-dir=/app --haddock-for-hackage --enable-documentation

# Generate the sdist
RUN cabal sdist all --project-file=cabal.project --project-dir=/app

FROM build as final
COPY --from=build \
   /app/dist-newstyle/sdist/*.tar.gz  \
   /app/dist-newstyle/*-docs.tar.gz \
   /dist/
