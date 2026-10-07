# syntax=docker/dockerfile:1.7-labs
# ^ we want the --parent flag for COPY

FROM ubuntu:noble as build

SHELL ["/bin/bash", "-o", "pipefail", "-c"]

# Args
ARG USER_NAME=haskeller
ARG GHC_VERSION=9.14.1
ARG CABAL_VERSION=3.18.1.0
ARG UID=1001
ARG GID=1001
ARG DUCKDB_PREVIEW=false

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
      cmake \
      python3 \
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
COPY scripts/build-duckdb-preview.sh scripts/duckdb-api.py /preview/scripts/
COPY duckdb-ffi/vendor/duckdb-api.* /preview/duckdb-ffi/vendor/
COPY duckdb-ffi/cbits/duckdb.h /preview/duckdb-ffi/cbits/duckdb.h
ARG DUCKDB_BUILD_JOBS=2
RUN if [ "$DUCKDB_PREVIEW" = true ]; then \
    DUCKDB_BUILD_JOBS="$DUCKDB_BUILD_JOBS" bash /preview/scripts/build-duckdb-preview.sh /tmp/duckdb-preview && \
    cp /tmp/duckdb-preview/native/libduckdb.so /usr/lib/ && \
    cp /tmp/duckdb-preview/native/duckdb*.h /usr/include/ && \
    ldconfig; \
    else \
    curl --fail --location --proto '=https' --proto-redir '=https' -o /tmp/libduckdb.zip https://github.com/duckdb/duckdb/releases/download/v1.5.6/libduckdb-linux-amd64.zip && \
    echo 'b845005f5132a7d8180057c35e14a7626632258782f871a90861b19c1c03841b  /tmp/libduckdb.zip' | sha256sum -c - && \
    unzip libduckdb.zip && \
    mv libduckdb.so /usr/lib/libduckdb.so && \
    mv duckdb.h /usr/include/ && \
    ldconfig && \
    rm libduckdb.zip; \
    fi

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
RUN sed -i "s/with-compiler: ghc-.*/with-compiler: ghc-${GHC_VERSION}/" /app/cabal.project

WORKDIR /app
# Using the cabal files, we can build the dependencies
RUN printf 'package duckdb-ffi\n  flags: +systemlib\n' > cabal.project.local
RUN if [ "$DUCKDB_PREVIEW" = true ]; then \
    printf 'package duckdb-ffi\n  flags: +systemlib +duckdb-v2\npackage duckdb-simple\n  flags: +duckdb-v2\n' > cabal.project.local; \
    fi
RUN cabal update && \
    cabal build all --only-dependencies --project-file=cabal.project --project-dir=/app

COPY --link --parents --chown=${UID}:${GID}  duckdb-* /app/



WORKDIR /app
# Build all the packages
RUN cabal build all --project-file=cabal.project --project-dir=/app





# Test the packages
RUN if [ "$DUCKDB_PREVIEW" = true ]; then native_version=2.0.0-dev0; else native_version=1.5.6; fi && \
    DUCKDB_TEST_VERSION="$native_version" cabal test all --project-file=cabal.project --project-dir=/app --test-show-details=streaming

# Generate Haddocks for all packages
RUN cabal haddock all --project-file=cabal.project --project-dir=/app --haddock-for-hackage --enable-documentation

# Generate the sdist
RUN cabal sdist all --project-file=cabal.project --project-dir=/app

FROM build as final
COPY --from=build \
   /app/dist-newstyle/sdist/*.tar.gz  \
   /app/dist-newstyle/*-docs.tar.gz \
   /dist/
