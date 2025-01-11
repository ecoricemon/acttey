FROM rust:1.82-slim-bookworm

# For cargo build
RUN apt-get update && apt-get -y install pkg-config libssl-dev

# For wasm
RUN apt-get -y install curl && \
    curl -fsSL https://deb.nodesource.com/setup_22.x | bash - && \
    apt-get -y install nodejs && \
    rustup toolchain install nightly-2024-06-20 --component rust-src && \
    curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh && \
    npx -y playwright@1.46.1 install chromium --with-deps
