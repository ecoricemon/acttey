# Project development environment on docker

## How to run

Using compose, you can easily run the environment.
This compose will download an image from registry.

```sh
# Run detached container
docker compose up -d

# Attach to the container
docker compose exec rust-wasm-dev bash
```

Of course you can build image from Dockerfile on your own.

```sh
docker build -t <name> .
```

If you're successfully attached to the container, 
you can run some examples or test the project.

```sh
# Run an example
cargo run --example parallel

# Test (unit + integration + wasm)
./run.sh test
```

## Other docker commands you may need
```sh
# Stop the container
docker compose stop rust-wasm-dev

# Start the container
docker compose start rust-wasm-dev

# Remove the container
docker compose down
```
