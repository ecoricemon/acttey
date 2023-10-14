## How to build in **debug** mode

```sh
wasm-pack build --dev
```

## How to build in **release** mode

```sh
wasm-pack build
```

## How to test

```sh
# Chrome (chromedriver)
wasm-pack test --chrome --headless --workspace
# Firefox
wasm-pack test --firefox --headless --workspace
# Safari
wasm-pack test --safari --headless --workspace
```
