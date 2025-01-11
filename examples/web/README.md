# Demo for web target

This demo shows drawing [Mandelbrot fractal](https://en.wikipedia.org/wiki/Mandelbrot_set) structure on browsers using this crate.

You can choose single or multiple CPUs or GPU to draw the image.

![image](Mandelbrot.png)

## Prerequisites

Node.js installation

## How to build

```sh
npm install
npm run build-r
```

## How to start server

```sh
npm run start
```

Then you can see an URL in terminal.

## Try docker

There's a docker image you can try without build in [docker hub](https://hub.docker.com).

```sh
docker run --rm -p 8080:8080 ecoricemon/my-ecs-example
```

Then, visit http://localhost:8080 on your browser.
