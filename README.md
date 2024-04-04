**WIP**

# Star Trek Action Figures Inc.

This repo was put together for a informal talk about Functional Programming. 

## Functional Programming

### Functions all the way down
> Programming as if functions truly matter

![A call-graph showing how the impure, IO-performing part of the application is small](images/callgraph.png "Our Application Call Graph")

## Algebraic Data Types (ADTs)

### Sums

### Products

## Function composition

### Simple composition

### Effectful composition

### Chaining IO actions

## Haskell Syntax

### Function Application

### Currying

### `Do` notation

## Domain Modeling with FP

## Further reading

## Running the app

### Docker
There is a Dockerfile included that can be used to build a Docker image and run the app containerized. 

This is a good option if you don't have and don't want the Haskell toolchain installed in your machine. The Dockerfile leverages caching so you should be able to experiment with the source code without having to compile the whole application each time.

The following command will build the image and run the container. The app will be listening for requests on port 3000.

`$ docker build . -t star-trek-haskell && docker run -t -p 3000:3000 star-trek-haskell`

Remember to re-run the above command everytime you change the source code.

### Cabal
Run `cabal run`. This should (re)compile the application and run it immediately after.

#### GHCUp - Haskell toolchain
For most systems, the easiest way to install the Haskell toolchain is by installing [GHCUp](https://www.haskell.org/ghcup/).

Once that is done, use it to install [Cabal](https://www.haskell.org/cabal/), the Haskell build and package system.
