**WIP**

# Star Trek Action Figures Inc.

This repo was put together for a informal talk about Functional Programming. 

## Functional Programming

### Functions all the way down
> Programming as if functions truly matter

![A call-graph showing how the impure, IO-performing part of the application is small](images/callgraph.png "Our Application Call Graph")

## Algebraic Data Types (ADTs)

We can see types as sets of values with a certain structure. We usually say a value _inhabits_ a given type if it belongs to the set of values of that type. For instance, the type `Bool` is inhabited by 2 values: `True` and `False`. We say the size of `Bool` is 2. The type `String` is inhabited by all possible sequences of characters, the type `Int32` by all signed integers representable with 32 bits, and so on.

When building programs, we very often need to build data with some structure. Haskell let's us express structured, composable data with __Algebraic Data Types__. As we can see next, this powerful feature allows the developer to build big types out of small ones, like russian dolls or Lego, combining them at will in order to express sofisticated domains.

### Sums

`Bool` is a sum type and `True` and `False` are its data constructors. Data constructors are functions which return a value that inhabits the type they belong to. In this case, `True` and `False` take no arguments, and are thus called _nullary_ constructors. Any given value of type `Bool` can _either_ be `True` or `False`, not both!

```haskell
data Bool = True | False
```

`Maybe a` is also a sum type. It is used to express the possibility of something not existing. For instance, if we want to parse a string to obtain an integer, we need to account for the possibility that the parsing can fail. This could be expressed by a function that returns a `Maybe Int`. 

```haskell
data Maybe a = Nothing | Just a
```

The `a` in `Maybe a` is a __type variable__, also known sometimes as a __generic__. `Just` is a data constructor that takes a value of type `a` and returns a value of type `Maybe a`. Some of the values inhabiting `Maybe Int` are the following: `Nothing`, `Just (-2345)`, `Just 0`, `Just 1`, `Just 42`, etc. Some values _not_ inhabiting `Maybe Int` are: `Just "foo"`, `Just True`, `Just 3.14159`. These last ones are not acceptable because we are _fixing_ the type variable `a` (in `Maybe a`) to be an `Int`.

### Products

### Pattern Matching

## Function composition

### Simple composition

### Effectful composition

### Chaining IO actions

## Haskell Syntax

### Function Application

### Currying

### `do` notation

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
