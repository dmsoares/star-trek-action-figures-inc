**WIP**

# Star Trek Action Figures Inc.

This repo was put together for an informal talk about strongly typed functional programming. 

It is a very simple web service with only one route: POST /order. It accepts an order of Star Trek action figures, and runs a `PlaceOrder` workflow that checks the validity of the order and publishes events for other hypothetical services to consume.

Try running the server and make a request with this payload (see [here](#running-the-app) on how to run the server):

```json
{
	"customerId": 1,
	"orderLines": [
		{
			"productName": "Picard",
			"quantity": 3
		}
	],
	"shippingAddress": "Earth"
}
```

For the different characters available, you can check the file `/db/products.txt`.



TODO: right now, the only way to see anything happening is to check `/db/events.json` for new events. The server should probably respond to successful requests with the `OrderPlaced` event.

## Functional Programming

### Functions all the way down
> Programming as if functions truly matter. (Scott Wlaschin, author of _Domain Modeling Made Functional_)

![A call-graph showing how the impure, IO-performing part of the application is small](images/callgraph.png "Our Application Call Graph")

## Algebraic Data Types (ADTs)

When building programs, we very often need to build data with some structure. Haskell let's us express structured, composable data with __Algebraic Data Types__. As we can see next, this powerful feature allows the developer to build big types out of small ones, like russian dolls or Lego, combining them at will in order to express sofisticated domains.

We can see types as sets of values with a certain structure. We usually say a value _inhabits_ a given type if it belongs to the set of values of that type. For instance, the type `Bool` is inhabited by 2 values: `True` and `False`. We say the size of `Bool` is 2. The type `String` is inhabited by all possible sequences of characters, the type `Int32` by all signed integers representable with 32 bits, and so on.

Haskell's type system is a __nominal type system__. All values are originally produced by some __data constructor__, which determines their type. This is in contrast with, for example, the typing system used by Typescript where two values are of the same type if their structure (e.g, the entries of an object) is the same.

A __nominal__ system is in some ways less flexible than a __structural__ one, but brings better type-safety by preventing accidental type equivalence.

__Data constructors__ are functions which return a value that inhabits the type they belong to. The 'mark' of the data constructor remains with the value throughout its life and gives rise to a very convenient idiom called __pattern matching__, which we'll see in a moment.

The building blocks of Algebraic Data Types are __Sums__ and __Products__.

### Sums

#### Bool

`Bool` is a sum type and `True` and `False` are its data constructors. In this case, `True` and `False` take no arguments, and are thus called _nullary_ constructors. Any given value of type `Bool` can _either_ be `True` or `False`, not both!

```haskell
data Bool = True | False
```

#### Maybe

`Maybe a` is also a sum type. It is used to express the possibility of something not existing. For instance, if we want to parse a string to obtain an integer, we need to account for the possibility that the parsing can fail. This could be expressed by a function that returns a `Maybe Int`. 

```haskell
data Maybe a = Nothing | Just a
```

The `a` in `Maybe a` is a __type variable__, also known sometimes as a __generic__. `Just` is a (unary) data constructor that takes a value of type `a` and returns a value of type `Maybe a`. Some of the values inhabiting `Maybe Int` are the following: `Nothing`, `Just (-2345)`, `Just 0`, `Just 1`, `Just 42`, etc. Some values _not_ inhabiting `Maybe Int` are: `Just "foo"`, `Just True`, `Just 3.14159`. These last ones are not acceptable because we are _fixing_ the type variable `a` (in `Maybe a`) to be an `Int`.

The size of `Maybe Int` is the _sum_ of all `Int`s (in `Just Int` ) plus 1 (for `Nothing`). 

#### Either

`Either a b` is another sum type. 

```haskell
data Either a b = Left a | Right b
```

Following the same logic above, its size is the _sum_ of all possible `a`s and all possible `b`s.

`Either a b` can be used to express a computation that can have one of two types of outcome. If we want to tokenize a string of code, for example, it may be interesting to _either_ return, say, a list of tokens or an error message signalling at which character the parsing failed. This could be achieved with `Either Error (List Token)`, where `Error` could be an alias for `String`.

### Products

#### Tuple

A tuple is the canonical product type:

```haskell
data (,) a b = (,) a b
```

It can be a bit hard to see it immediately, so let's define an equivalent type:

```haskell
data Tuple a b = MkTuple a b
```

A `Tuple` is a type with only one data constructor `MkTuple`. As always, this data constructor is a function. In this case, one which, given two arguments of type `a` and `b`, returns a value which inhabits the type `Tuple`.

Tuples are often used when we have heterogenous data that we want to package together. We may want to package arguments to a function that outputs a formatted string with the name and age of a person: `Tuple Name Age`, or simply `(Name, Age)` if we choose to use the canonical `(,)` type. 

Naturally, we are free to produce our own product types. Also note that their data constructor can have as many parameters as we wish:

```haskell
-- these are simple type aliases and are mainly 
-- used to make our code more expressive
type Name = String
type Age = Int
type PlaceOfBirth = String

data Person = Person Name Age PlaceOfBirth
--   ^ type   ^ data constructor
-- types and data constructors live in different namespaces and can be named the same  
```

We can have as many `Person` values as the possible combinations of `Name`, `Age` and `PlaceOfBirth`. Thus, the size of the type `Person` can be calculated as the _product_ of the sizes of the arguments to its data constructor. Put simply: size of `Person` = (size of `Name`) * (size of `Age`) * (size of `PlaceOfBirth`).

#### Records

In Haskell, __record types__ are not first class citizens. This has been a somewhat contentious topic, but some recent developments have made operating with records more ergonomic, although we won't go into much detail here.

In Haskell, a __record__ is just a product type with some syntactic sugar to allow named fields:

```haskell
data Person = Person {
    name         :: Name,
    age          :: Age,
    placeOfBirth :: PlaceOfBirth
}
```

This type is the same type we declared before. Only now we can access the fields of a `Person` value by name. `name`, `age` and `placeOfBirth` are functions that, when applied to a `Person`, return the respective field.

The `::` symbol translates to "_of type_". `name :: Name` reads "_name of type Name_".

### Composing Types

### Recursive Types

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
