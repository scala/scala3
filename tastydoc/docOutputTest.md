## package Ident(example)

### import scala.collection.List(Ident(_))

### import scala.List(Ident(deprecated))
# class: Documentation
This class is used for testing tasty doc generation
@constructor create new object
@tparam T class type parameter
/

**c1**	class parameter 1

**c2**	class parameter 2

## Methods :
```scala
def <init>(ac: scala.Predef.String) : scala.Unit
```
Auxiliary constructor
/

**ac**	auxiliary parameter

```scala
def methodsWithParams(x: T, y: scala.Int) : immutable.List[scala.collection.Map[scala.Int, T]]
```
Test methods with params


/

**x**	parameter 1

**y**	parameter 2


**return** something is returned

```scala
def protectedMethod : scala.Nothing
```

```scala
def privateMethod : scala.Nothing
```

```scala
def abstractDefinition : scala.Int
```

```scala
def apply(idx: scala.Int) : T
```

```scala
def iterator : scala.collection.Iterator[T]
```

```scala
def length : scala.Int
```

```scala
def docWithMd : scala.Nothing
```
An example documention with markdown formatting

**I'm bold**

*I'm italic*

`some code`
```scala
def someScalaCode(x: String) = println("Hello " + x)
```
1. I'm a list
/
## Values :
```scala
val c1 : scala.Predef.String
```

```scala
val c2 : collection.immutable.List[T]
```

```scala
val v : scala.Int
```
Test value
/
```scala
val protectedVal : scala.Nothing
```

```scala
val privateVal : scala.Nothing
```

## Types :

