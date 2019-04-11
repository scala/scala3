example
level2
import scala.collection.Ident(_)

import scala.Ident(deprecated)

import scala.annotation.Ident(_)

import scala.math.{Ident(Pi), Ident(max)}

# class Documentation

## Companion object : example.level2.Documentation

```scala
sealed abstract class Documentation[T, Z <: scala.Int]
```

Some(ParsedComment(This class is used for testing tasty doc generation,List(Bryan Abate),List(),List(),Map(),Map(c1 -> class parameter 1, c2 -> class parameter 2),Map(T -> class type parameter),List(),List(),List(),List(),List(),List(),List(create new object),List(),Map(),Map(),Map(),List(),List()))
## Annotations:


## Constructors:
```scala
def this(c1: scala.Predef.String, c2: scala.collection.immutable.List[T])
```
```scala
def this(ac: scala.Predef.String)
```
```scala
def this()
```
```scala
def this(x: T)
```

## Members:

### Definitions: 
```scala
val c2: scala.collection.immutable.List[Documentation.this.T]
```
None
```scala
class innerDocumentationClass
```

*None*
```scala
sealed class CaseImplementThis
```

*None*
```scala
class IAmACaseClass
```

*None*
```scala
final val IAmACaseClass: Documentation.this.IAmACaseClass
```
None
```scala
final class IAmACaseClass$
```

*None*
```scala
final val testObject: Documentation.this.testObject
```
None
```scala
final class testObject$
```

*None*
```scala
def methodsWithParams(x: Documentation.this.T, y: scala.Int): scala.collection.immutable.List[scala.collection.Map[scala.Int, Documentation.this.T]]
```
Some(ParsedComment(Test methods with params
,List(),List(),List(something is returned),Map(),Map(y -> parameter 2, x -> parameter 1),Map(),List(),List(),List(),List(),List(),List(),List(),List(),Map(),Map(),Map(),List(),List()))
```scala
def methodsWithImplicit(x: scala.Int)(implicit imp: scala.Int, notImp: scala.Predef.String): scala.Nothing
```
None
```scala
val v: scala.Int
```
Some(ParsedComment(Test value
,List(),List(),List(),Map(),Map(),Map(),List(),List(),List(),List(),List(),List(),List(),List(),Map(),Map(),Map(),List(),List()))
```scala
protected def protectedMethod: scala.Nothing
```
None
```scala
private def privateMethod: scala.Nothing
```
None
```scala
protected val protectedVal: scala.Nothing
```
None
```scala
private val privateVal: scala.Nothing
```
None
```scala
def abstractDefinition: scala.Int
```
None
```scala
def apply(idx: scala.Int): Documentation.this.T
```
None
```scala
def iterator: scala.collection.Iterator[Documentation.this.T]
```
None
```scala
def length: scala.Int
```
None
```scala
def docWithMd: scala.Nothing
```
Some(ParsedComment(An example documention with markdown formatting

**I'm bold**

*I'm italic*

`some code`
```scala
def someScalaCode(x: String) = println("Hello " + x)
```
1. I'm a list
,List(),List(),List(),Map(),Map(),Map(),List(),List(),List(),List(),List(),List(),List(),List(),Map(),Map(),Map(),List(),List()))
```scala
def functionWithType[U >: scala.Predef.String](): U
```
None
```scala
val complexTypeVal: scala.Int | scala.collection.immutable.List[scala.collection.immutable.List[Documentation.this.T]] & scala.Predef.String | scala.Tuple2[scala.Double | scala.Int, scala.Double] | scala.Function1[scala.Int, scala.Predef.String]
```
None
```scala
type typeExamle: 
```
None


```scala
final val Documentation: example.level2.Documentation
```
Some(ParsedComment(Companion object
,List(),List(),List(),Map(),Map(),Map(),List(),List(),List(),List(),List(),List(),List(),List(),Map(),Map(),Map(),List(),List()))

# class Documentation$

## Companion object : example.level2.Documentation$

```scala
final class Documentation$
```

Some(ParsedComment(Companion object
,List(),List(),List(),Map(),Map(),Map(),List(),List(),List(),List(),List(),List(),List(),List(),Map(),Map(),Map(),List(),List()))
## Annotations:


## Constructors:
```scala
def this()
```

## Members:

### Definitions: 
```scala
private def writeReplace(): scala.AnyRef
```
None

