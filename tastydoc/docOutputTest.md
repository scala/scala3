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

Some(Comment(<p>This class is used for testing tasty doc generation</p>,<p>This class is used for testing tasty doc generation</p>,List(<p>Bryan Abate</p>),List(),None,Map(),Map(c1 -> <p>class parameter 1</p>, c2 -> <p>class parameter 2</p>),Map(T -> <p>class type parameter</p>),None,None,List(),None,List(),List(),Some(<p>create new object</p>),None,Map(),Map(),Map(),List()))
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
Some(Comment(<p>Test methods with params
</p>,<p>Test methods with params
</p>,List(),List(),Some(<p>something is returned</p>),Map(),Map(y -> <p>parameter 2</p>, x -> <p>parameter 1</p>),Map(),None,None,List(),None,List(),List(),None,None,Map(),Map(),Map(),List()))
```scala
def methodsWithImplicit(x: scala.Int)(implicit imp: scala.Int, notImp: scala.Predef.String): scala.Nothing
```
None
```scala
val v: scala.Int
```
Some(Comment(<p>Test value
</p>,<p>Test value
</p>,List(),List(),None,Map(),Map(),Map(),None,None,List(),None,List(),List(),None,None,Map(),Map(),Map(),List()))
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
Some(Comment(<p>An example documention with markdown formatting</p><p>**I'm bold**</p><p>*I'm italic*</p><p><code>some code</code>
<code><code><code>scala
def someScalaCode(x: String) = println("Hello " + x)
<code><code><code>
1. I'm a list
</code></code></code></code></code></code></p>,<p>An example documention with markdown formatting</p>,List(),List(),None,Map(),Map(),Map(),None,None,List(),None,List(),List(),None,None,Map(),Map(),Map(),List()))
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
Some(Comment(<p>Companion object
</p>,<p>Companion object
</p>,List(),List(),None,Map(),Map(),Map(),None,None,List(),None,List(),List(),None,None,Map(),Map(),Map(),List()))

# class Documentation$

## Companion object : example.level2.Documentation$

```scala
final class Documentation$
```

Some(Comment(<p>Companion object
</p>,<p>Companion object
</p>,List(),List(),None,Map(),Map(),Map(),None,None,List(),None,List(),List(),None,None,Map(),Map(),Map(),List()))
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

