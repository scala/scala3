scala
collection
immutable
import scala.collection.generic.Ident(_)

import scala.collection.mutable.{Ident(Builder), Ident(ListBuffer)}

import scala.annotation.Ident(tailrec)

import java.io.{Ident(ObjectOutputStream), Ident(ObjectInputStream)}

# class List

## Companion object : scala.collection.immutable.List

```scala
sealed abstract class List[A] extends new scala.collection.AbstractSeq[List.this.A]() with scala.collection.immutable.LinearSeq[List.this.A] with scala.Product with scala.collection.generic.GenericTraversableTemplate[List.this.A, [+A >: scala.Nothing <: scala.Any] => scala.collection.immutable.List[+A]] with scala.collection.LinearSeqOptimized[List.this.A, scala.collection.immutable.List[List.this.A]] with scala.Serializable
```

A class for immutable linked lists representing ordered collections
of elements of type `A`.

This class comes with two implementing case classes `scala.Nil`
and `scala.::` that implement the abstract members `isEmpty`,
`head` and `tail`.

This class is optimal for last-in-first-out (LIFO), stack-like access patterns. If you need another access
pattern, for example, random access or FIFO, consider using a collection more suited to this than `List`.

$usesMutableState

## Performance
**Time:** `List` has `O(1)` prepend and head/tail access. Most other operations are `O(n)` on the number of elements in the list.
This includes the index-based lookup of elements, `length`, `append` and `reverse`.

**Space:** `List` implements **structural sharing** of the tail list. This means that many operations are either
zero- or constant-memory cost.

```scala
val mainList = List(3, 2, 1)
val with4 =    4 :: mainList  // re-uses mainList, costs one :: instance
val with42 =   42 :: mainList // also re-uses mainList, cost one :: instance
val shorter =  mainList.tail  // costs nothing as it uses the same 2::1::Nil instances as mainList
```
***authors*** Martin Odersky and others

***see*** ["Scala's Collection Library overview"](http://docs.scala-lang.org/overviews/collections/concrete-immutable-collection-classes.html#lists)
section on `Lists` for more information.

***since*** 1.0

***Note*** The functional list is characterized by persistence and structural sharing, thus offering considerable
      performance and space consumption benefits in some scenarios if used correctly.
      However, note that objects having multiple references into the same functional list (that is,
      objects that rely on structural sharing), will be serialized and deserialized with multiple lists, one for
      each reference to it. I.e. structural sharing is lost after serialization/deserialization.

***Example*** 

```scala
// Make a list via the companion object factory
val days = List("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
// Make a list element-by-element
val when = "AM" :: "PM" :: List()
// Pattern match
days match {
  case firstDay :: otherDays =>
    println("The first day of the week is: " + firstDay)
  case List() =>
    println("There don't seem to be any week days.")
}
```

## Annotations:


## Constructors:
```scala
List()
```

## Members:

### Definitions: 
#### companion
```scala
override def companion: scala.collection.generic.GenericCompanion[[+A >: scala.Nothing <: scala.Any] => scala.collection.immutable.List[+A]]
```


#### isEmpty
```scala
def isEmpty: scala.Boolean
```


#### head
```scala
def head: List.this.A
```


#### tail
```scala
def tail: scala.collection.immutable.List[List.this.A]
```


#### ::
```scala
def ::[B >: List.this.A](x: B): scala.collection.immutable.List[B]
```

Adds an element at the beginning of this list.

***return*** a list which contains `x` as first element and
         which continues with this list.

***x*** the element to prepend.


#### :::
```scala
def :::[B >: List.this.A](prefix: scala.collection.immutable.List[B]): scala.collection.immutable.List[B]
```

Adds the elements of a given list in front of this list.

***return*** a list resulting from the concatenation of the given
  list `prefix` and this list.

***prefix*** The list elements to prepend.


#### reverse_:::
```scala
def reverse_:::[B >: List.this.A](prefix: scala.collection.immutable.List[B]): scala.collection.immutable.List[B]
```

Adds the elements of a given list in reverse order in front of this list.
`xs reverse_::: ys` is equivalent to
`xs.reverse ::: ys` but is more efficient.


***return*** the concatenation of the reversed prefix and the current list.

***prefix*** the prefix to reverse and then prepend


#### mapConserve
```scala
final def mapConserve[B >: List.this.A <: scala.AnyRef](f: scala.Function1[List.this.A, B]): scala.collection.immutable.List[B]
```

Builds a new list by applying a function to all elements of this list.
Like `xs map f`, but returns `xs` unchanged if function
`f` maps all elements to themselves (as determined by `eq`).


***return*** a list resulting from applying the given function
              `f` to each element of this list and collecting the results.

***f*** the function to apply to each element.

***B*** the element type of the returned collection.


#### ++
```scala
override def ++[B >: List.this.A, That](that: scala.collection.GenTraversableOnce[B])(bf: scala.collection.generic.CanBuildFrom[scala.collection.immutable.List[List.this.A], B, That]): That
```


#### +:
```scala
override def +:[B >: List.this.A, That](elem: B)(bf: scala.collection.generic.CanBuildFrom[scala.collection.immutable.List[List.this.A], B, That]): That
```


#### toList
```scala
override def toList: scala.collection.immutable.List[List.this.A]
```


#### take
```scala
override def take(n: scala.Int): scala.collection.immutable.List[List.this.A]
```


#### drop
```scala
override def drop(n: scala.Int): scala.collection.immutable.List[List.this.A]
```


#### slice
```scala
override def slice(from: scala.Int, until: scala.Int): scala.collection.immutable.List[List.this.A]
```

***Example*** 

```scala
// Given a list
val letters = List('a','b','c','d','e')
// `slice` returns all elements beginning at index `from` and afterwards,
// up until index `until` (excluding index `until`.)
letters.slice(1,3) // Returns List('b','c')
```

#### takeRight
```scala
override def takeRight(n: scala.Int): scala.collection.immutable.List[List.this.A]
```


#### splitAt
```scala
override def splitAt(n: scala.Int): scala.Tuple2[scala.collection.immutable.List[List.this.A], scala.collection.immutable.List[List.this.A]]
```


#### map
```scala
override final def map[B, That](f: scala.Function1[List.this.A, B])(bf: scala.collection.generic.CanBuildFrom[scala.collection.immutable.List[List.this.A], B, That]): That
```


#### collect
```scala
override final def collect[B, That](pf: scala.PartialFunction[List.this.A, B])(bf: scala.collection.generic.CanBuildFrom[scala.collection.immutable.List[List.this.A], B, That]): That
```


#### flatMap
```scala
override final def flatMap[B, That](f: scala.Function1[List.this.A, scala.collection.GenTraversableOnce[B]])(bf: scala.collection.generic.CanBuildFrom[scala.collection.immutable.List[List.this.A], B, That]): That
```


#### takeWhile
```scala
override final def takeWhile(p: scala.Function1[List.this.A, scala.Boolean]): scala.collection.immutable.List[List.this.A]
```


#### dropWhile
```scala
override final def dropWhile(p: scala.Function1[List.this.A, scala.Boolean]): scala.collection.immutable.List[List.this.A]
```


#### span
```scala
override final def span(p: scala.Function1[List.this.A, scala.Boolean]): scala.Tuple2[scala.collection.immutable.List[List.this.A], scala.collection.immutable.List[List.this.A]]
```


#### foreach
```scala
override final def foreach[U](f: scala.Function1[List.this.A, U]): scala.Unit
```


#### reverse
```scala
override def reverse: scala.collection.immutable.List[List.this.A]
```


#### foldRight
```scala
override def foldRight[B](z: B)(op: scala.Function2[List.this.A, B, B]): B
```


#### stringPrefix
```scala
override def stringPrefix: scala.Predef.String
```


#### toStream
```scala
override def toStream: scala.collection.immutable.Stream[List.this.A]
```


#### writeReplace
```scala
protected final def writeReplace(): scala.AnyRef
```



### Values: 

### Types: 


```scala
final val List: scala.collection.immutable.List
```

$factoryInfo



# object List$

## Companion class : scala.collection.immutable.List$

```scala
final class List$
```

$factoryInfo


## Annotations:


## Members:

### Definitions: 
#### writeReplace
```scala
private def writeReplace(): scala.AnyRef
```


#### canBuildFrom
```scala
implicit def canBuildFrom[A]: scala.collection.generic.CanBuildFrom[scala.collection.immutable.List.Coll, A, scala.collection.immutable.List[A]]
```

$genericCanBuildFromInfo


#### newBuilder
```scala
def newBuilder[A]: scala.collection.mutable.Builder[A, scala.collection.immutable.List[A]]
```


#### empty
```scala
override def empty[A]: scala.collection.immutable.List[A]
```


#### apply
```scala
override def apply[A](xs: scala.collection.Seq[A] @scala.annotation.internal.Repeated): scala.collection.immutable.List[A]
```



### Values: 
#### partialNotApplied
```scala
val partialNotApplied: scala.Function1[scala.Any, scala.Any]
```



### Types: 

