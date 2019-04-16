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

<p>A class for immutable linked lists representing ordered collections
of elements of type <code>A</code>.</p><p>This class comes with two implementing case classes <code>scala.Nil</code>
and <code>scala.::</code> that implement the abstract members <code>isEmpty</code>,
<code>head</code> and <code>tail</code>.</p><p>This class is optimal for last-in-first-out (LIFO), stack-like access patterns. If you need another access
pattern, for example, random access or FIFO, consider using a collection more suited to this than <code>List</code>.</p><p>$usesMutableState</p><h2>Performance</h2><p><b>Time:</b> <code>List</code> has <code>O(1)</code> prepend and head/tail access. Most other operations are <code>O(n)</code> on the number of elements in the list.
This includes the index-based lookup of elements, <code>length</code>, <code>append</code> and <code>reverse</code>.</p><p><b>Space:</b> <code>List</code> implements <b>structural sharing</b> of the tail list. This means that many operations are either
zero- or constant-memory cost.</p><pre><code class="scala">val mainList = List(3, 2, 1)
val with4 =    4 :: mainList  // re-uses mainList, costs one :: instance
val with42 =   42 :: mainList // also re-uses mainList, cost one :: instance
val shorter =  mainList.tail  // costs nothing as it uses the same 2::1::Nil instances as mainList</code></pre>

***authors*** <p>Martin Odersky and others</p>
***see*** <p><a href=http://docs.scala-lang.org/overviews/collections/concrete-immutable-collection-classes.html#lists target="_blank">"Scala's Collection Library overview"</a>
section on <code>Lists</code> for more information.</p>
***since*** <p>1.0</p>
***Note*** <p>The functional list is characterized by persistence and structural sharing, thus offering considerable
      performance and space consumption benefits in some scenarios if used correctly.
      However, note that objects having multiple references into the same functional list (that is,
      objects that rely on structural sharing), will be serialized and deserialized with multiple lists, one for
      each reference to it. I.e. structural sharing is lost after serialization/deserialization.</p>
***Example*** <p></p><pre><code class="scala">// Make a list via the companion object factory
val days = List("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
// Make a list element-by-element
val when = "AM" :: "PM" :: List()
// Pattern match
days match {
  case firstDay :: otherDays =>
    println("The first day of the week is: " + firstDay)
  case List() =>
    println("There don't seem to be any week days.")
}</code></pre>

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

<p>Adds an element at the beginning of this list.</p>

***return*** <p>a list which contains <code>x</code> as first element and
         which continues with this list.</p>
***x*** <p>the element to prepend.</p>

#### :::
```scala
def :::[B >: List.this.A](prefix: scala.collection.immutable.List[B]): scala.collection.immutable.List[B]
```

<p>Adds the elements of a given list in front of this list.</p>

***return*** <p>a list resulting from the concatenation of the given
  list <code>prefix</code> and this list.</p>
***prefix*** <p>The list elements to prepend.</p>

#### reverse_:::
```scala
def reverse_:::[B >: List.this.A](prefix: scala.collection.immutable.List[B]): scala.collection.immutable.List[B]
```

<p>Adds the elements of a given list in reverse order in front of this list.
<code>xs reverse_::: ys</code> is equivalent to
<code>xs.reverse ::: ys</code> but is more efficient.
</p>

***return*** <p>the concatenation of the reversed prefix and the current list.</p>
***prefix*** <p>the prefix to reverse and then prepend</p>

#### mapConserve
```scala
final def mapConserve[B >: List.this.A <: scala.AnyRef](f: scala.Function1[List.this.A, B]): scala.collection.immutable.List[B]
```

<p>Builds a new list by applying a function to all elements of this list.
Like <code>xs map f</code>, but returns <code>xs</code> unchanged if function
<code>f</code> maps all elements to themselves (as determined by <code>eq</code>).
</p>

***return*** <p>a list resulting from applying the given function
              <code>f</code> to each element of this list and collecting the results.</p>
***f*** <p>the function to apply to each element.</p>
***B*** <p>the element type of the returned collection.</p>

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



***Example*** <p></p><pre><code class="scala">// Given a list
val letters = List('a','b','c','d','e')
// `slice` returns all elements beginning at index `from` and afterwards,
// up until index `until` (excluding index `until`.)
letters.slice(1,3) // Returns List('b','c')</code></pre>

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

<p>$factoryInfo</p>



# object List$

## Companion class : scala.collection.immutable.List$

```scala
final class List$
```

<p>$factoryInfo</p>


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

<p>$genericCanBuildFromInfo</p>


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

