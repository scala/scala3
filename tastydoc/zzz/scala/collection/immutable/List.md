# class List

## Companion object List

<pre><code class="language-scala" >sealed abstract class List[A] extends AbstractSeq[A] with LinearSeq[A] with Product with GenericTraversableTemplate[A, [+A >: scala.Nothing <: scala.Any] => scala.collection.immutable.List[+A]] with LinearSeqOptimized[A, List[A]] with Serializable</pre></code>
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
@SerialVersionUID 
## Constructors:
<pre><code class="language-scala" >List()</pre></code>

## Concrete Value Members:
### !=
<pre><code class="language-scala" >final def !=(x$0: Any): Boolean</pre></code>

### ##
<pre><code class="language-scala" >final def ##: Int</pre></code>

### $asInstanceOf$
<pre><code class="language-scala" >final def $asInstanceOf$[X0]: X0</pre></code>

### $isInstanceOf$
<pre><code class="language-scala" >final def $isInstanceOf$[X0]: Boolean</pre></code>

### ++
<pre><code class="language-scala" >override def ++[B >: List.this.A, That](that: GenTraversableOnce[B])(bf: CanBuildFrom[List[A], B, That]): That</pre></code>

### ++:
<pre><code class="language-scala" >def ++:[B, That](that: Traversable[B])(bf: CanBuildFrom[Repr, B, That]): That</pre></code>

### ++:
<pre><code class="language-scala" >def ++:[B, That](that: TraversableOnce[B])(bf: CanBuildFrom[Repr, B, That]): That</pre></code>

### +:
<pre><code class="language-scala" >override def +:[B >: List.this.A, That](elem: B)(bf: CanBuildFrom[List[A], B, That]): That</pre></code>

### /:
<pre><code class="language-scala" >def /:[B](z: B)(op: (B, A) => B): B</pre></code>

### :+
<pre><code class="language-scala" >def :+[B, That](elem: B)(bf: CanBuildFrom[Repr, B, That]): That</pre></code>

### ::
<pre><code class="language-scala" >def ::[B >: List.this.A](x: B): List[B]</pre></code>
Adds an element at the beginning of this list.

***return*** a list which contains `x` as first element and
         which continues with this list.

***x*** the element to prepend.

### :::
<pre><code class="language-scala" >def :::[B >: List.this.A](prefix: List[B]): List[B]</pre></code>
Adds the elements of a given list in front of this list.

***return*** a list resulting from the concatenation of the given
  list `prefix` and this list.

***prefix*** The list elements to prepend.

### :\
<pre><code class="language-scala" >def :\[B](z: B)(op: (A, B) => B): B</pre></code>

### ==
<pre><code class="language-scala" >final def ==(x$0: Any): Boolean</pre></code>

### addString
<pre><code class="language-scala" >def addString(b: StringBuilder): StringBuilder</pre></code>

### addString
<pre><code class="language-scala" >def addString(b: StringBuilder, sep: String): StringBuilder</pre></code>

### addString
<pre><code class="language-scala" >def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder</pre></code>

### aggregate
<pre><code class="language-scala" >def aggregate[B](z: => B)(seqop: (B, A) => B, combop: (B, B) => B): B</pre></code>

### andThen
<pre><code class="language-scala" >override def andThen[C](k: (B) => C): PartialFunction[A, C]</pre></code>

### apply
<pre><code class="language-scala" >def apply(n: Int): A</pre></code>

### applyOrElse
<pre><code class="language-scala" >def applyOrElse[A1, B1](x: A1, default: (A1) => B1): B1</pre></code>

### asInstanceOf
<pre><code class="language-scala" >final def asInstanceOf[X0]: X0</pre></code>

### canEqual
<pre><code class="language-scala" >override def canEqual(that: Any): Boolean</pre></code>

### clone
<pre><code class="language-scala" >protected def clone(): Object</pre></code>

### collect
<pre><code class="language-scala" >override final def collect[B, That](pf: PartialFunction[A, B])(bf: CanBuildFrom[List[A], B, That]): That</pre></code>

### collectFirst
<pre><code class="language-scala" >def collectFirst[B](pf: PartialFunction[A, B]): Option[B]</pre></code>

### combinations
<pre><code class="language-scala" >def combinations(n: Int): Iterator[Repr]</pre></code>

### companion
<pre><code class="language-scala" >override def companion: GenericCompanion[[+A >: scala.Nothing <: scala.Any] => scala.collection.immutable.List[+A]]</pre></code>

### compose
<pre><code class="language-scala" >@unspecialized def compose[A](g: (A) => T1): (A) => R</pre></code>

### contains
<pre><code class="language-scala" >override def contains[A1](elem: A1): Boolean</pre></code>

### containsSlice
<pre><code class="language-scala" >def containsSlice[B](that: GenSeq[B]): Boolean</pre></code>

### copyToArray
<pre><code class="language-scala" >override def copyToArray[B](xs: Array[B], start: Int, len: Int): Unit</pre></code>

### copyToArray
<pre><code class="language-scala" >def copyToArray[B](xs: Array[B], start: Int): Unit</pre></code>

### copyToArray
<pre><code class="language-scala" >def copyToArray[B](xs: Array[B]): Unit</pre></code>

### copyToBuffer
<pre><code class="language-scala" >def copyToBuffer[B](dest: Buffer[B]): Unit</pre></code>

### corresponds
<pre><code class="language-scala" >@tailrec override final def corresponds[B](that: GenSeq[B])(p: (A, B) => Boolean): Boolean</pre></code>

### count
<pre><code class="language-scala" >def count(p: (A) => Boolean): Int</pre></code>

### diff
<pre><code class="language-scala" >def diff[B](that: GenSeq[B]): Repr</pre></code>

### distinct
<pre><code class="language-scala" >def distinct: Repr</pre></code>

### drop
<pre><code class="language-scala" >override def drop(n: Int): List[A]</pre></code>

### dropRight
<pre><code class="language-scala" >override def dropRight(n: Int): Repr</pre></code>

### dropWhile
<pre><code class="language-scala" >@inline override final def dropWhile(p: (A) => Boolean): List[A]</pre></code>

### endsWith
<pre><code class="language-scala" >def endsWith[B](that: GenSeq[B]): Boolean</pre></code>

### eq
<pre><code class="language-scala" >final def eq(x$0: Object): Boolean</pre></code>

### equals
<pre><code class="language-scala" >override def equals(that: Any): Boolean</pre></code>

### exists
<pre><code class="language-scala" >override def exists(p: (A) => Boolean): Boolean</pre></code>

### filter
<pre><code class="language-scala" >def filter(p: (A) => Boolean): Repr</pre></code>

### filterImpl
<pre><code class="language-scala" >private[scala] def filterImpl(p: (A) => Boolean, isFlipped: Boolean): Repr</pre></code>

### filterNot
<pre><code class="language-scala" >def filterNot(p: (A) => Boolean): Repr</pre></code>

### finalize
<pre><code class="language-scala" >protected def finalize(): Unit</pre></code>

### find
<pre><code class="language-scala" >override def find(p: (A) => Boolean): Option[A]</pre></code>

### flatMap
<pre><code class="language-scala" >override final def flatMap[B, That](f: (A) => GenTraversableOnce[B])(bf: CanBuildFrom[List[A], B, That]): That</pre></code>

### flatten
<pre><code class="language-scala" >def flatten[B](asTraversable: (A) => GenTraversableOnce[B]): CC[B]</pre></code>

### fold
<pre><code class="language-scala" >def fold[A1](z: A1)(op: (A1, A1) => A1): A1</pre></code>

### foldLeft
<pre><code class="language-scala" >override def foldLeft[B](z: B)(op: (B, A) => B): B</pre></code>

### foldRight
<pre><code class="language-scala" >override def foldRight[B](z: B)(op: (A, B) => B): B</pre></code>

### forall
<pre><code class="language-scala" >override def forall(p: (A) => Boolean): Boolean</pre></code>

### foreach
<pre><code class="language-scala" >@inline override final def foreach[U](f: (A) => U): Unit</pre></code>

### genericBuilder
<pre><code class="language-scala" >def genericBuilder[B]: Builder[B, CC[B]]</pre></code>

### getClass
<pre><code class="language-scala" >final def getClass(): Class[]</pre></code>

### groupBy
<pre><code class="language-scala" >def groupBy[K](f: (A) => K): Map[K, Repr]</pre></code>

### grouped
<pre><code class="language-scala" >def grouped(size: Int): Iterator[Repr]</pre></code>

### hasDefiniteSize
<pre><code class="language-scala" >def hasDefiniteSize: Boolean</pre></code>

### hashCode
<pre><code class="language-scala" >override def hashCode(): Int</pre></code>

### head
<pre><code class="language-scala" >def head: A</pre></code>

### headOption
<pre><code class="language-scala" >def headOption: Option[A]</pre></code>

### indexOf
<pre><code class="language-scala" >def indexOf[B](elem: B, from: Int): Int</pre></code>

### indexOf
<pre><code class="language-scala" >def indexOf[B](elem: B): Int</pre></code>

### indexOfSlice
<pre><code class="language-scala" >def indexOfSlice[B](that: GenSeq[B], from: Int): Int</pre></code>

### indexOfSlice
<pre><code class="language-scala" >def indexOfSlice[B](that: GenSeq[B]): Int</pre></code>

### indexWhere
<pre><code class="language-scala" >override def indexWhere(p: (A) => Boolean, from: Int): Int</pre></code>

### indexWhere
<pre><code class="language-scala" >def indexWhere(p: (A) => Boolean): Int</pre></code>

### indices
<pre><code class="language-scala" >def indices: Range</pre></code>

### init
<pre><code class="language-scala" >def init: Repr</pre></code>

### inits
<pre><code class="language-scala" >def inits: Iterator[Repr]</pre></code>

### intersect
<pre><code class="language-scala" >def intersect[B](that: GenSeq[B]): Repr</pre></code>

### isDefinedAt
<pre><code class="language-scala" >override def isDefinedAt(x: Int): Boolean</pre></code>

### isEmpty
<pre><code class="language-scala" >def isEmpty: Boolean</pre></code>

### isInstanceOf
<pre><code class="language-scala" >final def isInstanceOf[X0]: Boolean</pre></code>

### isTraversableAgain
<pre><code class="language-scala" >final def isTraversableAgain: Boolean</pre></code>

### iterator
<pre><code class="language-scala" >override def iterator: Iterator[A]</pre></code>

### last
<pre><code class="language-scala" >override def last: A</pre></code>

### lastIndexOf
<pre><code class="language-scala" >def lastIndexOf[B](elem: B, end: Int): Int</pre></code>

### lastIndexOf
<pre><code class="language-scala" >def lastIndexOf[B](elem: B): Int</pre></code>

### lastIndexOfSlice
<pre><code class="language-scala" >def lastIndexOfSlice[B](that: GenSeq[B], end: Int): Int</pre></code>

### lastIndexOfSlice
<pre><code class="language-scala" >def lastIndexOfSlice[B](that: GenSeq[B]): Int</pre></code>

### lastIndexWhere
<pre><code class="language-scala" >override def lastIndexWhere(p: (A) => Boolean, end: Int): Int</pre></code>

### lastIndexWhere
<pre><code class="language-scala" >def lastIndexWhere(p: (A) => Boolean): Int</pre></code>

### lastOption
<pre><code class="language-scala" >def lastOption: Option[A]</pre></code>

### length
<pre><code class="language-scala" >def length: Int</pre></code>

### lengthCompare
<pre><code class="language-scala" >override def lengthCompare(len: Int): Int</pre></code>

### lift
<pre><code class="language-scala" >def lift: (A) => Option[B]</pre></code>

### map
<pre><code class="language-scala" >override final def map[B, That](f: (A) => B)(bf: CanBuildFrom[List[A], B, That]): That</pre></code>

### mapConserve
<pre><code class="language-scala" >@inline final def mapConserve[B >: List.this.A <: scala.AnyRef](f: (A) => B): List[B]</pre></code>
Builds a new list by applying a function to all elements of this list.
Like `xs map f`, but returns `xs` unchanged if function
`f` maps all elements to themselves (as determined by `eq`).


***return*** a list resulting from applying the given function
              `f` to each element of this list and collecting the results.

***f*** the function to apply to each element.

***B*** the element type of the returned collection.

### max
<pre><code class="language-scala" >def max[B](cmp: Ordering[B]): A</pre></code>

### maxBy
<pre><code class="language-scala" >def maxBy[B](f: (A) => B)(cmp: Ordering[B]): A</pre></code>

### min
<pre><code class="language-scala" >def min[B](cmp: Ordering[B]): A</pre></code>

### minBy
<pre><code class="language-scala" >def minBy[B](f: (A) => B)(cmp: Ordering[B]): A</pre></code>

### mkString
<pre><code class="language-scala" >def mkString: String</pre></code>

### mkString
<pre><code class="language-scala" >def mkString(sep: String): String</pre></code>

### mkString
<pre><code class="language-scala" >def mkString(start: String, sep: String, end: String): String</pre></code>

### ne
<pre><code class="language-scala" >final def ne(x$0: Object): Boolean</pre></code>

### newBuilder
<pre><code class="language-scala" >protected def newBuilder: Builder[A, CC[A]]</pre></code>

### nonEmpty
<pre><code class="language-scala" >def nonEmpty: Boolean</pre></code>

### notify
<pre><code class="language-scala" >final def notify(): Unit</pre></code>

### notifyAll
<pre><code class="language-scala" >final def notifyAll(): Unit</pre></code>

### orElse
<pre><code class="language-scala" >def orElse[A1, B1](that: PartialFunction[A1, B1]): PartialFunction[A1, B1]</pre></code>

### padTo
<pre><code class="language-scala" >def padTo[B, That](len: Int, elem: B)(bf: CanBuildFrom[Repr, B, That]): That</pre></code>

### par
<pre><code class="language-scala" >def par: ParRepr</pre></code>

### parCombiner
<pre><code class="language-scala" >protected override def parCombiner: Combiner[A, ParSeq[A]]</pre></code>

### partition
<pre><code class="language-scala" >def partition(p: (A) => Boolean): (Repr, Repr)</pre></code>

### patch
<pre><code class="language-scala" >def patch[B, That](from: Int, patch: GenSeq[B], replaced: Int)(bf: CanBuildFrom[Repr, B, That]): That</pre></code>

### permutations
<pre><code class="language-scala" >def permutations: Iterator[Repr]</pre></code>

### prefixLength
<pre><code class="language-scala" >def prefixLength(p: (A) => Boolean): Int</pre></code>

### product
<pre><code class="language-scala" >def product[B](num: Numeric[B]): B</pre></code>

### productArity
<pre><code class="language-scala" >def productArity: Int</pre></code>

### productElement
<pre><code class="language-scala" >def productElement(n: Int): Any</pre></code>

### productIterator
<pre><code class="language-scala" >def productIterator: Iterator[Any]</pre></code>

### productPrefix
<pre><code class="language-scala" >def productPrefix: String</pre></code>

### reduce
<pre><code class="language-scala" >def reduce[A1](op: (A1, A1) => A1): A1</pre></code>

### reduceLeft
<pre><code class="language-scala" >override def reduceLeft[B](op: (B, A) => B): B</pre></code>

### reduceLeftOption
<pre><code class="language-scala" >def reduceLeftOption[B](op: (B, A) => B): Option[B]</pre></code>

### reduceOption
<pre><code class="language-scala" >def reduceOption[A1](op: (A1, A1) => A1): Option[A1]</pre></code>

### reduceRight
<pre><code class="language-scala" >override def reduceRight[B](op: (A, B) => B): B</pre></code>

### reduceRightOption
<pre><code class="language-scala" >def reduceRightOption[B](op: (A, B) => B): Option[B]</pre></code>

### repr
<pre><code class="language-scala" >def repr: Repr</pre></code>

### reverse
<pre><code class="language-scala" >override def reverse: List[A]</pre></code>

### reverseIterator
<pre><code class="language-scala" >def reverseIterator: Iterator[A]</pre></code>

### reverseMap
<pre><code class="language-scala" >def reverseMap[B, That](f: (A) => B)(bf: CanBuildFrom[Repr, B, That]): That</pre></code>

### reverse_:::
<pre><code class="language-scala" >def reverse_:::[B >: List.this.A](prefix: List[B]): List[B]</pre></code>
Adds the elements of a given list in reverse order in front of this list.
`xs reverse_::: ys` is equivalent to
`xs.reverse ::: ys` but is more efficient.


***return*** the concatenation of the reversed prefix and the current list.

***prefix*** the prefix to reverse and then prepend

### reversed
<pre><code class="language-scala" >protected def reversed: List[A]</pre></code>

### runWith
<pre><code class="language-scala" >def runWith[U](action: (B) => U): (A) => Boolean</pre></code>

### sameElements
<pre><code class="language-scala" >override def sameElements[B](that: GenIterable[B]): Boolean</pre></code>

### scan
<pre><code class="language-scala" >def scan[B, That](z: B)(op: (B, B) => B)(cbf: CanBuildFrom[Repr, B, That]): That</pre></code>

### scanLeft
<pre><code class="language-scala" >def scanLeft[B, That](z: B)(op: (B, A) => B)(bf: CanBuildFrom[Repr, B, That]): That</pre></code>

### scanRight
<pre><code class="language-scala" >@migration def scanRight[B, That](z: B)(op: (A, B) => B)(bf: CanBuildFrom[Repr, B, That]): That</pre></code>

### segmentLength
<pre><code class="language-scala" >override def segmentLength(p: (A) => Boolean, from: Int): Int</pre></code>

### seq
<pre><code class="language-scala" >override def seq: LinearSeq[A]</pre></code>

### size
<pre><code class="language-scala" >override def size: Int</pre></code>

### sizeHintIfCheap
<pre><code class="language-scala" >private[collection] def sizeHintIfCheap: Int</pre></code>

### slice
<pre><code class="language-scala" >override def slice(from: Int, until: Int): List[A]</pre></code>
***Example*** 

```scala
// Given a list
val letters = List('a','b','c','d','e')
// `slice` returns all elements beginning at index `from` and afterwards,
// up until index `until` (excluding index `until`.)
letters.slice(1,3) // Returns List('b','c')
```
### sliceWithKnownBound
<pre><code class="language-scala" >private[scala] def sliceWithKnownBound(from: Int, until: Int): Repr</pre></code>

### sliceWithKnownDelta
<pre><code class="language-scala" >private[scala] def sliceWithKnownDelta(from: Int, until: Int, delta: Int): Repr</pre></code>

### sliding
<pre><code class="language-scala" >def sliding(size: Int, step: Int): Iterator[Repr]</pre></code>

### sliding
<pre><code class="language-scala" >def sliding(size: Int): Iterator[Repr]</pre></code>

### sortBy
<pre><code class="language-scala" >def sortBy[B](f: (A) => B)(ord: Ordering[B]): Repr</pre></code>

### sortWith
<pre><code class="language-scala" >def sortWith(lt: (A, A) => Boolean): Repr</pre></code>

### sorted
<pre><code class="language-scala" >def sorted[B](ord: Ordering[B]): Repr</pre></code>

### span
<pre><code class="language-scala" >@inline override final def span(p: (A) => Boolean): (List[A], List[A])</pre></code>

### splitAt
<pre><code class="language-scala" >override def splitAt(n: Int): (List[A], List[A])</pre></code>

### startsWith
<pre><code class="language-scala" >def startsWith[B](that: GenSeq[B], offset: Int): Boolean</pre></code>

### startsWith
<pre><code class="language-scala" >def startsWith[B](that: GenSeq[B]): Boolean</pre></code>

### stringPrefix
<pre><code class="language-scala" >override def stringPrefix: String</pre></code>

### sum
<pre><code class="language-scala" >def sum[B](num: Numeric[B]): B</pre></code>

### synchronized
<pre><code class="language-scala" >final def synchronized[X0](x$0: X0): X0</pre></code>

### tail
<pre><code class="language-scala" >def tail: List[A]</pre></code>

### tails
<pre><code class="language-scala" >override def tails: Iterator[Repr]</pre></code>

### take
<pre><code class="language-scala" >override def take(n: Int): List[A]</pre></code>

### takeRight
<pre><code class="language-scala" >override def takeRight(n: Int): List[A]</pre></code>

### takeWhile
<pre><code class="language-scala" >@inline override final def takeWhile(p: (A) => Boolean): List[A]</pre></code>

### thisCollection
<pre><code class="language-scala" >protected override def thisCollection: LinearSeq[A]</pre></code>

### to
<pre><code class="language-scala" >override def to[Col](cbf: CanBuildFrom[Nothing, A, Col[A]]): Col[A]</pre></code>

### toArray
<pre><code class="language-scala" >def toArray[B](evidence$1: ClassTag[B]): Array[B]</pre></code>

### toBuffer
<pre><code class="language-scala" >def toBuffer[B]: Buffer[B]</pre></code>

### toCollection
<pre><code class="language-scala" >protected override def toCollection(repr: Repr): LinearSeq[A]</pre></code>

### toIndexedSeq
<pre><code class="language-scala" >def toIndexedSeq: IndexedSeq[A]</pre></code>

### toIterable
<pre><code class="language-scala" >override def toIterable: Iterable[A]</pre></code>

### toIterator
<pre><code class="language-scala" >@deprecatedOverriding override def toIterator: Iterator[A]</pre></code>

### toList
<pre><code class="language-scala" >override def toList: List[A]</pre></code>

### toMap
<pre><code class="language-scala" >def toMap[T, U](ev: <:<[A, (T, U)]): Map[T, U]</pre></code>

### toSeq
<pre><code class="language-scala" >override def toSeq: Seq[A]</pre></code>

### toSet
<pre><code class="language-scala" >def toSet[B]: Set[B]</pre></code>

### toStream
<pre><code class="language-scala" >override def toStream: Stream[A]</pre></code>

### toString
<pre><code class="language-scala" >override def toString(): String</pre></code>

### toTraversable
<pre><code class="language-scala" >@deprecatedOverriding def toTraversable: Traversable[A]</pre></code>

### toVector
<pre><code class="language-scala" >def toVector: Vector[A]</pre></code>

### transpose
<pre><code class="language-scala" >@migration def transpose[B](asTraversable: (A) => GenTraversableOnce[B]): CC[CC[B]]</pre></code>

### union
<pre><code class="language-scala" >override def union[B, That](that: GenSeq[B])(bf: CanBuildFrom[Repr, B, That]): That</pre></code>

### unzip
<pre><code class="language-scala" >def unzip[A1, A2](asPair: (A) => (A1, A2)): (CC[A1], CC[A2])</pre></code>

### unzip3
<pre><code class="language-scala" >def unzip3[A1, A2, A3](asTriple: (A) => (A1, A2, A3)): (CC[A1], CC[A2], CC[A3])</pre></code>

### updated
<pre><code class="language-scala" >def updated[B, That](index: Int, elem: B)(bf: CanBuildFrom[Repr, B, That]): That</pre></code>

### view
<pre><code class="language-scala" >override def view(from: Int, until: Int): SeqView[A, Repr]</pre></code>

### view
<pre><code class="language-scala" >override def view: AnyRef & SeqView[A, Repr]</pre></code>

### wait
<pre><code class="language-scala" >final def wait(x$0: Long, x$1: Int): Unit</pre></code>

### wait
<pre><code class="language-scala" >final def wait(x$0: Long): Unit</pre></code>

### wait
<pre><code class="language-scala" >final def wait(): Unit</pre></code>

### withFilter
<pre><code class="language-scala" >def withFilter(p: (A) => Boolean): FilterMonadic[A, Repr]</pre></code>

### writeReplace
<pre><code class="language-scala" >protected final def writeReplace(): AnyRef</pre></code>

### zip
<pre><code class="language-scala" >def zip[A1, B, That](that: GenIterable[B])(bf: CanBuildFrom[Repr, (A1, B), That]): That</pre></code>

### zipAll
<pre><code class="language-scala" >def zipAll[B, A1, That](that: GenIterable[B], thisElem: A1, thatElem: B)(bf: CanBuildFrom[Repr, (A1, B), That]): That</pre></code>

### zipWithIndex
<pre><code class="language-scala" >def zipWithIndex[A1, That](bf: CanBuildFrom[Repr, (A1, Int), That]): That</pre></code>

