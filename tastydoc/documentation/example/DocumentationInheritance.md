example
# class DocumentationInheritance

<pre><code class="language-scala" >abstract class DocumentationInheritance[T, A <: scala.Int, B >: scala.Predef.String, X, Y] extends Documentation[<a href="./DocumentationInheritance.md#T">T</a>, <a href="./DocumentationInheritance.md#A">A</a>, <a href="./DocumentationInheritance.md#B">B</a>, <a href="./DocumentationInheritance.md#X">X</a>, <a href="./DocumentationInheritance.md#Y">Y</a>]</pre></code>
## Concrete Value Members:
### ++
<pre><code class="language-scala" >def ++[B, That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[Repr, B, That]): That</pre></code>

### ++:
<pre><code class="language-scala" >def ++:[B, That](that: Traversable[B])(implicit bf: CanBuildFrom[Repr, B, That]): That</pre></code>

### ++:
<pre><code class="language-scala" >def ++:[B, That](that: TraversableOnce[B])(implicit bf: CanBuildFrom[Repr, B, That]): That</pre></code>

### +:
<pre><code class="language-scala" >def +:[B, That](elem: B)(implicit bf: CanBuildFrom[Repr, B, That]): That</pre></code>

### /:
<pre><code class="language-scala" >def /:[B](z: B)(op: (B, A) => B): B</pre></code>

### :+
<pre><code class="language-scala" >def :+[B, That](elem: B)(implicit bf: CanBuildFrom[Repr, B, That]): That</pre></code>

### :\
<pre><code class="language-scala" >def :\[B](z: B)(op: (A, B) => B): B</pre></code>

### abstractDefinition
<pre><code class="language-scala" >def abstractDefinition: Int</pre></code>

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
<pre><code class="language-scala" >def apply(idx: Int): <a href="./level2/Documentation.md#T">T</a></pre></code>

### applyOrElse
<pre><code class="language-scala" >def applyOrElse[A1, B1](x: A1, default: (A1) => B1): B1</pre></code>

### canEqual
<pre><code class="language-scala" >override def canEqual(that: Any): Boolean</pre></code>

### collect
<pre><code class="language-scala" >def collect[B, That](pf: PartialFunction[A, B])(implicit bf: CanBuildFrom[Repr, B, That]): That</pre></code>

### collectFirst
<pre><code class="language-scala" >def collectFirst[B](pf: PartialFunction[A, B]): Option[B]</pre></code>

### combinations
<pre><code class="language-scala" >def combinations(n: Int): Iterator[Repr]</pre></code>

### companion
<pre><code class="language-scala" >override def companion: GenericCompanion[[+A >: scala.Nothing <: scala.Any] => scala.collection.Seq[+A]]</pre></code>

### compose
<pre><code class="language-scala" >@unspecialized def compose[A](g: (A) => T1): (A) => R</pre></code>

### contains
<pre><code class="language-scala" >def contains[A1](elem: A1): Boolean</pre></code>

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
<pre><code class="language-scala" >def corresponds[B](that: GenSeq[B])(p: (A, B) => Boolean): Boolean</pre></code>

### count
<pre><code class="language-scala" >def count(p: (A) => Boolean): Int</pre></code>

### defReturningInnerClass
<pre><code class="language-scala" >def defReturningInnerClass(): <a href="./level2/Documentation/innerDocumentationClass.md">innerDocumentationClass</a></pre></code>

### diff
<pre><code class="language-scala" >def diff[B](that: GenSeq[B]): Repr</pre></code>

### distinct
<pre><code class="language-scala" >def distinct: Repr</pre></code>

### docWithMd
<pre><code class="language-scala" >def docWithMd: Nothing</pre></code>
An example documention with markdown formatting
**I'm bold**
*I'm italic*
`some code`
```scala
def someScalaCode(x: String) = println("Hello " + x)
`````````
# Title of level 1
# Title of level 1
1. I'm a list
* Multilevel List
1. level 2

1. level 2 2
* level 1 again

* multilevel try2
  * try2 level2

### drop
<pre><code class="language-scala" >override def drop(n: Int): Repr</pre></code>

### dropRight
<pre><code class="language-scala" >def dropRight(n: Int): Repr</pre></code>

### dropWhile
<pre><code class="language-scala" >def dropWhile(p: (A) => Boolean): Repr</pre></code>

### endsWith
<pre><code class="language-scala" >def endsWith[B](that: GenSeq[B]): Boolean</pre></code>

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

### find
<pre><code class="language-scala" >override def find(p: (A) => Boolean): Option[A]</pre></code>

### flatMap
<pre><code class="language-scala" >def flatMap[B, That](f: (A) => GenTraversableOnce[B])(implicit bf: CanBuildFrom[Repr, B, That]): That</pre></code>

### flatten
<pre><code class="language-scala" >def flatten[B](implicit asTraversable: (A) => GenTraversableOnce[B]): CC[B]</pre></code>

### fold
<pre><code class="language-scala" >def fold[A1](z: A1)(op: (A1, A1) => A1): A1</pre></code>

### foldLeft
<pre><code class="language-scala" >def foldLeft[B](z: B)(op: (B, A) => B): B</pre></code>

### foldRight
<pre><code class="language-scala" >override def foldRight[B](z: B)(op: (A, B) => B): B</pre></code>

### forall
<pre><code class="language-scala" >override def forall(p: (A) => Boolean): Boolean</pre></code>

### foreach
<pre><code class="language-scala" >def foreach[U](f: (A) => U): Unit</pre></code>

### functionWithType
<pre><code class="language-scala" >def functionWithType[U >: scala.Predef.String](): U</pre></code>

### genericBuilder
<pre><code class="language-scala" >def genericBuilder[B]: Builder[B, CC[B]]</pre></code>

### groupBy
<pre><code class="language-scala" >def groupBy[K](f: (A) => K): Map[K, Repr]</pre></code>

### grouped
<pre><code class="language-scala" >def grouped(size: Int): Iterator[Repr]</pre></code>

### hasDefiniteSize
<pre><code class="language-scala" >def hasDefiniteSize: Boolean</pre></code>

### hashCode
<pre><code class="language-scala" >override def hashCode(): Int</pre></code>

### head
<pre><code class="language-scala" >override def head: A</pre></code>

### headOption
<pre><code class="language-scala" >def headOption: Option[A]</pre></code>

### iAmAVar_=
<pre><code class="language-scala" >def iAmAVar_=(x$1: Nothing): Unit</pre></code>

### indexOf
<pre><code class="language-scala" >def indexOf[B](elem: B, from: Int): Int</pre></code>

### indexOf
<pre><code class="language-scala" >def indexOf[B](elem: B): Int</pre></code>

### indexOfSlice
<pre><code class="language-scala" >def indexOfSlice[B](that: GenSeq[B], from: Int): Int</pre></code>

### indexOfSlice
<pre><code class="language-scala" >def indexOfSlice[B](that: GenSeq[B]): Int</pre></code>

### indexWhere
<pre><code class="language-scala" >def indexWhere(p: (A) => Boolean, from: Int): Int</pre></code>

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
<pre><code class="language-scala" >def isDefinedAt(idx: Int): Boolean</pre></code>

### isEmpty
<pre><code class="language-scala" >override def isEmpty: Boolean</pre></code>

### isTraversableAgain
<pre><code class="language-scala" >final def isTraversableAgain: Boolean</pre></code>

### iterator
<pre><code class="language-scala" >def iterator: Iterator[<a href="./level2/Documentation.md#T">T</a>]</pre></code>

### last
<pre><code class="language-scala" >def last: A</pre></code>

### lastIndexOf
<pre><code class="language-scala" >def lastIndexOf[B](elem: B, end: Int): Int</pre></code>

### lastIndexOf
<pre><code class="language-scala" >def lastIndexOf[B](elem: B): Int</pre></code>

### lastIndexOfSlice
<pre><code class="language-scala" >def lastIndexOfSlice[B](that: GenSeq[B], end: Int): Int</pre></code>

### lastIndexOfSlice
<pre><code class="language-scala" >def lastIndexOfSlice[B](that: GenSeq[B]): Int</pre></code>

### lastIndexWhere
<pre><code class="language-scala" >def lastIndexWhere(p: (A) => Boolean, end: Int): Int</pre></code>

### lastIndexWhere
<pre><code class="language-scala" >def lastIndexWhere(p: (A) => Boolean): Int</pre></code>

### lastOption
<pre><code class="language-scala" >def lastOption: Option[A]</pre></code>

### length
<pre><code class="language-scala" >override def length: Int</pre></code>

### lengthCompare
<pre><code class="language-scala" >def lengthCompare(len: Int): Int</pre></code>

### lift
<pre><code class="language-scala" >def lift: (A) => Option[B]</pre></code>

### linkMethodInDoc
<pre><code class="language-scala" >def linkMethodInDoc(): Nothing</pre></code>
method: [example.UserDocLinkingClass.linkMeFromUserDoc](../../example/UserDocLinkingClass.md#linkMeFromUserDoc)
method:[example.level2.Documentation.apply](../../example/level2/Documentation.md#apply)
class: [example.UserDocLinkingClass](../../example/UserDocLinkingClass.md)

### linkingGraph
<pre><code class="language-scala" >def linkingGraph(g: <a href="./level2/Documentation/Graph.md">Graph</a>): Node</pre></code>

### map
<pre><code class="language-scala" >def map[B, That](f: (A) => B)(implicit bf: CanBuildFrom[Repr, B, That]): That</pre></code>

### max
<pre><code class="language-scala" >def max[B](implicit cmp: Ordering[B]): A</pre></code>

### maxBy
<pre><code class="language-scala" >def maxBy[B](f: (A) => B)(implicit cmp: Ordering[B]): A</pre></code>

### methodsWithCallByName
<pre><code class="language-scala" >def methodsWithCallByName(x: => Int): Nothing</pre></code>

### methodsWithDefault
<pre><code class="language-scala" >def methodsWithDefault(x: Int): Nothing</pre></code>

### methodsWithImplicit
<pre><code class="language-scala" >def methodsWithImplicit(x: Int)(implicit imp: Int, notImp: String): Nothing</pre></code>

### methodsWithParams
<pre><code class="language-scala" >def methodsWithParams(x: <a href="./level2/Documentation.md#T">T</a>, y: Int): List[Map[Int, <a href="./level2/Documentation.md#T">T</a>]]</pre></code>
Test methods with params

***return*** something is returned

***y*** parameter 2

***x*** parameter 1

### min
<pre><code class="language-scala" >def min[B](implicit cmp: Ordering[B]): A</pre></code>

### minBy
<pre><code class="language-scala" >def minBy[B](f: (A) => B)(implicit cmp: Ordering[B]): A</pre></code>

### mkString
<pre><code class="language-scala" >def mkString: String</pre></code>

### mkString
<pre><code class="language-scala" >def mkString(sep: String): String</pre></code>

### mkString
<pre><code class="language-scala" >def mkString(start: String, sep: String, end: String): String</pre></code>

### newBuilder
<pre><code class="language-scala" >protected def newBuilder: Builder[A, CC[A]]</pre></code>

### nonEmpty
<pre><code class="language-scala" >def nonEmpty: Boolean</pre></code>

### orElse
<pre><code class="language-scala" >def orElse[A1, B1](that: PartialFunction[A1, B1]): PartialFunction[A1, B1]</pre></code>

### padTo
<pre><code class="language-scala" >def padTo[B, That](len: Int, elem: B)(implicit bf: CanBuildFrom[Repr, B, That]): That</pre></code>

### par
<pre><code class="language-scala" >def par: ParRepr</pre></code>

### parCombiner
<pre><code class="language-scala" >protected override def parCombiner: Combiner[A, ParSeq[A]]</pre></code>

### partition
<pre><code class="language-scala" >def partition(p: (A) => Boolean): (Repr, Repr)</pre></code>

### patch
<pre><code class="language-scala" >def patch[B, That](from: Int, patch: GenSeq[B], replaced: Int)(implicit bf: CanBuildFrom[Repr, B, That]): That</pre></code>

### permutations
<pre><code class="language-scala" >def permutations: Iterator[Repr]</pre></code>

### prefixLength
<pre><code class="language-scala" >def prefixLength(p: (A) => Boolean): Int</pre></code>

### product
<pre><code class="language-scala" >def product[B](implicit num: Numeric[B]): B</pre></code>

### productArity
<pre><code class="language-scala" >def productArity: Int</pre></code>

### productElement
<pre><code class="language-scala" >def productElement(n: Int): Any</pre></code>

### productIterator
<pre><code class="language-scala" >def productIterator: Iterator[Any]</pre></code>

### productPrefix
<pre><code class="language-scala" >def productPrefix: String</pre></code>

### protectedMethod
<pre><code class="language-scala" >protected def protectedMethod: Nothing</pre></code>

### reduce
<pre><code class="language-scala" >def reduce[A1](op: (A1, A1) => A1): A1</pre></code>

### reduceLeft
<pre><code class="language-scala" >def reduceLeft[B](op: (B, A) => B): B</pre></code>

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
<pre><code class="language-scala" >def reverse: Repr</pre></code>

### reverseIterator
<pre><code class="language-scala" >def reverseIterator: Iterator[A]</pre></code>

### reverseMap
<pre><code class="language-scala" >def reverseMap[B, That](f: (A) => B)(implicit bf: CanBuildFrom[Repr, B, That]): That</pre></code>

### reversed
<pre><code class="language-scala" >protected def reversed: List[A]</pre></code>

### runWith
<pre><code class="language-scala" >def runWith[U](action: (B) => U): (A) => Boolean</pre></code>

### sameElements
<pre><code class="language-scala" >def sameElements[B](that: GenIterable[B]): Boolean</pre></code>

### scan
<pre><code class="language-scala" >def scan[B, That](z: B)(op: (B, B) => B)(implicit cbf: CanBuildFrom[Repr, B, That]): That</pre></code>

### scanLeft
<pre><code class="language-scala" >def scanLeft[B, That](z: B)(op: (B, A) => B)(implicit bf: CanBuildFrom[Repr, B, That]): That</pre></code>

### scanRight
<pre><code class="language-scala" >@migration def scanRight[B, That](z: B)(op: (A, B) => B)(implicit bf: CanBuildFrom[Repr, B, That]): That</pre></code>

### segmentLength
<pre><code class="language-scala" >def segmentLength(p: (A) => Boolean, from: Int): Int</pre></code>

### seq
<pre><code class="language-scala" >override def seq: Seq[A]</pre></code>

### size
<pre><code class="language-scala" >override def size: Int</pre></code>

### sizeHintIfCheap
<pre><code class="language-scala" >private[collection] def sizeHintIfCheap: Int</pre></code>

### slice
<pre><code class="language-scala" >override def slice(from: Int, until: Int): Repr</pre></code>

### sliceWithKnownBound
<pre><code class="language-scala" >private[scala] def sliceWithKnownBound(from: Int, until: Int): Repr</pre></code>

### sliceWithKnownDelta
<pre><code class="language-scala" >private[scala] def sliceWithKnownDelta(from: Int, until: Int, delta: Int): Repr</pre></code>

### sliding
<pre><code class="language-scala" >def sliding(size: Int, step: Int): Iterator[Repr]</pre></code>

### sliding
<pre><code class="language-scala" >def sliding(size: Int): Iterator[Repr]</pre></code>

### sortBy
<pre><code class="language-scala" >def sortBy[B](f: (A) => B)(implicit ord: Ordering[B]): Repr</pre></code>

### sortWith
<pre><code class="language-scala" >def sortWith(lt: (A, A) => Boolean): Repr</pre></code>

### sorted
<pre><code class="language-scala" >def sorted[B](implicit ord: Ordering[B]): Repr</pre></code>

### span
<pre><code class="language-scala" >def span(p: (A) => Boolean): (Repr, Repr)</pre></code>

### splitAt
<pre><code class="language-scala" >def splitAt(n: Int): (Repr, Repr)</pre></code>

### startsWith
<pre><code class="language-scala" >def startsWith[B](that: GenSeq[B], offset: Int): Boolean</pre></code>

### startsWith
<pre><code class="language-scala" >def startsWith[B](that: GenSeq[B]): Boolean</pre></code>

### stringPrefix
<pre><code class="language-scala" >def stringPrefix: String</pre></code>

### sum
<pre><code class="language-scala" >def sum[B](implicit num: Numeric[B]): B</pre></code>

### tail
<pre><code class="language-scala" >override def tail: Repr</pre></code>

### tails
<pre><code class="language-scala" >def tails: Iterator[Repr]</pre></code>

### take
<pre><code class="language-scala" >override def take(n: Int): Repr</pre></code>

### takeRight
<pre><code class="language-scala" >def takeRight(n: Int): Repr</pre></code>

### takeWhile
<pre><code class="language-scala" >override def takeWhile(p: (A) => Boolean): Repr</pre></code>

### thisCollection
<pre><code class="language-scala" >protected override def thisCollection: Seq[A]</pre></code>

### to
<pre><code class="language-scala" >override def to[Col](implicit cbf: CanBuildFrom[Nothing, A, Col[A]]): Col[A]</pre></code>

### toArray
<pre><code class="language-scala" >def toArray[B](implicit evidence$1: ClassTag[B]): Array[B]</pre></code>

### toBuffer
<pre><code class="language-scala" >def toBuffer[B]: Buffer[B]</pre></code>

### toCollection
<pre><code class="language-scala" >protected override def toCollection(repr: Repr): Seq[A]</pre></code>

### toIndexedSeq
<pre><code class="language-scala" >def toIndexedSeq: IndexedSeq[A]</pre></code>

### toIterable
<pre><code class="language-scala" >override def toIterable: Iterable[A]</pre></code>

### toIterator
<pre><code class="language-scala" >@deprecatedOverriding override def toIterator: Iterator[A]</pre></code>

### toList
<pre><code class="language-scala" >def toList: List[A]</pre></code>

### toMap
<pre><code class="language-scala" >def toMap[T, U](implicit ev: <:<[A, (T, U)]): Map[T, U]</pre></code>

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
<pre><code class="language-scala" >@migration def transpose[B](implicit asTraversable: (A) => GenTraversableOnce[B]): CC[CC[B]]</pre></code>

### union
<pre><code class="language-scala" >override def union[B, That](that: GenSeq[B])(implicit bf: CanBuildFrom[Repr, B, That]): That</pre></code>

### unzip
<pre><code class="language-scala" >def unzip[A1, A2](implicit asPair: (A) => (A1, A2)): (CC[A1], CC[A2])</pre></code>

### unzip3
<pre><code class="language-scala" >def unzip3[A1, A2, A3](implicit asTriple: (A) => (A1, A2, A3)): (CC[A1], CC[A2], CC[A3])</pre></code>

### updated
<pre><code class="language-scala" >def updated[B, That](index: Int, elem: B)(implicit bf: CanBuildFrom[Repr, B, That]): That</pre></code>

### useOfOutsideType
<pre><code class="language-scala" >def useOfOutsideType(): <a href="./ReturnTypeClass.md">ReturnTypeClass</a>[<a href="./level2/Documentation.md#T">T</a>]</pre></code>

### useOfOutsideTypeInsideObject
<pre><code class="language-scala" >def useOfOutsideTypeInsideObject(): <a href="./ReturnObjectWithType$.md#returnType">returnType</a></pre></code>

### useOfSameLevelOutsideType
<pre><code class="language-scala" >def useOfSameLevelOutsideType(): <a href="./level2/SameLevelTypeLinking.md">SameLevelTypeLinking</a></pre></code>

### view
<pre><code class="language-scala" >override def view(from: Int, until: Int): SeqView[A, Repr]</pre></code>

### view
<pre><code class="language-scala" >override def view: AnyRef & SeqView[A, Repr]</pre></code>

### withFilter
<pre><code class="language-scala" >def withFilter(p: (A) => Boolean): FilterMonadic[A, Repr]</pre></code>

### zip
<pre><code class="language-scala" >def zip[A1, B, That](that: GenIterable[B])(implicit bf: CanBuildFrom[Repr, (A1, B), That]): That</pre></code>

### zipAll
<pre><code class="language-scala" >def zipAll[B, A1, That](that: GenIterable[B], thisElem: A1, thatElem: B)(implicit bf: CanBuildFrom[Repr, (A1, B), That]): That</pre></code>

### zipWithIndex
<pre><code class="language-scala" >def zipWithIndex[A1, That](implicit bf: CanBuildFrom[Repr, (A1, Int), That]): That</pre></code>

