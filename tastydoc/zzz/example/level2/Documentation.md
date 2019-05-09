# class Documentation

## Companion object : example.Documentation

<pre><code class="language-scala" >sealed abstract class Documentation[T, A <: scala.Int, B >: scala.Predef.String, X, Y] extends <a href="../../scala/collection.md#Seq">Seq</a>[<a href="Documentation.md#T">T</a>] with <a href="../../scala.md#Product">Product</a> with <a href="../../scala.md#Serializable">Serializable</a></pre></code>
This class is used for testing tasty doc generation

***authors*** Bryan Abate

***c1*** class parameter 1


***c2*** class parameter 2

***T*** class type parameter

***Constructor*** create new object


[link](#T)

# T

## Annotations:


## Constructors:
<pre><code class="language-scala" >Documentation(c1: <a href="../../scala/Predef.md#String">String</a>, c2: <a href="../../immutable.md#List">List</a>[<a href="#T">T</a>])</pre></code>
<pre><code class="language-scala" >Documentation(ac: <a href="../../scala/Predef.md#String">String</a>)</pre></code>
Auxiliary constructor

***ac*** auxiliary parameter

<pre><code class="language-scala" >Documentation()</pre></code>
<pre><code class="language-scala" >Documentation(x: <a href="#T">T</a>)</pre></code>

## Members:

### Definitions: 
#### methodsWithCallByName
<pre><code class="language-scala" >def methodsWithCallByName(x: => <a href="../../scala.md#Int">Int</a>): <a href="../../scala.md#Nothing">Nothing</a></pre></code>

#### grouped
<pre><code class="language-scala" >def grouped(size: <a href="../../scala/Int.md">Int</a>): <a href="../../collection.md#Iterator">Iterator</a>[<a href="../../collection/IterableLike.md#Repr">Repr</a>]</pre></code>

#### to
<pre><code class="language-scala" >override def to[Col](cbf: <a href="../../generic.md#CanBuildFrom">CanBuildFrom</a>[<a href="../../scala/Nothing.md">Nothing</a>, <a href="../../collection/TraversableLike.md#A">A</a>, <a href="#Col">Col</a>[<a href="../../collection/TraversableLike.md#A">A</a>]]): <a href="#Col">Col</a>[<a href="../../collection/TraversableLike.md#A">A</a>]</pre></code>

#### scanLeft
<pre><code class="language-scala" >def scanLeft[B, That](z: <a href="#B">B</a>)(op: (<a href="#B">B</a>, <a href="../../collection/TraversableLike.md#A">A</a>) => <a href="#B">B</a>)(bf: <a href="../../generic.md#CanBuildFrom">CanBuildFrom</a>[<a href="../../collection/TraversableLike.md#Repr">Repr</a>, <a href="#B">B</a>, <a href="#That">That</a>]): <a href="#That">That</a></pre></code>

#### ++:
<pre><code class="language-scala" >def ++:[B, That](that: <a href="../../collection.md#Traversable">Traversable</a>[<a href="#B">B</a>])(bf: <a href="../../generic.md#CanBuildFrom">CanBuildFrom</a>[<a href="../../collection/TraversableLike.md#Repr">Repr</a>, <a href="#B">B</a>, <a href="#That">That</a>]): <a href="#That">That</a></pre></code>

#### ++:
<pre><code class="language-scala" >def ++:[B, That](that: <a href="../../collection.md#TraversableOnce">TraversableOnce</a>[<a href="#B">B</a>])(bf: <a href="../../generic.md#CanBuildFrom">CanBuildFrom</a>[<a href="../../collection/TraversableLike.md#Repr">Repr</a>, <a href="#B">B</a>, <a href="#That">That</a>]): <a href="#That">That</a></pre></code>

#### toStream
<pre><code class="language-scala" >override def toStream: <a href="../../immutable.md#Stream">Stream</a>[<a href="../../collection/IterableLike.md#A">A</a>]</pre></code>

#### lengthCompare
<pre><code class="language-scala" >def lengthCompare(len: <a href="../../scala/Int.md">Int</a>): <a href="../../scala/Int.md">Int</a></pre></code>

#### parCombiner
<pre><code class="language-scala" >protected override def parCombiner: <a href="../../parallel.md#Combiner">Combiner</a>[<a href="../../collection/SeqLike.md#A">A</a>, <a href="../../parallel.md#ParSeq">ParSeq</a>[<a href="../../collection/SeqLike.md#A">A</a>]]</pre></code>

#### indexOfSlice
<pre><code class="language-scala" >def indexOfSlice[B](that: <a href="../../collection.md#GenSeq">GenSeq</a>[<a href="#B">B</a>], from: <a href="../../scala/Int.md">Int</a>): <a href="../../scala/Int.md">Int</a></pre></code>

#### indexOfSlice
<pre><code class="language-scala" >def indexOfSlice[B](that: <a href="../../collection.md#GenSeq">GenSeq</a>[<a href="#B">B</a>]): <a href="../../scala/Int.md">Int</a></pre></code>

#### :\
<pre><code class="language-scala" >def :\[B](z: <a href="#B">B</a>)(op: (<a href="../../collection/TraversableOnce.md#A">A</a>, <a href="#B">B</a>) => <a href="#B">B</a>): <a href="#B">B</a></pre></code>

#### combinations
<pre><code class="language-scala" >def combinations(n: <a href="../../scala/Int.md">Int</a>): <a href="../../collection.md#Iterator">Iterator</a>[<a href="../../collection/SeqLike.md#Repr">Repr</a>]</pre></code>

#### patch
<pre><code class="language-scala" >def patch[B, That](from: <a href="../../scala/Int.md">Int</a>, patch: <a href="../../collection.md#GenSeq">GenSeq</a>[<a href="#B">B</a>], replaced: <a href="../../scala/Int.md">Int</a>)(bf: <a href="../../generic.md#CanBuildFrom">CanBuildFrom</a>[<a href="../../collection/SeqLike.md#Repr">Repr</a>, <a href="#B">B</a>, <a href="#That">That</a>]): <a href="#That">That</a></pre></code>

#### toIterator
<pre><code class="language-scala" >override def toIterator: <a href="../../collection.md#Iterator">Iterator</a>[<a href="../../collection/IterableLike.md#A">A</a>]</pre></code>

#### getClass
<pre><code class="language-scala" >final def getClass(): <a href="../../lang.md#Class">Class</a>[<a href="../../scala/Nothing.md">Nothing</a>>:<a href="../../scala/Any.md">Any</a>]</pre></code>

#### copyToBuffer
<pre><code class="language-scala" >def copyToBuffer[B](dest: <a href="../../mutable.md#Buffer">Buffer</a>[<a href="#B">B</a>]): <a href="../../scala/Unit.md">Unit</a></pre></code>

#### hasDefiniteSize
<pre><code class="language-scala" >def hasDefiniteSize: <a href="../../scala/Boolean.md">Boolean</a></pre></code>

#### isTraversableAgain
<pre><code class="language-scala" >final def isTraversableAgain: <a href="../../scala/Boolean.md">Boolean</a></pre></code>

#### apply
<pre><code class="language-scala" >def apply(idx: <a href="../../scala.md#Int">Int</a>): <a href="Documentation.md#T">T</a></pre></code>

#### reverseMap
<pre><code class="language-scala" >def reverseMap[B, That](f: (<a href="../../collection/SeqLike.md#A">A</a>) => <a href="#B">B</a>)(bf: <a href="../../generic.md#CanBuildFrom">CanBuildFrom</a>[<a href="../../collection/SeqLike.md#Repr">Repr</a>, <a href="#B">B</a>, <a href="#That">That</a>]): <a href="#That">That</a></pre></code>

#### takeWhile
<pre><code class="language-scala" >override def takeWhile(p: (<a href="../../collection/IterableLike.md#A">A</a>) => <a href="../../scala/Boolean.md">Boolean</a>): <a href="../../collection/IterableLike.md#Repr">Repr</a></pre></code>

#### headOption
<pre><code class="language-scala" >def headOption: <a href="../../scala.md#Option">Option</a>[<a href="../../collection/TraversableLike.md#A">A</a>]</pre></code>

#### toVector
<pre><code class="language-scala" >def toVector: <a href="../../immutable.md#Vector">Vector</a>[<a href="../../collection/TraversableOnce.md#A">A</a>]</pre></code>

#### segmentLength
<pre><code class="language-scala" >def segmentLength(p: (<a href="../../collection/SeqLike.md#A">A</a>) => <a href="../../scala/Boolean.md">Boolean</a>, from: <a href="../../scala/Int.md">Int</a>): <a href="../../scala/Int.md">Int</a></pre></code>

#### :+
<pre><code class="language-scala" >def :+[B, That](elem: <a href="#B">B</a>)(bf: <a href="../../generic.md#CanBuildFrom">CanBuildFrom</a>[<a href="../../collection/SeqLike.md#Repr">Repr</a>, <a href="#B">B</a>, <a href="#That">That</a>]): <a href="#That">That</a></pre></code>

#### sortWith
<pre><code class="language-scala" >def sortWith(lt: (<a href="../../collection/SeqLike.md#A">A</a>, <a href="../../collection/SeqLike.md#A">A</a>) => <a href="../../scala/Boolean.md">Boolean</a>): <a href="../../collection/SeqLike.md#Repr">Repr</a></pre></code>

#### aggregate
<pre><code class="language-scala" >def aggregate[B](z: => <a href="#B">B</a>)(seqop: (<a href="#B">B</a>, <a href="../../collection/TraversableOnce.md#A">A</a>) => <a href="#B">B</a>, combop: (<a href="#B">B</a>, <a href="#B">B</a>) => <a href="#B">B</a>): <a href="#B">B</a></pre></code>

#### min
<pre><code class="language-scala" >def min[B](cmp: <a href="../../math.md#Ordering">Ordering</a>[<a href="#B">B</a>]): <a href="../../collection/TraversableOnce.md#A">A</a></pre></code>

#### privateMethod
<pre><code class="language-scala" >private def privateMethod: <a href="../../scala.md#Nothing">Nothing</a></pre></code>

#### repr
<pre><code class="language-scala" >def repr: <a href="../../collection/TraversableLike.md#Repr">Repr</a></pre></code>

#### transpose
<pre><code class="language-scala" >def transpose[B](asTraversable: (<a href="../../generic/GenericTraversableTemplate.md#A">A</a>) => <a href="../../collection.md#GenTraversableOnce">GenTraversableOnce</a>[<a href="#B">B</a>]): <a href="../../generic/GenericTraversableTemplate.md#CC">CC</a>[<a href="../../generic/GenericTraversableTemplate.md#CC">CC</a>[<a href="#B">B</a>]]</pre></code>

#### isDefinedAt
<pre><code class="language-scala" >def isDefinedAt(idx: <a href="../../scala/Int.md">Int</a>): <a href="../../scala/Boolean.md">Boolean</a></pre></code>

#### clone
<pre><code class="language-scala" >protected def clone(): <a href="../../lang/Object.md">Object</a></pre></code>

#### indices
<pre><code class="language-scala" >def indices: <a href="../../immutable/Range.md">Range</a></pre></code>

#### find
<pre><code class="language-scala" >override def find(p: (<a href="../../collection/IterableLike.md#A">A</a>) => <a href="../../scala/Boolean.md">Boolean</a>): <a href="../../scala.md#Option">Option</a>[<a href="../../collection/IterableLike.md#A">A</a>]</pre></code>

#### tail
<pre><code class="language-scala" >override def tail: <a href="../../collection/TraversableLike.md#Repr">Repr</a></pre></code>

#### copyToArray
<pre><code class="language-scala" >override def copyToArray[B](xs: <a href="../../scala.md#Array">Array</a>[<a href="#B">B</a>], start: <a href="../../scala/Int.md">Int</a>, len: <a href="../../scala/Int.md">Int</a>): <a href="../../scala/Unit.md">Unit</a></pre></code>

#### copyToArray
<pre><code class="language-scala" >def copyToArray[B](xs: <a href="../../scala.md#Array">Array</a>[<a href="#B">B</a>], start: <a href="../../scala/Int.md">Int</a>): <a href="../../scala/Unit.md">Unit</a></pre></code>

#### copyToArray
<pre><code class="language-scala" >def copyToArray[B](xs: <a href="../../scala.md#Array">Array</a>[<a href="#B">B</a>]): <a href="../../scala/Unit.md">Unit</a></pre></code>

#### lastIndexOf
<pre><code class="language-scala" >def lastIndexOf[B](elem: <a href="#B">B</a>, end: <a href="../../scala/Int.md">Int</a>): <a href="../../scala/Int.md">Int</a></pre></code>

#### lastIndexOf
<pre><code class="language-scala" >def lastIndexOf[B](elem: <a href="#B">B</a>): <a href="../../scala/Int.md">Int</a></pre></code>

#### $asInstanceOf$
<pre><code class="language-scala" >final def $asInstanceOf$[X0]: <a href="#X0">X0</a></pre></code>

#### lift
<pre><code class="language-scala" >def lift: (<a href="../../scala/PartialFunction.md#A">A</a>) => <a href="../../scala.md#Option">Option</a>[<a href="../../scala/PartialFunction.md#B">B</a>]</pre></code>

#### toString
<pre><code class="language-scala" >override def toString(): <a href="../../scala/Predef.md#String">String</a></pre></code>

#### zip
<pre><code class="language-scala" >def zip[A1, B, That](that: <a href="../../collection.md#GenIterable">GenIterable</a>[<a href="#B">B</a>])(bf: <a href="../../generic.md#CanBuildFrom">CanBuildFrom</a>[<a href="../../collection/IterableLike.md#Repr">Repr</a>, (<a href="#A1">A1</a>, <a href="#B">B</a>), <a href="#That">That</a>]): <a href="#That">That</a></pre></code>

#### sizeHintIfCheap
<pre><code class="language-scala" >private[<a href="../../scala/collection.md">collection</a>] def sizeHintIfCheap: <a href="../../scala/Int.md">Int</a></pre></code>

#### unzip3
<pre><code class="language-scala" >def unzip3[A1, A2, A3](asTriple: (<a href="../../generic/GenericTraversableTemplate.md#A">A</a>) => (<a href="#A1">A1</a>, <a href="#A2">A2</a>, <a href="#A3">A3</a>)): (<a href="../../generic/GenericTraversableTemplate.md#CC">CC</a>[<a href="#A1">A1</a>], <a href="../../generic/GenericTraversableTemplate.md#CC">CC</a>[<a href="#A2">A2</a>], <a href="../../generic/GenericTraversableTemplate.md#CC">CC</a>[<a href="#A3">A3</a>])</pre></code>

#### sorted
<pre><code class="language-scala" >def sorted[B](ord: <a href="../../math.md#Ordering">Ordering</a>[<a href="#B">B</a>]): <a href="../../collection/SeqLike.md#Repr">Repr</a></pre></code>

#### lastIndexOfSlice
<pre><code class="language-scala" >def lastIndexOfSlice[B](that: <a href="../../collection.md#GenSeq">GenSeq</a>[<a href="#B">B</a>], end: <a href="../../scala/Int.md">Int</a>): <a href="../../scala/Int.md">Int</a></pre></code>

#### lastIndexOfSlice
<pre><code class="language-scala" >def lastIndexOfSlice[B](that: <a href="../../collection.md#GenSeq">GenSeq</a>[<a href="#B">B</a>]): <a href="../../scala/Int.md">Int</a></pre></code>

#### indexWhere
<pre><code class="language-scala" >def indexWhere(p: (<a href="../../collection/SeqLike.md#A">A</a>) => <a href="../../scala/Boolean.md">Boolean</a>, from: <a href="../../scala/Int.md">Int</a>): <a href="../../scala/Int.md">Int</a></pre></code>

#### indexWhere
<pre><code class="language-scala" >def indexWhere(p: (<a href="../../collection/GenSeqLike.md#A">A</a>) => <a href="../../scala/Boolean.md">Boolean</a>): <a href="../../scala/Int.md">Int</a></pre></code>

#### intersect
<pre><code class="language-scala" >def intersect[B](that: <a href="../../collection.md#GenSeq">GenSeq</a>[<a href="#B">B</a>]): <a href="../../collection/SeqLike.md#Repr">Repr</a></pre></code>

#### ne
<pre><code class="language-scala" >final def ne(x$0: <a href="../../lang/Object.md">Object</a>): <a href="../../scala/Boolean.md">Boolean</a></pre></code>

#### isInstanceOf
<pre><code class="language-scala" >final def isInstanceOf[X0]: <a href="../../scala/Boolean.md">Boolean</a></pre></code>

#### newBuilder
<pre><code class="language-scala" >protected def newBuilder: <a href="../../mutable.md#Builder">Builder</a>[<a href="../../generic/GenericTraversableTemplate.md#A">A</a>, <a href="../../generic/GenericTraversableTemplate.md#CC">CC</a>[<a href="../../generic/GenericTraversableTemplate.md#A">A</a>]]</pre></code>

#### maxBy
<pre><code class="language-scala" >def maxBy[B](f: (<a href="../../collection/TraversableOnce.md#A">A</a>) => <a href="#B">B</a>)(cmp: <a href="../../math.md#Ordering">Ordering</a>[<a href="#B">B</a>]): <a href="../../collection/TraversableOnce.md#A">A</a></pre></code>

#### zipAll
<pre><code class="language-scala" >def zipAll[B, A1, That](that: <a href="../../collection.md#GenIterable">GenIterable</a>[<a href="#B">B</a>], thisElem: <a href="#A1">A1</a>, thatElem: <a href="#B">B</a>)(bf: <a href="../../generic.md#CanBuildFrom">CanBuildFrom</a>[<a href="../../collection/IterableLike.md#Repr">Repr</a>, (<a href="#A1">A1</a>, <a href="#B">B</a>), <a href="#That">That</a>]): <a href="#That">That</a></pre></code>

#### docWithMd
<pre><code class="language-scala" >def docWithMd: <a href="../../scala.md#Nothing">Nothing</a></pre></code>
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



#### toCollection
<pre><code class="language-scala" >protected override def toCollection(repr: <a href="../../collection/SeqLike.md#Repr">Repr</a>): <a href="../../collection.md#Seq">Seq</a>[<a href="../../collection/SeqLike.md#A">A</a>]</pre></code>

#### flatten
<pre><code class="language-scala" >def flatten[B](asTraversable: (<a href="../../generic/GenericTraversableTemplate.md#A">A</a>) => <a href="../../collection.md#GenTraversableOnce">GenTraversableOnce</a>[<a href="#B">B</a>]): <a href="../../generic/GenericTraversableTemplate.md#CC">CC</a>[<a href="#B">B</a>]</pre></code>

#### asInstanceOf
<pre><code class="language-scala" >final def asInstanceOf[X0]: <a href="#X0">X0</a></pre></code>

#### size
<pre><code class="language-scala" >override def size: <a href="../../scala/Int.md">Int</a></pre></code>

#### permutations
<pre><code class="language-scala" >def permutations: <a href="../../collection.md#Iterator">Iterator</a>[<a href="../../collection/SeqLike.md#Repr">Repr</a>]</pre></code>

#### synchronized
<pre><code class="language-scala" >final def synchronized[X0](x$0: <a href="#X0">X0</a>): <a href="#X0">X0</a></pre></code>

#### diff
<pre><code class="language-scala" >def diff[B](that: <a href="../../collection.md#GenSeq">GenSeq</a>[<a href="#B">B</a>]): <a href="../../collection/SeqLike.md#Repr">Repr</a></pre></code>

#### stringPrefix
<pre><code class="language-scala" >def stringPrefix: <a href="../../scala/Predef.md#String">String</a></pre></code>

#### scan
<pre><code class="language-scala" >def scan[B, That](z: <a href="#B">B</a>)(op: (<a href="#B">B</a>, <a href="#B">B</a>) => <a href="#B">B</a>)(cbf: <a href="../../generic.md#CanBuildFrom">CanBuildFrom</a>[<a href="../../collection/TraversableLike.md#Repr">Repr</a>, <a href="#B">B</a>, <a href="#That">That</a>]): <a href="#That">That</a></pre></code>

#### filter
<pre><code class="language-scala" >def filter(p: (<a href="../../collection/TraversableLike.md#A">A</a>) => <a href="../../scala/Boolean.md">Boolean</a>): <a href="../../collection/TraversableLike.md#Repr">Repr</a></pre></code>

#### forall
<pre><code class="language-scala" >override def forall(p: (<a href="../../collection/IterableLike.md#A">A</a>) => <a href="../../scala/Boolean.md">Boolean</a>): <a href="../../scala/Boolean.md">Boolean</a></pre></code>

#### length
<pre><code class="language-scala" >override def length: <a href="../../scala.md#Int">Int</a></pre></code>

#### +:
<pre><code class="language-scala" >def +:[B, That](elem: <a href="#B">B</a>)(bf: <a href="../../generic.md#CanBuildFrom">CanBuildFrom</a>[<a href="../../collection/SeqLike.md#Repr">Repr</a>, <a href="#B">B</a>, <a href="#That">That</a>]): <a href="#That">That</a></pre></code>

#### mkString
<pre><code class="language-scala" >def mkString: <a href="../../scala/Predef.md#String">String</a></pre></code>

#### mkString
<pre><code class="language-scala" >def mkString(sep: <a href="../../scala/Predef.md#String">String</a>): <a href="../../scala/Predef.md#String">String</a></pre></code>

#### mkString
<pre><code class="language-scala" >def mkString(start: <a href="../../scala/Predef.md#String">String</a>, sep: <a href="../../scala/Predef.md#String">String</a>, end: <a href="../../scala/Predef.md#String">String</a>): <a href="../../scala/Predef.md#String">String</a></pre></code>

#### foldRight
<pre><code class="language-scala" >override def foldRight[B](z: <a href="#B">B</a>)(op: (<a href="../../collection/IterableLike.md#A">A</a>, <a href="#B">B</a>) => <a href="#B">B</a>): <a href="#B">B</a></pre></code>

#### toArray
<pre><code class="language-scala" >def toArray[B](evidence$1: <a href="../../reflect.md#ClassTag">ClassTag</a>[<a href="#B">B</a>]): <a href="../../scala.md#Array">Array</a>[<a href="#B">B</a>]</pre></code>

#### equals
<pre><code class="language-scala" >override def equals(that: <a href="../../scala/Any.md">Any</a>): <a href="../../scala/Boolean.md">Boolean</a></pre></code>

#### runWith
<pre><code class="language-scala" >def runWith[U](action: (<a href="../../scala/PartialFunction.md#B">B</a>) => <a href="#U">U</a>): (<a href="../../scala/PartialFunction.md#A">A</a>) => <a href="../../scala/Boolean.md">Boolean</a></pre></code>

#### sum
<pre><code class="language-scala" >def sum[B](num: <a href="../../math.md#Numeric">Numeric</a>[<a href="#B">B</a>]): <a href="#B">B</a></pre></code>

#### ==
<pre><code class="language-scala" >final def ==(x$0: <a href="../../scala/Any.md">Any</a>): <a href="../../scala/Boolean.md">Boolean</a></pre></code>

#### $isInstanceOf$
<pre><code class="language-scala" >final def $isInstanceOf$[X0]: <a href="../../scala/Boolean.md">Boolean</a></pre></code>

#### productPrefix
<pre><code class="language-scala" >def productPrefix: <a href="../../lang/String.md">String</a></pre></code>

#### /:
<pre><code class="language-scala" >def /:[B](z: <a href="#B">B</a>)(op: (<a href="#B">B</a>, <a href="../../collection/TraversableOnce.md#A">A</a>) => <a href="#B">B</a>): <a href="#B">B</a></pre></code>

#### toList
<pre><code class="language-scala" >def toList: <a href="../../immutable.md#List">List</a>[<a href="../../collection/TraversableOnce.md#A">A</a>]</pre></code>

#### init
<pre><code class="language-scala" >def init: <a href="../../collection/TraversableLike.md#Repr">Repr</a></pre></code>

#### functionWithType
<pre><code class="language-scala" >def functionWithType[U >: scala.Predef.String](): <a href="#U">U</a></pre></code>

#### notify
<pre><code class="language-scala" >final def notify(): <a href="../../scala/Unit.md">Unit</a></pre></code>

#### productIterator
<pre><code class="language-scala" >def productIterator: <a href="../../collection.md#Iterator">Iterator</a>[<a href="../../scala/Any.md">Any</a>]</pre></code>

#### drop
<pre><code class="language-scala" >override def drop(n: <a href="../../scala/Int.md">Int</a>): <a href="../../collection/IterableLike.md#Repr">Repr</a></pre></code>

#### take
<pre><code class="language-scala" >override def take(n: <a href="../../scala/Int.md">Int</a>): <a href="../../collection/IterableLike.md#Repr">Repr</a></pre></code>

#### distinct
<pre><code class="language-scala" >def distinct: <a href="../../collection/SeqLike.md#Repr">Repr</a></pre></code>

#### prefixLength
<pre><code class="language-scala" >def prefixLength(p: (<a href="../../collection/GenSeqLike.md#A">A</a>) => <a href="../../scala/Boolean.md">Boolean</a>): <a href="../../scala/Int.md">Int</a></pre></code>

#### toTraversable
<pre><code class="language-scala" >def toTraversable: <a href="../../collection.md#Traversable">Traversable</a>[<a href="../../collection/TraversableLike.md#A">A</a>]</pre></code>

#### andThen
<pre><code class="language-scala" >override def andThen[C](k: (<a href="../../scala/PartialFunction.md#B">B</a>) => <a href="#C">C</a>): <a href="../../scala.md#PartialFunction">PartialFunction</a>[<a href="../../scala/PartialFunction.md#A">A</a>, <a href="#C">C</a>]</pre></code>

#### dropRight
<pre><code class="language-scala" >def dropRight(n: <a href="../../scala/Int.md">Int</a>): <a href="../../collection/IterableLike.md#Repr">Repr</a></pre></code>

#### contains
<pre><code class="language-scala" >def contains[A1](elem: <a href="#A1">A1</a>): <a href="../../scala/Boolean.md">Boolean</a></pre></code>

#### reduceLeftOption
<pre><code class="language-scala" >def reduceLeftOption[B](op: (<a href="#B">B</a>, <a href="../../collection/TraversableOnce.md#A">A</a>) => <a href="#B">B</a>): <a href="../../scala.md#Option">Option</a>[<a href="#B">B</a>]</pre></code>

#### reversed
<pre><code class="language-scala" >protected def reversed: <a href="../../immutable.md#List">List</a>[<a href="../../collection/TraversableOnce.md#A">A</a>]</pre></code>

#### product
<pre><code class="language-scala" >def product[B](num: <a href="../../math.md#Numeric">Numeric</a>[<a href="#B">B</a>]): <a href="#B">B</a></pre></code>

#### sameElements
<pre><code class="language-scala" >def sameElements[B](that: <a href="../../collection.md#GenIterable">GenIterable</a>[<a href="#B">B</a>]): <a href="../../scala/Boolean.md">Boolean</a></pre></code>

#### endsWith
<pre><code class="language-scala" >def endsWith[B](that: <a href="../../collection.md#GenSeq">GenSeq</a>[<a href="#B">B</a>]): <a href="../../scala/Boolean.md">Boolean</a></pre></code>

#### reduce
<pre><code class="language-scala" >def reduce[A1](op: (<a href="#A1">A1</a>, <a href="#A1">A1</a>) => <a href="#A1">A1</a>): <a href="#A1">A1</a></pre></code>

#### sliceWithKnownDelta
<pre><code class="language-scala" >private[<a href="#scala">scala</a>] def sliceWithKnownDelta(from: <a href="../../scala/Int.md">Int</a>, until: <a href="../../scala/Int.md">Int</a>, delta: <a href="../../scala/Int.md">Int</a>): <a href="../../collection/TraversableLike.md#Repr">Repr</a></pre></code>

#### reduceRight
<pre><code class="language-scala" >override def reduceRight[B](op: (<a href="../../collection/IterableLike.md#A">A</a>, <a href="#B">B</a>) => <a href="#B">B</a>): <a href="#B">B</a></pre></code>

#### minBy
<pre><code class="language-scala" >def minBy[B](f: (<a href="../../collection/TraversableOnce.md#A">A</a>) => <a href="#B">B</a>)(cmp: <a href="../../math.md#Ordering">Ordering</a>[<a href="#B">B</a>]): <a href="../../collection/TraversableOnce.md#A">A</a></pre></code>

#### inits
<pre><code class="language-scala" >def inits: <a href="../../collection.md#Iterator">Iterator</a>[<a href="../../collection/TraversableLike.md#Repr">Repr</a>]</pre></code>

#### union
<pre><code class="language-scala" >override def union[B, That](that: <a href="../../collection.md#GenSeq">GenSeq</a>[<a href="#B">B</a>])(bf: <a href="../../generic.md#CanBuildFrom">CanBuildFrom</a>[<a href="../../collection/SeqLike.md#Repr">Repr</a>, <a href="#B">B</a>, <a href="#That">That</a>]): <a href="#That">That</a></pre></code>

#### ##
<pre><code class="language-scala" >final def ##: <a href="../../scala/Int.md">Int</a></pre></code>

#### ++
<pre><code class="language-scala" >def ++[B, That](that: <a href="../../collection.md#GenTraversableOnce">GenTraversableOnce</a>[<a href="#B">B</a>])(bf: <a href="../../generic.md#CanBuildFrom">CanBuildFrom</a>[<a href="../../collection/TraversableLike.md#Repr">Repr</a>, <a href="#B">B</a>, <a href="#That">That</a>]): <a href="#That">That</a></pre></code>

#### last
<pre><code class="language-scala" >def last: <a href="../../collection/TraversableLike.md#A">A</a></pre></code>

#### toSeq
<pre><code class="language-scala" >override def toSeq: <a href="../../collection.md#Seq">Seq</a>[<a href="../../collection/SeqLike.md#A">A</a>]</pre></code>

#### methodsWithImplicit
<pre><code class="language-scala" >def methodsWithImplicit(x: <a href="../../scala.md#Int">Int</a>)(implicit imp: <a href="../../scala.md#Int">Int</a>, notImp: <a href="../../scala/Predef.md#String">String</a>): <a href="../../scala.md#Nothing">Nothing</a></pre></code>

#### toSet
<pre><code class="language-scala" >def toSet[B]: <a href="../../immutable.md#Set">Set</a>[<a href="#B">B</a>]</pre></code>

#### zipWithIndex
<pre><code class="language-scala" >def zipWithIndex[A1, That](bf: <a href="../../generic.md#CanBuildFrom">CanBuildFrom</a>[<a href="../../collection/IterableLike.md#Repr">Repr</a>, (<a href="#A1">A1</a>, <a href="../../scala/Int.md">Int</a>), <a href="#That">That</a>]): <a href="#That">That</a></pre></code>

#### !=
<pre><code class="language-scala" >final def !=(x$0: <a href="../../scala/Any.md">Any</a>): <a href="../../scala/Boolean.md">Boolean</a></pre></code>

#### reverseIterator
<pre><code class="language-scala" >def reverseIterator: <a href="../../collection.md#Iterator">Iterator</a>[<a href="../../collection/SeqLike.md#A">A</a>]</pre></code>

#### partition
<pre><code class="language-scala" >def partition(p: (<a href="../../collection/TraversableLike.md#A">A</a>) => <a href="../../scala/Boolean.md">Boolean</a>): (<a href="../../collection/TraversableLike.md#Repr">Repr</a>, <a href="../../collection/TraversableLike.md#Repr">Repr</a>)</pre></code>

#### startsWith
<pre><code class="language-scala" >def startsWith[B](that: <a href="../../collection.md#GenSeq">GenSeq</a>[<a href="#B">B</a>], offset: <a href="../../scala/Int.md">Int</a>): <a href="../../scala/Boolean.md">Boolean</a></pre></code>

#### startsWith
<pre><code class="language-scala" >def startsWith[B](that: <a href="../../collection.md#GenSeq">GenSeq</a>[<a href="#B">B</a>]): <a href="../../scala/Boolean.md">Boolean</a></pre></code>

#### sliding
<pre><code class="language-scala" >def sliding(size: <a href="../../scala/Int.md">Int</a>, step: <a href="../../scala/Int.md">Int</a>): <a href="../../collection.md#Iterator">Iterator</a>[<a href="../../collection/IterableLike.md#Repr">Repr</a>]</pre></code>

#### sliding
<pre><code class="language-scala" >def sliding(size: <a href="../../scala/Int.md">Int</a>): <a href="../../collection.md#Iterator">Iterator</a>[<a href="../../collection/IterableLike.md#Repr">Repr</a>]</pre></code>

#### seq
<pre><code class="language-scala" >override def seq: <a href="../../collection.md#Seq">Seq</a>[<a href="../../collection/Seq.md#A">A</a>]</pre></code>

#### notifyAll
<pre><code class="language-scala" >final def notifyAll(): <a href="../../scala/Unit.md">Unit</a></pre></code>

#### finalize
<pre><code class="language-scala" >protected def finalize(): <a href="../../scala/Unit.md">Unit</a></pre></code>

#### productElement
<pre><code class="language-scala" >def productElement(n: <a href="../../scala/Int.md">Int</a>): <a href="../../scala/Any.md">Any</a></pre></code>

#### padTo
<pre><code class="language-scala" >def padTo[B, That](len: <a href="../../scala/Int.md">Int</a>, elem: <a href="#B">B</a>)(bf: <a href="../../generic.md#CanBuildFrom">CanBuildFrom</a>[<a href="../../collection/SeqLike.md#Repr">Repr</a>, <a href="#B">B</a>, <a href="#That">That</a>]): <a href="#That">That</a></pre></code>

#### flatMap
<pre><code class="language-scala" >def flatMap[B, That](f: (<a href="../../collection/TraversableLike.md#A">A</a>) => <a href="../../collection.md#GenTraversableOnce">GenTraversableOnce</a>[<a href="#B">B</a>])(bf: <a href="../../generic.md#CanBuildFrom">CanBuildFrom</a>[<a href="../../collection/TraversableLike.md#Repr">Repr</a>, <a href="#B">B</a>, <a href="#That">That</a>]): <a href="#That">That</a></pre></code>

#### slice
<pre><code class="language-scala" >override def slice(from: <a href="../../scala/Int.md">Int</a>, until: <a href="../../scala/Int.md">Int</a>): <a href="../../collection/IterableLike.md#Repr">Repr</a></pre></code>

#### eq
<pre><code class="language-scala" >final def eq(x$0: <a href="../../lang/Object.md">Object</a>): <a href="../../scala/Boolean.md">Boolean</a></pre></code>

#### dropWhile
<pre><code class="language-scala" >def dropWhile(p: (<a href="../../collection/TraversableLike.md#A">A</a>) => <a href="../../scala/Boolean.md">Boolean</a>): <a href="../../collection/TraversableLike.md#Repr">Repr</a></pre></code>

#### toBuffer
<pre><code class="language-scala" >def toBuffer[B]: <a href="../../mutable.md#Buffer">Buffer</a>[<a href="#B">B</a>]</pre></code>

#### foreach
<pre><code class="language-scala" >def foreach[U](f: (<a href="../../collection/IterableLike.md#A">A</a>) => <a href="#U">U</a>): <a href="../../scala/Unit.md">Unit</a></pre></code>

#### reduceRightOption
<pre><code class="language-scala" >def reduceRightOption[B](op: (<a href="../../collection/TraversableOnce.md#A">A</a>, <a href="#B">B</a>) => <a href="#B">B</a>): <a href="../../scala.md#Option">Option</a>[<a href="#B">B</a>]</pre></code>

#### updated
<pre><code class="language-scala" >def updated[B, That](index: <a href="../../scala/Int.md">Int</a>, elem: <a href="#B">B</a>)(bf: <a href="../../generic.md#CanBuildFrom">CanBuildFrom</a>[<a href="../../collection/SeqLike.md#Repr">Repr</a>, <a href="#B">B</a>, <a href="#That">That</a>]): <a href="#That">That</a></pre></code>

#### scanRight
<pre><code class="language-scala" >def scanRight[B, That](z: <a href="#B">B</a>)(op: (<a href="../../collection/TraversableLike.md#A">A</a>, <a href="#B">B</a>) => <a href="#B">B</a>)(bf: <a href="../../generic.md#CanBuildFrom">CanBuildFrom</a>[<a href="../../collection/TraversableLike.md#Repr">Repr</a>, <a href="#B">B</a>, <a href="#That">That</a>]): <a href="#That">That</a></pre></code>

#### genericBuilder
<pre><code class="language-scala" >def genericBuilder[B]: <a href="../../mutable.md#Builder">Builder</a>[<a href="#B">B</a>, <a href="../../generic/GenericTraversableTemplate.md#CC">CC</a>[<a href="#B">B</a>]]</pre></code>

#### toMap
<pre><code class="language-scala" >def toMap[T, U](ev: <a href="../../scala/Predef.md#<:<"><:<</a>[<a href="../../collection/TraversableOnce.md#A">A</a>, (<a href="#T">T</a>, <a href="#U">U</a>)]): <a href="../../immutable.md#Map">Map</a>[<a href="#T">T</a>, <a href="#U">U</a>]</pre></code>

#### applyOrElse
<pre><code class="language-scala" >def applyOrElse[A1, B1](x: <a href="#A1">A1</a>, default: (<a href="#A1">A1</a>) => <a href="#B1">B1</a>): <a href="#B1">B1</a></pre></code>

#### protectedMethod
<pre><code class="language-scala" >protected def protectedMethod: <a href="../../scala.md#Nothing">Nothing</a></pre></code>

#### defReturningInnerClass
<pre><code class="language-scala" >def defReturningInnerClass(): <a href="Documentation/innerDocumentationClass.md">innerDocumentationClass</a></pre></code>

#### reverse
<pre><code class="language-scala" >def reverse: <a href="../../collection/SeqLike.md#Repr">Repr</a></pre></code>

#### addString
<pre><code class="language-scala" >def addString(b: <a href="../../scala/package.md#StringBuilder">StringBuilder</a>): <a href="../../scala/package.md#StringBuilder">StringBuilder</a></pre></code>

#### addString
<pre><code class="language-scala" >def addString(b: <a href="../../scala/package.md#StringBuilder">StringBuilder</a>, sep: <a href="../../scala/Predef.md#String">String</a>): <a href="../../scala/package.md#StringBuilder">StringBuilder</a></pre></code>

#### addString
<pre><code class="language-scala" >def addString(b: <a href="../../scala/package.md#StringBuilder">StringBuilder</a>, start: <a href="../../scala/Predef.md#String">String</a>, sep: <a href="../../scala/Predef.md#String">String</a>, end: <a href="../../scala/Predef.md#String">String</a>): <a href="../../scala/package.md#StringBuilder">StringBuilder</a></pre></code>

#### view
<pre><code class="language-scala" >override def view(from: <a href="../../scala/Int.md">Int</a>, until: <a href="../../scala/Int.md">Int</a>): <a href="../../collection.md#SeqView">SeqView</a>[<a href="../../collection/SeqLike.md#A">A</a>, <a href="../../collection/SeqLike.md#Repr">Repr</a>]</pre></code>

#### view
<pre><code class="language-scala" >override def view: <a href="../../scala.md#AnyRef">AnyRef</a> & <a href="../../collection.md#SeqView">SeqView</a>[<a href="../../collection/SeqLike.md#A">A</a>, <a href="../../collection/SeqLike.md#Repr">Repr</a>]</pre></code>

#### sortBy
<pre><code class="language-scala" >def sortBy[B](f: (<a href="../../collection/SeqLike.md#A">A</a>) => <a href="#B">B</a>)(ord: <a href="../../math.md#Ordering">Ordering</a>[<a href="#B">B</a>]): <a href="../../collection/SeqLike.md#Repr">Repr</a></pre></code>

#### collect
<pre><code class="language-scala" >def collect[B, That](pf: <a href="../../scala.md#PartialFunction">PartialFunction</a>[<a href="../../collection/TraversableLike.md#A">A</a>, <a href="#B">B</a>])(bf: <a href="../../generic.md#CanBuildFrom">CanBuildFrom</a>[<a href="../../collection/TraversableLike.md#Repr">Repr</a>, <a href="#B">B</a>, <a href="#That">That</a>]): <a href="#That">That</a></pre></code>

#### collectFirst
<pre><code class="language-scala" >def collectFirst[B](pf: <a href="../../scala.md#PartialFunction">PartialFunction</a>[<a href="../../collection/TraversableOnce.md#A">A</a>, <a href="#B">B</a>]): <a href="../../scala.md#Option">Option</a>[<a href="#B">B</a>]</pre></code>

#### groupBy
<pre><code class="language-scala" >def groupBy[K](f: (<a href="../../collection/TraversableLike.md#A">A</a>) => <a href="#K">K</a>): <a href="../../immutable.md#Map">Map</a>[<a href="#K">K</a>, <a href="../../collection/TraversableLike.md#Repr">Repr</a>]</pre></code>

#### hashCode
<pre><code class="language-scala" >override def hashCode(): <a href="../../scala/Int.md">Int</a></pre></code>

#### iterator
<pre><code class="language-scala" >def iterator: <a href="../../scala/collection.md#Iterator">Iterator</a>[<a href="Documentation.md#T">T</a>]</pre></code>

#### toIterable
<pre><code class="language-scala" >override def toIterable: <a href="../../collection.md#Iterable">Iterable</a>[<a href="../../collection/IterableLike.md#A">A</a>]</pre></code>

#### count
<pre><code class="language-scala" >def count(p: (<a href="../../collection/TraversableOnce.md#A">A</a>) => <a href="../../scala/Boolean.md">Boolean</a>): <a href="../../scala/Int.md">Int</a></pre></code>

#### splitAt
<pre><code class="language-scala" >def splitAt(n: <a href="../../scala/Int.md">Int</a>): (<a href="../../collection/TraversableLike.md#Repr">Repr</a>, <a href="../../collection/TraversableLike.md#Repr">Repr</a>)</pre></code>

#### foldLeft
<pre><code class="language-scala" >def foldLeft[B](z: <a href="#B">B</a>)(op: (<a href="#B">B</a>, <a href="../../collection/TraversableOnce.md#A">A</a>) => <a href="#B">B</a>): <a href="#B">B</a></pre></code>

#### useOfOutsideType
<pre><code class="language-scala" >def useOfOutsideType(): <a href="#ReturnTypeClass">ReturnTypeClass</a></pre></code>

#### productArity
<pre><code class="language-scala" >def productArity: <a href="../../scala/Int.md">Int</a></pre></code>

#### compose
<pre><code class="language-scala" >def compose[A](g: (<a href="#A">A</a>) => <a href="../../scala/Function1.md#T1">T1</a>): (<a href="#A">A</a>) => <a href="../../scala/Function1.md#R">R</a></pre></code>

#### fold
<pre><code class="language-scala" >def fold[A1](z: <a href="#A1">A1</a>)(op: (<a href="#A1">A1</a>, <a href="#A1">A1</a>) => <a href="#A1">A1</a>): <a href="#A1">A1</a></pre></code>

#### lastOption
<pre><code class="language-scala" >def lastOption: <a href="../../scala.md#Option">Option</a>[<a href="../../collection/TraversableLike.md#A">A</a>]</pre></code>

#### map
<pre><code class="language-scala" >def map[B, That](f: (<a href="../../collection/TraversableLike.md#A">A</a>) => <a href="#B">B</a>)(bf: <a href="../../generic.md#CanBuildFrom">CanBuildFrom</a>[<a href="../../collection/TraversableLike.md#Repr">Repr</a>, <a href="#B">B</a>, <a href="#That">That</a>]): <a href="#That">That</a></pre></code>

#### isEmpty
<pre><code class="language-scala" >override def isEmpty: <a href="../../scala/Boolean.md">Boolean</a></pre></code>

#### withFilter
<pre><code class="language-scala" >def withFilter(p: (<a href="../../collection/TraversableLike.md#A">A</a>) => <a href="../../scala/Boolean.md">Boolean</a>): <a href="../../generic.md#FilterMonadic">FilterMonadic</a>[<a href="../../collection/TraversableLike.md#A">A</a>, <a href="../../collection/TraversableLike.md#Repr">Repr</a>]</pre></code>

#### exists
<pre><code class="language-scala" >override def exists(p: (<a href="../../collection/IterableLike.md#A">A</a>) => <a href="../../scala/Boolean.md">Boolean</a>): <a href="../../scala/Boolean.md">Boolean</a></pre></code>

#### reduceLeft
<pre><code class="language-scala" >def reduceLeft[B](op: (<a href="#B">B</a>, <a href="../../collection/TraversableOnce.md#A">A</a>) => <a href="#B">B</a>): <a href="#B">B</a></pre></code>

#### wait
<pre><code class="language-scala" >final def wait(x$0: <a href="../../scala/Long.md">Long</a>, x$1: <a href="../../scala/Int.md">Int</a>): <a href="../../scala/Unit.md">Unit</a></pre></code>

#### wait
<pre><code class="language-scala" >final def wait(x$0: <a href="../../scala/Long.md">Long</a>): <a href="../../scala/Unit.md">Unit</a></pre></code>

#### wait
<pre><code class="language-scala" >final def wait(): <a href="../../scala/Unit.md">Unit</a></pre></code>

#### filterImpl
<pre><code class="language-scala" >private[<a href="#scala">scala</a>] def filterImpl(p: (<a href="../../collection/TraversableLike.md#A">A</a>) => <a href="../../scala/Boolean.md">Boolean</a>, isFlipped: <a href="../../scala/Boolean.md">Boolean</a>): <a href="../../collection/TraversableLike.md#Repr">Repr</a></pre></code>

#### canEqual
<pre><code class="language-scala" >override def canEqual(that: <a href="../../scala/Any.md">Any</a>): <a href="../../scala/Boolean.md">Boolean</a></pre></code>

#### reduceOption
<pre><code class="language-scala" >def reduceOption[A1](op: (<a href="#A1">A1</a>, <a href="#A1">A1</a>) => <a href="#A1">A1</a>): <a href="../../scala.md#Option">Option</a>[<a href="#A1">A1</a>]</pre></code>

#### methodsWithParams
<pre><code class="language-scala" >def methodsWithParams(x: <a href="Documentation.md#T">T</a>, y: <a href="../../scala.md#Int">Int</a>): <a href="../../immutable.md#List">List</a>[<a href="../../scala/collection.md#Map">Map</a>[<a href="../../scala.md#Int">Int</a>, <a href="Documentation.md#T">T</a>]]</pre></code>
Test methods with params


***return*** something is returned

***y*** parameter 2


***x*** parameter 1


#### thisCollection
<pre><code class="language-scala" >protected override def thisCollection: <a href="../../collection.md#Seq">Seq</a>[<a href="../../collection/SeqLike.md#A">A</a>]</pre></code>

#### toIndexedSeq
<pre><code class="language-scala" >def toIndexedSeq: <a href="../../immutable.md#IndexedSeq">IndexedSeq</a>[<a href="../../collection/TraversableOnce.md#A">A</a>]</pre></code>

#### nonEmpty
<pre><code class="language-scala" >def nonEmpty: <a href="../../scala/Boolean.md">Boolean</a></pre></code>

#### methodsWithDefault
<pre><code class="language-scala" >def methodsWithDefault(x: <a href="../../scala.md#Int">Int</a>): <a href="../../scala.md#Nothing">Nothing</a></pre></code>

#### max
<pre><code class="language-scala" >def max[B](cmp: <a href="../../math.md#Ordering">Ordering</a>[<a href="#B">B</a>]): <a href="../../collection/TraversableOnce.md#A">A</a></pre></code>

#### indexOf
<pre><code class="language-scala" >def indexOf[B](elem: <a href="#B">B</a>, from: <a href="../../scala/Int.md">Int</a>): <a href="../../scala/Int.md">Int</a></pre></code>

#### indexOf
<pre><code class="language-scala" >def indexOf[B](elem: <a href="#B">B</a>): <a href="../../scala/Int.md">Int</a></pre></code>

#### sliceWithKnownBound
<pre><code class="language-scala" >private[<a href="#scala">scala</a>] def sliceWithKnownBound(from: <a href="../../scala/Int.md">Int</a>, until: <a href="../../scala/Int.md">Int</a>): <a href="../../collection/TraversableLike.md#Repr">Repr</a></pre></code>

#### companion
<pre><code class="language-scala" >override def companion: <a href="../../generic.md#GenericCompanion">GenericCompanion</a>[[+A >: scala.Nothing <: scala.Any] => scala.collection.Seq[+A]]</pre></code>

#### unzip
<pre><code class="language-scala" >def unzip[A1, A2](asPair: (<a href="../../generic/GenericTraversableTemplate.md#A">A</a>) => (<a href="#A1">A1</a>, <a href="#A2">A2</a>)): (<a href="../../generic/GenericTraversableTemplate.md#CC">CC</a>[<a href="#A1">A1</a>], <a href="../../generic/GenericTraversableTemplate.md#CC">CC</a>[<a href="#A2">A2</a>])</pre></code>

#### par
<pre><code class="language-scala" >def par: <a href="../../collection/Parallelizable.md#ParRepr">ParRepr</a></pre></code>

#### takeRight
<pre><code class="language-scala" >def takeRight(n: <a href="../../scala/Int.md">Int</a>): <a href="../../collection/IterableLike.md#Repr">Repr</a></pre></code>

#### span
<pre><code class="language-scala" >def span(p: (<a href="../../collection/TraversableLike.md#A">A</a>) => <a href="../../scala/Boolean.md">Boolean</a>): (<a href="../../collection/TraversableLike.md#Repr">Repr</a>, <a href="../../collection/TraversableLike.md#Repr">Repr</a>)</pre></code>

#### orElse
<pre><code class="language-scala" >def orElse[A1, B1](that: <a href="../../scala.md#PartialFunction">PartialFunction</a>[<a href="#A1">A1</a>, <a href="#B1">B1</a>]): <a href="../../scala.md#PartialFunction">PartialFunction</a>[<a href="#A1">A1</a>, <a href="#B1">B1</a>]</pre></code>

#### containsSlice
<pre><code class="language-scala" >def containsSlice[B](that: <a href="../../collection.md#GenSeq">GenSeq</a>[<a href="#B">B</a>]): <a href="../../scala/Boolean.md">Boolean</a></pre></code>

#### methodsWithDefault$default$1
<pre><code class="language-scala" >def methodsWithDefault$default$1: <a href="../../scala.md#Int">Int</a></pre></code>

#### abstractDefinition
<pre><code class="language-scala" >def abstractDefinition: <a href="../../scala.md#Int">Int</a></pre></code>

#### corresponds
<pre><code class="language-scala" >def corresponds[B](that: <a href="../../collection.md#GenSeq">GenSeq</a>[<a href="#B">B</a>])(p: (<a href="../../collection/SeqLike.md#A">A</a>, <a href="#B">B</a>) => <a href="../../scala/Boolean.md">Boolean</a>): <a href="../../scala/Boolean.md">Boolean</a></pre></code>

#### tails
<pre><code class="language-scala" >def tails: <a href="../../collection.md#Iterator">Iterator</a>[<a href="../../collection/TraversableLike.md#Repr">Repr</a>]</pre></code>

#### filterNot
<pre><code class="language-scala" >def filterNot(p: (<a href="../../collection/TraversableLike.md#A">A</a>) => <a href="../../scala/Boolean.md">Boolean</a>): <a href="../../collection/TraversableLike.md#Repr">Repr</a></pre></code>

#### lastIndexWhere
<pre><code class="language-scala" >def lastIndexWhere(p: (<a href="../../collection/SeqLike.md#A">A</a>) => <a href="../../scala/Boolean.md">Boolean</a>, end: <a href="../../scala/Int.md">Int</a>): <a href="../../scala/Int.md">Int</a></pre></code>

#### lastIndexWhere
<pre><code class="language-scala" >def lastIndexWhere(p: (<a href="../../collection/GenSeqLike.md#A">A</a>) => <a href="../../scala/Boolean.md">Boolean</a>): <a href="../../scala/Int.md">Int</a></pre></code>

#### head
<pre><code class="language-scala" >override def head: <a href="../../collection/IterableLike.md#A">A</a></pre></code>


### Values: 
#### c2
<pre><code class="language-scala" >val c2: <a href="../../collection/immutable.md#List">List</a>[<a href="Documentation.md#T">T</a>]</pre></code>

#### IAmACaseClass
<pre><code class="language-scala" >final val IAmACaseClass: <a href="Documentation/IAmACaseClass$.md">IAmACaseClass$</a></pre></code>

#### IAmACaseObject
<pre><code class="language-scala" >final val IAmACaseObject: <a href="Documentation/IAmACaseObject$.md">IAmACaseObject$</a></pre></code>

#### testObject
<pre><code class="language-scala" >final val testObject: <a href="Documentation/testObject$.md">testObject$</a></pre></code>

#### v
<pre><code class="language-scala" >val v: <a href="../../scala.md#Int">Int</a></pre></code>
Test value



#### protectedVal
<pre><code class="language-scala" >protected val protectedVal: <a href="../../scala.md#Nothing">Nothing</a></pre></code>

#### complexTypeVal
<pre><code class="language-scala" >val complexTypeVal: <a href="../../scala.md#Int">Int</a> | <a href="../../immutable.md#List">List</a>[<a href="../../immutable.md#List">List</a>[<a href="Documentation.md#T">T</a>]] & <a href="../../scala/Predef.md#String">String</a> | (<a href="../../scala.md#Double">Double</a> | <a href="../../scala.md#Int">Int</a>, <a href="../../scala.md#Double">Double</a>) | (<a href="../../scala.md#Int">Int</a>) => <a href="../../scala/Predef.md#String">String</a></pre></code>

#### valWithScopeModifier
<pre><code class="language-scala" >private[<a href="#example">example</a>] val valWithScopeModifier: <a href="../../scala.md#Nothing">Nothing</a></pre></code>


### Types: 
#### typeExamle
<pre><code class="language-scala" >type typeExamle: </pre></code>


### Classes: 
#### innerDocumentationClass
<pre><code class="language-scala" >class innerDocumentationClass</pre></code>

#### CaseImplementThis
<pre><code class="language-scala" >sealed trait CaseImplementThis</pre></code>

#### IAmACaseClass
<pre><code class="language-scala" >case class IAmACaseClass</pre></code>


### Objects: 
#### IAmACaseClass$
<pre><code class="language-scala" >final object IAmACaseClass$ extends (<a href="Documentation.md#T">T</a>, <a href="../../scala.md#Int">Int</a>) => <a href="Documentation/IAmACaseClass.md">IAmACaseClass</a> with <a href="../../scala/Serializable.md">Serializable</a></pre></code>

#### IAmACaseObject$
<pre><code class="language-scala" >final case object IAmACaseObject$ extends <a href="Documentation/CaseImplementThis.md">CaseImplementThis</a> with <a href="../../scala/Product.md">Product</a> with <a href="../../scala/Serializable.md">Serializable</a></pre></code>

#### testObject$
<pre><code class="language-scala" >final object testObject$ extends <a href="../../scala/Serializable.md">Serializable</a></pre></code>


