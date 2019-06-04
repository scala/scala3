# Package immutable
## Members:
<pre><code class="language-scala" >final object <a href="./List.md">List</a></pre></code>
$factoryInfo

<pre><code class="language-scala" >final val List: List</pre></code>
$factoryInfo


<pre><code class="language-scala" >@<a href="../../SerialVersionUID.md">SerialVersionUID</a> sealed abstract class <a href="./List.md">List</a></pre></code>
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

