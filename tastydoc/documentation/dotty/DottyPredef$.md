dotty
# object DottyPredef

<pre><code class="language-scala" >final object DottyPredef extends Serializable</pre></code>
## Concrete Value Members:
### assert
<pre><code class="language-scala" >@forceInline final inline def assert(assertion: => Boolean): Unit</pre></code>

### assert
<pre><code class="language-scala" >@forceInline final inline def assert(assertion: => Boolean, message: => Any): Unit</pre></code>

### assertFail
<pre><code class="language-scala" >def assertFail(message: => Any): Unit</pre></code>

### assertFail
<pre><code class="language-scala" >def assertFail(): Unit</pre></code>

### implicitly
<pre><code class="language-scala" >@forceInline final inline def implicitly[T](implicit ev: T): T</pre></code>

### locally
<pre><code class="language-scala" >@forceInline inline def locally[T](body: => T): T</pre></code>

### the
<pre><code class="language-scala" >inline def the[T](x: T): x</pre></code>

### valueOf
<pre><code class="language-scala" >inline def valueOf[T]: T</pre></code>
Retrieve the single value of a type with a unique inhabitant.

***Example*** 
```scala
object Foo
val foo = valueOf[Foo.type]
// foo is Foo.type = Foo
val bar = valueOf[23]
// bar is 23.type = 23
```

***Group*** utilities

