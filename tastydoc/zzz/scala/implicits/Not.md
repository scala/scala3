scala.implicits
# class Not

## Companion object <a href="./Not$.md">Not</a>

<pre><code class="language-scala" >final class Not[T]</pre></code>
A special class used to implement negation in implicit search.
Consider the problem of using implicit `i1` for a query type `D` if an implicit
for some other class `C` is available, and using an implicit `i2` if no implicit
value of type `C` is available. If we do not want to prioritize `i1` and `i2` by
putting them in different traits we can instead define the following:
   implicit def i1: D(implicit ev: C) = ...
   implicit def i2: D(implicit ev: Not[C]) = ...
`Not` is treated specially in implicit search, similar to the way logical negation
is treated in Prolog: The implicit search for `Not[C]` succeeds if and only if the implicit
search for `C` fails.
In Scala 2 this form of negation can be simulated by setting up a conditional
ambiguous implicit and an unconditional fallback, the way it is done with the
`default`, `amb1` and `amb2` methods below. Due to the way these two methods are
defined, `Not` is also usable from Scala 2.
In Dotty, ambiguity is a global error, and therefore cannot be used to implement negation.
Instead, `Not` is treated natively in implicit search.

## Constructors:
<pre><code class="language-scala" >Not()</pre></code>

## Concrete Value Members:
### !=
<pre><code class="language-scala" >final def !=(x$0: Any): Boolean</pre></code>

### ##
<pre><code class="language-scala" >final def ##: Int</pre></code>

### ==
<pre><code class="language-scala" >final def ==(x$0: Any): Boolean</pre></code>

### asInstanceOf
<pre><code class="language-scala" >final def asInstanceOf[X0]: X0</pre></code>

### clone
<pre><code class="language-scala" >protected def clone(): Object</pre></code>

### eq
<pre><code class="language-scala" >final def eq(x$0: Object): Boolean</pre></code>

### equals
<pre><code class="language-scala" >def equals(x$0: Any): Boolean</pre></code>

### finalize
<pre><code class="language-scala" >protected def finalize(): Unit</pre></code>

### getClass
<pre><code class="language-scala" >final def getClass(): Class[Nothing <: Any]</pre></code>

### hashCode
<pre><code class="language-scala" >def hashCode(): Int</pre></code>

### isInstanceOf
<pre><code class="language-scala" >final def isInstanceOf[X0]: Boolean</pre></code>

### ne
<pre><code class="language-scala" >final def ne(x$0: Object): Boolean</pre></code>

### notify
<pre><code class="language-scala" >final def notify(): Unit</pre></code>

### notifyAll
<pre><code class="language-scala" >final def notifyAll(): Unit</pre></code>

### synchronized
<pre><code class="language-scala" >final def synchronized[X0](x$0: X0): X0</pre></code>

### toString
<pre><code class="language-scala" >def toString(): String</pre></code>

### wait
<pre><code class="language-scala" >final def wait(x$0: Long, x$1: Int): Unit</pre></code>

### wait
<pre><code class="language-scala" >final def wait(x$0: Long): Unit</pre></code>

### wait
<pre><code class="language-scala" >final def wait(): Unit</pre></code>

