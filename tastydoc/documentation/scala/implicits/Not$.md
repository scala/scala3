scala.implicits
# object Not

## Companion class <a href="./Not.md">Not</a>

<pre><code class="language-scala" >final object Not extends <a href="./LowPriorityNot.md">LowPriorityNot</a> with Serializable</pre></code>
## Concrete Value Members:
### amb1
<pre><code class="language-scala" >implicit def amb1[T](ev: T): <a href="./Not.md">Not</a>[T]</pre></code>
One of two ambiguous methods used to emulate negation in Scala 2

### amb2
<pre><code class="language-scala" >implicit def amb2[T](ev: T): <a href="./Not.md">Not</a>[T]</pre></code>
One of two ambiguous methods used to emulate negation in Scala 2

### default
<pre><code class="language-scala" >implicit def default[T]: <a href="./Not.md">Not</a>[T]</pre></code>
A fallback method used to emulate negation in Scala 2

### value
<pre><code class="language-scala" >def value: <a href="./Not.md">Not</a>[Nothing]</pre></code>
A value of type `Not` to signal a successful search for `Not[C]` (i.e. a failing
search for `C`). A reference to this value will be explicitly constructed by Dotty's
implicit search algorithm

