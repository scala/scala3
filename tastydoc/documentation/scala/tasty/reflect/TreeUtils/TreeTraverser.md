scala.tasty.reflect.TreeUtils
# class TreeTraverser

<pre><code class="language-scala" >abstract class TreeTraverser extends TreeAccumulator[Unit]</pre></code>
## Constructors:
<pre><code class="language-scala" >TreeTraverser()</pre></code>

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

### foldOverPattern
<pre><code class="language-scala" >def foldOverPattern(x: X, tree: Pattern)(ctx: Context): X</pre></code>

### foldOverTree
<pre><code class="language-scala" >def foldOverTree(x: X, tree: Tree)(ctx: Context): X</pre></code>

### foldPattern
<pre><code class="language-scala" >def foldPattern(x: Unit, tree: Pattern)(ctx: Context): Unit</pre></code>

### foldPatterns
<pre><code class="language-scala" >def foldPatterns(x: X, trees: Iterable[Pattern])(ctx: Context): X</pre></code>

### foldTree
<pre><code class="language-scala" >def foldTree(x: Unit, tree: Tree)(ctx: Context): Unit</pre></code>

### foldTrees
<pre><code class="language-scala" >def foldTrees(x: X, trees: Iterable[Tree])(ctx: Context): X</pre></code>

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

### traversePattern
<pre><code class="language-scala" >def traversePattern(tree: Pattern)(ctx: Context): Unit</pre></code>

### traversePatternChildren
<pre><code class="language-scala" >protected def traversePatternChildren(tree: Pattern)(ctx: Context): Unit</pre></code>

### traverseTree
<pre><code class="language-scala" >def traverseTree(tree: Tree)(ctx: Context): Unit</pre></code>

### traverseTreeChildren
<pre><code class="language-scala" >protected def traverseTreeChildren(tree: Tree)(ctx: Context): Unit</pre></code>

### wait
<pre><code class="language-scala" >final def wait(x$0: Long, x$1: Int): Unit</pre></code>

### wait
<pre><code class="language-scala" >final def wait(x$0: Long): Unit</pre></code>

### wait
<pre><code class="language-scala" >final def wait(): Unit</pre></code>

