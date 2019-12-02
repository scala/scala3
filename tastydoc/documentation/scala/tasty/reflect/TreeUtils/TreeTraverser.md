scala.tasty.reflect.TreeUtils
# class TreeTraverser

<pre><code class="language-scala" >abstract class TreeTraverser extends TreeAccumulator[Unit]</pre></code>
## Concrete Value Members:
### foldOverPattern
<pre><code class="language-scala" >def foldOverPattern(x: X, tree: Pattern)(implicit ctx: Context): X</pre></code>

### foldOverTree
<pre><code class="language-scala" >def foldOverTree(x: X, tree: Tree)(implicit ctx: Context): X</pre></code>

### foldPattern
<pre><code class="language-scala" >def foldPattern(x: Unit, tree: Pattern)(implicit ctx: Context): Unit</pre></code>

### foldPatterns
<pre><code class="language-scala" >def foldPatterns(x: X, trees: Iterable[Pattern])(implicit ctx: Context): X</pre></code>

### foldTree
<pre><code class="language-scala" >def foldTree(x: Unit, tree: Tree)(implicit ctx: Context): Unit</pre></code>

### foldTrees
<pre><code class="language-scala" >def foldTrees(x: X, trees: Iterable[Tree])(implicit ctx: Context): X</pre></code>

### traversePattern
<pre><code class="language-scala" >def traversePattern(tree: Pattern)(implicit ctx: Context): Unit</pre></code>

### traversePatternChildren
<pre><code class="language-scala" >protected def traversePatternChildren(tree: Pattern)(implicit ctx: Context): Unit</pre></code>

### traverseTree
<pre><code class="language-scala" >def traverseTree(tree: Tree)(implicit ctx: Context): Unit</pre></code>

### traverseTreeChildren
<pre><code class="language-scala" >protected def traverseTreeChildren(tree: Tree)(implicit ctx: Context): Unit</pre></code>

