scala.tasty.reflect.TreeUtils
# class TreeAccumulator

<pre><code class="language-scala" >abstract class TreeAccumulator[X]</pre></code>
## Concrete Value Members:
### foldOverPattern
<pre><code class="language-scala" >def foldOverPattern(x: X, tree: Pattern)(implicit ctx: Context): X</pre></code>

### foldOverTree
<pre><code class="language-scala" >def foldOverTree(x: X, tree: Tree)(implicit ctx: Context): X</pre></code>

### foldPattern
<pre><code class="language-scala" >def foldPattern(x: X, tree: Pattern)(implicit ctx: Context): X</pre></code>

### foldPatterns
<pre><code class="language-scala" >def foldPatterns(x: X, trees: Iterable[Pattern])(implicit ctx: Context): X</pre></code>

### foldTree
<pre><code class="language-scala" >def foldTree(x: X, tree: Tree)(implicit ctx: Context): X</pre></code>

### foldTrees
<pre><code class="language-scala" >def foldTrees(x: X, trees: Iterable[Tree])(implicit ctx: Context): X</pre></code>

