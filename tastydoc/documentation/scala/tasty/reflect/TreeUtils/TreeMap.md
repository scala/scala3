scala.tasty.reflect.TreeUtils
# class TreeMap

<pre><code class="language-scala" >abstract class TreeMap</pre></code>
## Concrete Value Members:
### transformCaseDef
<pre><code class="language-scala" >def transformCaseDef(tree: CaseDef)(implicit ctx: Context): CaseDef</pre></code>

### transformCaseDefs
<pre><code class="language-scala" >def transformCaseDefs(trees: List[CaseDef])(implicit ctx: Context): List[CaseDef]</pre></code>

### transformPattern
<pre><code class="language-scala" >def transformPattern(pattern: Pattern)(implicit ctx: Context): Pattern</pre></code>

### transformPatterns
<pre><code class="language-scala" >def transformPatterns(trees: List[Pattern])(implicit ctx: Context): List[Pattern]</pre></code>

### transformStatement
<pre><code class="language-scala" >def transformStatement(tree: Statement)(implicit ctx: Context): Statement</pre></code>

### transformStats
<pre><code class="language-scala" >def transformStats(trees: List[Statement])(implicit ctx: Context): List[Statement]</pre></code>

### transformSubTrees
<pre><code class="language-scala" >def transformSubTrees[Tr <: TreeUtils.this.Tree](trees: List[Tr])(implicit ctx: Context): List[Tr]</pre></code>

### transformTerm
<pre><code class="language-scala" >def transformTerm(tree: Term)(implicit ctx: Context): Term</pre></code>

### transformTerms
<pre><code class="language-scala" >def transformTerms(trees: List[Term])(implicit ctx: Context): List[Term]</pre></code>

### transformTree
<pre><code class="language-scala" >def transformTree(tree: Tree)(implicit ctx: Context): Tree</pre></code>

### transformTrees
<pre><code class="language-scala" >def transformTrees(trees: List[Tree])(implicit ctx: Context): List[Tree]</pre></code>

### transformTypeCaseDef
<pre><code class="language-scala" >def transformTypeCaseDef(tree: TypeCaseDef)(implicit ctx: Context): TypeCaseDef</pre></code>

### transformTypeCaseDefs
<pre><code class="language-scala" >def transformTypeCaseDefs(trees: List[TypeCaseDef])(implicit ctx: Context): List[TypeCaseDef]</pre></code>

### transformTypeTree
<pre><code class="language-scala" >def transformTypeTree(tree: TypeTree)(implicit ctx: Context): TypeTree</pre></code>

### transformTypeTrees
<pre><code class="language-scala" >def transformTypeTrees(trees: List[TypeTree])(implicit ctx: Context): List[TypeTree]</pre></code>

