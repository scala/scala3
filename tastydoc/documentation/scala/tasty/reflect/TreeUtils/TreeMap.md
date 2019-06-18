scala.tasty.reflect.TreeUtils
# class TreeMap

<pre><code class="language-scala" >abstract class TreeMap</pre></code>
## Concrete Value Members:
### transformCaseDef
<pre><code class="language-scala" >def transformCaseDef(tree: CaseDef)(ctx: Context): CaseDef</pre></code>

### transformCaseDefs
<pre><code class="language-scala" >def transformCaseDefs(trees: List[CaseDef])(ctx: Context): List[CaseDef]</pre></code>

### transformPattern
<pre><code class="language-scala" >def transformPattern(pattern: Pattern)(ctx: Context): Pattern</pre></code>

### transformPatterns
<pre><code class="language-scala" >def transformPatterns(trees: List[Pattern])(ctx: Context): List[Pattern]</pre></code>

### transformStatement
<pre><code class="language-scala" >def transformStatement(tree: Statement)(ctx: Context): Statement</pre></code>

### transformStats
<pre><code class="language-scala" >def transformStats(trees: List[Statement])(ctx: Context): List[Statement]</pre></code>

### transformSubTrees
<pre><code class="language-scala" >def transformSubTrees[Tr <: TreeUtils.this.Tree](trees: List[Tr])(ctx: Context): List[Tr]</pre></code>

### transformTerm
<pre><code class="language-scala" >def transformTerm(tree: Term)(ctx: Context): Term</pre></code>

### transformTerms
<pre><code class="language-scala" >def transformTerms(trees: List[Term])(ctx: Context): List[Term]</pre></code>

### transformTree
<pre><code class="language-scala" >def transformTree(tree: Tree)(ctx: Context): Tree</pre></code>

### transformTrees
<pre><code class="language-scala" >def transformTrees(trees: List[Tree])(ctx: Context): List[Tree]</pre></code>

### transformTypeCaseDef
<pre><code class="language-scala" >def transformTypeCaseDef(tree: TypeCaseDef)(ctx: Context): TypeCaseDef</pre></code>

### transformTypeCaseDefs
<pre><code class="language-scala" >def transformTypeCaseDefs(trees: List[TypeCaseDef])(ctx: Context): List[TypeCaseDef]</pre></code>

### transformTypeTree
<pre><code class="language-scala" >def transformTypeTree(tree: TypeTree)(ctx: Context): TypeTree</pre></code>

### transformTypeTrees
<pre><code class="language-scala" >def transformTypeTrees(trees: List[TypeTree])(ctx: Context): List[TypeTree]</pre></code>

