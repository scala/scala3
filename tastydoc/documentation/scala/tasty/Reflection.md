scala.tasty
# class Reflection

## Companion object <a href="./Reflection$.md">Reflection</a>

<pre><code class="language-scala" >class Reflection extends Core with ConstantOps with ContextOps with CommentOps with FlagsOps with IdOps with ImportSelectorOps with QuotedOps with PatternOps with PositionOps with Printers with ReportingOps with RootPosition with SignatureOps with StandardDefinitions with SymbolOps with TreeOps with TreeUtils with TypeOrBoundsOps</pre></code>
## Constructors:
<pre><code class="language-scala" >Reflection(kernel: Kernel)</pre></code>

## Concrete Type Members:
### typing
<pre><code class="language-scala" >final object <a href="./Reflection/typing$.md">typing</a></pre></code>
## Concrete Value Members:
### error
<pre><code class="language-scala" >def error(msg: => String, source: SourceFile, start: Int, end: Int)(implicit ctx: Context): Unit</pre></code>

### error
<pre><code class="language-scala" >def error(msg: => String, pos: Position)(implicit ctx: Context): Unit</pre></code>

### rootContext
<pre><code class="language-scala" >implicit def rootContext: Context</pre></code>
Context of the macro expansion

### rootPosition
<pre><code class="language-scala" >def rootPosition: Position</pre></code>
Root position of this tasty context. For macros it corresponds to the expansion site.

### typeOf
<pre><code class="language-scala" >def typeOf[T](implicit evidence$1: <a href="../quoted/Type.md">Type</a>[T]): <a href="./Reflection/Type.md">Type</a></pre></code>

### warning
<pre><code class="language-scala" >def warning(msg: => String, source: SourceFile, start: Int, end: Int)(implicit ctx: Context): Unit</pre></code>

### warning
<pre><code class="language-scala" >def warning(msg: => String, pos: Position)(implicit ctx: Context): Unit</pre></code>

### kernel
<pre><code class="language-scala" >val kernel: Kernel</pre></code>

### util
<pre><code class="language-scala" >val util: TreeUtils{ val reflect: <a href="./Reflection.md">Reflection</a> }</pre></code>

