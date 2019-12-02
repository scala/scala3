scala.tasty.reflect.TreeOps
# class ClassDefAPI

<pre><code class="language-scala" >class ClassDefAPI</pre></code>
## Constructors:
<pre><code class="language-scala" >ClassDefAPI(self: ClassDef)</pre></code>

## Concrete Value Members:
### body
<pre><code class="language-scala" >def body(implicit ctx: Context): List[Statement]</pre></code>

### constructor
<pre><code class="language-scala" >def constructor(implicit ctx: Context): DefDef</pre></code>

### derived
<pre><code class="language-scala" >def derived(implicit ctx: Context): List[TypeTree]</pre></code>

### parents
<pre><code class="language-scala" >def parents(implicit ctx: Context): List[Tree]</pre></code>

### self
<pre><code class="language-scala" >def self(implicit ctx: Context): Option[ValDef]</pre></code>

### symbol
<pre><code class="language-scala" >def symbol(implicit ctx: Context): ClassDefSymbol</pre></code>

