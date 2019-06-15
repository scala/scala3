scala.tasty.reflect.SymbolOps
# class ValDefSymbolAPI

<pre><code class="language-scala" >class ValDefSymbolAPI</pre></code>
## Constructors:
<pre><code class="language-scala" >ValDefSymbolAPI(self: ValDefSymbol)</pre></code>

## Concrete Value Members:
### companionClass
<pre><code class="language-scala" >def companionClass(ctx: Context): Option[ClassDefSymbol]</pre></code>

### moduleClass
<pre><code class="language-scala" >def moduleClass(ctx: Context): Option[ClassDefSymbol]</pre></code>
The class symbol of the companion module class

### tree
<pre><code class="language-scala" >def tree(ctx: Context): ValDef</pre></code>
ValDef tree of this defintion

