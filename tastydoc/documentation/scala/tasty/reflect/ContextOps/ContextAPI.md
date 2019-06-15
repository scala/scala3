scala.tasty.reflect.ContextOps
# class ContextAPI

<pre><code class="language-scala" >class ContextAPI</pre></code>
## Constructors:
<pre><code class="language-scala" >ContextAPI(self: Context)</pre></code>

## Concrete Value Members:
### owner
<pre><code class="language-scala" >def owner: Symbol</pre></code>
Returns the owner of the context

### printColors
<pre><code class="language-scala" >def printColors: Boolean</pre></code>
Returns true if the generated strings are allowed to use colors

### source
<pre><code class="language-scala" >def source: Path</pre></code>
Returns the source file being compiled. The path is relative to the current working directory.

### withColors
<pre><code class="language-scala" >def withColors: Context</pre></code>
Returns a new context where printColors is true

### withoutColors
<pre><code class="language-scala" >def withoutColors: Context</pre></code>
Returns a new context where printColors is false

