scala.tasty.reflect
# trait ContextOps

<pre><code class="language-scala" >trait ContextOps extends Core</pre></code>
## Known subclasses:
<a href="./ContextOps/ContextAPI.md">ContextAPI</a>
## Constructors:
<pre><code class="language-scala" >ContextOps()</pre></code>

## Concrete Type Members:
### ContextAPI
<pre><code class="language-scala" >class <a href="./ContextOps/ContextAPI.md">ContextAPI</a></pre></code>
## Concrete Value Members:
### ContextAPI
<pre><code class="language-scala" >final implicit def ContextAPI(self: Context): ContextAPI</pre></code>

### rootContext
<pre><code class="language-scala" >implicit def rootContext: Context</pre></code>
Context of the macro expansion

