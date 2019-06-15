scala.tasty.reflect.utils
# trait TreeUtils

<pre><code class="language-scala" >trait TreeUtils</pre></code>
## Constructors:
<pre><code class="language-scala" >TreeUtils()</pre></code>

## Concrete Value Members:
### let
<pre><code class="language-scala" >def let(rhs: Term)(body: (Ident) => Term): Term</pre></code>
Bind the `rhs` to a `val` and use it in `body`

### lets
<pre><code class="language-scala" >def lets(terms: List[Term])(body: (List[Term]) => Term): Term</pre></code>
Bind the given `terms` to names and use them in the `body`

