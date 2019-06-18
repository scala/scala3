scala.tasty.reflect.PatternOps.Pattern$
# object Unapply

<pre><code class="language-scala" >final object Unapply extends Serializable</pre></code>
## Concrete Value Members:
### copy
<pre><code class="language-scala" >def copy(original: Unapply)(fun: Term, implicits: List[Term], patterns: List[Pattern])(implicit ctx: Context): Unapply</pre></code>

### unapply
<pre><code class="language-scala" >def unapply(pattern: Pattern)(implicit ctx: Context): Option[(Term, List[Term], List[Pattern])]</pre></code>

