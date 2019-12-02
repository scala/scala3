scala.tasty.reflect.PatternOps.Pattern$
# object Alternatives

<pre><code class="language-scala" >final object Alternatives extends Serializable</pre></code>
## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply(patterns: List[Pattern])(implicit ctx: Context): Alternatives</pre></code>

### copy
<pre><code class="language-scala" >def copy(original: Alternatives)(patterns: List[Pattern])(implicit ctx: Context): Alternatives</pre></code>

### unapply
<pre><code class="language-scala" >def unapply(pattern: Pattern)(implicit ctx: Context): Option[List[Pattern]]</pre></code>

